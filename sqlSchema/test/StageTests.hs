{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the fragment protocol: 'stagePending' when a module is parsed,
-- 'publishStaged' at the end of its Core pipeline.
module StageTests (run) where

import           Control.Exception   (IOException, SomeException, try)
import           Control.Monad       (forM_, unless, when)
import qualified Data.ByteString     as BS
import           Data.List           (isSuffixOf, sort)
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing, createDirectoryLink, createFileLink,
                                      doesDirectoryExist, doesFileExist, listDirectory,
                                      pathIsSymbolicLink, removeDirectory, removeFile, withCurrentDirectory)
import           System.FilePath     (takeDirectory, (</>))

import           SqlSchema.Plugin    (loadCliOptions, pendingPath, publishStaged, stagePending)
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
  testFragmentsDirResolved
  testStageThenPublish
  testStageLeavesPublished
  testStageNothingDeletes
  testPublishWithoutStageFails
  testStageOverwritesStalePending
  testStageIgnoresSquattedTmpName
  testStageFailureLeavesNoTemp
  testPublishOntoDirectoryFails
  testLinksReplacedNotFollowed

-- ---------------------------------------------------------------------------
-- Test cases
-- ---------------------------------------------------------------------------

-- | A fragmentsDir that resolves to the working directory or one above it, through an absolute
-- path or a link, is refused before anything is written; the corpus's own spelling is not.
testFragmentsDirResolved :: IO ()
testFragmentsDirResolved = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  createDirectoryLink "." (tmp </> "here")
  createDirectoryLink ".." (tmp </> "up")
  createDirectoryIfMissing True (tmp </> "own")
  forM_ [tmp, takeDirectory tmp, "/", "here", "here/", "up", tmp </> "here"] $ \dir ->
    loadCliOptions ["{\"fragmentsDir\":" <> show dir <> "}"] >>= \case
      Left msg | show dir `isInfixOf` msg && "working directory" `isInfixOf` msg -> pure ()
      other -> failWith ("fragmentsDir " <> show dir <> ": expected it refused, got " <> show other)
  forM_ ["./dist/build/sql-schema-fragments", "own", tmp </> "own", "here/own"] $ \dir ->
    loadCliOptions ["{\"fragmentsDir\":" <> show dir <> "}"] >>= \case
      Right cli | fragmentsDir cli == dir -> pure ()
      other -> failWith ("fragmentsDir " <> show dir <> ": expected it accepted, got " <> show other)
  assertLeft "the pure checks first" =<< loadCliOptions ["{\"fragmentsDir\":\"a/..\"}"]
  assertEq "nothing written" ["here", "own", "up"] . sort =<< listDirectory tmp
  -- A working directory that can't be resolved (here removed) refuses every value.
  createDirectoryIfMissing True (tmp </> "gone")
  gone <- withCurrentDirectory (tmp </> "gone") $ do
    removeDirectory (tmp </> "gone")
    loadCliOptions ["{\"fragmentsDir\":\"own\"}"]
  case gone of
    Left msg | "cannot resolve fragmentsDir" `isInfixOf` msg -> pure ()
    other -> failWith ("an unresolvable working directory: expected a refusal, got " <> show other)

testStageThenPublish :: IO ()
testStageThenPublish = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
      f    = sampleFragment "Foo" "Foo.FooT" "foo"
  -- The merge reads only *.yaml, so the staged name must not end in it.
  assertEq "pendingPath" (frag <> ".pending") (pendingPath frag)
  stagePending frag (Just f)
  expectFile "staged" (pendingPath frag)
  expectNoTemp "after staging" (tmp </> "fragments")
  assertAbsent frag
  publishStaged frag
  assertEq "published" f =<< readFragment frag
  assertAbsent (pendingPath frag)
  expectNoTemp "after publishing" (tmp </> "fragments")

testStageLeavesPublished :: IO ()
testStageLeavesPublished = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
      new  = sampleFragment "Foo" "Foo.FooT" "new"
  stagePending frag (Just (sampleFragment "Foo" "Foo.FooT" "old"))
  publishStaged frag
  old <- BS.readFile frag
  stagePending frag (Just new)
  assertEq "published fragment after staging a new one" old =<< BS.readFile frag
  stagePending frag Nothing
  assertEq "published fragment after staging a deletion" old =<< BS.readFile frag
  stagePending frag (Just new)
  publishStaged frag
  assertEq "published after publishStaged" new =<< readFragment frag

testStageNothingDeletes :: IO ()
testStageNothingDeletes = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
  stagePending frag (Just (sampleFragment "Foo" "Foo.FooT" "foo"))
  publishStaged frag
  stagePending frag Nothing
  assertEq "a deletion stages an empty file" BS.empty =<< BS.readFile (pendingPath frag)
  publishStaged frag
  assertAbsent frag
  assertAbsent (pendingPath frag)
  -- Nothing published: the deletion still succeeds and clears what it staged.
  stagePending frag Nothing
  publishStaged frag
  assertAbsent (pendingPath frag)

testPublishWithoutStageFails :: IO ()
testPublishWithoutStageFails = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
  stagePending frag (Just (sampleFragment "Foo" "Foo.FooT" "foo"))
  publishStaged frag
  before <- BS.readFile frag
  expectStagedMissing "a published fragment, nothing staged" frag
  assertEq "published fragment untouched" before =<< BS.readFile frag
  let other = tmp </> "fragments" </> "Bar.yaml"
  expectStagedMissing "nothing published or staged" other
  assertAbsent other

testStageOverwritesStalePending :: IO ()
testStageOverwritesStalePending = withTmpProject $ \tmp -> do
  let frag  = tmp </> "fragments" </> "Foo.yaml"
      stale = sampleFragment "Foo" "Foo.FooT" "stale"
      fresh = sampleFragment "Foo" "Foo.FooT" "fresh"
  stagePending frag (Just stale)
  stagePending frag (Just fresh)
  publishStaged frag
  assertEq "a fragment over a staged fragment" fresh =<< readFragment frag
  stagePending frag (Just stale)
  stagePending frag Nothing
  publishStaged frag
  assertAbsent frag
  stagePending frag Nothing
  stagePending frag (Just fresh)
  publishStaged frag
  assertEq "a fragment over a staged deletion" fresh =<< readFragment frag

-- | Same-named modules compiled at once stage into one directory; a fixed temp
-- name would make them race.  A directory on the old fixed name stands in for
-- the other writer.
testStageIgnoresSquattedTmpName :: IO ()
testStageIgnoresSquattedTmpName = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
      f    = sampleFragment "Foo" "Foo.FooT" "foo"
  createDirectoryIfMissing True (pendingPath frag <> ".tmp")
  stagePending frag (Just f)
  publishStaged frag
  assertEq "published" f =<< readFragment frag
  assertEq "no temp file left" ["Foo.yaml", "Foo.yaml.pending.tmp"] . sort
    =<< listDirectory (tmp </> "fragments")

-- | A failed stage removes its own temp file and no other writer's.
testStageFailureLeavesNoTemp :: IO ()
testStageFailureLeavesNoTemp = withTmpProject $ \tmp -> do
  let dir   = tmp </> "fragments"
      frag  = dir </> "Foo.yaml"
      other = dir </> "Foo.yaml.pending4821-7.tmp"
      stageFails = do
        r <- try (stagePending frag (Just (sampleFragment "Foo" "Foo.FooT" "foo")))
        case r of
          Left (_ :: IOException) -> pure ()
          Right ()                -> failWith "staging onto a directory succeeded"
  createDirectoryIfMissing True (pendingPath frag)
  stageFails
  expectNoTemp "after a failed stage" dir
  BS.writeFile other BS.empty
  stageFails
  assertEq "only the squatter and the other temp file" ["Foo.yaml.pending", "Foo.yaml.pending4821-7.tmp"] . sort
    =<< listDirectory dir

-- | Only a missing published file is ignored: any other failure must fail the compile.
testPublishOntoDirectoryFails :: IO ()
testPublishOntoDirectoryFails = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Dir.yaml"
  createDirectoryIfMissing True frag
  stagePending frag Nothing
  expectPublishFails "a deletion onto a directory" frag
  -- A deletion consumes the staged file before it removes the fragment.
  assertAbsent (pendingPath frag)
  stagePending frag (Just (sampleFragment "Dir" "Dir.DirT" "dir"))
  expectPublishFails "a fragment onto a directory" frag
  expectFile "the staged fragment is kept" (pendingPath frag)
  isDir <- doesDirectoryExist frag
  unless isDir (failWith "the directory at the published path was removed")

testLinksReplacedNotFollowed :: IO ()
testLinksReplacedNotFollowed = withTmpProject $ \tmp -> do
  let dir     = tmp </> "fragments"
      frag    = dir </> "Foo.yaml"
      outside = tmp </> "outside.txt"
      f       = sampleFragment "Foo" "Foo.FooT" "foo"
      outsideKept label = assertEq label "outside" =<< BS.readFile outside
      published label = do
        assertEq label f =<< readFragment frag
        isLink <- pathIsSymbolicLink frag
        when isLink (failWith (label <> ": the published fragment is a link"))
  createDirectoryIfMissing True dir
  BS.writeFile outside "outside"
  createFileLink outside (pendingPath frag)
  stagePending frag (Just f)
  outsideKept "a link at the staged path, after staging"
  publishStaged frag
  outsideKept "a link at the staged path, after publishing"
  published "a link at the staged path"
  removeFile frag
  createFileLink outside frag
  stagePending frag (Just f)
  publishStaged frag
  outsideKept "a link at the published path, after publishing"
  published "a link at the published path"
  removeFile frag
  createFileLink outside frag
  stagePending frag Nothing
  publishStaged frag
  outsideKept "a link at the published path, after a deletion"
  assertAbsent frag

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

readFragment :: FilePath -> IO Fragment
readFragment p = do
  bs <- BS.readFile p
  either (\e -> failWith ("decode " <> p <> ": " <> show e)) pure (YAML.decodeEither' bs)

expectPublishFails :: String -> FilePath -> IO ()
expectPublishFails label frag = do
  r <- try (publishStaged frag)
  case r of
    Left (_ :: IOException) -> pure ()
    Right ()                -> failWith (label <> ": publishStaged succeeded")

expectStagedMissing :: String -> FilePath -> IO ()
expectStagedMissing label frag = do
  r <- try (publishStaged frag)
  case r of
    Left (e :: SomeException)
      | ("sql-schema: STAGED_FRAGMENT_MISSING: Nothing is staged at '" <> pendingPath frag <> "'") `isInfixOf` show e
                -> pure ()
      | otherwise -> failWith (label <> ": expected STAGED_FRAGMENT_MISSING, got " <> show e)
    Right () -> failWith (label <> ": publishStaged succeeded")

expectNoTemp :: String -> FilePath -> IO ()
expectNoTemp label dir = do
  temps <- filter (".tmp" `isSuffixOf`) <$> listDirectory dir
  unless (null temps) (failWith (label <> ": temp files left: " <> show temps))

expectFile :: String -> FilePath -> IO ()
expectFile label p = do
  ok <- doesFileExist p
  unless ok (failWith (label <> ": no file at " <> p))
