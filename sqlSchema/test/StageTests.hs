{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the fragment protocol: 'stagePending' when a module is parsed,
-- 'publishStaged' at the end of its Core pipeline.
module StageTests (run) where

import           Control.Exception   (IOException, SomeException, try)
import           Control.Monad       (unless, when)
import qualified Data.ByteString     as BS
import           Data.List           (isSuffixOf, sort)
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing, createFileLink, doesDirectoryExist,
                                      doesFileExist, doesPathExist, listDirectory,
                                      pathIsSymbolicLink, removeFile)
import           System.FilePath     ((</>))

import           SqlSchema.Plugin    (pendingPath, publishStaged, stagePending)
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
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

testStageThenPublish :: IO ()
testStageThenPublish = withTmpProject $ \tmp -> do
  let frag = tmp </> "fragments" </> "Foo.yaml"
      f    = sampleFragment "Foo" "Foo.FooT" "foo"
  -- The merge reads only *.yaml, so the staged name must not end in it.
  assertEq "pendingPath" (frag <> ".pending") (pendingPath frag)
  stagePending frag (Just f)
  expectFile "staged" (pendingPath frag)
  expectNoTemp "after staging" (tmp </> "fragments")
  expectNoPath "published before publishStaged" frag
  publishStaged frag
  assertEq "published" f =<< readFragment frag
  expectNoPath "pending after publishing" (pendingPath frag)
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
  expectNoPath "deleted" frag
  expectNoPath "pending after the deletion" (pendingPath frag)
  -- Nothing published: the deletion still succeeds and clears what it staged.
  stagePending frag Nothing
  publishStaged frag
  expectNoPath "pending after a deletion of nothing" (pendingPath frag)

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
  expectNoPath "nothing created" other

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
  expectNoPath "a deletion over a staged fragment" frag
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
  expectNoPath "the staged deletion" (pendingPath frag)
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
  expectNoPath "the link, after a deletion" frag

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

expectNoPath :: String -> FilePath -> IO ()
expectNoPath label p = do
  found <- doesPathExist p
  when found (failWith (label <> ": " <> p <> " exists"))
