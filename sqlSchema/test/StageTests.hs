{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the fragment protocol: 'stagePending' when a module is parsed,
-- 'stagedTables' after type checking, 'publishStaged' at the end of its Core pipeline.
module StageTests (run) where

import           Control.Exception   (SomeException, try)
import           Control.Monad       (forM_, unless, when)
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List           (isSuffixOf, sort)
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing, createDirectoryLink, createFileLink,
                                      doesDirectoryExist, doesFileExist, listDirectory,
                                      pathIsSymbolicLink, removeDirectory, removeFile, withCurrentDirectory)
import           System.FilePath     (takeDirectory, (</>))

import           SqlSchema.Plugin    (loadCliOptions, pendingPath, publishStaged, stagePending, stagedTableNames,
                                      stagedTables)
import           SqlSchema.Plugin    (stagePendingFrom)
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
  testFileFailuresAreCodedErrors
  testStagedTableNames
  testStageRecordsSource

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
  expectStagedMissing "a published fragment, nothing staged" frag (publishStaged frag)
  assertEq "published fragment untouched" before =<< BS.readFile frag
  let other = tmp </> "fragments" </> "Bar.yaml"
  expectStagedMissing "nothing published or staged" other (publishStaged other)
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
      stageFails = expectCodedFailure "staging onto a directory" (pendingPath frag)
        (stagePending frag (Just (sampleFragment "Foo" "Foo.FooT" "foo")))
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

-- | A fragmentsDir the system can't write to fails staging with a coded error that names the
-- path: a file where the directory belongs, and a chain of links past the system's limit, which
-- 'loadCliOptions' can't tell from a directory.
testFileFailuresAreCodedErrors :: IO ()
testFileFailuresAreCodedErrors = withTmpProject $ \tmp -> do
  let notDir = tmp </> "file"
  BS.writeFile notDir ""
  expectCodedFailure "a file as the directory" (pendingPath (notDir </> "Foo.yaml"))
    (stagePending (notDir </> "Foo.yaml") (Just (sampleFragment "Foo" "Foo.FooT" "foo")))
  createDirectoryIfMissing True (tmp </> "own")
  linkChain False 64 (tmp </> "chain") (tmp </> "own")
  let long = tmp </> "chain" </> "c1" </> "Foo.yaml"
  refused <- refusesToFollow (tmp </> "chain" </> "c1")
  whenRefused refused "testFileFailuresAreCodedErrors" "a chain of 64 links is a coded error" $ do
    expectCodedFailure "a chain of links past the limit, staging" (pendingPath long)
      (stagePending long (Just (sampleFragment "Foo" "Foo.FooT" "foo")))
    assertEq "nothing written below the chain" [] =<< listDirectory (tmp </> "own")

-- | The type-check step reads the staged tables back: nothing staged is STAGED_FRAGMENT_MISSING, as
-- at publish; a staged file it cannot read is FRAGMENT_IO_ERROR, as any file operation of the plugin;
-- and one that does not decode is STAGED_FRAGMENT_UNREADABLE, with the decode error.
testStagedTableNames :: IO ()
testStagedTableNames = withTmpProject $ \tmp -> do
  let frag     = tmp </> "fragments" </> "Foo.yaml"
      pending  = pendingPath frag
      readBack = () <$ stagedTables frag
      f        = (sampleFragment "Foo" "Foo.FooT" "foo")
                   { fragmentTables = [sampleTable "Foo.FooT" "Foo" "foo", sampleTable "Foo.BarT" "Foo" "bar"] }
      undecodable label bytes = do
        BS.writeFile pending bytes
        reason <- either (pure . takeWhile (/= '\n') . YAML.prettyPrintParseException)
                         (\fr -> failWith (label <> ": decodes as " <> show (fr :: Fragment)))
                         (YAML.decodeEither' bytes)
        try readBack >>= \case
          Left (e :: SomeException)
            | ("sql-schema: STAGED_FRAGMENT_UNREADABLE: The file staged at '" <> pending
                 <> "' does not decode as this module's fragment: " <> reason) `isInfixOf` show e
              -> pure ()
          other -> failWith (label <> ": expected STAGED_FRAGMENT_UNREADABLE naming " <> pending <> " and " <> show reason
                             <> ", got " <> show other)
  stagePending frag (Just f)
  assertEq "staged YAML" ["FooT", "BarT"] =<< stagedTableNames frag
  assertEq "staged tables" (fragmentTables f) =<< stagedTables frag
  stagePending frag Nothing
  assertEq "a staged deletion" [] =<< stagedTableNames frag
  removeFile pending
  expectStagedMissing "nothing staged" frag readBack
  createDirectoryIfMissing True pending
  expectCodedFailure "a directory at the staged path" pending readBack
  try readBack >>= \case
    Left (e :: SomeException) | ("Could not read the fragment at '" <> pending <> "': ") `isInfixOf` show e -> pure ()
    other -> failWith ("a directory at the staged path: expected the action 'read', got " <> show other)
  removeDirectory pending
  undecodable "garbage" "fragmentModule: [unclosed"
  undecodable "YAML that is not a fragment" "just a string"

-- | The plugin records the module's source file in the fragment it stages, so the publish moves
-- both at once; a stage without one writes the fragment as before, and a deletion stays empty.
testStageRecordsSource :: IO ()
testStageRecordsSource = withTmpProject $ \tmp -> do
  let frag  = tmp </> "fragments" </> "Foo.yaml"
      plain = tmp </> "fragments" </> "Bar.yaml"
      f     = sampleFragment "Foo" "Foo.FooT" "foo"
  stagePendingFrom (Just "src/Foo.hs") frag (Just f)
  assertEq "the staged tables" (fragmentTables f) =<< stagedTables frag
  publishStaged frag
  published <- BS.readFile frag
  either (failWith . show) (assertEq "the published fragment and its source" (f, Just "src/Foo.hs"))
    (decodeFragmentFile published)
  assertEq "the source as a key of the fragment" (encodeYaml f) (BS.concat (BC.lines published `without` "fragmentSourceFile: src/Foo.hs"))
  stagePending plain (Just f)
  assertEq "a stage without a source" (encodeYaml f) =<< BS.readFile (pendingPath plain)
  stagePendingFrom (Just "src/Foo.hs") frag Nothing
  assertEq "a deletion" BS.empty =<< BS.readFile (pendingPath frag)
  where
    without ls l = [x <> "\n" | x <- ls, x /= BC.pack l]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

readFragment :: FilePath -> IO Fragment
readFragment p = do
  bs <- BS.readFile p
  either (\e -> failWith ("decode " <> p <> ": " <> show e)) pure (YAML.decodeEither' bs)

expectPublishFails :: String -> FilePath -> IO ()
expectPublishFails label frag = expectCodedFailure label frag (publishStaged frag)

-- | A file operation of the protocol that fails is a coded compile error naming the path, not an
-- uncoded I/O error.
expectCodedFailure :: String -> FilePath -> IO () -> IO ()
expectCodedFailure label path act = try act >>= \case
  Left (e :: SomeException)
    | ("sql-schema: FRAGMENT_IO_ERROR: Could not " `isInfixOf` show e) && ("'" <> path <> "': ") `isInfixOf` show e
    , not (("'" <> path <> "': \n") `isInfixOf` show e)
              -> pure ()
    | otherwise -> failWith (label <> ": expected FRAGMENT_IO_ERROR naming " <> path <> ", got " <> show e)
  Right () -> failWith (label <> ": succeeded")

expectStagedMissing :: String -> FilePath -> IO () -> IO ()
expectStagedMissing label frag act = do
  r <- try act
  case r of
    Left (e :: SomeException)
      | ("sql-schema: STAGED_FRAGMENT_MISSING: Nothing is staged at '" <> pendingPath frag <> "'") `isInfixOf` show e
                -> pure ()
      | otherwise -> failWith (label <> ": expected STAGED_FRAGMENT_MISSING, got " <> show e)
    Right () -> failWith (label <> ": succeeded")

expectNoTemp :: String -> FilePath -> IO ()
expectNoTemp label dir = do
  temps <- filter (".tmp" `isSuffixOf`) <$> listDirectory dir
  unless (null temps) (failWith (label <> ": temp files left: " <> show temps))

expectFile :: String -> FilePath -> IO ()
expectFile label p = do
  ok <- doesFileExist p
  unless ok (failWith (label <> ": no file at " <> p))
