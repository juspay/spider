{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the merge library, 'SqlSchema.Merge'.
module MergeTests (run) where

import           Control.Exception   (IOException, finally, try)
import           Control.Monad       (forM_, unless)
import qualified Data.ByteString     as BS
import           Data.Foldable       (toList)
import           Data.List           (intercalate, sort)
import qualified Data.Yaml           as YAML
import           System.Directory    (canonicalizePath, createDirectoryIfMissing,
                                      createDirectoryLink, createFileLink, emptyPermissions,
                                      getPermissions, removeFile, searchable, setOwnerReadable,
                                      setOwnerSearchable, setOwnerWritable, setPermissions)
import           System.FilePath     (splitDirectories, (</>))

import           SqlSchema.Merge
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
  testHappyPath
  testStalePruneByModule
  testNoSourceDirsNoPrune
  testSourceDirTrailingSlash
  testAllFragmentsPrunedFatal
  testMissingSourceDirNotFatal
  testMissingSourceDirWithPruneFatal
  testNotADirectorySourceDirWithPruneFatal
  testNotADirectorySourceDirNothingPruned
  testSourceDirProblemsNameEveryEntry
  testUnparseableFragmentCountsAsPruned
  testAllPrunedBecauseSourceDirMissing
  testPrunedModuleNameEscaped
  testFragmentsPathIsFileFatal
  testMissingFragmentsDirIsEmpty
  testPathKind
  testUnsearchablePathThrows
  testWalkLinkBelowFileThrows
  testWalkIgnoresDanglingLink
  testWalkSkipsLinksBackUp
  testWalkAliasKeepsBothNames
  testWalkAliasIsThePathOfItsModules
  testWalkEntryThatIsALink
  testWalkFollowsLinkOutsideTree
  testWalkEveryLinkToOneDirectoryIsWalked
  testWalkLinksInsideLinkTargetsAreWalked
  testWalkRealDirectoryBelowLinkTargetIsWalkedAgain
  testWalkWarnsInNameOrder
  testWalkPrintsEveryWarning
  testWalkFlushesWarningsBeforeRethrow
  testWalkChainReachesItsDirectory
  testWalkChainBackToAnAncestorIsSkipped
  testWalkChainCycleHasNoDirectory
  testWalkChainBelowAFileThrows
  testWalkChainPastTheHopCapThrows
  testWalkChainEndingAtAFileOrNothing
  testWalkEntryCapTrips
  testWalkChainHopsCountTowardsTheCap
  testWalkLinkCostsItsComponents
  testWalkFileLinkCostsItsComponents
  testWalkCapIsAnOption
  testWalkDepthLimit
  testWalkDeepLinkChainFailsFast
  testWalkChainsCountLevels
  testSkippedLinkWithPruneFatal
  testLiveModulesFromDirsReportsProblems
  testLoadFragmentsPathKinds
  testUnparseableFragmentFatal
  testUnparseableLiveFragmentFatal
  testUnparseableStaleFragmentPruned
  testIncludeOnlyWithSourceDirs
  testAllPrunedWithIncludeFatal
  testOtherSourceExtensionsLive
  testOwnTablesCounted
  testPublishProblem
  testRunMergeGatesBeforeWrite
  testRefusalLinesStateCounts
  testEmptyIncludeFatal
  testEmptyIncludeAllowed
  testUnmatchedOverrideFatal
  testUnmatchedOverrideAllowed
  testColumnlessTableFatal
  testColumnlessTableAllowed
  testColumnlessTableCountedAfterDedup
  testOptOutsAreIndependent
  testOptOutsKeepOtherChecks
  testChecksReportedTogether
  testLoadIncludedMergedListsEmptyFiles
  testOverridesOnlyIncludeIsEmpty
  testIdenticalOverridesCollapse
  testOverridesWithDifferentKeysDoNotConflict
  testOverrideKeyWithTwoSqlNamesFatal
  testOverrideKeyWithTwoSqlNamesAcrossIncludeFatal
  testHostileFieldsEscaped
  testHostileFragmentFileNameEscaped
  testCliSuppliedPathsShown
  testOverrideMatchEmitted
  testOverrideMismatchFatal
  testOverrideAmbiguousAgreeing
  testOverrideAmbiguousConflictFatal
  testDedupIdenticalSilent
  testDuplicateMismatchFatal
  testTableNameWarning
  testIncludeMerged
  testIncludeMergedMissingFile
  testIncludeMergedMalformedFile
  testGeneratorStamped
  testIncludeWithoutGenerator
  testIncludedStampIgnored
  testMergeIgnoresStagedFiles

-- ---------------------------------------------------------------------------
-- Test cases
-- ---------------------------------------------------------------------------

testHappyPath :: IO ()
testHappyPath = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      src     = tmp </> "src"
  createDirectoryIfMissing True (src </> "")
  touchFile (src </> "Foo.hs")
  touchFile (src </> "Bar.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Foo.yaml")
    (sampleFragment "Foo" "Foo.FooT" "foo")
  writeFragYaml (fragDir </> "Bar.yaml")
    (sampleFragment "Bar" "Bar.BarT" "bar")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "fragmentsRead" 2 (mrFragmentsRead report)
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)
  merged <- decodeFile outFile
  assertEq "merged.tables.size" 2 (length (mergedTables merged))
  let names = map modelTableName (mergedTables merged)
  unless (names == ["bar", "foo"]) $
    failWith ("expected sorted ['bar','foo'] got " <> show names)

testStalePruneByModule :: IO ()
testStalePruneByModule = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  -- A live module + a fragment for a module whose .hs file doesn't exist.
  touchFile (src </> "Alive.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml")
    (sampleFragment "Alive" "Alive.AliveT" "alive")
  writeFragYaml (fragDir </> "Gone.yaml")
    (sampleFragment "Gone" "Gone.GoneT" "gone")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "fragmentsRead" 2 (mrFragmentsRead report)
  assertEq "stalePruned" 1 (mrStalePruned report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  merged <- decodeFile outFile
  let names = map modelTableName (mergedTables merged)
  unless (names == ["alive"]) $
    failWith ("expected only ['alive'], got " <> show names)

testNoSourceDirsNoPrune :: IO ()
testNoSourceDirsNoPrune = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
  createDirectoryIfMissing True fragDir
  -- No source-dirs given: even a fragment with no backing .hs is kept.
  writeFragYaml (fragDir </> "Orphan.yaml")
    (sampleFragment "Orphan" "Orphan.OrphanT" "orphan")
  report <- expectOk =<< runMerge (baseOpts tmp)
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

-- | "src/" names the same modules as "src"; the trailing slash used to shift
-- every module name, so every fragment was pruned.
testSourceDirTrailingSlash :: IO ()
testSourceDirTrailingSlash = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Alive.hs")
  touchFile (src </> "Lib" </> "Nested.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  writeFragYaml (fragDir </> "Lib.Nested.yaml")
    (sampleFragment "Lib.Nested" "Lib.Nested.NestedT" "nested")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src <> "/"] }
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

-- | Pruning every fragment means --source-dirs points at the wrong place.
testAllFragmentsPrunedFatal :: IO ()
testAllFragmentsPrunedFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      other   = tmp </> "other"
  touchFile (other </> "Unrelated.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  expectMergeError "pruned as stale" =<< runMerge (baseOpts tmp) { moSourceDirs = [other] }

-- | A listed source dir may not exist (a .cabal can name one); while nothing is
-- pruned that only warns.
testMissingSourceDirNotFatal :: IO ()
testMissingSourceDirNotFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Alive.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src, tmp </> "absent"] }
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  assertEq "stalePruned" 0 (mrStalePruned report)

fragmentsAB :: FilePath -> IO ()
fragmentsAB tmp = do
  touchFile (tmp </> "src" </> "A.hs")
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a")
  writeFragYaml (tmp </> "fragments" </> "B.yaml") (sampleFragment "B" "B.BT" "b")

refusalText :: Either [String] MergeReport -> IO String
refusalText = \case
  Left errs -> pure (unlines errs)
  Right _   -> failWith "expected the merge to be refused, but it succeeded"

-- | A mistyped entry makes the modules that live there look deleted, so their
-- fragments would be pruned and their tables dropped.
testMissingSourceDirWithPruneFatal :: IO ()
testMissingSourceDirWithPruneFatal = withTmpProject $ \tmp -> do
  let gne = tmp </> "gne"
  fragmentsAB tmp
  let opts = (baseOpts tmp) { moSourceDirs = [tmp </> "src", gne] }
  text <- refusalText =<< runMerge opts
  assertMentions "the entry" ("source dir " <> show gne <> " does not exist") text
  assertMentions "the pruned module" "pruned fragment of \"B\"" text
  assertLacks "a live module" "pruned fragment of \"A\"" text
  assertMentions "the consequence" "drops its tables" text
  assertMentions "the fix" "delete the stale fragments" text
  assertAbsent (moOutFile opts)
  BS.writeFile (moOutFile opts) "old contract\n"
  _ <- refusalText =<< runMerge opts
  kept <- BS.readFile (moOutFile opts)
  assertEq "existing --out" "old contract\n" kept

testNotADirectorySourceDirWithPruneFatal :: IO ()
testNotADirectorySourceDirWithPruneFatal = withTmpProject $ \tmp -> do
  let file = tmp </> "gne"
  fragmentsAB tmp
  touchFile file
  text <- refusalText =<< runMerge (baseOpts tmp) { moSourceDirs = [tmp </> "src", file] }
  assertMentions "the entry" ("source dir " <> show file <> " is not a directory") text
  assertMentions "the pruned module" "pruned fragment of \"B\"" text
  assertLacks "a live module" "pruned fragment of \"A\"" text
  assertAbsent (tmp </> "merged.yaml")

testNotADirectorySourceDirNothingPruned :: IO ()
testNotADirectorySourceDirNothingPruned = withTmpProject $ \tmp -> do
  fragmentsAB tmp
  touchFile (tmp </> "src" </> "B.hs")
  touchFile (tmp </> "notadir.hs")
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moSourceDirs = [tmp </> "src", tmp </> "notadir.hs"] }
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

testSourceDirProblemsNameEveryEntry :: IO ()
testSourceDirProblemsNameEveryEntry = withTmpProject $ \tmp -> do
  let gone = tmp </> "gone"
      file = tmp </> "notadir.hs"
  fragmentsAB tmp
  writeFragYaml (tmp </> "fragments" </> "C.yaml") (sampleFragment "C" "C.CT" "c")
  touchFile file
  text <- refusalText =<< runMerge (baseOpts tmp) { moSourceDirs = [gone, tmp </> "src", file] }
  assertMentions "the missing entry" ("source dir " <> show gone <> " does not exist") text
  assertMentions "the file entry" ("source dir " <> show file <> " is not a directory") text
  forM_ ["B", "C"] $ \m -> assertMentions ("pruned " <> m) ("pruned fragment of " <> show m) text
  assertLacks "a live module" "pruned fragment of \"A\"" text

-- | A fragment that doesn't parse, of a module not found, is pruned too.
testUnparseableFragmentCountsAsPruned :: IO ()
testUnparseableFragmentCountsAsPruned = withTmpProject $ \tmp -> do
  let gne = tmp </> "gne"
  touchFile (tmp </> "src" </> "A.hs")
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a")
  BS.writeFile (tmp </> "fragments" </> "Gone.yaml") ":::not::valid:::yaml:::"
  text <- refusalText =<< runMerge (baseOpts tmp) { moSourceDirs = [tmp </> "src", gne] }
  assertMentions "the entry" ("source dir " <> show gne <> " does not exist") text
  assertMentions "the pruned module" "pruned fragment of \"Gone\"" text

-- | The usual typo: the only entry is wrong, so every fragment is pruned.  The
-- error that says so doesn't name the entry; the other does.
testAllPrunedBecauseSourceDirMissing :: IO ()
testAllPrunedBecauseSourceDirMissing = withTmpProject $ \tmp -> do
  let gne = tmp </> "gne"
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a")
  text <- refusalText =<< runMerge (baseOpts tmp) { moSourceDirs = [gne] }
  assertMentions "all pruned" "all 1 fragments were pruned" text
  assertMentions "the entry" ("source dir " <> show gne <> " does not exist") text
  assertMentions "the pruned module" "pruned fragment of \"A\"" text

-- | A module name holding a newline or an escape character is printed escaped, so it can't forge a line.
testPrunedModuleNameEscaped :: IO ()
testPrunedModuleNameEscaped = withTmpProject $ \tmp -> do
  let evil = "Gone\nsql-schema-merge: 9 fragments read\ESC[31m"
  fragmentsAB tmp
  writeFragYaml (tmp </> "fragments" </> "Evil.yaml") (sampleFragment evil "Gone.GoneT" "gone")
  text <- refusalText =<< runMerge (baseOpts tmp) { moSourceDirs = [tmp </> "src", tmp </> "gne"] }
  assertMentions "the name, escaped" ("pruned fragment of " <> show evil) text
  assertLacks "a forged line" "\nsql-schema-merge: 9 fragments read" text
  assertLacks "an escape character" "\ESC" text

testFragmentsPathIsFileFatal :: IO ()
testFragmentsPathIsFileFatal = withTmpProject $ \tmp -> do
  let file = tmp </> "frags"
      inc  = tmp </> "upstream.yaml"
  BS.writeFile file "x\n"
  writeIncluded inc
  forM_ [[], [inc]] $ \includes -> do
    text <- refusalText =<< runMerge (baseOpts tmp)
      { moFragmentsDir = file, moAllowEmpty = True, moNoOwnTables = True, moIncludeMerged = includes }
    assertMentions "the path" ("--fragments is not a directory: " <> show file) text
    assertMentions "the fix" "fragmentsDir" text
    assertAbsent (tmp </> "merged.yaml")

-- | A @--fragments@ directory that doesn't exist still reads as no fragments,
-- unlike a file.
testMissingFragmentsDirIsEmpty :: IO ()
testMissingFragmentsDirIsEmpty = withTmpProject $ \tmp -> do
  let inc = tmp </> "upstream.yaml"
  writeIncluded inc
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moFragmentsDir = tmp </> "absent", moIncludeMerged = [inc], moNoOwnTables = True }
  assertEq "fragmentsRead" 0 (mrFragmentsRead report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

assertThrowsIO :: String -> IO a -> IO ()
assertThrowsIO label act = try act >>= \case
  Left (_ :: IOException) -> pure ()
  Right _                 -> failWith (label <> ": expected an IOException")

testPathKind :: IO ()
testPathKind = withTmpProject $ \tmp -> do
  let dir  = tmp </> "dir"
      file = tmp </> "file"
  createDirectoryIfMissing True dir
  BS.writeFile file ""
  createDirectoryLink dir (tmp </> "dirlink")
  createFileLink (tmp </> "nowhere") (tmp </> "dangling")
  createFileLink "loopB" (tmp </> "loopA")
  createFileLink "loopA" (tmp </> "loopB")
  kinds <- mapM (pathKind . (tmp </>)) ["dir", "file", "dirlink", "dangling", "absent"]
  assertEq "kinds" [PathDirectory, PathNotDirectory, PathDirectory, PathMissing, PathMissing] kinds
  assertThrowsIO "below a file" (pathKind (file </> "below"))
  assertThrowsIO "a file with a trailing slash" (pathKind (file <> "/"))
  assertThrowsIO "a link loop" (pathKind (tmp </> "loopA"))

-- | The same for an error that only a permission produces: also for a link inside a source
-- directory that leads into a directory that can't be searched.
testUnsearchablePathThrows :: IO ()
testUnsearchablePathThrows = withTmpProject $ \tmp -> do
  let locked = tmp </> "locked"
      inner  = locked </> "inner"
      src    = tmp </> "src"
      open   = setOwnerSearchable True (setOwnerReadable True (setOwnerWritable True emptyPermissions))
  createDirectoryIfMissing True inner
  createDirectoryIfMissing True (tmp </> "fragments")
  touchFile (src </> "A.hs")
  createDirectoryLink inner (src </> "link")
  setPermissions locked emptyPermissions
  (do canSearch <- searchable <$> getPermissions locked
      whenWithheld (not canSearch) "testUnsearchablePathThrows"
        "an error other than \"does not exist\" is thrown, not read as a missing path or a file" $ do
        assertThrowsIO "pathKind" (pathKind inner)
        assertThrowsIO "loadFragments" (loadFragments inner)
        assertThrowsIO "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [inner])
        assertThrowsIO "liveModulesFromDirs, a link into it" (liveModulesFromDirs walkEntryCap [src])
        assertThrowsIO "runMerge, --fragments" (runMerge (baseOpts tmp) { moFragmentsDir = inner })
        assertThrowsIO "runMerge, --source-dirs" (runMerge (baseOpts tmp) { moSourceDirs = [inner] }))
    `finally` setPermissions locked open

-- | An entry that can't be inspected throws.  A link below a file needs no permission to fail, so
-- this holds as root too.
testWalkLinkBelowFileThrows :: IO ()
testWalkLinkBelowFileThrows = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (tmp </> "file")
  createFileLink (tmp </> "file" </> "x") (src </> "bad")
  r <- try (liveModulesFromDirs walkEntryCap [src])
  case r of
    Left (e :: IOException) -> assertMentions "the error names the link" (src </> "bad") (show e)
    Right _                 -> failWith "liveModulesFromDirs, a link below a file: expected an IOException"

-- | A dangling link is no error; like a file, it counts as a module if its name says so.
testWalkIgnoresDanglingLink :: IO ()
testWalkIgnoresDanglingLink = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  createFileLink (tmp </> "nowhere") (src </> "dangling")
  createFileLink (tmp </> "nowhere") (src </> "Gone.hs")
  (live, problems) <- liveModulesFromDirs walkEntryCap [src]
  assertEq "live modules" ["A", "Gone"] (toList live)
  assertEq "problems" [] problems

-- | Links back to a directory above them (or the one they are in) are skipped, with a warning: the
-- walk ends, and no module is invented through them.
testWalkSkipsLinksBackUp :: IO ()
testWalkSkipsLinksBackUp = withTmpProject $ \tmp -> do
  let src   = tmp </> "src"
      links = [src </> "a", src </> "b", src </> "Lib" </> "up", src </> "Lib" </> "self"]
  touchFile (src </> "A.hs")
  touchFile (src </> "Lib" </> "B.hs")
  mapM_ (uncurry createDirectoryLink) (zip [".", ".", "..", "."] links)
  ((live, problems), err) <- withStderr tmp $
    withinSeconds 20 "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["A", "Lib.B"] (toList live)
  assertEq "problems, in walk order" (map SourceDirLinkSkipped [src </> "Lib" </> "self", src </> "Lib" </> "up", src </> "a", src </> "b"]) problems
  real <- canonicalizePath src
  forM_ links $ \l -> assertMentions "stderr" ("warning: skipped " <> show l) err
  assertMentions "stderr" ("it leads back to " <> show real) err
  assertEq "warnings" (length links) (length (lines err))

-- | Any other link is walked under its own name, an alias included: both names are live
-- (over-inclusive, which is safe), whichever sorts first.
testWalkAliasKeepsBothNames :: IO ()
testWalkAliasKeepsBothNames = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "Real" </> "X.hs")
  createDirectoryLink "Real" (src </> "Alias")
  createDirectoryLink "Real" (src </> "Zeta")
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["Alias.X", "Real.X", "Zeta.X"] (toList live)
  assertEq "stderr" "" err

-- | The alias can be the path of its modules: @module Compat.X@ in @Legacy/Compat/X.hs@, which only
-- @src/Compat@ spells right.
testWalkAliasIsThePathOfItsModules :: IO ()
testWalkAliasIsThePathOfItsModules = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "Legacy" </> "Compat" </> "X.hs")
  createDirectoryLink ("Legacy" </> "Compat") (src </> "Compat")
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["Compat.X", "Legacy.Compat.X"] (toList live)
  assertEq "stderr" "" err

-- | The entry itself can be a link: modules are named from the path the entry was given by.
testWalkEntryThatIsALink :: IO ()
testWalkEntryThatIsALink = withTmpProject $ \tmp -> do
  let real  = tmp </> "real"
      entry = tmp </> "entry"
  touchFile (real </> "A.hs")
  touchFile (real </> "Lib" </> "B.hs")
  createDirectoryLink "." (real </> "self")
  createDirectoryLink "real" entry
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [entry])
  assertEq "live modules" ["A", "Lib.B"] (toList live)
  assertMentions "stderr" ("warning: skipped " <> show (entry </> "self")) err

-- | A link to a directory outside the entry is followed, and so is a link found inside it.
testWalkFollowsLinkOutsideTree :: IO ()
testWalkFollowsLinkOutsideTree = withTmpProject $ \tmp -> do
  let src    = tmp </> "src"
      vendor = tmp </> "vendor"
  touchFile (src </> "A.hs")
  touchFile (vendor </> "Vendor" </> "X.hs")
  touchFile (vendor </> "Other" </> "Y.hs")
  createDirectoryLink (vendor </> "Vendor") (src </> "Vendor")
  createDirectoryLink (vendor </> "Other") (vendor </> "Vendor" </> "Inner")
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["A", "Vendor.Inner.Y", "Vendor.X"] (toList live)
  assertEq "stderr" "" err

-- | Every link to one directory is walked: the live set doesn't depend on which comes first.
testWalkEveryLinkToOneDirectoryIsWalked :: IO ()
testWalkEveryLinkToOneDirectoryIsWalked = withTmpProject $ \tmp -> do
  let src   = tmp </> "src"
      names = ["V5", "V3", "V8", "V1", "V7", "V2", "V6", "V4"]
  touchFile (tmp </> "vendor" </> "X.hs")
  createDirectoryIfMissing True src
  forM_ names $ \n -> createDirectoryLink (tmp </> "vendor") (src </> n)
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" [n <> ".X" | n <- ["V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"]] (toList live)
  assertEq "stderr" "" err

-- | A link found in a link's target is walked whichever path reached the target first.
testWalkLinksInsideLinkTargetsAreWalked :: IO ()
testWalkLinksInsideLinkTargetsAreWalked = withTmpProject $ \tmp -> do
  let src    = tmp </> "src"
      vendor = tmp </> "vendor"
  touchFile (vendor </> "V1" </> "X.hs")
  touchFile (vendor </> "V2" </> "Y.hs")
  createDirectoryLink (vendor </> "V2") (vendor </> "V1" </> "Inner")
  createDirectoryIfMissing True src
  createDirectoryLink (vendor </> "V1") (src </> "A")
  createDirectoryLink (vendor </> "V2") (src </> "B")
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["A.Inner.Y", "A.X", "B.Y"] (toList live)
  assertEq "stderr" "" err

-- | A real directory below a link's target is walked again when a link led to it first.
testWalkRealDirectoryBelowLinkTargetIsWalkedAgain :: IO ()
testWalkRealDirectoryBelowLinkTargetIsWalkedAgain = withTmpProject $ \tmp -> do
  let src    = tmp </> "src"
      vendor = tmp </> "vendor"
  touchFile (vendor </> "T1" </> "S" </> "X.hs")
  createDirectoryIfMissing True src
  createDirectoryLink (vendor </> "T1" </> "S") (src </> "L0")
  createDirectoryLink (vendor </> "T1") (src </> "L1")
  ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "live modules" ["L0.X", "L1.S.X"] (toList live)
  assertEq "stderr" "" err

-- | Warnings come in name order, whatever order the file system lists the links in.
testWalkWarnsInNameOrder :: IO ()
testWalkWarnsInNameOrder = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  forM_ ["s5", "s3", "s8", "s1", "s7", "s2", "s6", "s4"] $ \n -> createDirectoryLink "." (src </> n)
  (_, err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  assertEq "warnings" 8 (length (lines err))
  forM_ (zip (lines err) ["s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"]) $ \(l, n) ->
    assertMentions "warning order" ("skipped " <> show (src </> n)) l

-- | The warnings are written in chunks, and none is lost between or after them.
testWalkPrintsEveryWarning :: IO ()
testWalkPrintsEveryWarning = withTmpProject $ \tmp -> do
  let src   = tmp </> "src"
      names = ["s" <> replicate (4 - length (show i)) '0' <> show i | i <- [0 .. 2499 :: Int]]
  touchFile (src </> "A.hs")
  forM_ names $ \n -> createDirectoryLink "." (src </> n)
  (_, err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
  let ls = lines err
  assertEq "warnings" 2500 (length ls)
  forM_ [0, 998, 999, 1000, 1001, 1999, 2000, 2498, 2499] $ \i ->
    assertMentions "warning order" ("skipped " <> show (src </> (names !! i))) (ls !! i)

-- | Warnings printed before the walk fails are not lost.
testWalkFlushesWarningsBeforeRethrow :: IO ()
testWalkFlushesWarningsBeforeRethrow = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (tmp </> "file")
  createDirectoryLink "." (src </> "a")
  createFileLink (tmp </> "file" </> "x") (src </> "z")
  (r, err) <- withStderr tmp (try (liveModulesFromDirs walkEntryCap [src]))
  case r of
    Left (_ :: IOException) -> pure ()
    Right _                 -> failWith "expected the walk to fail on the link below a file"
  assertMentions "stderr" ("warning: skipped " <> show (src </> "a")) err

-- | A chain of links the system refuses to follow is resolved one link at a time: one that ends at
-- a directory is an alias, as a shorter chain is.
testWalkChainReachesItsDirectory :: IO ()
testWalkChainReachesItsDirectory = forM_ [(40, False), (64, False), (64, True)] $ \(n, slash) ->
  withTmpProject $ \tmp -> do
    let src = tmp </> "src"
    touchFile (src </> "A.hs")
    touchFile (tmp </> "legacy" </> "Compat" </> "X.hs")
    linkChain slash n (tmp </> "chain") (tmp </> "legacy" </> "Compat")
    createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Compat")
    refused <- refusesToFollow (src </> "Compat")
    whenRefused refused "testWalkChainReachesItsDirectory"
      ("a chain of " <> show n <> " links ends at its directory" <> if slash then ", with a slash at each hop" else "") $ do
      ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
      assertEq "live modules" ["A", "Compat.X"] (toList live)
      assertEq "stderr" "" err

-- | A chain of links that ends at the directory it starts in, or one above it, is a cycle like a
-- single link back up: skipped, with a warning.
testWalkChainBackToAnAncestorIsSkipped :: IO ()
testWalkChainBackToAnAncestorIsSkipped = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (src </> "Lib" </> "B.hs")
  linkChain False 40 (tmp </> "chain") src
  createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Lib" </> "Up")
  refused <- refusesToFollow (src </> "Lib" </> "Up")
  whenRefused refused "testWalkChainBackToAnAncestorIsSkipped" "a chain of 40 links ends at an ancestor" $ do
    ((live, _), err) <- withStderr tmp (withinSeconds 20 "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [src]))
    assertEq "live modules" ["A", "Lib.B"] (toList live)
    assertMentions "stderr" ("warning: skipped " <> show (src </> "Lib" </> "Up")) err
    assertEq "warnings" 1 (length (lines err))

-- | A chain that leads back to itself has no directory at its end: it is skipped, with a warning.
testWalkChainCycleHasNoDirectory :: IO ()
testWalkChainCycleHasNoDirectory = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (src </> "Lib" </> "B.hs")
  createFileLink "loopB" (src </> "loopA")
  createFileLink "loopA" (src </> "loopB")
  createFileLink "Lib" (src </> "Lib" </> "Lib")
  createFileLink "./SpellB" (src </> "SpellA")
  createFileLink (".." </> "src" </> "SpellA") (src </> "SpellB")
  linkChain False 40 (tmp </> "chain") "c1"
  createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Cyc")
  ((live, problems), err) <- withStderr tmp (withinSeconds 20 "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [src]))
  assertEq "live modules" ["A", "Lib.B"] (toList live)
  let links = [src </> "loopA", src </> "loopB", src </> "Lib" </> "Lib", src </> "SpellA", src </> "SpellB", src </> "Cyc"]
  forM_ links $ \l -> assertMentions "stderr" ("warning: skipped " <> show l) err
  assertEq "warnings" 6 (length (lines err))
  assertEq "problems: the skipped links" (sort links) (sort [l | SourceDirLinkSkipped l <- problems])
  assertEq "problems" 6 (length problems)
  assertMentions "stderr" "leads back to itself" err

-- | A chain that ends below a file can't be resolved: the walk throws, as it does for a single link.
testWalkChainBelowAFileThrows :: IO ()
testWalkChainBelowAFileThrows = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (tmp </> "file")
  linkChain False 40 (tmp </> "chain") (tmp </> "file" </> "x")
  createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Bad")
  refused <- refusesToFollow (src </> "Bad")
  whenRefused refused "testWalkChainBelowAFileThrows" "a chain of 40 links ends below a file" $
    assertThrowsIO "liveModulesFromDirs, a chain below a file" (liveModulesFromDirs walkEntryCap [src])

-- | A chain of more links than the hop cap, the link in the source directory counted, exits 1; one
-- of exactly the cap ends at its directory.
testWalkChainPastTheHopCapThrows :: IO ()
testWalkChainPastTheHopCapThrows = do
  assertEq "hop cap" 1000 maxLinkHops
  forM_ [(maxLinkHops - 1, True), (maxLinkHops, False)] $ \(n, ok) -> withTmpProject $ \tmp -> do
    let src = tmp </> "src"
    touchFile (src </> "A.hs")
    touchFile (tmp </> "legacy" </> "X.hs")
    linkChain False n (tmp </> "chain") (tmp </> "legacy")
    createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Long")
    r <- try (liveModulesFromDirs walkEntryCap [src])
    case (r, ok) of
      (Right (live, _), True)       -> assertEq "live modules" ["A", "Long.X"] (toList live)
      (Left (e :: IOException), False) -> do
        assertMentions "message" ("more than " <> show maxLinkHops <> " symbolic links") (show e)
        assertMentions "the fix" "replace the chain with one link to its end, or remove it" (show e)
      (Left e, True)                -> failWith ("a chain of exactly the cap failed: " <> show e)
      (Right _, False)              -> failWith "a chain past the cap was followed"

-- | A chain that ends at a file, or at nothing, counts like the link itself: as a file.
testWalkChainEndingAtAFileOrNothing :: IO ()
testWalkChainEndingAtAFileOrNothing = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (tmp </> "real" </> "M.hs")
  linkChain False 40 (tmp </> "chainF") (tmp </> "real" </> "M.hs")
  linkChain False 40 (tmp </> "chainN") (tmp </> "nowhere")
  createFileLink (tmp </> "chainF" </> "c1") (src </> "M.hs")
  createFileLink (tmp </> "chainN" </> "c1") (src </> "Gone.hs")
  refused <- refusesToFollow (src </> "M.hs")
  whenRefused refused "testWalkChainEndingAtAFileOrNothing" "a chain of 40 links ends at a file, or at nothing" $ do
    ((live, _), err) <- withStderr tmp (liveModulesFromDirs walkEntryCap [src])
    assertEq "live modules" ["A", "Gone", "M"] (toList live)
    assertEq "stderr" "" err

-- | More than the cap of entries visited, however they are reached (here every path of a diamond of
-- links), exits 1 naming the cap and the entry.
testWalkEntryCapTrips :: IO ()
testWalkEntryCapTrips = withTmpProject $ \tmp -> do
  let root = tmp </> "dia" </> "lvl0"
      lvl i = tmp </> "dia" </> ("lvl" <> show (i :: Int))
  assertEq "default cap" 200000 walkEntryCap
  forM_ [0 .. 11] $ \i -> do
    createDirectoryIfMissing True (lvl i)
    forM_ ["a", "b"] $ \n -> createDirectoryLink (lvl (i + 1)) (lvl i </> n)
  createDirectoryIfMissing True (lvl 12)
  touchFile (lvl 12 </> "Leaf.hs")
  r <- try (liveModulesFromDirs 100 [root])
  case r of
    Left (e :: IOException) -> do
      assertMentions "the cap" "more than 100 walk steps" (show e)
      assertMentions "the entry, shown" ("cannot walk " <> show root) (show e)
    Right _ -> failWith "expected the diamond to trip the cap"
  (live, _) <- liveModulesFromDirs 100000 [lvl 9]
  assertEq "under the cap" 8 (length live)
  mapM_ (touchFile . (\i -> tmp </> "five" </> ("F" <> show i <> ".hs"))) [1 .. 5 :: Int]
  (five, _) <- liveModulesFromDirs 5 [tmp </> "five"]
  assertEq "exactly the cap" 5 (length five)
  touchFile (tmp </> "five" </> "F6.hs")
  assertThrowsIO "one past the cap" (liveModulesFromDirs 5 [tmp </> "five"])

-- | Each hop of a chain costs the path components it walks, so the cap bounds chain work too: 65 hops
-- of at least two components each pass no cap of 100.
testWalkChainHopsCountTowardsTheCap :: IO ()
testWalkChainHopsCountTowardsTheCap = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
  touchFile (src </> "A.hs")
  touchFile (tmp </> "legacy" </> "X.hs")
  linkChain False 64 (tmp </> "chain") (tmp </> "legacy")
  createDirectoryLink (tmp </> "chain" </> "c1") (src </> "Long")
  refused <- refusesToFollow (src </> "Long")
  whenRefused refused "testWalkChainHopsCountTowardsTheCap" "64 links of a chain count towards the cap" $ do
    r <- try (liveModulesFromDirs 100 [src])
    case r of
      Left (e :: IOException) -> assertMentions "the cap" "more than 100 walk steps" (show e)
      Right _                 -> failWith "a chain of 64 links passed a cap of 100 steps"
    (live, _) <- liveModulesFromDirs walkEntryCap [src]
    assertEq "under the cap" ["A", "Long.X"] (toList live)

-- | A link followed costs the components of its path and of its target's, so a fan-out of links deep
-- in a tree passes the cap long before its entries would.
testWalkLinkCostsItsComponents :: IO ()
testWalkLinkCostsItsComponents = withTmpProject $ \tmp -> do
  let one = tmp </> "one"
      target = tmp </> "target"
  createDirectoryIfMissing True target
  createDirectoryIfMissing True one
  createDirectoryLink target (one </> "l")
  oneReal <- canonicalizePath one
  targetReal <- canonicalizePath target
  -- The entry listed, then the components of the link's path and of its target's.
  let steps = 1 + length (splitDirectories (oneReal </> "l")) + length (splitDirectories targetReal)
  _ <- liveModulesFromDirs steps [one]
  short <- try (liveModulesFromDirs (steps - 1) [one])
  case short of
    Left (e :: IOException) -> assertMentions "the rule" "a link followed one per path component its resolution walks" (show e)
    Right _                 -> failWith "one step short of a link's cost passed"
  -- A tree with no links takes one step per entry: a, b, c and M.hs.
  touchFile (tmp </> "plain" </> "a" </> "b" </> "c" </> "M.hs")
  _ <- liveModulesFromDirs 4 [tmp </> "plain"]
  assertThrowsIO "three steps" (liveModulesFromDirs 3 [tmp </> "plain"])
  -- Ten links 30 levels down: 40 entries, but each link costs more than 30 steps.
  let deep = foldl (</>) (tmp </> "deep") (replicate 30 "d")
  createDirectoryIfMissing True deep
  forM_ [1 .. 10 :: Int] $ \i -> createDirectoryLink target (deep </> ("l" <> show i))
  r <- try (liveModulesFromDirs 100 [tmp </> "deep"])
  case r of
    Left (e :: IOException) -> assertMentions "the cap" "more than 100 walk steps" (show e)
    Right _                 -> failWith "ten links 30 levels down passed a cap of 100 steps"
  (live, _) <- liveModulesFromDirs walkEntryCap [tmp </> "deep"]
  assertEq "no modules" [] (toList live)

-- | A link to a file, or a dangling one, costs what a link to a directory does: the system walks
-- the same components to resolve it.
testWalkFileLinkCostsItsComponents :: IO ()
testWalkFileLinkCostsItsComponents = withTmpProject $ \tmp -> do
  touchFile (tmp </> "target" </> "F.hs")
  forM_ [("file", tmp </> "target" </> "F.hs"), ("dangling", tmp </> "target" </> "gone" </> "G.hs")] $ \(kind, to) -> do
    let one = tmp </> kind
    createDirectoryIfMissing True one
    createFileLink to (one </> "l.hs")
    oneReal <- canonicalizePath one
    toReal <- canonicalizePath to
    let steps = 1 + length (splitDirectories (oneReal </> "l.hs")) + length (splitDirectories toReal)
    _ <- liveModulesFromDirs steps [one]
    short <- try (liveModulesFromDirs (steps - 1) [one])
    case short of
      Left (e :: IOException) -> assertMentions (kind <> ": the rule") "a link followed one per path component its resolution walks" (show e)
      Right _                 -> failWith (kind <> ": one step short of a link's cost passed")
  -- Ten file links 30 levels down: 40 entries, but each link costs more than 30 steps.
  let deep = foldl (</>) (tmp </> "deep") (replicate 30 "d")
  createDirectoryIfMissing True deep
  forM_ [1 .. 10 :: Int] $ \i -> createFileLink (tmp </> "target" </> "F.hs") (deep </> ("l" <> show i <> ".hs"))
  r <- try (liveModulesFromDirs 100 [tmp </> "deep"])
  case r of
    Left (e :: IOException) -> assertMentions "the cap" "more than 100 walk steps" (show e)
    Right _                 -> failWith "ten file links 30 levels down passed a cap of 100 steps"
  (live, _) <- liveModulesFromDirs walkEntryCap [tmp </> "deep"]
  assertEq "every link is a module" 10 (length live)

-- | M-3: the walk's cap is a merge option, defaulted once for the library and the CLI.
testWalkCapIsAnOption :: IO ()
testWalkCapIsAnOption = withTmpProject $ \tmp -> do
  assertEq "default option" 200000 (moWalkEntryCap (defaultMergeOptions "f" "o"))
  let src = tmp </> "src"
  mapM_ (\n -> touchFile (src </> (n <> ".hs"))) ["A", "B", "C"]
  ownFragment tmp
  r <- try (runMerge (baseOpts tmp) { moSourceDirs = [src], moWalkEntryCap = 2 })
  case r of
    Left (e :: IOException) -> assertMentions "the cap" "more than 2 walk steps" (show e)
    Right _                 -> failWith "runMerge walked past its moWalkEntryCap"
  assertAbsent (tmp </> "merged.yaml")

-- | I-4: a walk lists directories at most 'maxWalkDepth' levels below its entry, and fails naming
-- the limit and the entry past it.
testWalkDepthLimit :: IO ()
testWalkDepthLimit = withTmpProject $ \tmp -> do
  assertEq "depth limit" 256 maxWalkDepth
  let nested n = foldl (</>) (tmp </> ("n" <> show n)) (replicate n "d")
  touchFile (nested 256 </> "Leaf.hs")
  (live, _) <- liveModulesFromDirs walkEntryCap [tmp </> "n256"]
  assertEq "256 levels down" [intercalate "." (replicate 256 "d" <> ["Leaf"])] (toList live)
  touchFile (nested 257 </> "Leaf.hs")
  r <- try (liveModulesFromDirs walkEntryCap [tmp </> "n257"])
  case r of
    Left (e :: IOException) -> do
      assertMentions "the limit" "more than 256 directory levels below it" (show e)
      assertMentions "the entry" (tmp </> "n257") (show e)
    Right _ -> failWith "a directory 257 levels down was walked"

-- | Each link of a chain of directories, each holding a link to the next, is a level: a long one
-- fails within seconds instead of taking time that grows with the square of its length.
testWalkDeepLinkChainFailsFast :: IO ()
testWalkDeepLinkChainFailsFast = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
      d i = tmp </> "d" </> show (i :: Int)
  touchFile (src </> "A.hs")
  forM_ [0 .. 300] (createDirectoryIfMissing True . d)
  forM_ [0 .. 299] $ \i -> createDirectoryLink (".." </> show (i + 1)) (d i </> "l")
  touchFile (d 300 </> "Leaf.hs")
  createDirectoryLink (".." </> "d" </> "0") (src </> "l")
  r <- try (withinSeconds 20 "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [src]))
  case r of
    Left (e :: IOException) -> assertMentions "the limit" "more than 256 directory levels below it" (show e)
    Right _                 -> failWith "a chain of 301 directory levels was walked"

-- | A directory reached through a chain of links the system refuses to follow is a level too.
testWalkChainsCountLevels :: IO ()
testWalkChainsCountLevels = withTmpProject $ \tmp -> do
  let d i = tmp </> "d" </> show (i :: Int)
  forM_ [0 .. 257] (createDirectoryIfMissing True . d)
  touchFile (d 257 </> "Leaf.hs")
  forM_ [0 .. 256] $ \i -> do
    linkChain False 40 (tmp </> "ch" </> show i) (d (i + 1))
    createDirectoryLink (tmp </> "ch" </> show i </> "c1") (d i </> "next")
  refused <- refusesToFollow (d 0 </> "next")
  whenRefused refused "testWalkChainsCountLevels" "a chain of 40 links is followed one link at a time" $ do
    r <- try (withinSeconds 20 "liveModulesFromDirs" (liveModulesFromDirs walkEntryCap [d 0]))
    case r of
      Left (e :: IOException) -> assertMentions "the limit" "more than 256 directory levels below it" (show e)
      Right _                 -> failWith "257 levels reached through chains of links were walked"

-- | A module whose file declares the name through a skipped link is named otherwise in the walk
-- (@src/Foo -> .@ with @module Foo.A@ in @src/A.hs@), so pruning in a merge that skipped a link
-- fails, naming both; without pruning the skip only warns.
testSkippedLinkWithPruneFatal :: IO ()
testSkippedLinkWithPruneFatal = withTmpProject $ \tmp -> do
  let src  = tmp </> "src"
      link = src </> "Foo"
  touchFile (src </> "A.hs")
  touchFile (src </> "B.hs")
  createDirectoryLink "." link
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "B.yaml") (sampleFragment "B" "B.BT" "b_tab")
  (kept, warned) <- withStderr tmp (runMerge (baseOpts tmp) { moSourceDirs = [src] })
  assertEq "nothing pruned" 0 . mrStalePruned =<< expectOk kept
  assertMentions "the warning" ("warning: skipped " <> show link) warned
  removeFile (tmp </> "merged.yaml")
  writeFragYaml (tmp </> "fragments" </> "Foo.A.yaml") (sampleFragment "Foo.A" "Foo.A.AT" "a_tab")
  forM_ everyFlagCombination $ \flags -> do
    text <- refusalText . fst =<< withStderr tmp (runMerge (flags (baseOpts tmp)) { moSourceDirs = [src] })
    assertMentions "the error" "fragments were pruned as stale while the walk of --source-dirs skipped links" text
    assertMentions "the link" ("skipped link " <> show link) text
    assertMentions "the pruned module" "pruned fragment of \"Foo.A\"" text
    assertAbsent (tmp </> "merged.yaml")

testLiveModulesFromDirsReportsProblems :: IO ()
testLiveModulesFromDirsReportsProblems = withTmpProject $ \tmp -> do
  let src  = tmp </> "src"
      gone = tmp </> "gone"
      file = tmp </> "notadir.hs"
  touchFile (src </> "A.hs")
  touchFile (src </> "Lib" </> "B.hs")
  touchFile file
  (live, problems) <- liveModulesFromDirs walkEntryCap [gone, src, file]
  assertEq "live modules" ["A", "Lib.B"] (toList live)
  assertEq "problems, in order" [SourceDirMissing gone, SourceDirNotADirectory file] problems

testLoadFragmentsPathKinds :: IO ()
testLoadFragmentsPathKinds = withTmpProject $ \tmp -> do
  let file = tmp </> "frags"
  BS.writeFile file "x\n"
  missing <- loadFragments (tmp </> "absent")
  assertEq "a missing directory" (0, [], []) missing
  assertThrowsIO "a file" (loadFragments file)

-- | Skipping a live module's unreadable fragment would drop its tables.
testUnparseableFragmentFatal :: IO ()
testUnparseableFragmentFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  BS.writeFile (fragDir </> "Broken.yaml") ":::not::valid:::yaml:::"
  expectMergeError "unparseable fragment" =<< runMerge (baseOpts tmp)

-- | The same with --source-dirs, as every real merge runs: the module is live,
-- so its fragment is not stale, and the error is not mistaken for a bad
-- --source-dirs.
testUnparseableLiveFragmentFatal :: IO ()
testUnparseableLiveFragmentFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Alive.hs")
  createDirectoryIfMissing True fragDir
  BS.writeFile (fragDir </> "Alive.yaml") ":::not::valid:::yaml:::"
  result <- runMerge (baseOpts tmp) { moSourceDirs = [src] }
  expectMergeError "unparseable fragment" result
  case result of
    Left errs | any ("pruned as stale" `isInfixOf`) errs ->
      failWith ("unparseable live fragment reported as pruned: " <> show errs)
    _ -> pure ()

-- | ... unless --source-dirs shows its module is gone: then it is stale.
testUnparseableStaleFragmentPruned :: IO ()
testUnparseableStaleFragmentPruned = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Alive.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  BS.writeFile (fragDir </> "Gone.yaml") ":::not::valid:::yaml:::"
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "stalePruned" 1 (mrStalePruned report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

-- | A package with no tables of its own composes an included contract; with no
-- fragments, nothing is pruned and the merge succeeds.
testIncludeOnlyWithSourceDirs :: IO ()
testIncludeOnlyWithSourceDirs = withTmpProject $ \tmp -> do
  let src = tmp </> "src"
      inc = tmp </> "upstream.yaml"
  touchFile (src </> "Other.hs")
  writeIncluded inc
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moSourceDirs = [src], moIncludeMerged = [inc], moNoOwnTables = True }
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

-- | Pruning every fragment fails even when an included contract supplies tables.
testAllPrunedWithIncludeFatal :: IO ()
testAllPrunedWithIncludeFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
      inc     = tmp </> "upstream.yaml"
  touchFile (src </> "Other.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Gone.yaml") (sampleFragment "Gone" "Gone.GoneT" "gone")
  writeIncluded inc
  expectMergeError "pruned as stale" =<< runMerge (baseOpts tmp)
    { moSourceDirs = [src], moIncludeMerged = [inc] }

-- | Modules cabal preprocesses (.hsc, .x, …) are live too.
testOtherSourceExtensionsLive :: IO ()
testOtherSourceExtensionsLive = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Gen.hsc")
  touchFile (src </> "Lex.x")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Gen.yaml") (sampleFragment "Gen" "Gen.GenT" "gen")
  writeFragYaml (fragDir </> "Lex.yaml") (sampleFragment "Lex" "Lex.LexT" "lex")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

-- | Own tables are those of the package's own fragments, not included ones.
testOwnTablesCounted :: IO ()
testOwnTablesCounted = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      inc     = tmp </> "upstream.yaml"
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  writeIncluded inc
  report <- expectOk =<< runMerge (baseOpts tmp) { moIncludeMerged = [inc] }
  assertEq "ownTables" 1 (mrOwnTables report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

-- | Included tables must not hide that the package's own plugin never ran;
-- --no-own-tables declares there is nothing of its own to miss, and own tables
-- prove that declaration stale; --allow-empty permits only a 0-table contract.
testPublishProblem :: IO ()
testPublishProblem = do
  let report ownFrags ownTables tables = MergeReport
        { mrFragmentsRead = ownFrags, mrStalePruned = 0, mrIncludedFiles = 1
        , mrIncludedTables = tables - ownTables, mrDeduped = 0
        , mrTablesEmitted = tables, mrOverridesEmitted = 0, mrOwnTables = ownTables }
      check name flags r expected =
        assertEq name expected (uncurry publishProblem flags r)
      none = (False, False); allowEmpty = (True, False); noOwn = (False, True)
  check "own tables" none (report 2 2 3) Nothing
  check "include only" none (report 0 0 3) (Just NoOwnFragments)
  check "include only, --no-own-tables" noOwn (report 0 0 3) Nothing
  check "include only, --allow-empty" allowEmpty (report 0 0 3) (Just NoOwnFragments)
  check "own overrides only" none (report 1 0 3) Nothing
  check "own overrides only, --no-own-tables" noOwn (report 1 0 3) Nothing
  check "0 tables" none (report 0 0 0) (Just NoTables)
  check "0 tables, --allow-empty" allowEmpty (report 0 0 0) Nothing
  check "0 tables, --no-own-tables" noOwn (report 0 0 0) (Just NoTables)
  check "stale --no-own-tables" noOwn (report 2 2 3) (Just (OwnTablesDeclaredNone 2))
  check "stale --no-own-tables, --allow-empty" (True, True) (report 2 2 3)
    (Just (OwnTablesDeclaredNone 2))

-- | A refused 'runMerge' creates no @--out@ and leaves an existing one as it was.
testRunMergeGatesBeforeWrite :: IO ()
testRunMergeGatesBeforeWrite = withTmpProject $ \tmp -> do
  let out = tmp </> "merged.yaml"
      inc = tmp </> "upstream.yaml"
      own = tmp </> "own"
      refused name opts needle = do
        expectMergeError needle =<< runMerge opts
        assertAbsent out
        BS.writeFile out "old contract\n"
        expectMergeError needle =<< runMerge opts
        kept <- BS.readFile out
        assertEq (name <> ": existing --out") "old contract\n" kept
        removeFile out
  createDirectoryIfMissing True (tmp </> "fragments")
  createDirectoryIfMissing True own
  writeFragYaml (own </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  writeIncluded inc
  refused "0 tables" (baseOpts tmp) "0 tables"
  refused "no own fragments" ((baseOpts tmp) { moIncludeMerged = [inc] })
    "no fragment of this package's own modules"
  refused "--no-own-tables with own tables"
    ((baseOpts tmp) { moFragmentsDir = own, moNoOwnTables = True }) "--no-own-tables was given"
  report <- expectOk =<< runMerge (baseOpts tmp) { moAllowEmpty = True }
  assertEq "tablesEmitted" 0 (mrTablesEmitted report)
  merged <- decodeFile out
  assertEq "--allow-empty contract" 0 (length (mergedTables merged))

-- | A refusal replaces the summary line, so it carries the counts itself.
testRefusalLinesStateCounts :: IO ()
testRefusalLinesStateCounts = do
  let report = MergeReport
        { mrFragmentsRead = 3, mrStalePruned = 1, mrIncludedFiles = 2, mrIncludedTables = 5
        , mrDeduped = 0, mrTablesEmitted = 5, mrOverridesEmitted = 0, mrOwnTables = 0 }
      opts = defaultMergeOptions "frags-dir" "out.yaml"
  forM_ [ (NoTables, "the contract has 0 tables")
        , (NoOwnFragments, "no fragment of this package's own modules was read")
        , (OwnTablesDeclaredNone 4, "define 4 table(s)")
        ] $ \(problem, why) -> do
    let text = unlines (refusalLines opts report problem)
    forM_ [ why, "3 fragments read", "1 stale pruned", "2 included contracts (5 tables)"
          , "out.yaml", "frags-dir" ] $ \needle ->
      unless (needle `isInfixOf` text) $
        failWith (show problem <> ": expected " <> show needle <> " in " <> show text)

-- | One own fragment, so that the checks under test are the only ones that can fail.
ownFragment :: FilePath -> IO ()
ownFragment tmp = do
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "Foo.yaml") (sampleFragment "Foo" "Foo.FooT" "foo")

-- | An included contract with no tables usually means its package's plugin did not run.
testEmptyIncludeFatal :: IO ()
testEmptyIncludeFatal = withTmpProject $ \tmp -> do
  let empty = tmp </> "empty.yaml"
      full  = tmp </> "upstream.yaml"
  ownFragment tmp
  writeEmptyIncluded empty
  writeIncluded full
  text <- refusalText =<< runMerge (baseOpts tmp) { moIncludeMerged = [full, empty] }
  assertMentions "the file" ("--include-merged file " <> show empty <> " holds no tables") text
  assertMentions "the opt-out" "--allow-empty-include" text
  assertLacks "a contract with tables" full text
  assertAbsent (tmp </> "merged.yaml")

testEmptyIncludeAllowed :: IO ()
testEmptyIncludeAllowed = withTmpProject $ \tmp -> do
  let empty = tmp </> "empty.yaml"
  ownFragment tmp
  writeEmptyIncluded empty
  (result, err) <- withStderr tmp (runMerge (baseOpts tmp)
    { moIncludeMerged = [empty], moAllowEmptyInclude = True })
  report <- expectOk result
  assertEq "includedFiles" 1 (mrIncludedFiles report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  assertMentions "stderr" ("warning: --include-merged file " <> show empty <> " holds no tables") err
  assertLacks "stderr" "ERROR" err

-- | The m4 layout: a module whose override names a table type that no fragment defines.
unmatchedOverrideProject :: FilePath -> IO MergeOptions
unmatchedOverrideProject tmp = do
  touchFile (tmp </> "src" </> "A.hs")
  touchFile (tmp </> "src" </> "Db.hs")
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a_tab")
  writeFragYaml (tmp </> "fragments" </> "Db.yaml") (overrideFragment "MissingT")
  pure (baseOpts tmp) { moSourceDirs = [tmp </> "src"] }

testUnmatchedOverrideFatal :: IO ()
testUnmatchedOverrideFatal = withTmpProject $ \tmp -> do
  opts <- unmatchedOverrideProject tmp
  text <- refusalText =<< runMerge opts
  assertMentions "the override" "dbEntityOverride for \"MissingT\" in \"Db\" has no matching table" text
  assertMentions "the opt-out" "--allow-unmatched-override" text
  assertAbsent (moOutFile opts)

testUnmatchedOverrideAllowed :: IO ()
testUnmatchedOverrideAllowed = withTmpProject $ \tmp -> do
  opts <- unmatchedOverrideProject tmp
  (result, err) <- withStderr tmp (runMerge opts { moAllowUnmatchedOverride = True })
  report <- expectOk result
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)
  assertMentions "stderr" "warning: dbEntityOverride for \"MissingT\" in \"Db\" has no matching table" err
  assertLacks "stderr" "ERROR" err

-- | The shape another plugin leaves when it rewrites a record first: a table with no columns.
testColumnlessTableFatal :: IO ()
testColumnlessTableFatal = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "LR.yaml") columnlessFragment
  text <- refusalText =<< runMerge (baseOpts tmp)
  assertMentions "the table" "table \"LR.FooT\" (SQL table \"foo\") has no columns" text
  assertMentions "the opt-out" "--allow-columnless-table" text
  assertAbsent (tmp </> "merged.yaml")

testColumnlessTableAllowed :: IO ()
testColumnlessTableAllowed = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "LR.yaml") columnlessFragment
  (result, err) <- withStderr tmp (runMerge (baseOpts tmp) { moAllowColumnlessTable = True })
  report <- expectOk result
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  merged <- decodeFile (tmp </> "merged.yaml")
  assertEq "columns" [[]] (map columns (mergedTables merged))
  assertMentions "stderr" "warning: table \"LR.FooT\" (SQL table \"foo\") has no columns" err
  assertLacks "stderr" "ERROR" err

-- | Included tables are checked too, and a table that arrives twice counts once.
testColumnlessTableCountedAfterDedup :: IO ()
testColumnlessTableCountedAfterDedup = withTmpProject $ \tmp -> do
  let inc = tmp </> "upstream.yaml"
  createDirectoryIfMissing True (tmp </> "fragments")
  BS.writeFile inc (YAML.encode MergedSchema
    { mergedTables = fragmentTables columnlessFragment, mergedDbEntityOverrides = [], generator = Nothing })
  text <- refusalText =<< runMerge (baseOpts tmp) { moIncludeMerged = [inc], moNoOwnTables = True }
  assertMentions "an included table" "table \"LR.FooT\" (SQL table \"foo\") has no columns" text
  assertAbsent (tmp </> "merged.yaml")
  writeFragYaml (tmp </> "fragments" </> "LR.yaml") columnlessFragment
  result <- runMerge (baseOpts tmp) { moIncludeMerged = [inc] }
  case result of
    Left errs -> assertEq "one error for the one table" 1 (length errs)
    Right _   -> failWith "expected the column-less table to be refused"
  assertAbsent (tmp </> "merged.yaml")

-- | A project with all three problems: each opt-out turns its own error into a warning and no other.
testOptOutsAreIndependent :: IO ()
testOptOutsAreIndependent = withTmpProject $ \tmp -> do
  base <- unmatchedOverrideProject tmp
  touchFile (tmp </> "src" </> "LR.hs")
  writeFragYaml (tmp </> "fragments" </> "LR.yaml") columnlessFragment
  writeEmptyIncluded (tmp </> "empty.yaml")
  let opts = base { moIncludeMerged = [tmp </> "empty.yaml"] }
      checks =
        [ ("empty include", "holds no tables", \o -> o { moAllowEmptyInclude = True })
        , ("unmatched override", "has no matching table", \o -> o { moAllowUnmatchedOverride = True })
        , ("column-less table", "has no columns", \o -> o { moAllowColumnlessTable = True }) ]
  forM_ checks $ \(name, needle, _) -> do
    let optedOut = foldr (\(other, _, allow) o -> if other == name then o else allow o) opts checks
    text <- refusalText =<< runMerge optedOut
    assertMentions (name <> " without its opt-out") needle text
    forM_ [n | (other, n, _) <- checks, other /= name] $ \n ->
      assertLacks (name <> " without its opt-out") n text
    assertAbsent (moOutFile opts)
  (result, err) <- withStderr tmp (runMerge (foldr (\(_, _, allow) -> allow) opts checks))
  _ <- expectOk result
  forM_ checks $ \(name, needle, _) -> assertMentions (name <> " warning") needle err
  assertLacks "stderr" "ERROR" err

-- | The opt-outs relax their own checks only: with all three set, every other check still refuses.
testOptOutsKeepOtherChecks :: IO ()
testOptOutsKeepOtherChecks = forM_ cases $ \(needle, setup) -> withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "fragments")
  tweak <- setup tmp
  expectMergeError needle =<< runMerge (tweak (baseOpts tmp)
    { moAllowEmptyInclude = True, moAllowUnmatchedOverride = True, moAllowColumnlessTable = True })
  assertAbsent (tmp </> "merged.yaml")
  where
    fragment tmp name = writeFragYaml (tmp </> "fragments" </> (name <> ".yaml"))
    cases =
      [ ("the contract has 0 tables", \_ -> pure id)
      , ("disagrees with modelTableName", \tmp -> do
          fragment tmp "A" (sampleFragment "A" "A.AT" "a_tab")
          fragment tmp "Db" (overrideFragment "AT")
          pure id)
      , ("Ambiguous dbEntityOverride", \tmp -> do
          fragment tmp "F" (sampleFragment "M1" "M1.T" "x")
            { fragmentTables = [sampleTable "M1.T" "M1" "x", sampleTable "M2.T" "M2" "y"] }
          fragment tmp "Db" (overrideFragment "T")
          pure id)
      , ("conflicting definitions", \tmp -> do
          let dup = tmp </> "dup.yaml"
          fragment tmp "A" (sampleFragment "A" "A.AT" "a_tab")
          BS.writeFile dup (YAML.encode MergedSchema
            { mergedTables = fragmentTables (sampleFragment "A" "A.AT" "a_other")
            , mergedDbEntityOverrides = [], generator = Nothing })
          pure (\o -> o { moIncludeMerged = [dup] }))
      , ("unparseable fragment", \tmp -> do
          fragment tmp "A" (sampleFragment "A" "A.AT" "a_tab")
          BS.writeFile (tmp </> "fragments" </> "Broken.yaml") ":::not::valid:::yaml:::"
          pure id)
      ]

testChecksReportedTogether :: IO ()
testChecksReportedTogether = withTmpProject $ \tmp -> do
  opts <- unmatchedOverrideProject tmp
  touchFile (tmp </> "src" </> "LR.hs")
  writeFragYaml (tmp </> "fragments" </> "LR.yaml") columnlessFragment
  text <- refusalText =<< runMerge opts
  assertMentions "the override" "has no matching table" text
  assertMentions "the table" "has no columns" text
  assertAbsent (moOutFile opts)

testLoadIncludedMergedListsEmptyFiles :: IO ()
testLoadIncludedMergedListsEmptyFiles = withTmpProject $ \tmp -> do
  let empty  = tmp </> "empty.yaml"
      onlyOv = tmp </> "overrides-only.yaml"
      full   = tmp </> "upstream.yaml"
  writeEmptyIncluded empty
  BS.writeFile onlyOv (YAML.encode MergedSchema
    { mergedTables = [], mergedDbEntityOverrides = [matchingOverride], generator = Nothing })
  writeIncluded full
  (files, tables, overrides, errs, emptyFiles) <-
    loadIncludedMerged [empty, onlyOv, full, tmp </> "missing.yaml"]
  assertEq "files parsed" 3 files
  assertEq "tables" 1 (length tables)
  assertEq "overrides" 1 (length overrides)
  assertEq "errors" 1 (length errs)
  assertEq "files without tables, overrides or not" [empty, onlyOv] emptyFiles

-- | An override of the table @A.AT@ (SQL name @a_tab@), as the table's own module would record it.
matchingOverride :: DbEntityOverride
matchingOverride = DbEntityOverride
  { dbField = "f", dbTableType = "AT", sqlName = "a_tab"
  , overrideSourceModule = "Db", overrideSourceFile = "src/x.hs" }

-- | A contract that holds overrides but no tables carries nothing a table check could use, so it
-- counts as empty like one that holds nothing.
testOverridesOnlyIncludeIsEmpty :: IO ()
testOverridesOnlyIncludeIsEmpty = withTmpProject $ \tmp -> do
  let inc = tmp </> "overrides-only.yaml"
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a_tab")
  BS.writeFile inc (YAML.encode MergedSchema
    { mergedTables = [], mergedDbEntityOverrides = [matchingOverride], generator = Nothing })
  text <- refusalText =<< runMerge (baseOpts tmp) { moIncludeMerged = [inc] }
  assertMentions "the file" ("--include-merged file " <> show inc <> " holds no tables") text
  assertAbsent (tmp </> "merged.yaml")
  (result, err) <- withStderr tmp (runMerge (baseOpts tmp) { moIncludeMerged = [inc], moAllowEmptyInclude = True })
  report <- expectOk result
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)
  assertMentions "stderr" ("warning: --include-merged file " <> show inc <> " holds no tables") err

-- | Only identical overrides collapse: the same key and SQL name from two places is one entry, even
-- when the source file differs.
testIdenticalOverridesCollapse :: IO ()
testIdenticalOverridesCollapse = withTmpProject $ \tmp -> do
  let inc = tmp </> "upstream.yaml"
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "A.yaml") (sampleFragment "A" "A.AT" "a_tab")
  writeFragYaml (tmp </> "fragments" </> "Db.yaml") Fragment
    { fragmentModule = "Db", fragmentTables = [], fragmentDbEntityOverrides = [matchingOverride] }
  BS.writeFile inc (YAML.encode MergedSchema
    { mergedTables = [sampleTable "Up.UpT" "Up" "up_tab"]
    , mergedDbEntityOverrides = [matchingOverride { overrideSourceFile = "src/y.hs" }]
    , generator = Nothing })
  report <- expectOk =<< runMerge (baseOpts tmp) { moIncludeMerged = [inc] }
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)

-- | Overrides that differ in module, field or table type are different keys: each keeps its own name.
testOverridesWithDifferentKeysDoNotConflict :: IO ()
testOverridesWithDifferentKeysDoNotConflict = withTmpProject $ \tmp -> do
  let base = matchingOverride { dbTableType = "WidgetT", sqlName = "n1" }
      keys = [ base, base { overrideSourceModule = "Db2", sqlName = "n2" }
             , base { dbField = "g", sqlName = "n3" }, base { dbTableType = "GadgetT", sqlName = "n4" } ]
  createDirectoryIfMissing True (tmp </> "fragments")
  writeFragYaml (tmp </> "fragments" </> "Db.yaml") Fragment
    { fragmentModule = "Db", fragmentTables = [], fragmentDbEntityOverrides = keys }
  ownFragment tmp
  report <- expectOk =<< runMerge (baseOpts tmp) { moAllowUnmatchedOverride = True }
  assertEq "overridesEmitted" 4 (mrOverridesEmitted report)

-- | One key with two SQL names can't be recorded, so it is an error whatever the flags.
testOverrideKeyWithTwoSqlNamesFatal :: IO ()
testOverrideKeyWithTwoSqlNamesFatal = withTmpProject $ \tmp -> do
  let two = Fragment { fragmentModule = "Db", fragmentTables = []
                     , fragmentDbEntityOverrides = [ovNamed "w_one", ovNamed "w_two"] }
  ownFragment tmp
  writeFragYaml (tmp </> "fragments" </> "Db.yaml") two
  forM_ everyFlagCombination $ \tweak -> do
    text <- refusalText =<< runMerge (tweak (baseOpts tmp))
    assertMentions "the key" "dbEntityOverride for \"WidgetT\" (field \"f\") in \"Db\" gives different SQL names" text
    assertMentions "the first name" "\"w_one\"" text
    assertMentions "the second name" "\"w_two\"" text
    assertAbsent (tmp </> "merged.yaml")
  where
    ovNamed n = matchingOverride { dbTableType = "WidgetT", sqlName = n }

-- | The same through an included contract: the second step of a merge that includes the first's
-- output, and a contract that holds the pair itself.
testOverrideKeyWithTwoSqlNamesAcrossIncludeFatal :: IO ()
testOverrideKeyWithTwoSqlNamesAcrossIncludeFatal = withTmpProject $ \tmp -> do
  let upstream = tmp </> "upstream.yaml"
      paired   = tmp </> "paired.yaml"
      ovNamed n = matchingOverride { dbTableType = "WidgetT", sqlName = n }
      contract file ovs = BS.writeFile file (YAML.encode MergedSchema
        { mergedTables = [sampleTable "Up.UpT" "Up" "up_tab"], mergedDbEntityOverrides = ovs
        , generator = Nothing })
  ownFragment tmp
  writeFragYaml (tmp </> "fragments" </> "Db.yaml") Fragment
    { fragmentModule = "Db", fragmentTables = [], fragmentDbEntityOverrides = [ovNamed "w_two"] }
  contract upstream [ovNamed "w_one"]
  contract paired [ovNamed "w_one", ovNamed "w_two"]
  forM_ everyFlagCombination $ \flags -> do
    text <- refusalText =<< runMerge (flags (baseOpts tmp)) { moIncludeMerged = [upstream] }
    assertMentions "own against included" "gives different SQL names" text
    assertMentions "the included name" "\"w_one\"" text
    assertMentions "the own name" "\"w_two\"" text
    assertAbsent (tmp </> "merged.yaml")
  removeFile (tmp </> "fragments" </> "Db.yaml")
  forM_ everyFlagCombination $ \flags -> do
    text' <- refusalText =<< runMerge (flags (baseOpts tmp)) { moIncludeMerged = [paired] }
    assertMentions "both in one contract" "gives different SQL names" text'
    assertAbsent (tmp </> "merged.yaml")

-- | Every combination of the merge's opt-out flags, for the checks that hold whatever the flags.
everyFlagCombination :: [MergeOptions -> MergeOptions]
everyFlagCombination =
  [ \o -> o { moAllowEmpty = a, moNoOwnTables = b, moAllowEmptyInclude = c, moAllowUnmatchedOverride = d
            , moAllowColumnlessTable = e }
  | a <- bools, b <- bools, c <- bools, d <- bools, e <- bools ]
  where bools = [False, True]

mergeOutput :: FilePath -> MergeOptions -> IO String
mergeOutput tmp opts = do
  (result, err) <- withStderr tmp (runMerge opts)
  pure (err <> either unlines (const "") result)

hostileTable :: String -> String -> String -> TableSchema
hostileTable modName code sql = (sampleTable (modName <> "." <> code) modName sql)
  { primaryKey = PrimaryKeyInfo hostileField ["id"] }

hostileOverride :: String -> String -> DbEntityOverride
hostileOverride ty sql = DbEntityOverride
  { dbField = hostileField, dbTableType = ty, sqlName = sql
  , overrideSourceModule = hostileField, overrideSourceFile = hostileField }

-- | Every field a merge message prints comes from a fragment, so each is printed escaped: a value
-- with a newline and an escape character can't forge a line of the merge's output.
testHostileFieldsEscaped :: IO ()
testHostileFieldsEscaped = forM_ scenarios $ \(label, phrase, tables, overrides, tweak) ->
  withTmpProject $ \tmp -> do
    createDirectoryIfMissing True (tmp </> "fragments")
    writeFragYaml (tmp </> "fragments" </> "F.yaml")
      Fragment { fragmentModule = "F", fragmentTables = tables, fragmentDbEntityOverrides = overrides }
    text <- mergeOutput tmp (tweak (baseOpts tmp))
    assertMentions label phrase text
    assertHostileEscaped label text
  where
    h = hostileField
    bare = (\t -> t { columns = [] }) . hostileTable h "T"
    scenarios =
      [ ("unmatched override", "has no matching table", [], [hostileOverride h h], id)
      , ("unmatched override, opted out", "warning: dbEntityOverride", [], [hostileOverride h h]
        , \o -> o { moAllowUnmatchedOverride = True })
      , ("override mismatch", "disagrees with modelTableName"
        , [hostileTable h "T" (h <> "a")], [hostileOverride "T" (h <> "b")], id)
      , ("ambiguous override", "Ambiguous dbEntityOverride"
        , [hostileTable "M1" h "x", hostileTable "M2" h "y"], [hostileOverride h "z"], id)
      , ("duplicate conflict", "conflicting definitions"
        , [hostileTable h "T" (h <> "1"), hostileTable h "T" (h <> "2")], [], id)
      , ("shared SQL name", "modelled by multiple Haskell types"
        , [hostileTable (h <> "1") "T" h, hostileTable (h <> "2") "T" h], [], id)
      , ("override key with two SQL names", "gives different SQL names", []
        , [hostileOverride h (h <> "1"), hostileOverride h (h <> "2")], id)
      , ("table without columns", "has no columns", [bare h], [], id)
      , ("table without columns, opted out", "warning: table", [bare h], []
        , \o -> o { moAllowColumnlessTable = True })
      ]

-- | The paths the command line names are printed escaped too: one with a newline and an escape
-- character can't forge a line of the merge's output.
testCliSuppliedPathsShown :: IO ()
testCliSuppliedPathsShown = forM_ scenarios $ \(label, phrase, prepare) ->
  withTmpProject $ \tmp -> do
    opts <- prepare tmp
    text <- mergeOutput tmp opts
    assertMentions label phrase text
    assertHostileEscaped label text
  where
    bad tmp = tmp </> hostileField
    withFragment tmp o = ownFragment tmp >> pure o
    scenarios :: [(String, String, FilePath -> IO MergeOptions)]
    scenarios =
      [ ("--fragments missing", "does not exist; reading it as no fragments"
        , \tmp -> pure (baseOpts tmp) { moFragmentsDir = bad tmp })
      , ("refusal, --out", "(not written)"
        , \tmp -> pure (baseOpts tmp) { moOutFile = bad tmp })
      , ("--fragments a file", "--fragments is not a directory: "
        , \tmp -> touchFile (bad tmp) >> pure (baseOpts tmp) { moFragmentsDir = bad tmp })
      , ("empty include", "holds no tables"
        , \tmp -> writeEmptyIncluded (bad tmp) >> withFragment tmp (baseOpts tmp) { moIncludeMerged = [bad tmp] })
      , ("empty include, opted out", "warning: --include-merged file "
        , \tmp -> writeEmptyIncluded (bad tmp)
            >> withFragment tmp (baseOpts tmp) { moIncludeMerged = [bad tmp], moAllowEmptyInclude = True })
      , ("include missing", "--include-merged file does not exist: "
        , \tmp -> withFragment tmp (baseOpts tmp) { moIncludeMerged = [bad tmp] })
      , ("include unparseable", "failed to parse"
        , \tmp -> BS.writeFile (bad tmp) ":::not::valid:::yaml:::"
            >> withFragment tmp (baseOpts tmp) { moIncludeMerged = [bad tmp] })
      , ("source dir missing, warning", "does not exist; treating as empty"
        , \tmp -> touchFile (tmp </> "src" </> "Foo.hs")
            >> withFragment tmp (baseOpts tmp) { moSourceDirs = [bad tmp, tmp </> "src"] })
      , ("source dir a file, warning", "is not a directory; treating as empty"
        , \tmp -> touchFile (bad tmp) >> touchFile (tmp </> "src" </> "Foo.hs")
            >> withFragment tmp (baseOpts tmp) { moSourceDirs = [bad tmp, tmp </> "src"] })
      , ("source dir missing, with a prune", "while a --source-dirs entry is unusable"
        , \tmp -> touchFile (tmp </> "src" </> "Other.hs")
            >> withFragment tmp (baseOpts tmp) { moSourceDirs = [bad tmp, tmp </> "src"] })
      , ("source dir a file, with a prune", "while a --source-dirs entry is unusable"
        , \tmp -> touchFile (bad tmp) >> touchFile (tmp </> "src" </> "Other.hs")
            >> withFragment tmp (baseOpts tmp) { moSourceDirs = [bad tmp, tmp </> "src"] })
      ]

-- | The file name of a fragment comes from the directory, and the YAML error text from its content.
testHostileFragmentFileNameEscaped :: IO ()
testHostileFragmentFileNameEscaped = withTmpProject $ \tmp -> do
  let broken = ":::not::valid:::yaml:::"
  createDirectoryIfMissing True (tmp </> "fragments")
  BS.writeFile (tmp </> "fragments" </> (hostileField <> ".yaml")) broken
  text <- mergeOutput tmp (baseOpts tmp)
  assertMentions "the message" "unparseable fragment" text
  assertHostileEscaped "the file name" text
  case YAML.decodeEither' broken :: Either YAML.ParseException Fragment of
    Left e  -> assertMentions "the YAML error" (show (YAML.prettyPrintParseException e)) text
    Right _ -> failWith "the fixture must not parse"

testOverrideMatchEmitted :: IO ()
testOverrideMatchEmitted = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "Gadgets.hs"); touchFile (src </> "AppDb.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Gadgets.yaml")
    (sampleFragment "Gadgets" "Gadgets.GadgetsT" "Gadgets")
  writeFragYaml (fragDir </> "AppDb.yaml") $
    Fragment
      { fragmentModule = "AppDb"
      , fragmentTables = []
      , fragmentDbEntityOverrides =
          [ DbEntityOverride
              { dbField              = "gadgets"
              , dbTableType          = "GadgetsT"
              , sqlName              = "Gadgets"     -- matches tableName
              , overrideSourceModule = "AppDb"
              , overrideSourceFile   = "src/AppDb.hs"
              }
          ]
      }
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)

testOverrideMismatchFatal :: IO ()
testOverrideMismatchFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "Gadgets.hs"); touchFile (src </> "AppDb.hs")
  createDirectoryIfMissing True fragDir
  -- modelTableName = "Gadgets" but setEntityName claims "DifferentName".
  writeFragYaml (fragDir </> "Gadgets.yaml")
    (sampleFragment "Gadgets" "Gadgets.GadgetsT" "Gadgets")
  writeFragYaml (fragDir </> "AppDb.yaml") $
    Fragment
      { fragmentModule = "AppDb"
      , fragmentTables = []
      , fragmentDbEntityOverrides =
          [ DbEntityOverride
              { dbField              = "gadgets"
              , dbTableType          = "GadgetsT"
              , sqlName              = "DifferentName"
              , overrideSourceModule = "AppDb"
              , overrideSourceFile   = "src/AppDb.hs"
              }
          ]
      }
  result <- runMerge (baseOpts tmp) { moSourceDirs = [src] }
  case result of
    Right _   -> failWith "expected mismatch to abort merge, but it succeeded"
    Left errs ->
      unless (any ("setEntityName override disagrees" `isInfixOf`) errs) $
        failWith ("expected disagreement error, got: " <> show errs)

-- | Same haskellType in two fragments with identical content → dedup,
-- no error.  This is the production case: a transitively-included dep
-- contract restates tables the dep already published once.
testDedupIdenticalSilent :: IO ()
testDedupIdenticalSilent = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "Foo.hs")
  createDirectoryIfMissing True fragDir
  let frag = sampleFragment "Foo" "Foo.FooT" "foo"
  writeFragYaml (fragDir </> "Foo.yaml") frag
  -- Include a pre-merged contract that contains the SAME table.
  let included = MergedSchema
        { mergedTables = fragmentTables frag
        , mergedDbEntityOverrides = []
        , generator = Nothing
        }
      includedPath = tmp </> "dep.yaml"
  BS.writeFile includedPath (YAML.encode included)
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [includedPath], moSourceDirs = [src] }
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  assertEq "deduped" 1 (mrDeduped report)

-- | Same haskellType in two fragments with DIFFERENT content → fatal.
-- This is the diamond-dep-different-versions failure mode.
testDuplicateMismatchFatal :: IO ()
testDuplicateMismatchFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "Foo.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Foo.yaml")
    (sampleFragment "Foo" "Foo.FooT" "foo_v1")
  let included = MergedSchema
        { mergedTables = fragmentTables (sampleFragment "Foo" "Foo.FooT" "foo_v2")
        , mergedDbEntityOverrides = []
        , generator = Nothing
        }
  BS.writeFile (tmp </> "dep.yaml") (YAML.encode included)
  result <- runMerge (baseOpts tmp)
    { moIncludeMerged = [tmp </> "dep.yaml"], moSourceDirs = [src] }
  case result of
    Right _   -> failWith "expected mismatch to abort merge"
    Left errs ->
      unless (any ("conflicting definitions" `isInfixOf`) errs) $
        failWith ("expected conflict error, got: " <> show errs)

-- | Two different haskellTypes mapping to the same SQL tableName →
-- warning, not error.  Both tables emitted.
testTableNameWarning :: IO ()
testTableNameWarning = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "A.hs"); touchFile (src </> "B.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "A.yaml")
    (sampleFragment "A" "A.ThingT" "shared_table")
  writeFragYaml (fragDir </> "B.yaml")
    (sampleFragment "B" "B.OtherThingT" "shared_table")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src] }
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

-- | An upstream contract renames JobT and a downstream package models
-- its own JobT: both candidates already carry the overridden name.
testOverrideAmbiguousAgreeing :: IO ()
testOverrideAmbiguousAgreeing = withTmpProject $ \tmp -> do
  result <- runAmbiguousOverride tmp "Job"
  report <- expectOk result
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)

testOverrideAmbiguousConflictFatal :: IO ()
testOverrideAmbiguousConflictFatal = withTmpProject $ \tmp -> do
  result <- runAmbiguousOverride tmp "queue_job"
  case result of
    Right _   -> failWith "expected a disagreeing ambiguous override to abort merge"
    Left errs ->
      unless (any ("Ambiguous dbEntityOverride" `isInfixOf`) errs) $
        failWith ("expected ambiguity error, got: " <> show errs)

-- | Merge a downstream fragment table (named @downstreamName@) with an included
-- contract whose override targets a same-named table type.
runAmbiguousOverride :: FilePath -> String -> IO (Either [String] MergeReport)
runAmbiguousOverride tmp downstreamName = do
  let fragDir = tmp </> "fragments"
      included = MergedSchema
        { mergedTables = [ sampleTable "App.Jobs.JobT" "App.Jobs" "Job" ]
        , mergedDbEntityOverrides =
            [ DbEntityOverride
                { dbField              = "job"
                , dbTableType          = "JobT"
                , sqlName              = "Job"
                , overrideSourceModule = "App.AppDb"
                , overrideSourceFile   = "src/App/AppDb.hs"
                }
            ]
        , generator = Nothing
        }
      includedPath = tmp </> "app-db.yaml"
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Queue.Jobs.yaml")
    (sampleFragment "Queue.Jobs" "Queue.Jobs.JobT" downstreamName)
  BS.writeFile includedPath (YAML.encode included)
  runMerge (baseOpts tmp) { moIncludeMerged = [includedPath] }

testIncludeMerged :: IO ()
testIncludeMerged = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
  createDirectoryIfMissing True fragDir
  -- Empty fragments dir, but two tables come in via --include-merged.
  let included = MergedSchema
        { mergedTables =
            [ sampleTable "A.AT" "A" "a_tab"
            , sampleTable "B.BT" "B" "b_tab"
            ]
        , mergedDbEntityOverrides = []
        , generator = Nothing
        }
      includedPath = tmp </> "dep.yaml"
  BS.writeFile includedPath (YAML.encode included)
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [includedPath], moNoOwnTables = True }
  assertEq "includedFiles" 1 (mrIncludedFiles report)
  assertEq "includedTables" 2 (mrIncludedTables report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

testGeneratorStamped :: IO ()
testGeneratorStamped = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      opts rev = (baseOpts tmp) { moGeneratorRev = rev }
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Foo.yaml") (sampleFragment "Foo" "Foo.FooT" "foo")
  _ <- expectOk =<< runMerge (opts (Just "abc123"))
  merged <- decodeFile outFile
  assertEq "generator" (Just (currentGenerator (Just "abc123"))) (generator merged)
  let g = currentGenerator (Just "abc123")
  assertEq "generatorName" "sql-schema" (generatorName g)
  assertEq "generatorRev" (Just "abc123") (generatorRev g)
  let v = generatorVersion g
  unless (not (null v) && all (`elem` ("0123456789." :: String)) v) $
    failWith ("generatorVersion not digits and dots: " <> show v)
  _ <- expectOk =<< runMerge (opts Nothing)
  merged' <- decodeFile outFile
  assertEq "rev Nothing" (Just (currentGenerator Nothing)) (generator merged')

-- | Contracts written before 0.4.3 have no @generator@ key.
testIncludeWithoutGenerator :: IO ()
testIncludeWithoutGenerator = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      includedPath = tmp </> "old.yaml"
  createDirectoryIfMissing True fragDir
  BS.writeFile includedPath (YAML.encode (YAML.object
    [ "mergedTables" YAML..= [sampleTable "A.AT" "A" "a_tab"]
    , "mergedDbEntityOverrides" YAML..= ([] :: [DbEntityOverride])
    ]))
  report <- expectOk =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [includedPath], moNoOwnTables = True }
  assertEq "includedTables" 1 (mrIncludedTables report)
  merged <- decodeFile outFile
  assertEq "tables" ["A.AT"] (map qualifiedType (mergedTables merged))

-- | The output carries this merge's stamp, never an included contract's.
testIncludedStampIgnored :: IO ()
testIncludedStampIgnored = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      includedPath = tmp </> "upstream.yaml"
  createDirectoryIfMissing True fragDir
  BS.writeFile includedPath (YAML.encode MergedSchema
    { mergedTables            = [sampleTable "A.AT" "A" "a_tab"]
    , mergedDbEntityOverrides = []
    , generator               = Just (Generator "sql-schema" "0.0.1" (Just "old"))
    })
  _ <- expectOk =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [includedPath], moGeneratorRev = Just "new", moNoOwnTables = True }
  merged <- decodeFile outFile
  assertEq "generator" (Just (currentGenerator (Just "new"))) (generator merged)

-- | A missing --include-merged file would drop every table it holds.
testIncludeMergedMissingFile :: IO ()
testIncludeMergedMissingFile = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "fragments")
  expectMergeError "does not exist" =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [tmp </> "does-not-exist.yaml"] }

-- | So would a malformed one; the YAML error text is printed escaped.
testIncludeMergedMalformedFile :: IO ()
testIncludeMergedMalformedFile = withTmpProject $ \tmp -> do
  let bad    = tmp </> "broken.yaml"
      broken = ":::not::valid:::yaml:::"
  createDirectoryIfMissing True (tmp </> "fragments")
  BS.writeFile bad broken
  result <- runMerge (baseOpts tmp) { moIncludeMerged = [bad] }
  expectMergeError "failed to parse" result
  case YAML.decodeEither' broken :: Either YAML.ParseException MergedSchema of
    Left e  -> expectMergeError (show (YAML.prettyPrintParseException e)) result
    Right _ -> failWith "the fixture must not parse"

-- | A build that only staged (-fno-code, haddock) leaves @.pending@ files, and a
-- killed stage a @.tmp@ file; the merge must read neither.
testMergeIgnoresStagedFiles :: IO ()
testMergeIgnoresStagedFiles = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "A.yaml") (sampleFragment "A" "A.AT" "a_tab")
  writeFragYaml (fragDir </> "B.yaml.pending") (sampleFragment "B" "B.BT" "b_tab")
  writeFragYaml (fragDir </> "C.yaml.pending.tmp") (sampleFragment "C" "C.CT" "c_tab")
  writeFragYaml (fragDir </> "E.yaml.pending4821-7.tmp") (sampleFragment "E" "E.ET" "e_tab")
  BS.writeFile (fragDir </> "D.yaml.pending") ""
  report <- expectOk =<< runMerge (baseOpts tmp)
  assertEq "fragmentsRead" 1 (mrFragmentsRead report)
  merged <- decodeFile (tmp </> "merged.yaml")
  assertEq "tables" ["A.AT"] (map qualifiedType (mergedTables merged))
