{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the merge CLI's library logic in 'SqlSchema.Merge', plus
-- the helpers exported by 'SqlSchema.Plugin': the column-name helper,
-- 'parseCliOptions', 'optionsFingerprint' and 'syncFragment'.
-- Plugin-side extraction runs inside a GHC compile, so it is exercised by
-- compiling model modules with the plugin, not by this suite.
module Main (main) where

import           Control.Exception   (IOException, throwIO, try, ErrorCall(..))
import           Control.Monad       (unless, when)
import qualified Data.ByteString     as BS
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing, doesFileExist)
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory)

import           SqlSchema.Merge
import           SqlSchema.Plugin    (beamDefaultColumnName, optionsFingerprint, parseCliOptions,
                                      syncFragment)
import           SqlSchema.Types     hiding (beamDefaultColumnName)

main :: IO ()
main = do
  testHappyPath
  testStalePruneByModule
  testNoSourceDirsNoPrune
  testSourceDirTrailingSlash
  testAllFragmentsPrunedFatal
  testMissingSourceDirNotFatal
  testUnparseableFragmentFatal
  testUnparseableLiveFragmentFatal
  testUnparseableStaleFragmentPruned
  testIncludeOnlyWithSourceDirs
  testAllPrunedWithIncludeFatal
  testOtherSourceExtensionsLive
  testOwnTablesCounted
  testPublishProblem
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
  testBeamDefaultColumnName
  testParseCliOptions
  testOptionsFingerprint
  testSyncFragment
  putStrLn "sql-schema-test: all cases passed"

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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
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
      outFile = tmp </> "merged.yaml"
  createDirectoryIfMissing True fragDir
  -- No source-dirs given: even a fragment with no backing .hs is kept.
  writeFragYaml (fragDir </> "Orphan.yaml")
    (sampleFragment "Orphan" "Orphan.OrphanT" "orphan")
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = []         -- opt out of pruning
    , moGeneratorRev   = Nothing
    }
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

-- | A listed source dir may not exist (a .cabal can name one); that only warns.
testMissingSourceDirNotFatal :: IO ()
testMissingSourceDirNotFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      src     = tmp </> "src"
  touchFile (src </> "Alive.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Alive.yaml") (sampleFragment "Alive" "Alive.AliveT" "alive")
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src, tmp </> "absent"] }
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

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
  report <- expectOk =<< runMerge (baseOpts tmp) { moSourceDirs = [src], moIncludeMerged = [inc] }
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

testOverrideMatchEmitted :: IO ()
testOverrideMatchEmitted = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)

testOverrideMismatchFatal :: IO ()
testOverrideMismatchFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
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
  result <- runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
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
      outFile = tmp </> "merged.yaml"
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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  assertEq "deduped" 1 (mrDeduped report)

-- | Same haskellType in two fragments with DIFFERENT content → fatal.
-- This is the diamond-dep-different-versions failure mode.
testDuplicateMismatchFatal :: IO ()
testDuplicateMismatchFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
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
  result <- runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [tmp </> "dep.yaml"]
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
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
      outFile = tmp </> "merged.yaml"
      src     = tmp </> "src"
  createDirectoryIfMissing True src
  touchFile (src </> "A.hs"); touchFile (src </> "B.hs")
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "A.yaml")
    (sampleFragment "A" "A.ThingT" "shared_table")
  writeFragYaml (fragDir </> "B.yaml")
    (sampleFragment "B" "B.OtherThingT" "shared_table")
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = []
    , moSourceDirs     = [src]
    , moGeneratorRev   = Nothing
    }
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
  runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = tmp </> "merged.yaml"
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = []
    , moGeneratorRev   = Nothing
    }

testIncludeMerged :: IO ()
testIncludeMerged = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = []
    , moGeneratorRev   = Nothing
    }
  assertEq "includedFiles" 1 (mrIncludedFiles report)
  assertEq "includedTables" 2 (mrIncludedTables report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

testGeneratorStamped :: IO ()
testGeneratorStamped = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      opts rev = MergeOptions
        { moFragmentsDir   = fragDir
        , moOutFile        = outFile
        , moIncludeMerged  = []
        , moSourceDirs     = []
        , moGeneratorRev   = rev
        }
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
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = []
    , moGeneratorRev   = Nothing
    }
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
  _ <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = []
    , moGeneratorRev   = Just "new"
    }
  merged <- decodeFile outFile
  assertEq "generator" (Just (currentGenerator (Just "new"))) (generator merged)

-- | A missing --include-merged file would drop every table it holds.
testIncludeMergedMissingFile :: IO ()
testIncludeMergedMissingFile = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "fragments")
  expectMergeError "does not exist" =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [tmp </> "does-not-exist.yaml"] }

-- | So would a malformed one.
testIncludeMergedMalformedFile :: IO ()
testIncludeMergedMalformedFile = withTmpProject $ \tmp -> do
  let bad = tmp </> "broken.yaml"
  createDirectoryIfMissing True (tmp </> "fragments")
  BS.writeFile bad ":::not::valid:::yaml:::"
  expectMergeError "failed to parse" =<< runMerge (baseOpts tmp)
    { moIncludeMerged = [bad] }

-- | Expected values: beam-core's @defaultFieldName (sel :| [])@, juspay/beam 25f1bcd960.
testBeamDefaultColumnName :: IO ()
testBeamDefaultColumnName = mapM_ check
  [ ("id", "id"), ("_id", "id"), ("enabled", "enabled"), ("widgetEnabled", "enabled")
  , ("ownerTag", "tag"), ("shopAccountId", "account_id"), ("_userFirstName", "first_name")
  , ("item_kind", "item_kind"), ("_x_y", "x_y"), ("_", "_"), ("__", "__"), ("URLPath", "ath")
  , ("userURL", "u_r_l"), ("allow3DmodeHelpText", "dmode_help_text"), ("level2XY", "x_y")
  , ("bigWordwithoutcaps", "wordwithoutcaps"), ("a", "a"), ("A", "a"), ("aB", "b")
  , ("createdAt", "at")
  -- U+0130 expands to "i" + U+0307 in a component (Text) but not as a head char (Char)
  , ("\304B", "i\775b"), ("a\304", "i")
  ]
  where
    check (sel, expected) = assertEq sel expected (beamDefaultColumnName sel)

testParseCliOptions :: IO ()
testParseCliOptions = do
  assertEq "no options" (Right defaultCliOptions) (parseCliOptions [])
  assertEq "fragmentsDir round-trips"
    (Right defaultCliOptions { fragmentsDir = "x" })
    (parseCliOptions ["{\"fragmentsDir\":\"x\"}"])
  case parseCliOptions ["{\"fragmentDir\":\"x\"}"] of
    Left msg | "fragmentDir" `isInfixOf` msg -> pure ()
    other -> failWith ("typo'd key: expected Left naming fragmentDir, got " <> show other)
  assertLeft "not json" (parseCliOptions ["not json"])
  assertLeft "two option strings"
    (parseCliOptions ["{\"fragmentsDir\":\"a\"}", "{\"blacklistModules\":[]}"])
  where
    assertLeft label = \case
      Left _  -> pure ()
      Right v -> failWith (label <> ": expected Left, got Right " <> show v)

testOptionsFingerprint :: IO ()
testOptionsFingerprint = do
  let a = ["{\"fragmentsDir\":\"a\"}"]
      b = ["{\"fragmentsDir\":\"b\"}"]
  assertEq "same options" (optionsFingerprint a) (optionsFingerprint a)
  when (optionsFingerprint a == optionsFingerprint b) $
    failWith "different options gave equal fingerprints"

-- | A recompiled module with nothing to record (or a blacklisted one) must
-- not leave its earlier fragment behind for the merge to pick up.
testSyncFragment :: IO ()
testSyncFragment = withTmpProject $ \tmp -> do
  let path = tmp </> "fragments" </> "Foo.yaml"
      frag = sampleFragment "Foo" "Foo.FooT" "foo"
  syncFragment path (Just frag)
  bs <- BS.readFile path
  assertEq "written" (Right frag) (either (Left . show) Right (YAML.decodeEither' bs))
  syncFragment path Nothing
  stale <- doesFileExist path
  when stale (failWith "stale fragment not removed")
  syncFragment path Nothing
  -- Only a missing file is ignored; any other failure must fail the compile.
  let dirPath = tmp </> "fragments" </> "Dir.yaml"
  createDirectoryIfMissing True dirPath
  r <- try (syncFragment dirPath Nothing)
  case r of
    Left (_ :: IOException) -> pure ()
    Right ()                -> failWith "deleting a directory fragment path did not fail"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleFragment :: String -> String -> String -> Fragment
sampleFragment modName ht tname = Fragment
  { fragmentModule = modName
  , fragmentTables = [ sampleTable ht modName tname ]
  , fragmentDbEntityOverrides = []
  }

sampleTable :: String -> String -> String -> TableSchema
sampleTable ht modName tname = TableSchema
  { codeName        = drop (length modName + 1) ht
  , sourceModule    = modName
  , modelTableName  = tname
  , modelTableType  = Just "CONFIG"
  , modelSchemaName = Nothing
  , primaryKey      = PrimaryKeyInfo "Id" ["id"]
  , columns         =
      [ ColumnInfo
          { hsField = "id", column = "id"
          , hsType = "Int", nullable = False
          , isPrimaryKey = True
          }
      ]
  }

-- | Options for a merge in @tmp@: fragments in @tmp/fragments@, no includes,
-- no pruning.
baseOpts :: FilePath -> MergeOptions
baseOpts tmp = MergeOptions
  { moFragmentsDir  = tmp </> "fragments"
  , moOutFile       = tmp </> "merged.yaml"
  , moIncludeMerged = []
  , moSourceDirs    = []
  , moGeneratorRev  = Nothing
  }

-- | A valid upstream contract with one table.
writeIncluded :: FilePath -> IO ()
writeIncluded p = BS.writeFile p (YAML.encode MergedSchema
  { mergedTables            = [sampleTable "Up.UpT" "Up" "up_tab"]
  , mergedDbEntityOverrides = []
  , generator               = Nothing
  })

expectMergeError :: String -> Either [String] MergeReport -> IO ()
expectMergeError needle = \case
  Left errs | any (needle `isInfixOf`) errs -> pure ()
  Left errs -> failWith ("expected an error containing " <> show needle <> ", got " <> show errs)
  Right _   -> failWith ("expected an error containing " <> show needle <> ", but the merge succeeded")

withTmpProject :: (FilePath -> IO a) -> IO a
withTmpProject = withSystemTempDirectory "sql-schema-test"

touchFile :: FilePath -> IO ()
touchFile p = do
  createDirectoryIfMissing True (takeDir p)
  BS.writeFile p ""
  where
    takeDir = reverse . dropWhile (/= '/') . reverse

writeFragYaml :: FilePath -> Fragment -> IO ()
writeFragYaml p f = BS.writeFile p (YAML.encode f)

decodeFile :: FilePath -> IO MergedSchema
decodeFile p = do
  bs <- BS.readFile p
  case YAML.decodeEither' bs of
    Right v -> pure v
    Left e  -> failWith ("decode " <> p <> ": " <> show e)

expectOk :: Show err => Either err a -> IO a
expectOk (Right a)   = pure a
expectOk (Left err)  = failWith ("expected Right, got Left: " <> show err)

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual
  | expected == actual = pure ()
  | otherwise          = failWith
      (label <> ": expected " <> show expected <> ", got " <> show actual)

failWith :: String -> IO a
failWith msg = throwIO (ErrorCall ("sql-schema-test failure: " <> msg))

isInfixOf :: String -> String -> Bool
isInfixOf needle hay = needle `elem` map (take (length needle)) (suffixes hay)
  where
    suffixes [] = [[]]
    suffixes xs@(_:xs') = xs : suffixes xs'
