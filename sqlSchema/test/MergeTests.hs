{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the merge library, 'SqlSchema.Merge'.
module MergeTests (run) where

import           Control.Monad       (unless)
import qualified Data.ByteString     as BS
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     ((</>))

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
