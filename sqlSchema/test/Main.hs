{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the merge CLI's library logic in 'SqlSchema.Merge'.
-- Plugin-side extraction is exercised by downstream builds (a synthetic
-- Beam fixture would need beam-core + sequelize; not worth the dep weight
-- for what would still be a coarse-grained test).
module Main (main) where

import           Control.Exception   (throwIO, ErrorCall(..))
import           Control.Monad       (unless, when)
import qualified Data.ByteString     as BS
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory)

import           SqlSchema.Merge
import           SqlSchema.Types

main :: IO ()
main = do
  testHappyPath
  testStalePruneByModule
  testNoSourceDirsNoPrune
  testOverrideMatchEmitted
  testOverrideMismatchFatal
  testDedupIdenticalSilent
  testDuplicateMismatchFatal
  testTableNameWarning
  testIncludeMerged
  testIncludeMergedMissingFile
  testIncludeMergedMalformedFile
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
    }
  assertEq "fragmentsRead" 2 (mrFragmentsRead report)
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)
  merged <- decodeFile outFile
  assertEq "merged.tables.size" 2 (length (mergedTables merged))
  let names = map tableName (mergedTables merged)
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
    }
  assertEq "fragmentsRead" 2 (mrFragmentsRead report)
  assertEq "stalePruned" 1 (mrStalePruned report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)
  merged <- decodeFile outFile
  let names = map tableName (mergedTables merged)
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
    }
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 1 (mrTablesEmitted report)

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
        }
      includedPath = tmp </> "dep.yaml"
  BS.writeFile includedPath (YAML.encode included)
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = [src]
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
        }
  BS.writeFile (tmp </> "dep.yaml") (YAML.encode included)
  result <- runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [tmp </> "dep.yaml"]
    , moSourceDirs     = [src]
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
    }
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

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
        }
      includedPath = tmp </> "dep.yaml"
  BS.writeFile includedPath (YAML.encode included)
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [includedPath]
    , moSourceDirs     = []
    }
  assertEq "includedFiles" 1 (mrIncludedFiles report)
  assertEq "includedTables" 2 (mrIncludedTables report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)

-- | Missing --include-merged file → warning to stderr, merge still
-- succeeds with zero contribution from that file.
testIncludeMergedMissingFile :: IO ()
testIncludeMergedMissingFile = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
  createDirectoryIfMissing True fragDir
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [tmp </> "does-not-exist.yaml"]
    , moSourceDirs     = []
    }
  assertEq "tablesEmitted" 0 (mrTablesEmitted report)
  assertEq "includedFiles" 0 (mrIncludedFiles report)

-- | Malformed --include-merged file → warning, merge still succeeds.
testIncludeMergedMalformedFile :: IO ()
testIncludeMergedMalformedFile = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      bad     = tmp </> "broken.yaml"
  createDirectoryIfMissing True fragDir
  BS.writeFile bad ":::not::valid:::yaml:::"
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir   = fragDir
    , moOutFile        = outFile
    , moIncludeMerged  = [bad]
    , moSourceDirs     = []
    }
  assertEq "tablesEmitted" 0 (mrTablesEmitted report)
  -- Malformed file is reported as a warning and not counted as
  -- successfully included.
  when (mrIncludedFiles report /= 0) $
    failWith ("expected includedFiles=0 for malformed, got "
              <> show (mrIncludedFiles report))

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
  { haskellType    = ht
  , sourceModule   = modName
  , tableName      = tname
  , modelTableType = Just "CONFIG"
  , primaryKey     = PrimaryKeyInfo "Id" ["id"]
  , columns        =
      [ ColumnInfo
          { hsField = "id", column = "id"
          , hsType = "Int", nullable = False
          , isPrimaryKey = True
          }
      ]
  }

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
