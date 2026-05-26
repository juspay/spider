{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the merge CLI's library logic in 'SqlSchema.Merge'.
-- Plugin-side extraction is exercised by downstream builds (a synthetic
-- Beam fixture would need beam-core + sequelize; not worth the dep weight
-- for what would still be a coarse-grained test).
module Main (main) where

import           Control.Exception   (throwIO, ErrorCall(..))
import           Control.Monad       (unless)
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
  testStalePruned
  testOverrideMatchEmitted
  testOverrideMismatchFatal
  testDuplicateHaskellTypeFatal
  putStrLn "sql-schema-test: all cases passed"

-- ---------------------------------------------------------------------------
-- Test cases
-- ---------------------------------------------------------------------------

testHappyPath :: IO ()
testHappyPath = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      foo     = "src/Foo.hs"
      bar     = "src/Bar.hs"
  touchFile (tmp </> foo)
  touchFile (tmp </> bar)
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Foo.yaml")
    (sampleFragment "Foo" foo "Foo.FooT" "foo")
  writeFragYaml (fragDir </> "Bar.yaml")
    (sampleFragment "Bar" bar "Bar.BarT" "bar")
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir = fragDir
    , moOutFile      = outFile
    , moProjectRoot  = Just tmp
    }
  assertEq "fragmentsRead" 2 (mrFragmentsRead report)
  assertEq "stalePruned" 0 (mrStalePruned report)
  assertEq "tablesEmitted" 2 (mrTablesEmitted report)
  merged <- decodeFile outFile
  assertEq "merged.tables.size" 2 (length (mergedTables merged))
  let names = map tableName (mergedTables merged)
  unless (names == ["bar", "foo"]) $
    failWith ("expected sorted ['bar','foo'] got " <> show names)

testStalePruned :: IO ()
testStalePruned = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      gone    = "src/Gone.hs"   -- intentionally not created
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Gone.yaml")
    (sampleFragment "Gone" gone "Gone.GoneT" "gone")
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir = fragDir
    , moOutFile      = outFile
    , moProjectRoot  = Just tmp
    }
  assertEq "stalePruned" 1 (mrStalePruned report)
  assertEq "tablesEmitted" 0 (mrTablesEmitted report)

testOverrideMatchEmitted :: IO ()
testOverrideMatchEmitted = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      gadgets  = "src/Gadgets.hs"
      db      = "src/AppDb.hs"
  touchFile (tmp </> gadgets); touchFile (tmp </> db)
  createDirectoryIfMissing True fragDir
  writeFragYaml (fragDir </> "Gadgets.yaml") $
    (sampleFragment "Gadgets" gadgets "Gadgets.GadgetsT" "Gadgets")
  writeFragYaml (fragDir </> "AppDb.yaml") $
    Fragment
      { fragmentModule = "AppDb"
      , fragmentFile   = db
      , fragmentTables = []
      , fragmentDbEntityOverrides =
          [ DbEntityOverride
              { dbField              = "gadgets"
              , dbTableType          = "GadgetsT"
              , sqlName              = "Gadgets"     -- matches tableName
              , overrideSourceModule = "AppDb"
              , overrideSourceFile   = db
              }
          ]
      }
  report <- expectOk =<< runMerge MergeOptions
    { moFragmentsDir = fragDir
    , moOutFile      = outFile
    , moProjectRoot  = Just tmp
    }
  assertEq "overridesEmitted" 1 (mrOverridesEmitted report)

testOverrideMismatchFatal :: IO ()
testOverrideMismatchFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      gadgets  = "src/Gadgets.hs"
      db      = "src/AppDb.hs"
  touchFile (tmp </> gadgets); touchFile (tmp </> db)
  createDirectoryIfMissing True fragDir
  -- modelTableName = "Gadgets" but setEntityName claims "DifferentName".
  writeFragYaml (fragDir </> "Gadgets.yaml") $
    sampleFragment "Gadgets" gadgets "Gadgets.GadgetsT" "Gadgets"
  writeFragYaml (fragDir </> "AppDb.yaml") $
    Fragment
      { fragmentModule = "AppDb"
      , fragmentFile   = db
      , fragmentTables = []
      , fragmentDbEntityOverrides =
          [ DbEntityOverride
              { dbField              = "gadgets"
              , dbTableType          = "GadgetsT"
              , sqlName              = "DifferentName"   -- ≠ tableName
              , overrideSourceModule = "AppDb"
              , overrideSourceFile   = db
              }
          ]
      }
  result <- runMerge MergeOptions
    { moFragmentsDir = fragDir
    , moOutFile      = outFile
    , moProjectRoot  = Just tmp
    }
  case result of
    Right _   -> failWith "expected mismatch to abort merge, but it succeeded"
    Left errs ->
      unless (any ("setEntityName override disagrees" `isInfixOf`) errs) $
        failWith ("expected disagreement error, got: " <> show errs)

testDuplicateHaskellTypeFatal :: IO ()
testDuplicateHaskellTypeFatal = withTmpProject $ \tmp -> do
  let fragDir = tmp </> "fragments"
      outFile = tmp </> "merged.yaml"
      a       = "src/A.hs"
      b       = "src/B.hs"
  touchFile (tmp </> a); touchFile (tmp </> b)
  createDirectoryIfMissing True fragDir
  -- Two different modules claiming the same haskellType.
  writeFragYaml (fragDir </> "A.yaml")
    (sampleFragment "A" a "Dup.DupT" "dup_a")
  writeFragYaml (fragDir </> "B.yaml")
    (sampleFragment "B" b "Dup.DupT" "dup_b")
  result <- runMerge MergeOptions
    { moFragmentsDir = fragDir
    , moOutFile      = outFile
    , moProjectRoot  = Just tmp
    }
  case result of
    Right _   -> failWith "expected duplicate to abort merge"
    Left errs ->
      unless (any ("Duplicate haskellType" `isInfixOf`) errs) $
        failWith ("expected duplicate error, got: " <> show errs)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleFragment :: String -> FilePath -> String -> String -> Fragment
sampleFragment modName srcFile ht tname = Fragment
  { fragmentModule = modName
  , fragmentFile   = srcFile
  , fragmentTables =
      [ TableSchema
          { haskellType    = ht
          , sourceModule   = modName
          , sourceFile     = srcFile
          , tableName      = tname
          , modelTableType = Just "CONFIG"
          , primaryKey     = PrimaryKeyInfo "Id" ["id"]
          , columns        =
              [ ColumnInfo { hsField = "id", column = "id"
                           , hsType = "Int", nullable = False
                           , isPrimaryKey = True
                           }
              ]
          }
      ]
  , fragmentDbEntityOverrides = []
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
