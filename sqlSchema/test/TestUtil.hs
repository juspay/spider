{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Assertions, temporary projects and sample contract values shared by the test groups.
module TestUtil
  ( assertEq
  , assertLeft
  , failWith
  , expectOk
  , expectMergeError
  , withTmpProject
  , touchFile
  , writeFragYaml
  , decodeFile
  , isInfixOf
  , sampleFragment
  , sampleTable
  , baseOpts
  , writeIncluded
  ) where

import           Control.Exception   (throwIO, ErrorCall(..))
import qualified Data.ByteString     as BS
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory)

import           SqlSchema.Merge
import           SqlSchema.Types

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

assertLeft :: Show b => String -> Either a b -> IO ()
assertLeft label = \case
  Left _  -> pure ()
  Right v -> failWith (label <> ": expected Left, got Right " <> show v)
