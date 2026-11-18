{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Assertions, temporary projects and sample contract values shared by the test groups.
module TestUtil
  ( assertEq
  , assertLeft
  , assertAbsent
  , assertMentions
  , assertLacks
  , whenWithheld
  , whenRefused
  , linkChain
  , refusesToFollow
  , redirected
  , withStderr
  , withinSeconds
  , hostileField
  , assertHostileEscaped
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
  , columnlessFragment
  , overrideFragment
  , baseOpts
  , writeIncluded
  , writeEmptyIncluded
  ) where

import           Control.Exception     (ErrorCall (..), finally, throwIO)
import           Control.Monad         (forM_, unless, when)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Yaml             as YAML
import           Foreign.C.Error       (Errno (..), eLOOP)
import           GHC.IO.Exception      (IOException (ioe_errno))
import           GHC.IO.Handle         (hDuplicate, hDuplicateTo)
import           System.Directory      (createDirectoryIfMissing, createDirectoryLink, createFileLink,
                                        doesPathExist, getPermissions)
import           System.FilePath       (addTrailingPathSeparator, (</>))
import           System.IO             (Handle, IOMode (WriteMode), hClose, hFlush, hPutStrLn,
                                        openFile, stderr)
import           System.IO.Error       (tryIOError)
import           System.IO.Temp        (withSystemTempDirectory)
import           System.Timeout        (timeout)

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

-- | What a record rewritten by another plugin leaves behind, once @allowUnknownColumnWrapper@
-- drops its fields: a table with no columns.
columnlessFragment :: Fragment
columnlessFragment = Fragment
  { fragmentModule = "LR"
  , fragmentTables =
      [ (sampleTable "LR.FooT" "LR" "foo")
          { primaryKey = PrimaryKeyInfo "FooId" ["fooId"], columns = [] } ]
  , fragmentDbEntityOverrides = []
  }

overrideFragment :: String -> Fragment
overrideFragment ty = Fragment
  { fragmentModule = "Db"
  , fragmentTables = []
  , fragmentDbEntityOverrides =
      [ DbEntityOverride
          { dbField = "f", dbTableType = ty, sqlName = "renamed_tab"
          , overrideSourceModule = "Db", overrideSourceFile = "src/x.hs" } ]
  }

-- | Options for a merge in @tmp@: fragments in @tmp/fragments@, no includes,
-- no pruning.
baseOpts :: FilePath -> MergeOptions
baseOpts tmp = defaultMergeOptions (tmp </> "fragments") (tmp </> "merged.yaml")

-- | A valid upstream contract with one table.
writeIncluded :: FilePath -> IO ()
writeIncluded p = BS.writeFile p (YAML.encode MergedSchema
  { mergedTables            = [sampleTable "Up.UpT" "Up" "up_tab"]
  , mergedDbEntityOverrides = []
  , generator               = Nothing
  })

writeEmptyIncluded :: FilePath -> IO ()
writeEmptyIncluded p = BS.writeFile p (YAML.encode MergedSchema
  { mergedTables            = []
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

assertAbsent :: FilePath -> IO ()
assertAbsent p = do
  present <- doesPathExist p
  when present (failWith ("expected no file at " <> p))

assertMentions, assertLacks :: String -> String -> String -> IO ()
assertMentions label needle text =
  unless (needle `isInfixOf` text) $
    failWith (label <> ": expected " <> show needle <> " in " <> show text)
assertLacks label needle text =
  when (needle `isInfixOf` text) $
    failWith (label <> ": unexpected " <> show needle <> " in " <> show text)

-- | Run @act@ when the permission a test withheld took effect.  Otherwise (root, or a mount
-- that ignores modes) say on stderr which guard this run leaves unpinned.
whenWithheld :: Bool -> String -> String -> IO () -> IO ()
whenWithheld withheld test guard act
  | withheld  = act
  | otherwise = hPutStrLn stderr
      ("SKIPPED " <> test <> ": the permission can't be withheld here (root, or a mount that ignores modes); not pinned: " <> guard)

redirected :: Handle -> FilePath -> IO a -> IO a
redirected h path act = do
  saved <- hDuplicate h
  file <- openFile path WriteMode
  hDuplicateTo file h
  hClose file
  act `finally` (hFlush h >> hDuplicateTo saved h >> hClose saved)

-- | Run @act@ and return what it wrote to stderr; @tmp@ holds the capture file.
withStderr :: FilePath -> IO a -> IO (a, String)
withStderr tmp act = do
  r <- redirected stderr (tmp </> "cli.stderr") act
  err <- BC.unpack <$> BS.readFile (tmp </> "cli.stderr")
  pure (r, err)

-- | Fail the test when @act@ takes longer than @secs@ seconds: the code under test may loop.
withinSeconds :: Int -> String -> IO a -> IO a
withinSeconds secs label act = timeout (secs * 1000000) act >>= \case
  Just a  -> pure a
  Nothing -> failWith (label <> ": did not finish within " <> show secs <> " s")

-- | A field value that forges a log line, then colours the terminal, when printed raw.
hostileField :: String
hostileField = "Evil\nsql-schema-merge: 9 fragments read\ESC[31m"

assertHostileEscaped :: String -> String -> IO ()
assertHostileEscaped label text = do
  assertMentions label (filter (/= '"') (show hostileField)) text
  assertLacks label "\nsql-schema-merge: 9 fragments read" text
  assertLacks label "\ESC" text

-- | Like 'whenWithheld', for a link chain the system must refuse to follow for the test to mean anything.
whenRefused :: Bool -> String -> String -> IO () -> IO ()
whenRefused refused test guard act =
  if refused then act
  else hPutStrLn stderr $ "SKIPPED " <> test <> ": the system follows a chain this long itself; not pinned: " <> guard

-- | Does the system refuse to resolve @p@ because it passes too many symbolic links?
refusesToFollow :: FilePath -> IO Bool
refusesToFollow p = tryIOError (getPermissions p) >>= \case
  Left e  -> pure ((Errno <$> ioe_errno e) == Just eLOOP)
  Right _ -> pure False

-- | @linkChain slash n dir target@ makes the links @dir/c1 -> c2 -> ... -> c<n> -> target@, each target
-- ending in a path separator when @slash@.
linkChain :: Bool -> Int -> FilePath -> FilePath -> IO ()
linkChain slash n dir target = do
  createDirectoryIfMissing True dir
  let end t = if slash then addTrailingPathSeparator t else t
  createDirectoryLink (end target) (dir </> ("c" <> show n))
  forM_ [n - 1, n - 2 .. 1] $ \i ->
    createFileLink (end ("c" <> show (i + 1))) (dir </> ("c" <> show i))
