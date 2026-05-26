{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Library used by the @sql-schema-merge@ CLI.  Combines all per-module
-- fragments written by 'SqlSchema.Plugin' into the single contract YAML
-- that downstream prod-DB validation reads.
--
-- The merge step is the only place where cross-module invariants are
-- enforced:
--
--   * stale fragments (whose source file no longer exists) are pruned.
--   * duplicate Haskell-type names across fragments are rejected.
--   * @setEntityName@ overrides are cross-checked against each table's
--     @modelTableName@; any disagreement is fatal because Beam's runtime
--     uses the override and the YAML would otherwise misrepresent what
--     SQL Beam actually emits.
module SqlSchema.Merge
  ( MergeOptions(..)
  , MergeReport(..)
  , runMerge
  , loadFragments
  ) where

import           Control.Exception   (IOException, try)
import           Control.Monad       (filterM, forM)
import qualified Data.ByteString     as BS
import           Data.List           (isSuffixOf, sortOn)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (catMaybes)
import qualified Data.Yaml           as YAML
import           System.Directory    (doesFileExist, listDirectory,
                                      renameFile)
import           System.FilePath     (takeExtension, (</>))
import           System.IO           (hPutStrLn, stderr)

import           SqlSchema.Types

data MergeOptions = MergeOptions
  { moFragmentsDir :: FilePath
  , moOutFile      :: FilePath
  , moProjectRoot  :: Maybe FilePath
    -- ^ If given, every fragment's @sourceFile@ is resolved relative to
    --   this directory when checking staleness.  Defaults to 'Nothing'
    --   (paths are checked as-is).
  } deriving (Show, Eq)

data MergeReport = MergeReport
  { mrFragmentsRead    :: Int
  , mrStalePruned      :: Int     -- ^ source file missing → pruned
  , mrTablesEmitted    :: Int
  , mrOverridesEmitted :: Int
  } deriving (Show, Eq)


-- | Read every @*.yaml@ file in 'moFragmentsDir', filter out stale ones,
-- validate, and write the merged YAML atomically.  Either returns the
-- success report or a (non-empty) list of error messages — the caller
-- decides how to surface them (the CLI prints + exits non-zero).
runMerge :: MergeOptions -> IO (Either [String] MergeReport)
runMerge opts = do
  (read', frags) <- loadFragments (moFragmentsDir opts)
  alive <- filterM (sourceAlive (moProjectRoot opts)) frags
  let pruned = read' - length alive
  case validate alive of
    Left errs -> pure (Left errs)
    Right (merged, warnings) -> do
      mapM_ (hPutStrLn stderr) warnings
      writeMerged (moOutFile opts) merged
      pure $ Right MergeReport
        { mrFragmentsRead    = read'
        , mrStalePruned      = pruned
        , mrTablesEmitted    = length (mergedTables merged)
        , mrOverridesEmitted = length (mergedDbEntityOverrides merged)
        }


-- | Load every @*.yaml@ file in the given directory as a 'Fragment'.
-- Returns @(filesTried, parsed)@; files that fail to parse are reported
-- to stderr and skipped, on the theory that a corrupt fragment is the
-- caller's bug to fix and the merge should still produce as much of the
-- YAML as possible while making the breakage visible.
loadFragments :: FilePath -> IO (Int, [Fragment])
loadFragments dir = do
  exists <- doesFileExist dir
  if exists
    then pure (0, [])     -- 'dir' is a file, not a directory; nothing to do
    else do
      mEntries <- try (listDirectory dir) :: IO (Either IOException [FilePath])
      case mEntries of
        Left e -> do
          hPutStrLn stderr $
            "sql-schema-merge: cannot read fragments directory " <> dir
            <> ": " <> show e
          pure (0, [])
        Right names -> do
          let yamls = [dir </> n | n <- names, takeExtension n == ".yaml"]
          parsed <- forM yamls $ \p -> do
            bs <- BS.readFile p
            case YAML.decodeEither' bs of
              Right frag -> pure (Just frag)
              Left e     -> do
                hPutStrLn stderr $
                  "sql-schema-merge: skipping unparseable fragment " <> p
                  <> ": " <> YAML.prettyPrintParseException e
                pure Nothing
          pure (length yamls, catMaybes parsed)


sourceAlive :: Maybe FilePath -> Fragment -> IO Bool
sourceAlive mRoot Fragment{ fragmentFile } = do
  let path = case mRoot of
        Just root -> root </> fragmentFile
        Nothing   -> fragmentFile
  exists <- doesFileExist path
  if exists
    then pure True
    else do
      hPutStrLn stderr $
        "sql-schema-merge: pruning fragment whose source no longer exists: "
        <> fragmentFile
      pure False


-- | Returns @Right (merged, warnings)@ on success.  Warnings are
-- non-fatal and the caller is expected to surface them (the CLI prints
-- them to stderr).  Errors short-circuit and abort the merge.
validate :: [Fragment] -> Either [String] (MergedSchema, [String])
validate frags =
  let tables    = concatMap fragmentTables frags
      overrides = concatMap fragmentDbEntityOverrides frags

      -- Duplicate haskellType across two different sourceModules is fatal.
      -- (Same module twice would mean two fragments for the same source,
      -- which only happens if the fragments dir wasn't cleaned between
      -- builds of two different sources — also worth surfacing.)
      grouped = Map.fromListWith (<>) [(haskellType t, [t]) | t <- tables]
      dupErrs =
        [ "Duplicate haskellType " <> ht <> " seen in: " <>
            commaSep [sourceModule t <> " (" <> sourceFile t <> ")" | t <- ts]
        | (ht, ts) <- Map.toList grouped
        , length ts > 1
        ]

      tableByHt = Map.fromList [(haskellType t, t) | t <- tables]
      -- Cross-check setEntityName overrides against tableName.
      checkOverride DbEntityOverride{..} =
        let matches = [t | t <- tables
                         , dbTableType `isSuffixOfDot` haskellType t]
        in case matches of
             []  -> Right (Just (warnUnmatched dbTableType overrideSourceModule))
             [t] | tableName t == sqlName -> Right Nothing
                 | otherwise              -> Left (mismatchMsg t)
             _   -> Left (ambiguousMsg matches)
        where
          warnUnmatched tt mn =
            "sql-schema-merge: warning: dbEntityOverride for "
            <> tt <> " in " <> mn
            <> " has no matching table in any fragment"
          mismatchMsg t = unlines
            [ "setEntityName override disagrees with modelTableName:"
            , "  table:           " <> haskellType t
            , "  modelTableName:  " <> tableName t
            , "  setEntityName:   " <> sqlName
            , "  declared in:     " <> overrideSourceModule <> " (" <> overrideSourceFile <> ")"
            , "Beam's runtime uses setEntityName, so the YAML would misreport"
            , "the SQL table name.  Either fix the modelTableName or remove"
            , "the setEntityName override."
            ]
          ambiguousMsg ms = unlines $
            [ "Ambiguous dbEntityOverride for table-type suffix '"
              <> dbTableType <> "': matches "
              <> show (length ms) <> " tables:"
            ] <> [ "  - " <> haskellType m | m <- ms ]

      overrideResults = map checkOverride overrides
      overrideErrs    = [e | Left e <- overrideResults]
      overrideWarns   = [w | Right (Just w) <- overrideResults]

  in if not (null dupErrs)
       then Left dupErrs
       else if not (null overrideErrs)
              then Left overrideErrs
              else Right
                ( MergedSchema
                    { mergedTables = sortOn haskellType (Map.elems tableByHt)
                    , mergedDbEntityOverrides = sortOn sortKey overrides
                    }
                , overrideWarns
                )
  where
    sortKey o = (overrideSourceModule o, dbField o, dbTableType o)


-- | Suffix match where the boundary must be a module-separator dot.  So
-- @\"GadgetT\" `isSuffixOfDot` \"App.Storage.Gadgets.GadgetT\"@
-- is True, but @\"OtherGadgetT\"@ wouldn't match @\"GadgetT\"@'s qualifier.
isSuffixOfDot :: String -> String -> Bool
isSuffixOfDot needle hay =
  needle == hay || ('.' : needle) `isSuffixOf` hay


commaSep :: [String] -> String
commaSep = \case
  []     -> ""
  [x]    -> x
  (x:xs) -> x <> ", " <> commaSep xs


writeMerged :: FilePath -> MergedSchema -> IO ()
writeMerged path merged = do
  let tmp = path <> ".tmp"
  BS.writeFile tmp (YAML.encode merged)
  renameFile tmp path
