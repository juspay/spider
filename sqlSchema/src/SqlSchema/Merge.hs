{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Library used by the @sql-schema-merge@ CLI.  Combines per-module
-- fragments written by 'SqlSchema.Plugin' (plus any pre-merged contracts
-- supplied via @--include-merged@) into the single contract YAML that
-- downstream prod-DB validation reads.
--
-- The merge step is the only place where cross-module invariants are
-- enforced:
--
--   * Stale fragments (whose module has no corresponding @.hs@ file in
--     any @--source-dirs@ entry) are pruned silently.  Path-based stale
--     detection was removed in 0.2.0 to make cross-repo composition work
--     without per-package projectRoot juggling.
--
--   * Duplicate @haskellType@ across fragments / included contracts is
--     handled by content-equality:
--       - All entries byte-equal       → silent dedup (this is the
--         common case for transitively-pulled dep contracts).
--       - Any pair of entries disagree → fatal error with a description
--         of the conflicting fields.  This means a diamond dep at two
--         different pinned versions surfaces as a build failure rather
--         than a silent picking of one side.
--
--   * Duplicate @tableName@ across distinct @haskellType@s emits a
--     warning, not an error.  Two repos legitimately modelling the
--     same SQL table from different perspectives is allowed; the
--     downstream diff tool decides policy.
--
--   * @setEntityName@ overrides are cross-checked against each table's
--     @modelTableName@; disagreement is fatal because Beam's runtime
--     uses the override and the YAML would otherwise misrepresent the
--     emitted SQL.
module SqlSchema.Merge
  ( MergeOptions(..)
  , MergeReport(..)
  , runMerge
  , currentGenerator
  , loadFragments
  , loadIncludedMerged
  , liveModulesFromDirs
  , pruneStaleByModule
  ) where

import           Control.Exception   (IOException, try)
import           Control.Monad       (forM)
import qualified Data.ByteString     as BS
import           Data.List           (isSuffixOf, sort, sortOn)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (catMaybes)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Yaml           as YAML
import           Data.Version        (showVersion)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory, renameFile)
import           System.FilePath     (dropExtension, takeExtension, (</>))
import           System.IO           (hPutStrLn, stderr)

import           SqlSchema.Types
import qualified Paths_sql_schema

data MergeOptions = MergeOptions
  { moFragmentsDir   :: FilePath
  , moOutFile        :: FilePath
  , moIncludeMerged  :: [FilePath]
    -- ^ Pre-merged contract YAMLs (typically dep packages' published
    --   @$out/share/sql-schema/sql-schema.yaml@) whose tables and
    --   overrides are unioned into the output.
  , moSourceDirs     :: [FilePath]
    -- ^ Directories to scan for live @.hs@ files.  Any fragment whose
    --   @fragmentModule@ does not map to a file under one of these dirs
    --   is pruned.  Empty list = no pruning (every fragment kept).
  , moGeneratorRev   :: Maybe String
    -- ^ Plugin source revision for the @generator@ stamp.
  } deriving (Show, Eq)

data MergeReport = MergeReport
  { mrFragmentsRead         :: Int
  , mrStalePruned           :: Int
  , mrIncludedFiles         :: Int
  , mrIncludedTables        :: Int
  , mrDeduped               :: Int     -- ^ Identical-content tables collapsed
  , mrTablesEmitted         :: Int
  , mrOverridesEmitted      :: Int
  } deriving (Show, Eq)


-- | The stamp 'runMerge' writes; the stamps of included contracts are ignored.
currentGenerator :: Maybe String -> Generator
currentGenerator rev = Generator
  { generatorName    = "sql-schema"
  , generatorVersion = showVersion Paths_sql_schema.version
  , generatorRev     = rev
  }

runMerge :: MergeOptions -> IO (Either [String] MergeReport)
runMerge MergeOptions{..} = do
  (read', frags)       <- loadFragments moFragmentsDir
  live                 <- liveModulesFromDirs moSourceDirs
  let (kept, pruned)   = pruneStaleByModule moSourceDirs live frags
  (incFiles, incTbls, incOvs, incWarns) <- loadIncludedMerged moIncludeMerged
  mapM_ (hPutStrLn stderr) incWarns
  let fragTables       = concatMap fragmentTables kept
      fragOverrides    = concatMap fragmentDbEntityOverrides kept
      allTables        = fragTables ++ incTbls
      allOverrides     = fragOverrides ++ incOvs
  case validate allTables allOverrides of
    Left errs -> pure (Left errs)
    Right (unstamped, deduped, warnings) -> do
      let merged = unstamped { generator = Just (currentGenerator moGeneratorRev) }
      mapM_ (hPutStrLn stderr) warnings
      writeMerged moOutFile merged
      pure $ Right MergeReport
        { mrFragmentsRead    = read'
        , mrStalePruned      = pruned
        , mrIncludedFiles    = incFiles
        , mrIncludedTables   = length incTbls
        , mrDeduped          = deduped
        , mrTablesEmitted    = length (mergedTables merged)
        , mrOverridesEmitted = length (mergedDbEntityOverrides merged)
        }


-- | Load every @*.yaml@ file in the given directory as a 'Fragment'.
-- Returns @(filesTried, parsed)@; files that fail to parse are reported
-- to stderr and skipped.  Missing directory → @(0, [])@.
loadFragments :: FilePath -> IO (Int, [Fragment])
loadFragments dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (0, [])
    else do
      mEntries <- try (listDirectory dir) :: IO (Either IOException [FilePath])
      case mEntries of
        Left e -> do
          hPutStrLn stderr $
            "sql-schema-merge: cannot read fragments directory " <> dir
            <> ": " <> show e
          pure (0, [])
        Right names -> do
          let yamls = sort [dir </> n | n <- names, takeExtension n == ".yaml"]
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


-- | Walk every directory in the list; collect dotted module names from
-- every @.hs@ file found beneath.  A missing directory is logged to
-- stderr but otherwise ignored (treated as empty).  Returned set is the
-- ground truth for @pruneStaleByModule@.
liveModulesFromDirs :: [FilePath] -> IO (Set String)
liveModulesFromDirs dirs = Set.unions <$> mapM oneDir dirs
  where
    oneDir d = do
      exists <- doesDirectoryExist d
      if not exists
        then do
          hPutStrLn stderr $
            "sql-schema-merge: warning: source dir " <> d
            <> " does not exist; treating as empty"
          pure Set.empty
        else Set.fromList . map (pathToModule d) <$> walkHs d
    pathToModule root p =
      -- root="pkg/gen", p="pkg/gen/Lib/Foo.hs"
      -- → "Lib.Foo"
      let rel = drop (length root + 1) p  -- strip "root/"
          noExt = dropExtension rel
          dotted = map (\c -> if c == '/' then '.' else c) noExt
      in dotted


-- | Depth-first walk of a directory, returning every @.hs@ file path.
walkHs :: FilePath -> IO [FilePath]
walkHs root = go root
  where
    go d = do
      mEntries <- try (listDirectory d) :: IO (Either IOException [FilePath])
      case mEntries of
        Left _ -> pure []
        Right names -> do
          let here = map (d </>) names
          fmap concat $ forM here $ \p -> do
            isDir <- doesDirectoryExist p
            if isDir
              then go p
              else
                if takeExtension p == ".hs" then pure [p] else pure []


-- | If 'moSourceDirs' was empty (caller opted out of pruning), keep
-- everything.  Otherwise drop any fragment whose @fragmentModule@ is not
-- in the live set.  Returns @(kept, prunedCount)@.
pruneStaleByModule
  :: [FilePath] -> Set String -> [Fragment] -> ([Fragment], Int)
pruneStaleByModule sourceDirs live frags
  | null sourceDirs = (frags, 0)
  | otherwise =
      let kept = [f | f <- frags, Set.member (fragmentModule f) live]
          dropped = [f | f <- frags, not (Set.member (fragmentModule f) live)]
      in (kept, length dropped)


-- | Parse each @--include-merged@ file as a 'MergedSchema' and return
-- the concatenated tables + overrides.  Missing/malformed files produce
-- warnings (not fatal); call sites that need fatality should validate
-- presence before invoking the merger.  Counts the files actually parsed.
loadIncludedMerged
  :: [FilePath]
  -> IO (Int, [TableSchema], [DbEntityOverride], [String])
loadIncludedMerged paths = do
  results <- forM paths $ \p -> do
    exists <- doesFileExist p
    if not exists
      then pure (Left ("--include-merged file does not exist: " <> p))
      else do
        bs <- BS.readFile p
        case YAML.decodeEither' bs of
          Left e ->
            pure (Left ("--include-merged file " <> p <> " failed to parse: "
                         <> YAML.prettyPrintParseException e))
          Right (m :: MergedSchema) ->
            pure (Right (mergedTables m, mergedDbEntityOverrides m))
  let okCount   = length [ () | Right _ <- results ]
      tables    = concat [ ts  | Right (ts, _) <- results ]
      overrides = concat [ os  | Right (_, os) <- results ]
      warns     = ["sql-schema-merge: warning: " <> w | Left w <- results]
  pure (okCount, tables, overrides, warns)


-- | Validate, dedup, and order.  Returns either a list of fatal errors,
-- or @(merged, dedupedCount, warnings)@.
validate
  :: [TableSchema]
  -> [DbEntityOverride]
  -> Either [String] (MergedSchema, Int, [String])
validate tables overrides =
  let groupedByHt :: Map.Map String [TableSchema]
      groupedByHt =
        Map.fromListWith (<>) [(qualifiedType t, [t]) | t <- tables]

      -- Dedup: if every entry in a group is byte-equal, keep one.
      -- Otherwise produce a mismatch error.
      resolvedGroups :: [Either String (TableSchema, Int)]
      resolvedGroups =
        [ resolveGroup ht ts | (ht, ts) <- Map.toList groupedByHt ]

      dupErrs    = [e        | Left e        <- resolvedGroups]
      uniqTables = [t        | Right (t, _)  <- resolvedGroups]
      dedupCount = sum [n - 1 | Right (_, n)  <- resolvedGroups, n > 1]

      -- Warn (don't fail) when distinct haskellTypes share a tableName.
      tableNameGroups =
        Map.fromListWith (<>) [(modelTableName t, [qualifiedType t]) | t <- uniqTables]
      tnWarns =
        [ "sql-schema-merge: warning: SQL table '" <> tn
            <> "' modelled by multiple Haskell types: "
            <> commaSep hts
        | (tn, hts) <- Map.toList tableNameGroups
        , length hts > 1
        ]

      -- Cross-check setEntityName overrides against tableName.
      checkOverride DbEntityOverride{..} =
        let matches = [t | t <- uniqTables
                         , dbTableType `isSuffixOfDot` qualifiedType t]
        in case matches of
             []  -> Right (Just (warnUnmatched dbTableType overrideSourceModule))
             [t] | modelTableName t == sqlName -> Right Nothing
                 | otherwise              -> Left (mismatchMsg t)
             -- A downstream package may model a same-named type; only a
             -- disagreeing candidate could be misreported.
             _   | all ((== sqlName) . modelTableName) matches -> Right Nothing
                 | otherwise -> Left (ambiguousMsg matches)
        where
          warnUnmatched tt mn =
            "sql-schema-merge: warning: dbEntityOverride for "
            <> tt <> " in " <> mn
            <> " has no matching table in any fragment"
          mismatchMsg t = unlines
            [ "setEntityName override disagrees with modelTableName:"
            , "  table:           " <> qualifiedType t
            , "  modelTableName:  " <> modelTableName t
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
            ] <> [ "  - " <> qualifiedType m | m <- ms ]

      overrideResults = map checkOverride overrides
      overrideErrs    = [e | Left e <- overrideResults]
      overrideWarns   = [w | Right (Just w) <- overrideResults]

      -- Dedup overrides too: identical entries collapse silently.
      dedupedOverrides =
        Map.elems $ Map.fromList
          [((overrideSourceModule o, dbField o, dbTableType o), o) | o <- overrides]

  in if not (null dupErrs)
       then Left dupErrs
       else if not (null overrideErrs)
              then Left overrideErrs
              else Right
                ( MergedSchema
                    { mergedTables = sortOn qualifiedType uniqTables
                    , mergedDbEntityOverrides = sortOn sortKey dedupedOverrides
                    , generator = Nothing
                    }
                , dedupCount
                , tnWarns ++ overrideWarns
                )
  where
    sortKey o = (overrideSourceModule o, dbField o, dbTableType o)


-- | All entries equal → keep one + size.  Any pair differs → describe
-- the disagreement.
resolveGroup :: String -> [TableSchema] -> Either String (TableSchema, Int)
resolveGroup _  [t]        = Right (t, 1)
resolveGroup ht ts@(t:_)
  | all (== t) ts = Right (t, length ts)
  | otherwise     = Left (mismatchMessage ht ts)
resolveGroup ht []         = Left ("internal: empty group for " <> ht)

mismatchMessage :: String -> [TableSchema] -> String
mismatchMessage ht ts = unlines $
  [ "Duplicate table type '" <> ht <> "' with conflicting definitions:"
  ] <> zipWith oneCopy [(1 :: Int)..] ts
  <> [ "All copies of the same Haskell type must agree exactly.  This"
     , "usually means two different pinned versions of the same upstream"
     , "package are reaching this merge (a diamond dep).  Align the"
     , "versions (nix flake follows) and retry."
     ]
  where
    oneCopy n t = unlines
      [ "  copy " <> show n <> ":"
      , "    sourceModule:    " <> sourceModule t
      , "    modelTableName:  " <> modelTableName t
      , "    modelTableType:  " <> show (modelTableType t)
      , "    modelSchemaName: " <> show (modelSchemaName t)
      , "    column count:    " <> show (length (columns t))
      , "    pk constructor:  " <> pkConstructor (primaryKey t)
      , "    pk columns:      " <> show (pkColumns (primaryKey t))
      ]


-- | Suffix match where the boundary must be a module-separator dot.
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
