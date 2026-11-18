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
--     any @--source-dirs@ entry) are pruned, each named on stderr.  Path-based
--     stale detection was removed in 0.2.0 to make cross-repo composition work
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
--     emitted SQL.  Two overrides with one key (module, field, table type)
--     and different SQL names are fatal too, whatever the flags.
--
--   * An included contract with no tables, an override that matches no table and
--     a table with no columns are errors (a plugin did not run, or a check could
--     not); 'moAllowEmptyInclude', 'moAllowUnmatchedOverride' and
--     'moAllowColumnlessTable' make each a warning.
--
--   * Fragment-derived text (module, table and override names, YAML errors) is
--     printed through 'show', so a value holding a newline cannot forge a line.
--
--   * The output is written last, after every check above and 'publishProblem'
--     have passed, so a refused merge leaves an existing output as it was.
module SqlSchema.Merge
  ( MergeOptions(..)
  , defaultMergeOptions
  , MergeReport(..)
  , runMerge
  , PublishProblem(..)
  , publishProblem
  , refusalLines
  , currentGenerator
  , loadFragments
  , loadIncludedMerged
  , PathKind(..)
  , pathKind
  , SourceDirProblem(..)
  , liveModulesFromDirs
  , walkEntryCap
  , maxWalkDepth
  , maxLinkHops
  , pruneStaleByModule
  ) where

import           Control.Exception   (bracketOnError, finally)
import           Control.Monad       (forM, unless, when)
import qualified Data.ByteString     as BS
import           Data.IORef          (modifyIORef', newIORef, readIORef, writeIORef)
import           Data.List           (isSuffixOf, partition, sort, sortOn)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Yaml           as YAML
import           Data.Version        (showVersion)
import           Foreign.C.Error     (Errno (..), eLOOP)
import           GHC.IO.Exception    (IOException (ioe_errno))
import           System.Directory    (canonicalizePath, doesDirectoryExist, doesFileExist,
                                      getPermissions, getSymbolicLinkTarget, listDirectory,
                                      pathIsSymbolicLink, removeFile, renameFile, searchable)
import           System.FilePath     (dropExtension, dropTrailingPathSeparator, isAbsolute,
                                      makeRelative, splitDirectories, takeBaseName, takeDirectory,
                                      takeExtension, takeFileName, (</>))
import           System.IO           (Handle, hClose, hPutStrLn,
                                      openBinaryTempFileWithDefaultPermissions, stderr)
import           System.IO.Error     (isDoesNotExistError, tryIOError)

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
    --   is pruned.  Empty list = no pruning (every fragment kept).  An entry
    --   that is missing or not a directory fails the merge if anything is
    --   pruned, and otherwise only warns.
  , moWalkEntryCap   :: Int
    -- ^ The most steps the walk of one @--source-dirs@ entry may take ('walkEntryCap').
  , moGeneratorRev   :: Maybe String
    -- ^ Plugin source revision for the @generator@ stamp.
  , moAllowEmpty     :: Bool
    -- ^ Accept a contract with no tables ('publishProblem').
  , moNoOwnTables    :: Bool
    -- ^ The package defines no tables of its own and only composes
    --   @--include-merged@ contracts ('publishProblem').
  , moAllowEmptyInclude :: Bool
    -- ^ Accept an included contract with no tables, with a warning.
  , moAllowUnmatchedOverride :: Bool
    -- ^ Accept an override that matches no table, with a warning.
  , moAllowColumnlessTable :: Bool
    -- ^ Accept a table with no columns, with a warning.
  } deriving (Show, Eq)

-- | Options with only the two required paths set.
defaultMergeOptions :: FilePath -> FilePath -> MergeOptions
defaultMergeOptions fragmentsDir outFile = MergeOptions
  { moFragmentsDir  = fragmentsDir
  , moOutFile       = outFile
  , moIncludeMerged = []
  , moSourceDirs    = []
  , moWalkEntryCap  = walkEntryCap
  , moGeneratorRev  = Nothing
  , moAllowEmpty    = False
  , moNoOwnTables   = False
  , moAllowEmptyInclude      = False
  , moAllowUnmatchedOverride = False
  , moAllowColumnlessTable   = False
  }

data MergeReport = MergeReport
  { mrFragmentsRead         :: Int
  , mrStalePruned           :: Int
  , mrIncludedFiles         :: Int
  , mrIncludedTables        :: Int
  , mrDeduped               :: Int     -- ^ Identical-content tables collapsed
  , mrTablesEmitted         :: Int
  , mrOverridesEmitted      :: Int
  , mrOwnTables             :: Int     -- ^ Tables in the package's own fragments
  } deriving (Show, Eq)


-- | The stamp 'runMerge' writes; the stamps of included contracts are ignored.
currentGenerator :: Maybe String -> Generator
currentGenerator rev = Generator
  { generatorName    = "sql-schema"
  , generatorVersion = showVersion Paths_sql_schema.version
  , generatorRev     = rev
  }

-- | @Left@ holds the stderr lines: input and validation errors, or the refusal
-- of 'publishProblem'.  The output is written only on @Right@.
runMerge :: MergeOptions -> IO (Either [String] MergeReport)
runMerge opts@MergeOptions{..} = do
  fragKind            <- pathKind moFragmentsDir
  (read', frags, bad) <- case fragKind of
    PathDirectory -> readFragmentFiles moFragmentsDir
    _             -> pure (0, [], [])
  when (fragKind == PathMissing) $ hPutStrLn stderr (fragmentsMissingWarning moFragmentsDir)
  (live, dirProblems) <- liveModulesFromDirs moWalkEntryCap moSourceDirs
  let (kept, prunedMods) = pruneStaleByModule moSourceDirs live frags
      -- An unparseable fragment is only safe to drop once --source-dirs shows
      -- its module is gone; otherwise its tables would silently vanish.
      (badStale, badLive) =
        partition (\(f, _) -> not (null moSourceDirs)
                               && not (Set.member (takeBaseName f) live)) bad
      prunedNames = prunedMods ++ map (takeBaseName . fst) badStale
      pruned = length prunedNames
  mapM_ (\m -> hPutStrLn stderr ("sql-schema-merge: pruned the fragment of " <> show m
                                 <> ": its module is in no --source-dirs entry"))
        prunedNames
  let unusable     = [p | p <- dirProblems, not (isSkippedLink p)]
      skippedLinks = [l | SourceDirLinkSkipped l <- dirProblems]
  when (pruned == 0) $ mapM_ (hPutStrLn stderr . sourceDirWarning) unusable
  (incFiles, incTbls, incOvs, incErrs, emptyIncs) <- loadIncludedMerged moIncludeMerged
  when moAllowEmptyInclude $ mapM_ (hPutStrLn stderr . emptyIncludeWarning) emptyIncs
  let inputErrs =
        [ fragmentsNotDirMsg moFragmentsDir | fragKind == PathNotDirectory ]
        ++ [ "sql-schema-merge: unparseable fragment " <> show f <> ": " <> show e | (f, e) <- badLive ]
        ++ [ "sql-schema-merge: " <> e | e <- incErrs ]
        ++ [ line | not moAllowEmptyInclude, p <- emptyIncs, line <- emptyIncludeMsg p ]
        ++ [ allPrunedMsg read' | read' > 0, pruned == read' ]
        -- The pruned modules may be the ones the unusable entry should have listed.
        ++ [ line | pruned > 0, not (null unusable)
                  , line <- unusableSourceDirsMsg unusable prunedNames ]
        -- Or modules the walk names only through a link it skipped.
        ++ [ line | pruned > 0, not (null skippedLinks)
                  , line <- skippedLinksMsg skippedLinks prunedNames ]
  let fragTables       = concatMap fragmentTables kept
      fragOverrides    = concatMap fragmentDbEntityOverrides kept
      allTables        = fragTables ++ incTbls
      allOverrides     = fragOverrides ++ incOvs
  case (if null inputErrs then validate opts allTables allOverrides else Left inputErrs) of
    Left errs -> pure (Left errs)
    Right (unstamped, deduped, warnings) -> do
      let merged = unstamped { generator = Just (currentGenerator moGeneratorRev) }
          report = MergeReport
            { mrFragmentsRead    = read'
            , mrStalePruned      = pruned
            , mrIncludedFiles    = incFiles
            , mrIncludedTables   = length incTbls
            , mrDeduped          = deduped
            , mrTablesEmitted    = length (mergedTables merged)
            , mrOverridesEmitted = length (mergedDbEntityOverrides merged)
            , mrOwnTables        = length fragTables
            }
      mapM_ (hPutStrLn stderr) warnings
      case publishProblem moAllowEmpty moNoOwnTables report of
        Just problem -> pure (Left (refusalLines opts report problem))
        Nothing -> do
          writeMerged moOutFile merged
          pure (Right report)


-- | Why 'runMerge' refuses to publish a merge's contract.
data PublishProblem
  = NoTables
  | NoOwnFragments
    -- ^ Included tables would hide that the package's own plugin never ran.
  | OwnTablesDeclaredNone Int
    -- ^ @--no-own-tables@ was given, but the package's own fragments define
    -- this many tables.
  deriving (Show, Eq)

-- | @publishProblem allowEmpty noOwnTables report@.  @allowEmpty@ permits only a
-- contract with no tables.  @noOwnTables@ declares that the package defines no
-- tables of its own: no own fragments is then expected, and an own table
-- proves the declaration stale.
publishProblem :: Bool -> Bool -> MergeReport -> Maybe PublishProblem
publishProblem allowEmpty noOwnTables r
  | noOwnTables && mrOwnTables r > 0 = Just (OwnTablesDeclaredNone (mrOwnTables r))
  | mrTablesEmitted r == 0 = if allowEmpty then Nothing else Just NoTables
  | not noOwnTables && mrFragmentsRead r - mrStalePruned r == 0 = Just NoOwnFragments
  | otherwise = Nothing

-- | The stderr lines for a refused merge.  No summary line follows a refusal,
-- so they carry the report's counts.
refusalLines :: MergeOptions -> MergeReport -> PublishProblem -> [String]
refusalLines opts report = \case
  OwnTablesDeclaredNone n ->
    [ "sql-schema-merge: ERROR: --no-own-tables was given, but this package's own"
    , "fragments define " <> show n <> " table(s).  Remove --no-own-tables so the"
    , "merge checks this package's own fragments again."
    ] <> context
  NoTables       -> pluginDidNotRun "the contract has 0 tables"
  NoOwnFragments -> pluginDidNotRun "no fragment of this package's own modules was read"
  where
    context =
      [ "  out:        " <> moOutFile opts <> " (not written)"
      , "  fragments:  " <> moFragmentsDir opts
      , "  counts:     " <> show (mrFragmentsRead report) <> " fragments read, "
          <> show (mrStalePruned report) <> " stale pruned, "
          <> show (mrIncludedFiles report) <> " included contracts ("
          <> show (mrIncludedTables report) <> " tables)"
      ]
    pluginDidNotRun why =
      ("sql-schema-merge: ERROR: refusing to publish: " <> why <> ".") : context ++
      [ "This almost always means the SqlSchema GHC plugin did not run during"
      , "compilation, NOT that the package has no Beam tables.  Publishing it is"
      , "a downstream false-pass against the prod DB schema."
      , "Fixes:"
      , "  * Ensure the package was built with the SqlSchema cabal flag enabled"
      , "    (on by default in nix; local cabal builds opt out via -SqlSchema)."
      , "  * If this package defines no tables of its own and only composes"
      , "    dependencies via --include-merged, pass --no-own-tables."
      , "  * If the contract is meant to have no tables at all, pass --allow-empty."
      ]


-- | Load every @*.yaml@ file in the given directory as a 'Fragment'.
-- Returns @(filesTried, parsed, unparseable)@, the last as @(path, error)@
-- for the caller to judge.  Missing directory → @(0, [], [])@; a path that is
-- not a directory, or an unreadable one, throws rather than reading as empty.
loadFragments :: FilePath -> IO (Int, [Fragment], [(FilePath, String)])
loadFragments dir = pathKind dir >>= \case
  PathMissing      -> pure (0, [], [])
  PathNotDirectory -> ioError (userError ("fragments path is not a directory: " <> show dir))
  PathDirectory    -> readFragmentFiles dir

-- | 'loadFragments' for a path already found to be a directory, so that 'runMerge' checks it once.
readFragmentFiles :: FilePath -> IO (Int, [Fragment], [(FilePath, String)])
readFragmentFiles dir = do
  names <- listDirectory dir
  let yamls = sort [dir </> n | n <- names, takeExtension n == ".yaml"]
  parsed <- forM yamls $ \p -> do
    bs <- BS.readFile p
    pure $ case YAML.decodeEither' bs of
      Right frag -> Right frag
      Left e     -> Left (p, YAML.prettyPrintParseException e)
  pure (length yamls, [f | Right f <- parsed], [b | Left b <- parsed])


-- | The error for @--source-dirs@ that prune every fragment.
allPrunedMsg :: Int -> String
allPrunedMsg n =
  "sql-schema-merge: all " <> show n <> " fragments were pruned as stale: no "
  <> "--source-dirs entry holds their modules. Check the --source-dirs paths, "
  <> "or delete the fragments of modules that were removed."


fragmentsMissingWarning :: FilePath -> String
fragmentsMissingWarning dir =
  "sql-schema-merge: warning: --fragments directory " <> dir <> " does not exist; reading it as no fragments"


fragmentsNotDirMsg :: FilePath -> String
fragmentsNotDirMsg dir =
  "sql-schema-merge: ERROR: --fragments is not a directory: " <> dir <> ".  Reading it as an "
  <> "empty directory would hide every fragment of this package; pass the directory the "
  <> "plugin writes its fragments to (its fragmentsDir option)."


emptyIncludeMsg :: FilePath -> [String]
emptyIncludeMsg p =
  [ "sql-schema-merge: ERROR: --include-merged file " <> p <> " holds no tables."
  , "An included contract with no tables usually means the plugin did not run when its package"
  , "was built, so none of that package's tables reach this contract.  Fix that package's build,"
  , "remove the file from --include-merged, or pass --allow-empty-include."
  ]


emptyIncludeWarning :: FilePath -> String
emptyIncludeWarning p = "sql-schema-merge: warning: --include-merged file " <> p <> " holds no tables"


unusableSourceDirsMsg :: [SourceDirProblem] -> [String] -> [String]
unusableSourceDirsMsg problems prunedNames =
  "sql-schema-merge: ERROR: fragments were pruned as stale while a --source-dirs entry is unusable:"
  : map (("  " <>) . sourceDirProblemText) problems
  ++ map (("  pruned fragment of " <>) . show) prunedNames
  ++ [ "Pruning a module whose fragment would have been kept with the right"
     , "--source-dirs entry drops its tables from the contract.  Fix the --source-dirs"
     , "path, or delete the stale fragments named above."
     ]


skippedLinksMsg :: [FilePath] -> [String] -> [String]
skippedLinksMsg links prunedNames =
  "sql-schema-merge: ERROR: fragments were pruned as stale while the walk of --source-dirs skipped links:"
  : map (("  skipped link " <>) . show) links
  ++ map (("  pruned fragment of " <>) . show) prunedNames
  ++ [ "A file reached through such a link may declare its module by the name through the link,"
     , "which the walk does not give it, so its fragment may be pruned wrongly.  Replace the link"
     , "with the directory it stands for, or delete the stale fragments named above."
     ]


sourceDirWarning :: SourceDirProblem -> String
sourceDirWarning p =
  "sql-schema-merge: warning: " <> sourceDirProblemText p <> "; treating as empty"


cycleWarning :: FilePath -> FilePath -> String
cycleWarning p real =
  "sql-schema-merge: warning: skipped " <> show p <> " in --source-dirs: it leads back to " <> show real
  <> ", a directory on the walk's path from the entry"


linkCycleWarning :: FilePath -> String
linkCycleWarning p =
  "sql-schema-merge: warning: skipped " <> show p <> " in --source-dirs: a chain of symbolic links "
  <> "that leads back to itself, with no directory at its end"


sourceDirProblemText :: SourceDirProblem -> String
sourceDirProblemText = \case
  SourceDirMissing d       -> "source dir " <> d <> " does not exist"
  SourceDirNotADirectory d -> "source dir " <> d <> " is not a directory"
  SourceDirLinkSkipped l   -> "skipped link " <> show l


data PathKind = PathMissing | PathDirectory | PathNotDirectory
  deriving (Eq, Show)

-- | What @p@ names, throwing when that can't be checked (e.g. a parent without
-- search permission): reading it as missing would drop every table under it.
pathKind :: FilePath -> IO PathKind
pathKind p = tryIOError (getPermissions p) >>= \case
  Left e | isDoesNotExistError e -> pure PathMissing
         | otherwise             -> ioError e
  Right _ -> do
    isDir <- doesDirectoryExist p
    pure (if isDir then PathDirectory else PathNotDirectory)


data SourceDirProblem
  = SourceDirMissing FilePath
  | SourceDirNotADirectory FilePath
  | SourceDirLinkSkipped FilePath
    -- ^ A link the walk skipped, as it leads back to a directory on the walk's path from the entry.
  deriving (Eq, Show)

isSkippedLink :: SourceDirProblem -> Bool
isSkippedLink = \case
  SourceDirLinkSkipped _ -> True
  _                      -> False

-- | The default 'moWalkEntryCap': the walk of one @--source-dirs@ entry fails once it has taken more
-- steps.  Each directory entry listed is one step, and each link followed, or hop of a chain, one
-- step per path component its resolution walks: the link's path, then its target's.  A directory is
-- read whole before its entries are counted, so one directory's listing can pass the cap.  A
-- directory that several links lead to is listed once per link, so links can multiply the steps.
walkEntryCap :: Int
walkEntryCap = 200000

-- | The most symbolic links one chain may pass; the system itself stops at a few dozen.
maxLinkHops :: Int
maxLinkHops = 1000

-- | The most directory levels a walk may descend below its @--source-dirs@ entry, a link to a
-- directory counting as a level.  The deepest directory in the trees this serves is 7 levels down.
maxWalkDepth :: Int
maxWalkDepth = 256

-- | Walk every directory in the list; collect dotted module names from
-- every module source file found beneath (see 'walkHs').  An entry that is
-- missing or not a directory contributes no modules and is returned as a
-- 'SourceDirProblem' for the caller to judge; one that can't be checked throws.
-- A link back to a directory on the walk's path from the entry is skipped, and
-- returned as a 'SourceDirLinkSkipped'; any other link is followed under its own
-- name.  The warnings for the links skipped go to stderr in writes of up to 1000
-- lines, also when the walk throws.  Each entry's walk takes at most @cap@ steps ('walkEntryCap').
-- Returned set is the ground truth for @pruneStaleByModule@.
liveModulesFromDirs :: Int -> [FilePath] -> IO (Set String, [SourceDirProblem])
liveModulesFromDirs cap dirs = do
  results <- mapM oneDir dirs
  pure (Set.unions (map fst results), concatMap snd results)
  where
    oneDir d = pathKind d >>= \case
      PathMissing      -> pure (Set.empty, [SourceDirMissing d])
      PathNotDirectory -> pure (Set.empty, [SourceDirNotADirectory d])
      PathDirectory    -> (\(hs, skipped) -> (Set.fromList (map (pathToModule d) hs), map SourceDirLinkSkipped skipped))
                            <$> walkHs cap d
    pathToModule root p =
      -- root="pkg/gen", p="pkg/gen/Lib/Foo.hs"
      -- → "Lib.Foo"; makeRelative also handles a root with a trailing slash.
      let rel = makeRelative root p
          noExt = dropExtension rel
          dotted = map (\c -> if c == '/' then '.' else c) noExt
      in dotted


-- | Walk a directory, returning every module source file: @.hs@ and the files cabal preprocesses
-- into modules, each named by the path it is reached through.  A link is followed under its own
-- name even when its directory is reached another way: both names are live, which is over-inclusive
-- and safe.  Only a link back to a directory on the walk's path from the entry is skipped (with a
-- warning), since every name through it would repeat one found on that path; the skipped links are
-- returned beside the files.
walkHs :: Int -> FilePath -> IO ([FilePath], [FilePath])
walkHs cap root = do
  listed   <- newIORef (0 :: Int)
  pending  <- newIORef (0 :: Int, [])
  skipped  <- newIORef []
  let -- Warnings go out in writes of up to 'warningChunk' lines, newest first while they wait.
      flush = do
        (_, ws) <- readIORef pending
        writeIORef pending (0, [])
        hPutLines stderr (reverse ws)
      warn w = do
        (n, ws) <- readIORef pending
        writeIORef pending (n + 1, w : ws)
        when (n + 1 >= warningChunk) flush
      -- @name@ is how the directory is reached from the entry, @real@ where it is (no link in it),
      -- @above@ the real paths of the directories being listed, this one included, and @depth@ its
      -- level below the entry.
      list depth above name real = do
        perms <- getPermissions real
        -- Throws when it can't be searched: read as empty, it would prune every module under it.
        unless (searchable perms) $
          ioError (userError ("cannot search source directory " <> show name))
        names <- sort <$> listDirectory real
        fmap concat $ forM names $ \n -> do
          charge 1
          entry depth above (name </> n) (real </> n)
      charge steps = do
        modifyIORef' listed (+ steps)
        count <- readIORef listed
        when (count > cap) $ ioError (userError (walkCapMessage cap root))
      skip name w = [] <$ (modifyIORef' skipped (name :) >> warn w)
      entry depth above name path = tryIOError (pathKind path) >>= \case
        Right PathDirectory -> descend depth above name path
        -- A dangling link ('PathMissing') counts like a file.
        Right _             -> pure [name | takeExtension name `elem` moduleExts]
        -- The error "too many links" says the system's limit was passed, not that the chain loops.
        -- Any other error throws: read as a file, the entry would drop the modules behind it.
        Left e
          | isSymlinkLoop e -> chain depth above name path
          | otherwise       -> ioError e
      descend depth above name path = do
        real <- canonicalizePath path
        -- @path@ is canonical unless it went through a link, whose resolution walked both paths.
        when (real /= path) $ charge (components path + components real)
        if Set.member real above
          then skip name (cycleWarning name real)
          else do
            when (depth >= maxWalkDepth) $ ioError (userError (walkDepthMessage root))
            list (depth + 1) (Set.insert real above) name real
      chain depth above name path = resolveChain charge path >>= \case
        Nothing  -> skip name (linkCycleWarning name)
        Just end -> pathKind end >>= \case
          PathDirectory -> descend depth above name end
          _             -> pure [name | takeExtension name `elem` moduleExts]
  rootReal <- canonicalizePath root
  files <- list (0 :: Int) (Set.singleton rootReal) root rootReal `finally` flush
  (,) files . reverse <$> readIORef skipped
  where
    moduleExts = [".hs", ".lhs", ".hsc", ".chs", ".x", ".y", ".ly"]
    warningChunk = 1000 :: Int
    components = length . splitDirectories


walkDepthMessage :: FilePath -> String
walkDepthMessage root =
  "cannot walk " <> show root <> ": it has a directory more than " <> show maxWalkDepth <> " directory levels below it "
  <> "(a link to a directory is a level); name a smaller --source-dirs entry or remove the links"


walkCapMessage :: Int -> FilePath -> String
walkCapMessage cap root =
  "cannot walk " <> show root <> ": more than " <> show cap <> " walk steps (a directory entry listed is one "
  <> "step, a link followed one per path component its resolution walks); name a smaller --source-dirs entry "
  <> "or remove the links"


-- | Follow the symbolic link @start@ one hop at a time, as the system gives up on a chain after a few
-- dozen links.  @Just end@ is the first path of the chain that is not a link (it may not exist);
-- @Nothing@ is a chain that leads back to one of its own links, so it has no end.  A chain of more
-- than 'maxLinkHops' links, or a link that can't be read, throws.  Before each hop, @charge@ gets the
-- components the hop walks: the link's directory, resolved, and the link itself.
resolveChain :: (Int -> IO ()) -> FilePath -> IO (Maybe FilePath)
resolveChain charge start = go Set.empty (0 :: Int) start
  where
    go seen hops cur = tryIOError (pathIsSymbolicLink cur) >>= \case
      Right True -> do
        self <- (</> takeFileName cur) <$> canonicalizePath (takeDirectory cur)
        if Set.member self seen
          then pure Nothing
          else do
            when (hops >= maxLinkHops) $ ioError (userError
              ("a chain of more than " <> show maxLinkHops <> " symbolic links starts at " <> show start
               <> "; replace the chain with one link to its end, or remove it"))
            charge (max 1 (length (splitDirectories cur)))
            target <- getSymbolicLinkTarget cur
            let next = if isAbsolute target then target else takeDirectory cur </> target
            go (Set.insert self seen) (hops + 1) (dropTrailingPathSeparator next)
      -- Not a link, or not there: the caller finds out which, and whether it can be read.
      _ -> pure (Just cur)


isSymlinkLoop :: IOError -> Bool
isSymlinkLoop e = (Errno <$> ioe_errno e) == Just eLOOP


-- | Write the lines to the handle in one call: stderr is unbuffered, and 'hPutStr' then costs a
-- system call per character.
hPutLines :: Handle -> [String] -> IO ()
hPutLines h ls = unless (null ls) (BS.hPut h (TE.encodeUtf8 (T.pack (unlines ls))))


-- | If 'moSourceDirs' was empty (caller opted out of pruning), keep
-- everything.  Otherwise drop any fragment whose @fragmentModule@ is not
-- in the live set.  Returns @(kept, prunedModules)@.
pruneStaleByModule
  :: [FilePath] -> Set String -> [Fragment] -> ([Fragment], [String])
pruneStaleByModule sourceDirs live frags
  | null sourceDirs = (frags, [])
  | otherwise =
      let kept = [f | f <- frags, Set.member (fragmentModule f) live]
          dropped = [f | f <- frags, not (Set.member (fragmentModule f) live)]
      in (kept, map fragmentModule dropped)


-- | Parse each @--include-merged@ file as a 'MergedSchema' and return
-- the concatenated tables + overrides, plus one error per missing or
-- malformed file (the caller fails the merge: skipping the file would drop
-- every table it holds).  Counts the files actually parsed, and lists those
-- that hold no tables.
loadIncludedMerged
  :: [FilePath]
  -> IO (Int, [TableSchema], [DbEntityOverride], [String], [FilePath])
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
                         <> show (YAML.prettyPrintParseException e)))
          Right (m :: MergedSchema) ->
            pure (Right (p, mergedTables m, mergedDbEntityOverrides m))
  let okCount    = length [ () | Right _ <- results ]
      tables     = concat [ ts  | Right (_, ts, _) <- results ]
      overrides  = concat [ os  | Right (_, _, os) <- results ]
      errs       = [e | Left e <- results]
      emptyFiles = [p | Right (p, [], _) <- results]
  pure (okCount, tables, overrides, errs, emptyFiles)


-- | Validate, dedup, and order.  Returns either a list of fatal errors,
-- or @(merged, dedupedCount, warnings)@.
validate
  :: MergeOptions
  -> [TableSchema]
  -> [DbEntityOverride]
  -> Either [String] (MergedSchema, Int, [String])
validate MergeOptions{moAllowUnmatchedOverride, moAllowColumnlessTable} tables overrides =
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
        [ "sql-schema-merge: warning: SQL table " <> show tn
            <> " modelled by multiple Haskell types: "
            <> commaSep (map show hts)
        | (tn, hts) <- Map.toList tableNameGroups
        , length hts > 1
        ]

      -- Cross-check setEntityName overrides against tableName.
      checkOverride DbEntityOverride{..} =
        let matches = [t | t <- uniqTables
                         , dbTableType `isSuffixOfDot` qualifiedType t]
        in case matches of
             []  | moAllowUnmatchedOverride -> Right (Just unmatchedWarning)
                 | otherwise                -> Left unmatchedMsg
             [t] | modelTableName t == sqlName -> Right Nothing
                 | otherwise              -> Left (mismatchMsg t)
             -- A downstream package may model a same-named type; only a
             -- disagreeing candidate could be misreported.
             _   | all ((== sqlName) . modelTableName) matches -> Right Nothing
                 | otherwise -> Left (ambiguousMsg matches)
        where
          overrideOf = "dbEntityOverride for " <> show dbTableType <> " in " <> show overrideSourceModule
          unmatchedWarning = "sql-schema-merge: warning: " <> overrideOf <> " has no matching table in any fragment"
          unmatchedMsg = unlines
            [ "sql-schema-merge: ERROR: " <> overrideOf <> " has no matching table in any fragment or"
            , "included contract.  The table is missing from the contract, and the override's setEntityName"
            , "could not be checked against a modelTableName.  Include the contract that defines the table"
            , "(--include-merged), fix --source-dirs if its fragment was pruned, or pass"
            , "--allow-unmatched-override to accept the override unchecked."
            ]
          mismatchMsg t = unlines
            [ "setEntityName override disagrees with modelTableName:"
            , "  table:           " <> show (qualifiedType t)
            , "  modelTableName:  " <> show (modelTableName t)
            , "  setEntityName:   " <> show sqlName
            , "  declared in:     " <> show overrideSourceModule <> " (" <> show overrideSourceFile <> ")"
            , "Beam's runtime uses setEntityName, so the YAML would misreport"
            , "the SQL table name.  Either fix the modelTableName or remove"
            , "the setEntityName override."
            ]
          ambiguousMsg ms = unlines $
            [ "Ambiguous dbEntityOverride for table-type suffix "
              <> show dbTableType <> ": matches "
              <> show (length ms) <> " tables:"
            ] <> [ "  - " <> show (qualifiedType m) | m <- ms ]

      overrideResults = map checkOverride overrides
      overrideErrs    = [e | Left e <- overrideResults]
      overrideWarns   = [w | Right (Just w) <- overrideResults]

      -- One key with two SQL names has no single entry to record: refused, whatever the flags.
      overrideNames = Map.fromListWith Set.union [(overrideKey o, Set.singleton (sqlName o)) | o <- overrides]
      conflictErrs  = [ overrideConflictMsg k (Set.toAscList ns)
                      | (k, ns) <- Map.toList overrideNames, Set.size ns > 1 ]

      -- After dedup, so a table that arrives from two places counts once.
      columnless      = [t | t <- uniqTables, null (columns t)]
      columnlessErrs  = [columnlessMsg t     | not moAllowColumnlessTable, t <- columnless]
      columnlessWarns = [columnlessWarning t | moAllowColumnlessTable, t <- columnless]
      checkErrs       = overrideErrs ++ conflictErrs ++ columnlessErrs

      -- Overrides with one key and one SQL name collapse, whatever their source file.
      dedupedOverrides = Map.elems $ Map.fromList [(overrideKey o, o) | o <- overrides]

  in if not (null dupErrs)
       then Left dupErrs
       else if not (null checkErrs)
              then Left checkErrs
              else Right
                ( MergedSchema
                    { mergedTables = sortOn qualifiedType uniqTables
                    , mergedDbEntityOverrides = sortOn overrideKey dedupedOverrides
                    , generator = Nothing
                    }
                , dedupCount
                , tnWarns ++ overrideWarns ++ columnlessWarns
                )
  where
    overrideKey o = (overrideSourceModule o, dbField o, dbTableType o)


-- | All entries equal → keep one + size.  Any pair differs → describe
-- the disagreement.
resolveGroup :: String -> [TableSchema] -> Either String (TableSchema, Int)
resolveGroup _  [t]        = Right (t, 1)
resolveGroup ht ts@(t:_)
  | all (== t) ts = Right (t, length ts)
  | otherwise     = Left (mismatchMessage ht ts)
resolveGroup ht []         = Left ("internal: empty group for " <> show ht)

mismatchMessage :: String -> [TableSchema] -> String
mismatchMessage ht ts = unlines $
  [ "Duplicate table type " <> show ht <> " with conflicting definitions:"
  ] <> zipWith oneCopy [(1 :: Int)..] ts
  <> [ "All copies of the same Haskell type must agree exactly.  This"
     , "usually means two different pinned versions of the same upstream"
     , "package are reaching this merge (a diamond dep).  Align the"
     , "versions (nix flake follows) and retry."
     ]
  where
    oneCopy n t = unlines
      [ "  copy " <> show n <> ":"
      , "    sourceModule:    " <> show (sourceModule t)
      , "    modelTableName:  " <> show (modelTableName t)
      , "    modelTableType:  " <> show (modelTableType t)
      , "    modelSchemaName: " <> show (modelSchemaName t)
      , "    column count:    " <> show (length (columns t))
      , "    pk constructor:  " <> show (pkConstructor (primaryKey t))
      , "    pk columns:      " <> show (pkColumns (primaryKey t))
      ]


overrideConflictMsg :: (String, String, String) -> [String] -> String
overrideConflictMsg (modName, field, ty) names = unlines
  [ "sql-schema-merge: ERROR: dbEntityOverride for " <> show ty <> " (field " <> show field <> ") in "
    <> show modName <> " gives different SQL names: " <> commaSep (map show names) <> "."
  , "The contract records one name per override, so keeping one would misreport the table under"
  , "the other.  Remove or reconcile all but one of the overrides."
  ]


columnlessWhat :: TableSchema -> String
columnlessWhat t =
  "table " <> show (qualifiedType t) <> " (SQL table " <> show (modelTableName t) <> ") has no columns"

columnlessWarning :: TableSchema -> String
columnlessWarning t = "sql-schema-merge: warning: " <> columnlessWhat t

columnlessMsg :: TableSchema -> String
columnlessMsg t = unlines
  [ "sql-schema-merge: ERROR: " <> columnlessWhat t <> "."
  , "The plugin read none of its fields, so the contract would not validate the table's real columns."
  , "Usual cause: another plugin rewrote the record first, and allowUnknownColumnWrapper dropped its"
  , "fields.  Fix the plugin order; if the table really has no columns, pass --allow-columnless-table."
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


-- | Rename a fresh temp file over @path@, so a reader never sees a partial contract.  The
-- file is created exclusively: nothing already next to @path@ is written through or removed.
-- It gets the mode of a plain write (the umask's), not the 0600 of 'openBinaryTempFile'.
writeMerged :: FilePath -> MergedSchema -> IO ()
writeMerged path merged =
  bracketOnError
    (openBinaryTempFileWithDefaultPermissions (takeDirectory path) (takeFileName path <> ".tmp"))
    (\(tmp, h) -> tryIOError (hClose h) >> tryIOError (removeFile tmp))
    (\(tmp, h) -> BS.hPut h (YAML.encode merged) >> hClose h >> renameFile tmp path)
