{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The @sql-schema-merge@ command line as a library function, so the
-- test-suite can run it in-process and check its exit codes.
module SqlSchema.MergeCli
  ( mergeMain
  , parseArgs
  , usage
  , ioErrorText
  ) where

import           Control.Exception   (IOException, try)
import           Control.Monad       (void, when)
import           Data.Char           (GeneralCategory (Format, LineSeparator, ParagraphSeparator), generalCategory,
                                      isControl, showLitChar)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (isNothing)
import           System.Exit         (ExitCode (..))
import           System.IO           (hFlush, hPutStrLn, stderr, stdout)
import           System.IO.Error     (ioeGetErrorString, isUserError, tryIOError)

import           SqlSchema.Merge

-- | Run the merge CLI on its arguments.  It returns:
--
--   * @ExitSuccess@: the contract was written.  A summary line goes to stdout; if it can't be
--     printed, a note goes to stderr.
--   * @ExitFailure 1@: the merge failed, was refused ('publishProblem'), or hit
--     an I/O error ('ioErrorText').  Nothing was written to @--out@.
--   * @ExitFailure 2@: an argument error ('parseArgs').  Nothing was read.
mergeMain :: [String] -> IO ExitCode
mergeMain args = case parseArgs args of
  Left err -> do
    hPutStrLn stderr ("sql-schema-merge: " <> err)
    hPutStrLn stderr usage
    pure (ExitFailure 2)
  Right opts -> try (runMerge opts) >>= \case
    Left (e :: IOException) -> do
      hPutStrLn stderr ("sql-schema-merge: " <> ioErrorText e)
      pure (ExitFailure 1)
    Right (Left errs) -> do
      mapM_ (hPutStrLn stderr) errs
      pure (ExitFailure 1)
    Right (Right report) -> do
      printed <- tryIOError (putStrLn (summary opts report) >> hFlush stdout)
      -- The contract is already in place, so a summary that can't be printed is only noted.
      either (void . tryIOError . hPutStrLn stderr . summaryNote) pure printed
      pure ExitSuccess

-- | The merge's own errors ('userError') as written, since each path in them is 'show'n already,
-- with any control or format character escaped all the same; any other error through 'show', as
-- the system's text quotes paths from the trees read as they are.
ioErrorText :: IOException -> String
ioErrorText e
  | isUserError e = concatMap escape (ioeGetErrorString e)
  | otherwise     = show (show e)
  where
    escape c
      | isControl c || generalCategory c `elem` [Format, LineSeparator, ParagraphSeparator] = showLitChar c ""
      | otherwise = [c]

summaryNote :: IOError -> String
summaryNote e =
  "sql-schema-merge: warning: the contract was written, but its summary line could not be printed: "
  <> show e

summary :: MergeOptions -> MergeReport -> String
summary opts report =
  "sql-schema-merge: " <> show (mrFragmentsRead report)
  <> " fragments read, " <> show (mrStalePruned report)
  <> " stale pruned, " <> show (mrIncludedFiles report)
  <> " included contracts (" <> show (mrIncludedTables report)
  <> " tables), " <> show (mrDeduped report)
  <> " duplicates collapsed, " <> show (mrTablesEmitted report)
  <> " tables emitted, " <> show (mrOverridesEmitted report)
  <> " overrides recorded -> " <> show (moOutFile opts)

usage :: String
usage = unlines
  [ "Usage: sql-schema-merge --fragments DIR --out FILE"
  , "                       [--source-dirs DIR ...]"
  , "                       [--include-merged FILE ...]"
  , "                       [--no-own-tables] [--allow-empty]"
  , "                       [--allow-empty-include] [--allow-unmatched-override]"
  , "                       [--allow-columnless-table]"
  , "                       [--generator-rev REV]"
  , ""
  , "  --fragments DIR        directory holding per-module *.yaml fragments.  A"
  , "                         missing path (or a dangling symlink) warns and reads as"
  , "                         no fragments.  A path that is not a directory, or that"
  , "                         can't be checked (below a file, a symlink loop, no"
  , "                         search permission), is an error (exit 1)."
  , "  --out FILE             path to write the merged YAML"
  , "  --source-dirs DIR      (repeatable) directory containing module sources"
  , "                         (.hs, .lhs, .hsc, .chs, .x, .y, .ly); fragments whose"
  , "                         module has no source file in any listed dir are"
  , "                         pruned, and named on stderr.  If omitted, no pruning"
  , "                         is done (every fragment is kept).  An entry that is"
  , "                         missing or not a directory holds no modules: it only"
  , "                         warns, unless a fragment was pruned, which is an error"
  , "                         (exit 1) since that module may live in the entry.  An"
  , "                         entry that can't be checked (as above) is always an"
  , "                         error (exit 1), and so is an entry inside a listed"
  , "                         directory that can't be inspected.  A link back to a"
  , "                         directory on the walk's path from the entry is"
  , "                         skipped, with a warning, and a fragment pruned in a"
  , "                         merge that skipped one is an error (exit 1); any"
  , "                         other link is followed under its own name, so a"
  , "                         directory several links lead to is listed once per"
  , "                         link.  More than 200000 walk steps for one entry"
  , "                         (an entry listed is one step, a link followed one"
  , "                         per path component resolving it walks), a"
  , "                         directory more than 256 levels below it (a link to"
  , "                         a directory is a level), or a chain of more than"
  , "                         1000 links, is an error (exit 1)."
  , "  --include-merged FILE  (repeatable) pre-merged contract YAML to union into"
  , "                         the output.  Typically '${dep}/share/sql-schema/"
  , "                         sql-schema.yaml' for a dep package whose tables"
  , "                         must compose into this service's contract."
  , "  --no-own-tables        this package defines no tables of its own and only"
  , "                         composes --include-merged contracts: a contract with"
  , "                         none of its own fragments is then accepted, and own"
  , "                         fragments that define a table are an error (exit 1)."
  , "                         Without it, no own fragments is an error, since it"
  , "                         usually means the SqlSchema plugin did not run."
  , "  --allow-empty          permit a contract with zero tables.  Without it, zero"
  , "                         tables is an error (exit 1), for the same reason."
  , "  --allow-empty-include  accept an --include-merged contract with no tables, with"
  , "                         a warning.  Without it that is an error (exit 1): the"
  , "                         dependency's plugin most likely did not run.  Input"
  , "                         errors stop the merge before the table checks, whose"
  , "                         errors show once they are fixed."
  , "  --allow-unmatched-override"
  , "                         accept a setEntityName override that matches no table,"
  , "                         with a warning.  Without it that is an error (exit 1):"
  , "                         the check against modelTableName could not run."
  , "  --allow-columnless-table"
  , "                         accept a table with no columns, with a warning.  Without"
  , "                         it that is an error (exit 1): the plugin most likely"
  , "                         read none of the record's fields."
  , "  --generator-rev REV    VCS revision of the plugin source, recorded in the"
  , "                         contract's generator stamp (omitted -> rev: null)."
  , ""
  , "Each --allow-* flag relaxes only its own check.  An override key (module,"
  , "field, table type) with two different SQL names is an error whatever the flags."
  , ""
  , "--fragments, --out and --generator-rev may be given once.  A value may not be"
  , "empty or start with \"--\": an unset shell variable would otherwise change"
  , "what is merged without a message.  Any of these is an argument error (exit 2)."
  , ""
  , "--out is written only when every check passes: a merge that exits 1 or 2"
  , "creates no --out and leaves an existing one as it was."
  ]

-- | Parse the command line.  @--fragments@, @--out@ and @--generator-rev@ may be
-- given once.  A value that is empty, or starts with @--@, is an error: that is
-- what an unset shell variable leaves (an empty argument, or none, so that the
-- next option is taken as the value).
parseArgs :: [String] -> Either String MergeOptions
parseArgs = go (defaultMergeOptions "" "")
  where
    -- An empty path means "not given yet": empty values are rejected, never stored.
    go o [] = do
      when (null (moFragmentsDir o)) (Left "missing --fragments")
      when (null (moOutFile o)) (Left "missing --out")
      pure o
    go o (flag@"--fragments" : rest) = withValue flag rest $ \v r ->
      once flag (null (moFragmentsDir o)) (go o { moFragmentsDir = v } r)
    go o (flag@"--out" : rest) = withValue flag rest $ \v r ->
      once flag (null (moOutFile o)) (go o { moOutFile = v } r)
    go o (flag@"--generator-rev" : rest) = withValue flag rest $ \v r ->
      once flag (isNothing (moGeneratorRev o)) (go o { moGeneratorRev = Just v } r)
    go o (flag@"--include-merged" : rest) = withValue flag rest $ \v r ->
      go o { moIncludeMerged = moIncludeMerged o ++ [v] } r
    go o (flag@"--source-dirs" : rest) = withValue flag rest $ \v r ->
      go o { moSourceDirs = moSourceDirs o ++ [v] } r
    go o ("--allow-empty" : rest) = go o { moAllowEmpty = True } rest
    go o ("--no-own-tables" : rest) = go o { moNoOwnTables = True } rest
    go o ("--allow-empty-include" : rest) = go o { moAllowEmptyInclude = True } rest
    go o ("--allow-unmatched-override" : rest) = go o { moAllowUnmatchedOverride = True } rest
    go o ("--allow-columnless-table" : rest) = go o { moAllowColumnlessTable = True } rest
    go _ (other : _) = Left ("unrecognised argument: " <> other)

    once flag firstTime next
      | firstTime = next
      | otherwise = Left (flag <> " given more than once")

    withValue flag rest next = case rest of
      [] -> Left (flag <> " needs a value")
      v : r
        | null v -> Left (flag <> " has an empty value; is a shell variable unset?")
        | "--" `isPrefixOf` v ->
            Left (flag <> " needs a value, but the next argument is the option " <> v)
        | otherwise -> next v r
