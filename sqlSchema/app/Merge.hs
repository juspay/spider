{-# LANGUAGE LambdaCase #-}

-- | Entry point for the @sql-schema-merge@ CLI.  Reads per-module
-- fragments written by 'SqlSchema.Plugin', optionally unions in
-- pre-merged contracts from dependency packages, prunes fragments
-- whose source module has been deleted, and writes the final contract
-- YAML.
--
-- Usage:
--   sql-schema-merge --fragments DIR --out FILE
--                    [--source-dirs DIR] ...
--                    [--include-merged FILE] ...
--                    [--no-own-tables] [--allow-empty]
--                    [--generator-rev REV]
--
-- Exit codes:
--   0  success (even if some fragments were pruned as stale)
--   1  validation failed (duplicate-type mismatch, override mismatch, …), an
--      input it would have to drop tables for, a contract it refuses to
--      publish ('publishProblem'), or an I/O error
--   2  argument error
module Main (main) where

import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitWith)
import           System.IO          (hPutStrLn, stderr)

import           SqlSchema.Merge

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr ("sql-schema-merge: " <> err)
      hPutStrLn stderr usage
      exitWith (ExitFailure 2)
    Right (opts, allowEmpty, noOwnTables) -> do
      result <- runMerge opts
      case result of
        Left errs -> do
          mapM_ (hPutStrLn stderr) errs
          exitWith (ExitFailure 1)
        Right report -> do
          putStrLn $ "sql-schema-merge: " <> show (mrFragmentsRead report)
            <> " fragments read, " <> show (mrStalePruned report)
            <> " stale pruned, " <> show (mrIncludedFiles report)
            <> " included contracts (" <> show (mrIncludedTables report)
            <> " tables), " <> show (mrDeduped report)
            <> " duplicates collapsed, " <> show (mrTablesEmitted report)
            <> " tables emitted, " <> show (mrOverridesEmitted report)
            <> " overrides recorded -> " <> moOutFile opts
          -- Safety gate: publishing a contract the plugin may not have
          -- produced is a downstream false-pass against the prod DB.
          case publishProblem allowEmpty noOwnTables report of
            Nothing      -> pure ()
            Just problem -> do
              mapM_ (hPutStrLn stderr) (refusal opts problem)
              exitWith (ExitFailure 1)

refusal :: MergeOptions -> PublishProblem -> [String]
refusal opts = \case
  OwnTablesDeclaredNone n ->
    [ "sql-schema-merge: ERROR: --no-own-tables was given, but this package's own"
    , "fragments define " <> show n <> " table(s).  Remove --no-own-tables so the"
    , "merge checks this package's own fragments again."
    , "  fragments:  " <> moFragmentsDir opts
    ]
  NoTables       -> pluginDidNotRun "the contract has 0 tables"
  NoOwnFragments -> pluginDidNotRun "no fragment of this package's own modules was read"
  where
    pluginDidNotRun why =
      [ "sql-schema-merge: ERROR: refusing to publish: " <> why <> "."
      , "  out:        " <> moOutFile opts
      , "  fragments:  " <> moFragmentsDir opts
      , "This almost always means the SqlSchema GHC plugin did not run during"
      , "compilation, NOT that the package has no Beam tables.  Publishing it is"
      , "a downstream false-pass against the prod DB schema."
      , "Fixes:"
      , "  * Ensure the package was built with the SqlSchema cabal flag enabled"
      , "    (on by default in nix; local cabal builds opt out via -SqlSchema)."
      , "  * If this package defines no tables of its own and only composes"
      , "    dependencies via --include-merged, pass --no-own-tables."
      , "  * If the contract is meant to have no tables at all, pass --allow-empty."
      ]

usage :: String
usage = unlines
  [ "Usage: sql-schema-merge --fragments DIR --out FILE"
  , "                       [--source-dirs DIR ...]"
  , "                       [--include-merged FILE ...]"
  , "                       [--no-own-tables] [--allow-empty]"
  , "                       [--generator-rev REV]"
  , ""
  , "  --fragments DIR        directory holding per-module *.yaml fragments"
  , "  --out FILE             path to write the merged YAML"
  , "  --source-dirs DIR      (repeatable) directory containing module sources"
  , "                         (.hs, .lhs, .hsc, .chs, .x, .y, .ly); fragments whose"
  , "                         module has no source file in any listed dir are"
  , "                         pruned, and named on stderr.  If omitted, no pruning"
  , "                         is done (every fragment is kept)."
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
  , "  --generator-rev REV    VCS revision of the plugin source, recorded in the"
  , "                         contract's generator stamp (omitted -> rev: null)."
  ]

-- | @(options, allowEmpty, noOwnTables)@.
parseArgs :: [String] -> Either String (MergeOptions, Bool, Bool)
parseArgs = go (MergeOptions "" "" [] [] Nothing) False False False False
  where
    go opts allowEmpty noOwn haveFrag haveOut [] =
      if not haveFrag then Left "missing --fragments"
      else if not haveOut then Left "missing --out"
      else Right (opts, allowEmpty, noOwn)
    go opts ae no _ ho ("--fragments" : v : rest) =
      go opts { moFragmentsDir = v } ae no True ho rest
    go opts ae no hf _ ("--out" : v : rest) =
      go opts { moOutFile = v } ae no hf True rest
    go opts ae no hf ho ("--include-merged" : v : rest) =
      go opts { moIncludeMerged = moIncludeMerged opts ++ [v] } ae no hf ho rest
    go opts ae no hf ho ("--source-dirs" : v : rest) =
      go opts { moSourceDirs = moSourceDirs opts ++ [v] } ae no hf ho rest
    go opts ae no hf ho ("--generator-rev" : v : rest) =
      go opts { moGeneratorRev = Just v } ae no hf ho rest
    go opts _ no hf ho ("--allow-empty" : rest) =
      go opts True no hf ho rest
    go opts ae _ hf ho ("--no-own-tables" : rest) =
      go opts ae True hf ho rest
    go _ _ _ _ _ (other : _) =
      Left ("unrecognised argument: " <> other)
