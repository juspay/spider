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
--
-- Exit codes:
--   0  success (even if some fragments were pruned as stale)
--   1  validation failed (duplicate-type mismatch, override mismatch, …)
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
    Right opts -> do
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

usage :: String
usage = unlines
  [ "Usage: sql-schema-merge --fragments DIR --out FILE"
  , "                       [--source-dirs DIR ...]"
  , "                       [--include-merged FILE ...]"
  , ""
  , "  --fragments DIR        directory holding per-module *.yaml fragments"
  , "  --out FILE             path to write the merged YAML"
  , "  --source-dirs DIR      (repeatable) directory containing .hs source files;"
  , "                         fragments whose module isn't backed by any .hs file"
  , "                         in any listed source dir are pruned.  If omitted,"
  , "                         no pruning is done (every fragment is kept)."
  , "  --include-merged FILE  (repeatable) pre-merged contract YAML to union into"
  , "                         the output.  Typically '${dep}/share/sql-schema/"
  , "                         sql-schema.yaml' for a dep package whose tables"
  , "                         must compose into this service's contract."
  ]

parseArgs :: [String] -> Either String MergeOptions
parseArgs = go (MergeOptions "" "" [] []) False False
  where
    go opts haveFrag haveOut [] =
      if not haveFrag then Left "missing --fragments"
      else if not haveOut then Left "missing --out"
      else Right opts
    go opts _ ho ("--fragments" : v : rest) =
      go opts { moFragmentsDir = v } True ho rest
    go opts hf _ ("--out" : v : rest) =
      go opts { moOutFile = v } hf True rest
    go opts hf ho ("--include-merged" : v : rest) =
      go opts { moIncludeMerged = moIncludeMerged opts ++ [v] } hf ho rest
    go opts hf ho ("--source-dirs" : v : rest) =
      go opts { moSourceDirs = moSourceDirs opts ++ [v] } hf ho rest
    go _ _ _ (other : _) =
      Left ("unrecognised argument: " <> other)
