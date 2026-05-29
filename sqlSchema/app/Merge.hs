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

import           Control.Monad      (when)
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
    Right (opts, allowEmpty) -> do
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
          -- Safety gate: a contract with zero tables almost always means the
          -- SqlSchema GHC plugin did not run during compilation (so no
          -- per-module fragments were written), NOT that the package has no
          -- Beam tables.  Publishing an empty contract that still exits 0 is a
          -- downstream false-pass against the prod DB.  Fail loudly unless the
          -- caller explicitly opted in via --allow-empty.
          when (mrTablesEmitted report == 0 && not allowEmpty) $ do
            mapM_ (hPutStrLn stderr)
              [ "sql-schema-merge: ERROR: refusing to publish a contract with 0 tables."
              , "  out:        " <> moOutFile opts
              , "  fragments:  " <> moFragmentsDir opts
              , "This almost always means the SqlSchema GHC plugin did not run during"
              , "compilation, NOT that the package has no Beam tables.  An empty"
              , "contract is a downstream false-pass against the prod DB schema."
              , "Fixes:"
              , "  * Ensure the package was built with the SqlSchema cabal flag enabled"
              , "    (on by default in nix; local cabal builds opt out via -SqlSchema)."
              , "  * If this package genuinely has no tables (only composes deps via"
              , "    --include-merged), pass --allow-empty."
              ]
            exitWith (ExitFailure 1)

usage :: String
usage = unlines
  [ "Usage: sql-schema-merge --fragments DIR --out FILE"
  , "                       [--source-dirs DIR ...]"
  , "                       [--include-merged FILE ...]"
  , "                       [--allow-empty]"
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
  , "  --allow-empty          permit a contract with zero tables.  Without this,"
  , "                         emitting 0 tables is a fatal error (exit 1), since"
  , "                         it usually means the SqlSchema plugin did not run."
  ]

parseArgs :: [String] -> Either String (MergeOptions, Bool)
parseArgs = go (MergeOptions "" "" [] []) False False False
  where
    go opts allowEmpty haveFrag haveOut [] =
      if not haveFrag then Left "missing --fragments"
      else if not haveOut then Left "missing --out"
      else Right (opts, allowEmpty)
    go opts ae _ ho ("--fragments" : v : rest) =
      go opts { moFragmentsDir = v } ae True ho rest
    go opts ae hf _ ("--out" : v : rest) =
      go opts { moOutFile = v } ae hf True rest
    go opts ae hf ho ("--include-merged" : v : rest) =
      go opts { moIncludeMerged = moIncludeMerged opts ++ [v] } ae hf ho rest
    go opts ae hf ho ("--source-dirs" : v : rest) =
      go opts { moSourceDirs = moSourceDirs opts ++ [v] } ae hf ho rest
    go opts _ hf ho ("--allow-empty" : rest) =
      go opts True hf ho rest
    go _ _ _ _ (other : _) =
      Left ("unrecognised argument: " <> other)
