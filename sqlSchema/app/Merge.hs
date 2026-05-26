{-# LANGUAGE LambdaCase #-}

-- | Entry point for the @sql-schema-merge@ CLI.  Reads per-module
-- fragments written by 'SqlSchema.Plugin' and produces the final
-- contract YAML.
--
-- Usage:
--   sql-schema-merge --fragments DIR --out FILE [--projectRoot DIR]
--
-- Exit codes:
--   0  success (even if some fragments were pruned as stale)
--   1  validation failed (duplicate table, override mismatch, …)
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
            <> " stale pruned, " <> show (mrTablesEmitted report)
            <> " tables emitted, " <> show (mrOverridesEmitted report)
            <> " overrides recorded -> " <> moOutFile opts

usage :: String
usage = unlines
  [ "Usage: sql-schema-merge --fragments DIR --out FILE [--projectRoot DIR]"
  , ""
  , "  --fragments DIR      directory holding per-module *.yaml fragments"
  , "  --out FILE           path to write the merged YAML"
  , "  --projectRoot DIR    if given, fragment sourceFile paths are"
  , "                       resolved relative to this directory when"
  , "                       checking staleness"
  ]

parseArgs :: [String] -> Either String MergeOptions
parseArgs = go (MergeOptions "" "" Nothing) False False
  where
    go opts haveFrag haveOut [] =
      if not haveFrag then Left "missing --fragments"
      else if not haveOut then Left "missing --out"
      else Right opts
    go opts _ ho ("--fragments" : v : rest) =
      go opts { moFragmentsDir = v } True ho rest
    go opts hf _ ("--out" : v : rest) =
      go opts { moOutFile = v } hf True rest
    go opts hf ho ("--projectRoot" : v : rest) =
      go opts { moProjectRoot = Just v } hf ho rest
    go _ _ _ (other : _) =
      Left ("unrecognised argument: " <> other)
