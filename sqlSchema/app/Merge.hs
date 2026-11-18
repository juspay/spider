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
--                    [--allow-empty-include] [--allow-unmatched-override]
--                    [--allow-columnless-table]
--                    [--generator-rev REV]
--
-- Exit codes:
--   0  success (even if some fragments were pruned as stale)
--   1  validation failed (duplicate-type mismatch, override mismatch, …), an
--      input it would have to drop tables for, a contract it refuses to
--      publish ('SqlSchema.Merge.publishProblem'), or an I/O error
--   2  argument error: a repeated @--fragments@, @--out@ or @--generator-rev@,
--      an empty value, a value that starts with @--@, a flag without its value,
--      an unknown argument, or a missing @--fragments@ or @--out@
--
-- @--out@ is written only after every check has passed: a merge that exits 1
-- or 2 never creates or replaces it.
module Main (main) where

import           System.Environment (getArgs)
import           System.Exit        (exitWith)

import           SqlSchema.MergeCli (mergeMain)

main :: IO ()
main = getArgs >>= mergeMain >>= exitWith
