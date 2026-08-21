module Main (main) where

-- This stanza points the plugin at .juspay/invalid/domainConfig.yaml, which
-- is malformed YAML. checkIntegrity's `Left err` branch must now raise a
-- compile error unconditionally (it can no longer be bypassed), so this
-- module is EXPECTED TO FAIL TO COMPILE.
--
-- `cabal build dc-test-invalid-yaml` failing IS the test passing.
main :: IO ()
main = putStrLn "unreachable: this module must fail to typecheck"
