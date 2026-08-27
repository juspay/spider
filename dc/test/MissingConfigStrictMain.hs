module Main (main) where

-- This stanza points the plugin at a domainConfigFile that does not exist,
-- with failOnFileNotFound left at its default (true). checkIntegrity must
-- raise a compile error rather than bypassing, so this module is EXPECTED TO
-- FAIL TO COMPILE.
--
-- `cabal build dc-test-missing-config-strict` failing IS the test passing.
-- Only built with -fnegative-tests (see dc.cabal); disabled by default so
-- downstream consumers of this package don't need to know about it.
main :: IO ()
main = putStrLn "unreachable: this module must fail to typecheck"
