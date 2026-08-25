module Main (main) where

-- The dc plugin (applied via this stanza's ghc-options) runs at compile
-- time, against a valid, parseable domainConfig.yaml. checkIntegrity must
-- take the Right branch and compile cleanly -- this module building and
-- running at all is the test passing.
main :: IO ()
main = putStrLn "OK: dc plugin ran against a valid config without error."
