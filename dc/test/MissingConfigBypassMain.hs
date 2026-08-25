module Main (main) where

-- This stanza points the plugin at a domainConfigFile that does not exist,
-- with failOnFileNotFound=false. checkIntegrity must bypass silently (no
-- compile error) rather than running its checks -- this module building and
-- running at all is the test passing.
main :: IO ()
main = putStrLn "OK: dc plugin bypassed cleanly when the config file was missing (failOnFileNotFound=false)."
