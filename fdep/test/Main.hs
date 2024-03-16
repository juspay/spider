{-# LANGUAGE RankNTypes #-}
module Main (main,demo) where

import qualified Fdep.Plugin ()

-- main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String)
    where
        test :: String -> String
        test "HI" = "HI"
        test2 "HI" = "HI"
        test10 = demo

demo "HI" = 100