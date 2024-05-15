{-# LANGUAGE RankNTypes #-}
module Main (main,demo,ddd) where

import qualified Fdep.Plugin ()

-- main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String) 
    where
        test :: String -> String
        test "HI" = "HI"
        test2 v = test (v <> ddd) 
        test10 :: String -> String
        test10 _ = demo "1000" "!))"

demo :: (Show x) => x -> x -> String
demo x y = (show y <> show x <> show 100)

ddd = "HITHERE"