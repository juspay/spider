{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main,demo) where

-- import qualified Fdep.Plugin ()

data A = A{x :: Int}


-- main :: IO ()
main = do
    let a = A 20
    print a.x
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String)
    where
        test :: String -> String
        test "HI" = "HI"
        test2 "HI" = "HI"
        test10 = demo

demo "HI" = 100