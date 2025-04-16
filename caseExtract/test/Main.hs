module Main (main) where

import System.Environment


data A = B | C
    deriving Show

main :: IO ()
main = do
    y <- getArgs
    let x = if length y > 1 then B else C
    print x
    case x of
        B -> pure ()
        C -> print "HII"