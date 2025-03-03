module Main (main) where

data A = B | C

main :: IO ()
main = putStrLn "Test suite not yet implemented."

test (B) = ""
test (C) = ""

test3 x y = 
    case [x,y] of
        _ -> pure ()