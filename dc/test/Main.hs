module Main (main) where

import Sample

main :: IO ()
main = do
    let s = testParentFunction
    putStrLn "Test suite not ye implemented."

testParentFunction :: String
testParentFunction = proxyFunction


