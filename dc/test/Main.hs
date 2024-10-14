module Main (main) where

import TestCases
import RestrictedFuncs
import PathsToConsider.Considered

main :: IO ()
main = do
    let s = testParentFunction
    putStrLn "Test suite not ye implemented."

testParentFunction :: String
testParentFunction = 
    let
        testCase1 = simpleReturn
        testCase2 = leftCaseError
        testCase3 = rightCaseError
        testCase4 = indirectSimpleReturn
        testCase5 = indirectLeftCaseError
        testCase6 = indirectRightCaseError
        testCase7 = whereClauseErr
        testCase8 = whereClauseCaseRightErr
        testCase9 = ignoredErr
        testCase10 = consideredFun
    in "Nothing"
