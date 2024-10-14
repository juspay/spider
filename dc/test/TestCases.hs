{-# OPTIONS_GHC -fplugin=DC.DefaultCheck  #-}

module TestCases where

import RestrictedFuncs
import IgnorePath.IgnoreModule

ignoredErr :: String
ignoredErr = ignoredFun

simpleReturn :: String
simpleReturn = throwErr

whereClauseCaseRightErr :: String
whereClauseCaseRightErr = case eitherValue of
    Left _ -> "Nothing"
    Right _ -> temp
    where
        temp = throwErr

leftCaseError :: String
leftCaseError = case eitherValue of
    Left _ -> throwErr
    Right _ -> "Nothing"

rightCaseError :: String
rightCaseError = case eitherValue of
    Left _ -> "Nothing"
    Right _ -> throwErr

indirectSimpleReturn :: String
indirectSimpleReturn = tempThrowErr

indirectLeftCaseError :: String
indirectLeftCaseError = case eitherValue of
    Left _ -> tempThrowErr
    Right _ -> "Nothing"

indirectRightCaseError :: String
indirectRightCaseError = case eitherValue of
    Left _ -> "Nothing"
    Right _ -> tempThrowErr


whereClauseErr :: String
whereClauseErr = temp
    where
        temp = throwErr

whereClauseCaseLeftErr :: String
whereClauseCaseLeftErr = case eitherValue of
    Left _ -> temp
    Right _ -> "Nothing"
    where
        temp = throwErr

tempThrowErr :: String
tempThrowErr = throwErr

eitherValue :: (Either String String)
eitherValue = Left "String"
