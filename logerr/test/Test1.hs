{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Test1 where

import qualified Logerr.Plugin ()
import Data.Text as T
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Encoding as DTE
import Data.Aeson as A
import Data.String
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL


-- Data Types Declarations
data A = A Int String
    deriving (Generic, Show, ToJSON, FromJSON)

data B = B {f1 :: Int, f2 :: A, f3 :: Text}
    deriving (Generic, Show, ToJSON, FromJSON)

data CC = C1 | C2 Text | C3 Int | C4 Bool
    deriving (Generic, Show, ToJSON, FromJSON)

-- Data objects
obA :: A
obA = A 25 "Hello ObjectA"

obB :: B
obB = B 20 obA "Hello ObjectB"

obC1 :: CC
obC1 = C1

obC2 :: CC
obC2 = C2 "Hello ObjectC"

obC3 :: CC
obC3 = C3 30

obC4 :: CC
obC4 = C4 False

str1 :: Text
str1 = encodeJSON ("Hello Str1" :: Text)

str2 :: Text
str2 = "Hello Str2"

str3 :: Text
str3 = T.pack $ show "Hello Str3"

str4 :: Text
str4 = T.pack $ show (T.pack "Hello Str3")

-- Helper function
encodeJSON :: (ToJSON a) => a -> Text
encodeJSON = DTE.decodeUtf8 . BSL.toStrict . A.encode

logErrorV :: (ToJSON a) => a -> IO ()
logErrorV = print . toJSON

logErrorT :: Text -> IO ()
logErrorT = print

logError :: String -> String -> IO ()
logError _ = print

-- Test Cases Objects
obAT1 :: Text
obAT1 = T.pack $ show obA

obAT2 :: Text
obAT2 = encodeJSON obA

obBT1 :: Text
obBT1 = T.pack $ show obB

obBT2 :: Text
obBT2 = encodeJSON obB

obC1T1 :: Text
obC1T1 = T.pack $ show obC1

obC1T2 :: Text
obC1T2 = encodeJSON obC1

obC2T1 :: Text
obC2T1 = T.pack $ show obC2

obC2T2 :: Text
obC2T2 = encodeJSON obC2

obC3T1 :: Text 
obC3T1 = T.pack $ show obC3

obC3T2 :: Text
obC3T2 = encodeJSON obC3

-- Test Case 1: Text inside logErrorT (No error should be raised by plugin)
-- Test Case 2: Text inside logErrorV (An error should be raised by plugin)
-- Test Case 3: Object inside logErrorV (No error should be generated)
-- Test Case 4: Object inside logErrorT (By default, compile time error)
-- Test Case 5: `show Object` inside logErrorT (An error should be raised by the plugin)
-- Test Case 6: `show object` inside logErrorV (An error should be raised by the plugin)
-- Test Case 7: `encode object` inside logErrorT (An error should be raised by the plugin)
-- Test Case 8: `encode object` inside logErrorV (An error should be raised by the plugin)

-- Also, from what sources we might be passing the value to logErrorT
-- 1. Received as function argument
-- 2. Created as local bind
-- 3. Imported from another module
-- 1 and 3 are same for me, I have to go and check from where I received it, change of module does not matter

-- Overall, when passing things inside log functions, we should never had `show` or `encode` or encodeJSON applied to them
-- If we are passing some object, then we should use logErrorV and ideally it should have instance of `ToJSON`

-- Scenario 1: Parameter sent to logger is modified in the current function and the before modification is not text (We just need to verify that the modification is not `encode`, `encodeJSON` or `show`)
-- Scenario 2: Parameter sent to logger is modified in the current function and the before modification is text and modification is stringification (We need to throw error without recursive backtracking)
-- Scenario 3: Parameter sent to logger is modified in the current function and the before modification is text and modification is not stringification (We need to mark current function as a logger function, and it needs to be checked all calls to this function)
-- Scenario 4: Parameter sent to logger is same as some argument in the current function and that argument is of text type (mark current function as a logger function, and it needs to be checked all calls to this function, it will behave like `logErrorT`)
-- Scenario 5: Parameter sent to logger is same as some argument in the current function and that argument is of non-text type (PASS case)

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String)
    let obAT1 = "Dummy"
    let b = logInfoT "tester" logger
    logError "tag" $ obAT1 <> show Test1.obAT1
  where
    logErrorT = Test1.logErrorT

logInfoT :: String -> (forall a b. (IsString b, Show a) => String -> a -> b) -> String
logInfoT x _ = x

logger :: forall a b. (IsString b, Show a) => String -> a -> b
logger _ = fromString . show

-- myFun :: A -> IO Text
-- myFun ob = do
--     let (A x y) = ob
--         res1 = A x "Modified"
--         res2 = encodeJSON 