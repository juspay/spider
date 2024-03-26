{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Logerr.Plugin ()
import Data.Text as T
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Encoding as DTE
import Data.Aeson as A
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Test1 as T1

-- -- Data Types Declarations
-- data A = A Int String
--     deriving (Generic, Show, ToJSON, FromJSON)

-- data B = B {f1 :: Int, f2 :: A, f3 :: Text}
--     deriving (Generic, Show, ToJSON, FromJSON)

-- data CC = C1 | C2 Text | C3 Int
--     deriving (Generic, Show, ToJSON, FromJSON)

-- -- Data objects
-- obA :: A
-- obA = A 25 "Hello ObjectA"

-- obB :: B
-- obB = B 20 obA "Hello ObjectB"

-- obC1 :: CC
-- obC1 = C1

-- obC2 :: CC
-- obC2 = C2 "Hello ObjectC"

-- obC3 :: CC
-- obC3 = C3 30

-- str1 :: Text
-- str1 = encodeJSON ("Hello Str1" :: Text)

-- str2 :: Text
-- str2 = "Hello Str2"

-- str3 :: Text
-- str3 = T.pack $ show "Hello Str3"

-- -- Helper function
-- encodeJSON :: (ToJSON a) => a -> Text
-- encodeJSON = DTE.decodeUtf8 . BSL.toStrict . A.encode

-- logErrorV :: (ToJSON a) => a -> IO ()
-- logErrorV = print . toJSON

-- logErrorT :: Text -> IO ()
-- logErrorT = print

-- -- Test Cases Objects
-- obAT1 :: Text
-- obAT1 = T.pack $ show obA

-- obAT2 :: Text
-- obAT2 = encodeJSON obA

-- obBT1 :: Text
-- obBT1 = T.pack $ show obB

-- obBT2 :: Text
-- obBT2 = encodeJSON obB

-- obC1T1 :: Text
-- obC1T1 = T.pack $ show obC1

-- obC1T2 :: Text
-- obC1T2 = encodeJSON obC1

-- obC2T1 :: Text
-- obC2T1 = T.pack $ show obC2

-- obC2T2 :: Text
-- obC2T2 = encodeJSON obC2

-- obC3T1 :: Text 
-- obC3T1 = T.pack $ show obC3

-- obC3T2 :: Text
-- obC3T2 = encodeJSON obC3

-- Test Case 1: Text inside logErrorT (No error should be raised by plugin)
-- Test Case 2: Text inside logErrorV (An error should be raised by plugin)
-- Test Case 3: Object inside logErrorV (No error should be generated)
-- Test Case 4: Object inside logErrorT (By default, compile time error)
-- Test Case 5: `show Object` inside logErrorT (An error should be raised by the plugin)
-- Test Case 6: `show object` inside logErrorV (An error should be raised by the plugin)
-- Test Case 7: `encode object` inside logErrorT (An error should be raised by the plugin)
-- Test Case 8: `encode object` inside logErrorV (An error should be raised by the plugin)

-- Overall, when passing things inside log functions, we should never had `show` or `encode` applied to them
-- If we are passing some object, then we should use logErrorV and ideally it should have instance of `ToJSON`

loger ::  Text -> IO ()
loger x = T1.logErrorT (T1.encodeJSON x <> (T.pack $ show "Hello"))

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("Hello there" :: String)
    let obAT1 = "Hello" <> (T.pack $ show "Dummy2")
    loger T1.obAT1
    loger obAT1
    loger obAT1
    T1.logErrorT (T.pack $ show obAT1)
    T1.logErrorT ((T.pack . show) obAT1)
    T1.logErrorT ((T.pack . show) "Hello" <> obAT1)
    T1.logErrorT (T.pack "Hello" <> obAT1)
  where
    obAT1 = T1.encodeJSON ("Dummy3" :: String)

-- main2 :: 