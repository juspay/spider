{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test1 where

import qualified Sheriff.Plugin ()
import qualified TestUtils as TU
import qualified TestUtils
import Data.Text as T
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Encoding as DTE
import Data.Aeson as A
import Data.String
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)


-- Data Types Declarations
data A = A Int String
    deriving (Generic, Show, ToJSON, FromJSON)

data B = B {f1 :: Int, f2 :: A, f3 :: Text}
    deriving (Generic, Show, ToJSON, FromJSON)

data DBT (f :: Type -> Type) = DB {dbf1 :: f Int}
    deriving (Generic)

type DB = DBT Identity

deriving stock instance Show (DBT Identity)
deriving anyclass instance FromJSON (DBT Identity)
deriving anyclass instance ToJSON (DBT Identity)

data CC = C1 | C2 Text | C3 Int | C4 Bool
    deriving (Generic, Show, ToJSON, FromJSON)

data SeqIs = SeqIs Text Text
    deriving (Generic, Show, ToJSON, FromJSON)

data EnumT = X | Y | Z
    deriving (Generic, Show, ToJSON, FromJSON)

data EnumT2 = U EnumT | V
    deriving (Generic, Show, ToJSON, FromJSON)

data EnumT3 x = M | N
    deriving (Generic, Show, ToJSON, FromJSON)

type P = Text

en :: EnumT
en = Y

en2 :: EnumT2
en2 = V

en21 :: P
en21 = "Hello"

en22 :: Text
en22 = "Hello"

en3 :: EnumT3 ()
en3 = M

ob :: SeqIs
ob = SeqIs "fldName" "fldValue"

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
str4 = T.pack $ show (T.pack "Hello Str4")

db1 :: DB
db1 = DB 500

-- Helper function
encodeJSON :: (ToJSON a) => a -> Text
encodeJSON = DTE.decodeUtf8 . BSL.toStrict . A.encode

runKVDB :: IO ()
runKVDB = print "Somehow it's runKVDB"

logErrorV :: (ToJSON a) => a -> IO ()
logErrorV = print . toJSON

logDebugT :: Text -> Text -> IO ()
logDebugT _ = print

logDebug :: (Show b) => a -> b -> IO ()
logDebug _ = print

forkErrorLog :: (Show b) => a -> b -> IO ()
forkErrorLog _ = print

logErrorT :: Text -> Text -> IO ()
logErrorT _ = print

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

num1 :: TU.Number
num1 = TU.Number 20

num2 :: TU.Number
num2 = TU.Number 10

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

addQuotes :: Text -> Text
addQuotes t = "\"" <> t <> "\""

noLogFn :: String -> String -> IO ()
noLogFn _ _ = pure ()

throwException :: ()
throwException = ()

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String)
    let obAT1 = "Dummy"
    let b = logInfoT "tester" logger
    logError "tag" $ show obC1
    logError "tag" $ show en
    logErrorT "tag" $ encodeJSON en
    logError "tag" $ show en2
    logErrorT (T.pack $ show "tag") $ encodeJSON en2
    logError "tag" $ show en3
    logErrorT "tag" $ encodeJSON en3
    logError "tag" $ show (en, "This is Text" :: String)
    logErrorT "tag" $ encodeJSON (en, "This is Text" :: String)
    logError "tag" $ show (en, 20 :: Int) -- Should not throw error because of show is allowed on both enums and int
    logErrorT "tag" $ encodeJSON (en, 20 :: Int) -- Should throw error because of encodeJSON
    logError "tag" $ obAT1 <> show Test1.obAT1
    fn $ logError "tag2" $ show obA
    fn $ logError "tag2" $ ("Hello" <> show obA)
    forkErrorLog "tag2" $ ("Hello" <> (show $ addQuotes "Tll"))
    noLogFn "tag2" $ ("Hello" <> (show $ addQuotes "Tll"))

    logDebug ("Some Tag" :: Text) (show "This to print")

    -- Test for Qualified Function Names Rules
    print $ TU.throwException "Hello" -- should throw error
    print throwException -- should NOT throw error
    print $ TU.throwExceptionV2 "Hello" -- should throw error as part of combined rule "Hello"
    print $ TU.throwExceptionV3 "Hello"
    print $ TU.throwExceptionV4 "Hello" -- should throw error as part of combined rule "Hello"

    print $ show temp
    print $ show temp1
    print $ (show) (temp1)
    print $ show temp2
    print $ show en2
    print $ show en21
    print $ show en22
    print $ show temp3
    print $ show temp4
    print $ show $ dbf1 db1
    logError "tag" $ show en2 <> show obA
    logError "tag" $ show en <> show obB
    logError "tag" $ show temp5
    logError "tag" $ show temp6
    
    let (TU.Number sRes) = num1 `TU.subtractNumber` num2
        (TU.Number aRes) = TU.addNumber num1 num2
        (TU.Number mRes) = (TU.*?) Nothing num1 num2
        (TU.Number n1) = TU.fstArg num1 num2
        (TU.Number n2) = TU.sndArg num1 num2

    print sRes
    print aRes
    print mRes
    print n1
    print n2
    print (n1 * n2)
    print ((*) 10 20)

    runKVDB -- Should be error

    logDebugT "validateMandate" $ "effective limit is: " <> T.pack (show 10) <> ", custom limit for key: " <> " is " <> T.pack (show (Just ("Hello" :: Text)))

    logErrorT "Incorrect Feature in DB" 
          $  "merchantId: " <> ", error: " <> T.pack (show (["A", "B"] :: [Text]))
  where
    logErrorT = Test1.logErrorT

(^*^) :: Num a => a -> a -> a
(^*^) a b = a 

logInfoT :: String -> (forall a b. (IsString b, Show a) => String -> a -> b) -> String
logInfoT x _ = x

logger :: forall a b. (IsString b, Show a) => String -> a -> b
logger _ = fromString . show

temp :: [Text]
temp = []

temp1 :: Maybe Text
temp1 = Nothing

temp2 :: (Text, Text)
temp2 = ("A", "B")

temp3 :: (Text, Int)
temp3 = ("A", 10)

temp4 :: (Int, Int)
temp4 = (20, 10)

temp5 :: [EnumT]
temp5 = []

temp6 :: [EnumT2]
temp6 = []

fn :: IO () -> IO ()
fn x = do
    _ <- x
    pure ()

-- myFun :: A -> IO Text
-- myFun ob = do
--     let (A x y) = ob
--         res1 = A x "Modified"
--         res2 = encodeJSON 