{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}


module FailingTypes where

import UnusedFieldChecker.FieldChecker (FieldChecker(..))
import GHC.Generics (Generic)

-- Basic record with unused field
data Person = Person
    { personName :: String
    , personAge :: Int
    , unusedField :: String
    , optionalField :: Maybe Int
    } deriving (Show, Generic)

instance FieldChecker Person

usePerson :: Person -> String
usePerson p = personName p ++ " is " ++ show (personAge p) ++ " years old"

describePerson :: Person -> String
describePerson Person { personName = name, personAge = age } =
    "Person: " ++ name ++ ", Age: " ++ show age

-- Nested records with unused fields
data Address = Address
    { street :: String
    , city :: String
    , zipCode :: String
    , country :: String  -- unused field
    } deriving (Show, Generic)

instance FieldChecker Address

data Employee = Employee
    { empName :: String
    , empId :: Int
    , empAddress :: Address
    , department :: String  -- unused field
    , salary :: Maybe Double
    } deriving (Show, Generic)

instance FieldChecker Employee

useEmployee :: Employee -> String
useEmployee emp = empName emp ++ " works in " ++ city (empAddress emp)

-- Parameterized record type
data Container a = Container
    { contents :: a
    , containerId :: Int
    , containerSize :: Int  -- unused field
    , containerLabel :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker (Container String)

useContainer :: Container String -> String
useContainer c = "Container " ++ show (containerId c) ++ ": " ++ contents c

-- Record with multiple constructors (sum type)
data Vehicle = Car
    { carMake :: String
    , carModel :: String
    , carYear :: Int
    , carColor :: String  -- unused field
    }
    | Bike
    { bikeMake :: String
    , bikeModel :: String
    , bikeType :: String  -- unused field
    }
    deriving (Show, Generic)

instance FieldChecker Vehicle

useVehicle :: Vehicle -> String
useVehicle (Car make model year _) = make ++ " " ++ model ++ " (" ++ show year ++ ")"
useVehicle (Bike make model) = make ++ " " ++ model

-- Newtype wrapper
newtype UserId = UserId
    { getUserId :: Int
    } deriving (Show, Generic)

instance FieldChecker UserId

useUserId :: UserId -> String
useUserId (UserId uid) = "User #" ++ show uid

-- Record with many fields
data Configuration = Configuration
    { cfgHost :: String
    , cfgPort :: Int
    , cfgTimeout :: Int
    , cfgRetries :: Int
    , cfgDebugMode :: Bool  -- unused field
    , cfgLogLevel :: String  -- unused field
    , cfgCacheSize :: Maybe Int
    , cfgEnableSsl :: Maybe Bool
    } deriving (Show, Generic)

instance FieldChecker Configuration

useConfig :: Configuration -> String
useConfig cfg = cfgHost cfg ++ ":" ++ show (cfgPort cfg)

-- Record with strict fields
data StrictRecord = StrictRecord
    { !strictField1 :: Int
    , !strictField2 :: String
    , !unusedStrict :: Double  -- unused strict field
    , optionalStrict :: Maybe Bool
    } deriving (Show, Generic)

instance FieldChecker StrictRecord

useStrictRecord :: StrictRecord -> String
useStrictRecord r = show (strictField1 r) ++ ": " ++ strictField2 r

-- Record with unpacked fields
data UnpackedRecord = UnpackedRecord
    { {-# UNPACK #-} unpackedInt :: Int
    , {-# UNPACK #-} unpackedDouble :: Double  -- unused unpacked field
    , regularField :: String
    , optionalUnpacked :: Maybe Int
    } deriving (Show, Generic)

instance FieldChecker UnpackedRecord

useUnpackedRecord :: UnpackedRecord -> String
useUnpackedRecord r = show (unpackedInt r) ++ ": " ++ regularField r
