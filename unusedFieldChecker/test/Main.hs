-- | Test entry point for UnusedFieldChecker plugin tests.
-- This test is expected to FAIL compilation due to unused non-Maybe fields in FailingTypes.
module Main where

import FailingTypes
import qualified AccessPatterns as AP
import qualified EdgeCases as EC

main :: IO ()
main = do
    putStrLn "=== Testing Basic Person Type ==="
    let person = Person
            { personName = "Alice"
            , personAge = 30
            , unusedField = "this field is never read"  -- Used in construction but never accessed
            , optionalField = Nothing
            }
    putStrLn $ usePerson person
    putStrLn $ describePerson person

    putStrLn "\n=== Testing Nested Records ==="
    let address = Address
            { street = "123 Main St"
            , city = "Springfield"
            , zipCode = "12345"
            , country = "USA"  -- unused field
            }
    let employee = Employee
            { empName = "Bob"
            , empId = 42
            , empAddress = address
            , department = "Engineering"  -- unused field
            , salary = Just 75000
            }
    putStrLn $ useEmployee employee

    putStrLn "\n=== Testing Parameterized Types ==="
    let container = Container
            { contents = "Important Data"
            , containerId = 1
            , containerSize = 100  -- unused field
            , containerLabel = Just "Label"
            }
    putStrLn $ useContainer container

    putStrLn "\n=== Testing Sum Types with Records ==="
    let car = Car
            { carMake = "Toyota"
            , carModel = "Camry"
            , carYear = 2020
            , carColor = "Blue"  -- unused field
            }
    let bike = Bike
            { bikeMake = "Trek"
            , bikeModel = "FX 3"
            , bikeType = "Hybrid"  -- unused field
            }
    putStrLn $ useVehicle car
    putStrLn $ useVehicle bike

    putStrLn "\n=== Testing Newtype ==="
    let userId = UserId { getUserId = 12345 }
    putStrLn $ useUserId userId

    putStrLn "\n=== Testing Configuration Record ==="
    let config = Configuration
            { cfgHost = "localhost"
            , cfgPort = 8080
            , cfgTimeout = 30
            , cfgRetries = 3
            , cfgDebugMode = False  -- unused field
            , cfgLogLevel = "INFO"  -- unused field
            , cfgCacheSize = Just 1024
            , cfgEnableSsl = Just True
            }
    putStrLn $ useConfig config

    putStrLn "\n=== Testing Strict Fields ==="
    let strictRec = StrictRecord
            { strictField1 = 42
            , strictField2 = "strict"
            , unusedStrict = 3.14  -- unused strict field
            , optionalStrict = Just True
            }
    putStrLn $ useStrictRecord strictRec

    putStrLn "\n=== Testing Unpacked Fields ==="
    let unpackedRec = UnpackedRecord
            { unpackedInt = 100
            , unpackedDouble = 2.718  -- unused unpacked field
            , regularField = "regular"
            , optionalUnpacked = Just 50
            }
    putStrLn $ useUnpackedRecord unpackedRec

    putStrLn "\n=== Testing Access Patterns ==="
    AP.testAllPatterns

    putStrLn "\n=== Testing Edge Cases ==="
    putStrLn "Single field:"
    let single = EC.SingleField { EC.singleValue = "test" }
    putStrLn $ EC.useSingle single

    putStrLn "\nTree structure:"
    let tree = EC.Node
            { EC.nodeValue = 1
            , EC.nodeLeft = EC.Leaf { EC.leafValue = 2, EC.leafId = 0, EC.leafOptional = Nothing }
            , EC.nodeRight = EC.Leaf { EC.leafValue = 3, EC.leafId = 1, EC.leafOptional = Nothing }
            , EC.nodeDepth = 1
            , EC.nodeOptional = Nothing
            }
    putStrLn $ "Tree sum: " ++ show (EC.sumTree tree)
    putStrLn $ "Tree depth: " ++ show (EC.getDepth tree)

    putStrLn "\nIf you see this, the test passed (but it shouldn't due to unused fields!)"
