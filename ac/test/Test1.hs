{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test1 where

import Data.Maybe
import GHC.StaticPtr

-- Data type with records
data Person = Person
    { name :: String
    , age :: Int
    , amount :: Int
    }
    deriving (Show)

data TxnDetail = TxnDetail
    { surchargeAmount :: Int
    , taxAmount :: Int
    }
    deriving (Show)

txnDetail :: TxnDetail
txnDetail = TxnDetail
    { surchargeAmount = 24
    , taxAmount = 12
    }

myNameHsLit :: String
myNameHsLit = "JHON"

myAmount :: Int
myAmount = surchargeAmount txnDetail

unknownVarHsOverLit = 20
-- Record creation
john :: Person
john = Person { name = myNameHsLit, age = 10, amount = incrementAmount (surchargeAmount txnDetail) }

testRecFld :: [Person] -> [String]
testRecFld persons = map name persons

incrementAmount :: Int -> Int
incrementAmount val = val + 1

testAge :: [Person] -> [Int]
testAge persons = map age persons

getName :: Person -> String -- not coming in FUN binds
getName p = name p

newtype Amount = Amount Int
  deriving (Show, Num)
  
val :: Amount
val = Amount (surchargeAmount txnDetail)

-- RecordDotSyntax test - requires GHC 9.2+
value :: Int
value = txnDetail.surchargeAmount + txnDetail.taxAmount

-- Record update - this should show HsRecFld in the field references
updatePerson :: Person -> Person
updatePerson p = p { age = 30, amount = surchargeAmount txnDetail }

newPerson :: Person
newPerson = Person { name = myNameHsLit, age = 10, amount = fromMaybe 0 $ handleSurchargeAmount (Just $ surchargeAmount txnDetail) }

processAmount :: Maybe Int -> Int
processAmount = \case
  Nothing -> 0
  Just surAmt -> surAmt + (surchargeAmount txnDetail)

testLet :: Int
testLet = 
  let surAmt = 100
      taxAmt = 20
  in if (surAmt + taxAmt) > 1 then surchargeAmount txnDetail else (surAmt + taxAmt)

listAmount :: [Int]
listAmount = [surchargeAmount txnDetail, taxAmount txnDetail, 1]

tupleAmount :: (Int, Int, Int)
tupleAmount = (surchargeAmount txnDetail, taxAmount txnDetail, 1)

amountList :: [TxnDetail] -> [Int]
amountList txns = [surchargeAmount txn + taxAmount txn | txn <- txns, surchargeAmount txn > 0]

testDo :: IO Int
testDo = do
  amt <- return (surchargeAmount txnDetail)
  print (surchargeAmount txnDetail)
  let x = amountList [txnDetail]
  let tax = taxAmount txnDetail
  print amt
  print x
  return (amt + tax)

testMultiIf :: Int -> Int
testMultiIf x = if
  | x > 100 -> surchargeAmount txnDetail
  | x > 50  -> amount john
  | otherwise -> 0

addSurcharge :: Int -> Int
addSurcharge = (surchargeAmount txnDetail +)

addToAmount :: Int -> Int
addToAmount = (+ surchargeAmount txnDetail)

negativeAmount :: Int
negativeAmount = -(surchargeAmount txnDetail)

staticAmount :: StaticPtr Int
staticAmount = static (surchargeAmount txnDetail)

myAmount1 :: Int
myAmount1 = (surchargeAmount txnDetail :: Int)

optimizedAmount :: Int
optimizedAmount = {-# SCC "optimized" #-} surchargeAmount txnDetail

parallelAmounts :: [TxnDetail] -> [TxnDetail] -> [(Int, Int)]
parallelAmounts txns1 txns2 = 
    [ (surchargeAmount t1, taxAmount t2) 
    | t1 <- txns1 
    | t2 <- txns2 
    ]

checkAmount :: Int -> Int
checkAmount x = case Just $ (surchargeAmount txnDetail) of
  Just amt -> (surchargeAmount txnDetail) + amt
  Nothing  -> 0

newPerson1 :: Person
newPerson1 = Person { name = myNameHsLit, age = 10, amount = ((surchargeAmount txnDetail) + 1)}
-- ============================================================================
-- HsAppType Test Cases - Type Applications with Amount Fields
-- ============================================================================

-- Test case 1: Simple type application with amount field selector
getAmountWithTypeApp :: Person -> Int
getAmountWithTypeApp p = (id @Int) (amount p)

-- Test case 2: Type application with surchargeAmount field selector
getSurchargeWithTypeApp :: TxnDetail -> Int
getSurchargeWithTypeApp txn = (id @Int) (surchargeAmount txn)

-- Test case 3: Type application in show with amount field
processAmountWithTypeApp :: Person -> String
processAmountWithTypeApp p = show @Int (amount p)

handleSurchargeAmount :: Maybe Int -> Maybe Int
handleSurchargeAmount surchargeAmount = 
    case surchargeAmount of
        Just amt -> surchargeAmount
        Nothing  -> Nothing

-- Main function

-- ============================================================================
-- OTHER EXAMPLES (COMMENTED OUT)
-- ============================================================================

-- -- 1. Simple function
-- add :: Int -> Int -> Int
-- add x y = x + y

-- -- 2. Pattern matching
-- isZero :: Int -> Bool
-- isZero 0 = True
-- isZero _ = False

-- -- 5. Record update
-- updateAge :: Person -> Int -> Person
-- updateAge p newAge = p { age = newAge }

-- -- 6. List operations
-- doubleList :: [Int] -> [Int]
-- doubleList xs = map (*2) xs

-- -- 7. Case expression
-- describeNum :: Int -> String
-- describeNum n = case n of
--     0 -> "zero"
--     1 -> "one"
--     _ -> "many"

-- -- 8. Let binding
-- calculate :: Int -> Int
-- calculate x = let y = x * 2
--               in y + 10

-- -- 9. Where clause
-- pythagoras :: Double -> Double -> Double
-- pythagoras a b = sqrt sumOfSquares
--   where
--     sumOfSquares = a*a + b*b

-- -- 10. Lambda
-- double :: Int -> Int
-- double = \x -> x * 2

-- -- 11. If expression
-- max' :: Int -> Int -> Int
-- max' a b = if a > b then a else b

-- -- 12. List comprehension
-- squares :: [Int]
-- squares = [x*x | x <- [1..5]]

-- -- 13. Guard
-- sign :: Int -> String
-- sign n
--     | n > 0  = "positive"
--     | n < 0  = "negative"
--     | otherwise = "zero"
