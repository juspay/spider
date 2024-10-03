{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SubTests.InfiniteRecursionTest where

import           Control.Concurrent (threadDelay)
import qualified Data.Aeson as A
import qualified TestUtils as TU

fn1 :: IO String
fn1 = pure "Fn1"

fn2 :: String
fn2 = "Fn2"

fn3 :: String -> String
fn3 a = a

data SumType = TypeA Int | TypeB 

instance A.ToJSON SumType where
  toJSON (TypeA v) = A.toJSON v 
  toJSON a = A.toJSON a -- STE :: Should Throw Error

data RecTypeA = RecTypeA {
    field1 :: Int,
    field2 :: Int
  }

-- Recursive but with different data/variable
pattern1 :: String -> IO String
pattern1 x = do
 let y = x <> "SameReturn1"
 x <- fn1
 z <- pattern1 x -- Should not throw error since x is changed
 pure (y <> z)

-- Self recursive variable
pattern2 :: String -> String
pattern2 _ = 
 let sameVal = sameVal <> "Dummy" in sameVal -- STE :: Should Throw Error since infinite self recursive variable usage

-- Self recursive function
pattern3 :: String -> String
pattern3 a = 
  let x = fn2
      y = "Dummy"
      z = pattern3 a -- STE :: Should Throw Error since infinite self recursive function invocation
  in x <> y <> z

-- Infinite Recursive function definition in `let`
pattern4 :: String -> String -> String
pattern4 x y =
  let recFn1 a b = fn3 $ recFn1 a b -- STE :: Should Throw error
  in recFn1 x y

-- Infinite Recursive function definition in `where`
pattern5 :: String -> String -> String
pattern5 x y = recFn2 x y
  where 
    recFn2 a b = fn3 $ recFn2 a b -- STE :: Should Throw error

-- Infinite recursion, but genuine case; not to be ignored here, but should be ignored in Test2
pattern6 :: IO a -> Int -> IO a
pattern6 flow delay = do
  !_ <- flow
  !_ <- threadDelay delay
  pattern6 flow delay -- STE :: Should Throw Error

-- Self recursive straight away
pattern7 :: Int -> Int
pattern7 val = pattern7 val -- STE :: Should Throw Error
  
-- Self recursive straight away but partial function
pattern8 :: Int -> Int
pattern8 = pattern8 -- STE :: Should Throw Error

-- Indirect infinite function
pattern9 :: String -> String
pattern9 a = 
  let x = fn2 <> y
      y = "Dummy" <> x
      z = pattern9 a -- STE :: Should throw error since infinite self recursive function invocation
  in x <> y <> z

-- Infinite recursion on pattern match on data function
pattern10 :: SumType -> SumType
pattern10 (TypeA num) = 
  let res = pattern10 (TypeA num) -- STE :: Should Throw Error
  in res
pattern10 _ = TypeB

-- Pattern matching with infinite recursion
pattern11 :: Int -> Int
pattern11 10 = pattern11 (10 :: Int) -- STE :: Should Throw Error
pattern11 _ = -1

-- Pattern matching with infinite recursion
pattern12 :: String -> String
pattern12 "Pattern" = pattern12 "Pattern" -- STE :: Should Throw Error
pattern12 _ = ""

-- Partial function with lambda case
pattern13 :: String -> String
pattern13 = \case
  "Pattern" -> pattern13 "Pattern" -- Should NOT Throw Error
  _         -> ""

-- Partial function with lambda case
pattern14 :: String -> String
pattern14 = \case
  "Pattern" -> "Terminate" 
  _         -> pattern14 "Pattern" -- Should NOT throw Error

-- Partial function with lambda case with extra args
pattern15 :: String -> String -> String
pattern15 a = \case
  "Pattern" -> "Terminate" 
  _         -> pattern15 a "Pattern" -- Should NOT throw Error

-- Partial function with lambda case with extra args
pattern16 :: String -> String -> String -> String
pattern16 a b = \case
  "Pattern" -> "Terminate" 
  _         -> pattern16 a b "Pattern" -- Should NOT throw Error

-- Partial function with lambda case with extra args but renamed function
pattern17 :: String -> String -> String -> String
pattern17 a b = \case
  "Pattern" -> "Terminate" 
  _         -> 
    let fn' = pattern17 a b -- Should NOT throw Error
        fn1' = pattern17 a -- Should NOT throw Error
    in fn' "Pattern"

-- Partial function with function composition
pattern18 :: String -> String -> String -> String
pattern18 a b = pattern15 "Hello" . pattern18 a b -- STE :: Should Throw Error

-- Partial function with function composition chain
pattern19 :: String -> String -> String -> String
pattern19 a b = pattern9 . pattern15 "Hello" . pattern19 a b -- STE :: Should Throw Error

-- Indirect recursion in where clause
pattern20 :: String -> String -> String
pattern20 a b = tempFn
  where
    tempFn :: String
    tempFn = pattern20 a b -- STE :: Should Throw Error

-- Partial function with let-in
pattern21 :: String -> String
pattern21 = 
  let z = "Dummy" 
      y = "Hello"
  in pattern21 -- STE :: Should Throw Error

-- Self recursive straight away
pattern22 :: Int
pattern22 = pattern22 -- STE :: Should Throw Error

-- Same function name but from different module
toJSON :: (A.ToJSON a) => a -> A.Value
toJSON = A.toJSON -- Should NOT Throw Error

pattern23 :: (Num a) => a -> a
pattern23 numVal = pattern23 numVal -- STE :: Should throw error

pattern24 :: forall a. (Num a) => a -> a
pattern24 numVal = pattern24 numVal -- STE :: Should throw error

pattern25 :: forall a. a -> a
pattern25 numVal = pattern25 numVal -- STE :: Should throw error

class TypeChanger a b where
  changeType :: a -> b

instance TypeChanger Integer Int where
  changeType = fromIntegral -- Should not throw error since no recursion

instance TypeChanger Integer SumType where
  changeType = TypeA . changeType -- Should NOT throw Error since type is changed

instance TypeChanger Integer Integer where
  changeType = changeType -- STE :: Should throw Error

main :: IO ()
main = do
  pure ()
  
  {- 

  Note:
  1. Shadow Binding are not an issue as of now

  TODO:
  1. Add check + tests for indirect infinite recursion
  2. Add check + tests for infinite list patterns
  3. Add check + tests for record construction patterns
  4. Add check + tests for Infix Constructor patterns
  5. Validate and add more tests for Pattern matching cases
  6. Add checks for infinite recursion involving guards with pattern matching

  -}