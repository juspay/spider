{-# LANGUAGE BangPatterns #-}

module SubTests.InfiniteRecursionTest where

import           Control.Concurrent (threadDelay)
import qualified TestUtils as TU

fn1 :: IO String
fn1 = pure "Fn1"

fn2 :: String
fn2 = "Fn2"

fn3 :: String -> String
fn3 a = a

-- Recursive but with different data/variable
pattern1 :: String -> IO String
pattern1 x = do
 let y = x <> "SameReturn1"
 x <- fn1
 z <- pattern1 x -- Should not throw error since x is changed
 pure (y <> z)

-- Self recursive variable
pattern2 :: String -> String
pattern2 x = 
 let x = x <> "Dummy" in x -- Should throw error since infinite self recursive variable usage

-- Self recursive function
pattern3 :: String -> String
pattern3 a = 
  let x = fn2
      y = "Dummy"
      z = pattern3 a -- Should throw error since infinite self recursive function invocation
  in x <> y <> z

-- Infinite Recursive function definition in `let`
pattern4 :: String -> String -> String
pattern4 x y =
  let recFn1 a b = fn3 $ recFn1 a b -- Should throw error
  in recFn1 x y

-- Infinite Recursive function definition in `where`
pattern5 :: String -> String -> String
pattern5 x y = recFn2 x y
  where 
    recFn2 a b = fn3 $ recFn2 a b -- Should throw error

-- Infinite recursion, but genuine case; not to be ignored here, but should be ignored in Test2
pattern6 :: IO a -> Int -> IO a
pattern6 flow delay = do
  !_ <- flow
  !_ <- threadDelay delay
  pattern6 flow delay

main :: IO ()
main = do
  res1 <- pattern1 "Main"
  print res1
  
  let res2 = pattern2 "Main"
  print res2

  let res3 = pattern3 "Main"
  print res3

  let res4 = pattern4 "Main1" "Main2"
  print res4

  let res5 = pattern5 "Main1" "Main2"
  print res5

  res6 <- pattern6 (print "IO Flow") 3000
  print res6

  pure ()
  
  {- 
    TODO:
    1. Add check + tests for indirect infinite recursion
    2. Add check + tests for infinite list patterns
  -}