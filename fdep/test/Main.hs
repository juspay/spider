{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Text as T
import Data.List (foldl')
import Control.Monad (when)
import Control.Applicative ((<|>))
import qualified Fdep.Plugin ()
import GHC.Base (undefined)

-- Basic data type
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

-- Algebraic data type
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving (Show)

-- Type with type variable
data Maybe' a = Just' a | Nothing'
  deriving (Show)

-- GADT
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool

-- Type family
type family Elem a
type instance Elem [a] = a
type instance Elem (Maybe a) = a

-- Associated type family
class Container c where
  type ContainerElem c
  empty :: c
  insert :: ContainerElem c -> c -> c

instance Container [a] where
  type ContainerElem [a] = a
  empty = []
  insert = (:)

-- Multi-parameter type class with functional dependency
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

-- Data kind and type-level operators
data Nat = Zero | Succ Nat

type family (a :: Nat) :+ (b :: Nat) :: Nat where
  'Zero :+ b = b
  ('Succ a) :+ b = 'Succ (a :+ b)

-- Existential type
data SomeShape = forall a. Show a => SomeShape a

-- Basic function
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Function with pattern matching
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Higher-order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Function using let binding
sumSquares :: Num a => a -> a -> a
sumSquares x y =
  let squareX = x * x
      squareY = y * y
  in squareX + squareY

-- Function using where clause
pythagoras :: Floating a => a -> a -> a
pythagoras a b = sqrt (aSq + bSq)
  where
    aSq = a * a
    bSq = b * b

-- Function with guards
absoluteValue :: (Num a, Ord a) => a -> a
absoluteValue x
  | x < 0     = -x
  | otherwise = x

-- List comprehension
evenSquares :: [Integer]
evenSquares = [x^2 | x <- [1..10], even x]

-- Do notation (with list monad)
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- Lambda function
multiplyBy :: Num a => a -> a -> a
multiplyBy = \x y -> x * y

-- Partial application
add5 :: Num a => a -> a
add5 = (+) 5

-- Function composition
lengthOfGreeting :: String -> Int
lengthOfGreeting = length . greet

-- Folding
sumList :: Num a => [a] -> a
sumList = foldl' (+) 0

-- Recursive data structure
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

-- Function on recursive data structure
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 0
treeDepth (Node left right) = 1 + max (treeDepth left) (treeDepth right)

-- Monadic function
safeDivide :: (MonadFail m) => Int -> Int -> m Int  -- Changed Monad m to MonadFail m
safeDivide _ 0 = fail "Division by zero"
safeDivide x y = return (x `div` y)

data Person' = Person' String Int String
  deriving (Show)

createPerson :: Maybe String -> Maybe Int -> Maybe String -> Maybe Person'
createPerson name age email = Person' <$> name <*> age <*> email

-- Type class instance
class Sizeable a where
  size :: a -> Int

instance Sizeable [a] where
  size = length

instance Sizeable (Tree a) where
  size (Leaf _) = 1
  size (Node left right) = 1 + size left + size right

main :: IO ()
main = pure ()
  -- putStrLn "What's your name?"
  -- name <- getLine
  -- putStrLn $ greet name
  -- let person = Person name 30
  -- print person
  -- when (age person > 18) $
  --   putStrLn "You are an adult."
  -- let shapes = [Circle 5, Rectangle 3 4, Triangle 3 4 5]
  -- mapM_ print shapes
  -- let someShapes = [SomeShape (Circle 1), SomeShape (Rectangle 2 3)]
  -- mapM_ (\(SomeShape s) -> print s) someShapes
  
  -- -- Fixed safeDivide calls
  -- result1 <- safeDivide 10 2
  -- print result1
  -- result2 <- safeDivide 10 0
  -- print result2
  
  -- -- Fixed createPerson call
  -- print $ createPerson (Just "John") (Just 25) (Just "john@example.com")
  
  -- let tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
  -- print $ treeDepth tree
  -- print $ size tree
  -- print evenSquares
  -- print $ cartesianProduct [1,2] ['a','b']

demo :: Person -> Person-> Person
demo (a@Person {}) c = Person {
  name = name,age = 10
}
  where
    Person {..} = f a c


f :: Person -> Person-> Person
f a b = a