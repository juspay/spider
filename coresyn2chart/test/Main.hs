{-# LANGUAGE ConstraintKinds #-}
module Main (main,test,test1,p,p', test2) where

import Control.Monad
import Data.Functor
import System.Environment

main :: IO ()
main = do
    x <- getArgs
    when ((length x) > 1) $ putStrLn "aravin"
    print $ test1 0
    if (10 > 200) then putStrLn ("Test suite not yet implemented." <> test1 1000) else putStrLn demo
    str <- pure $ case length x of
            0 -> "HI_THERE"
            _ -> "NOTHING_HERE"
    print str
    where
        demo :: String
        demo = test 100

test :: Int -> String
test 10 = (<> "HI") "HI"
test 0 = (<> "HIHIIIII") "HI"
test _ = (<> "HIHIHI") "HI"

data X = Y | Z
	deriving Show


test2 :: X -> String
-- test2 x = show x
test2 x = case x of
  Y -> "Y"
  Z -> "Z"

test1 :: Int -> String
test1 = test

-- p :: a -> a -> a -> a -> [a]
p a b c d = [a ,b ,c, d]

p' :: (Monoid a) => a -> a -> a -> a -> a
p' a b c d = mempty
