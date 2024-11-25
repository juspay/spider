-- {-# OPTIONS_GHC -ddump-tc-ast #-}

module Main (main) where

import Sample
import Data.Maybe (fromMaybe)

main :: IO ()
main = putStrLn "Test suite not yet implemented."


-- data SomeType = SomeType
--     { some :: Int}
--     deriving (Show)

-- create y = do
--     print y
--     pure y

list :: Int -> Int -> IO String
list num num2 = do
    let x = sample (getval num2) (getval2 num2 4)
    -- let b = Just $ getval2 num2 4
    --     d = getval num
    --     fin = sample d <$> b
    pure x
-- list :: Int -> Int -> IO String
-- list num num2 = do
--     let 
--         b = getval2 num2 4
--         c = num
--         d = getval c
--     -- _ <- do
--     --     let c = show num
--     --     getValueIn a
--     let y = SomeType {some = num}
--     _ <- create y
--     -- _ <- getValueInWHere b
--     -- _ <- pure $ getval2 num2 4
--     let k = Just "ff"
--     case k of
--     -- _ <- getValueInWHere b
--        Just val -> getValueInWHere b
--        Nothing -> pure ""
--     -- print y
--     -- pure $ sample a b
--   where
--     getValueInWHere :: String -> IO String
--     getValueInWHere x = do
--         print x
--         pure $ sample2 x "l"

