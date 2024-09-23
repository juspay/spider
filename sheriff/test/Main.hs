{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Sheriff.Plugin ()
import Data.Text as T
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Encoding as DTE
import Data.Aeson as A
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Test1 as T1

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."