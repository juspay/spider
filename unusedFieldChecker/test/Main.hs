{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens hiding ((.=))
import Data.Data (Data, gmapQ)
import Data.Generics.Product (field)
import GHC.Generics (Generic)
import GHC.Records (HasField(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- ============================================================================
-- DATA TYPES
-- ============================================================================

-- Simple User type (existing)
data User = User
    { userName :: Maybe String
    , userAge :: Int
    , userEmail :: Maybe String
    , phoneNumber :: Maybe String
    } deriving (Show, Eq, Ord, Generic, Data)


main :: IO ()
main = do
        let user1 = User { userName = Just "Alice", userAge = 30, userEmail = Nothing, phoneNumber = Just "123-456-7890" }
        putStrLn $ "User1: " ++ show user1