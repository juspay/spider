{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances,DeriveDataTypeable,DeriveAnyClass #-}

module FieldInspector.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text
import Data.Data
import qualified Data.Map as Map

data FieldUsage = FieldUsage {
    typeName :: Text
    , fieldName :: Text
    , fieldType :: Text
    , typeSrcLoc :: Text
    , beautifiedCode :: Text
}
    deriving (Generic, Data, Show, ToJSON, FromJSON)

data TypeInfo = TypeInfo
  { name     :: String
  , typeKind     :: String
  , dataConstructors :: [DataConInfo]
  } deriving (Generic, Data, Show, ToJSON, FromJSON)

data DataConInfo = DataConInfo
  { dataConName :: String
  , fields      :: Map.Map String String
  , sumTypes    :: [String]
  } deriving (Generic, Data, Show, ToJSON, FromJSON)