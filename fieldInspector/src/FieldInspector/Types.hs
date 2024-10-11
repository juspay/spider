
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances, DeriveAnyClass #-}

module FieldInspector.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text
import Data.Data
import qualified Data.Map as Map
import Data.Binary
import Control.DeepSeq

data FieldUsage = FieldUsage {
    typeName :: Text
    , fieldName :: Text
    , fieldType :: Text
    , typeSrcLoc :: Text
    , beautifiedCode :: Text
}
    deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data TypeInfo = TypeInfo
  { name     :: String
  , typeKind     :: String
  , dataConstructors :: [DataConInfo]
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data DataConInfo = DataConInfo
  { dataConNames :: String
  , fields      :: Map.Map String String
  , sumTypes    :: [String]
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data DataTypeUC = DataTypeUC {
    function_name_ :: [Text]
    , typeVsFields :: [TypeVsFields]
    } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data TypeVsFields = TypeVsFields {
    type_name :: Text
    , fieldsVsExprs :: Either [(FieldRep)] [Text]
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data FieldRep = FieldRep {
  field_name :: Text
  , expression :: Text
  , field_type :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)
