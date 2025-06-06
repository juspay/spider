
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

data CliOptions = CliOptions {
    path :: FilePath,
    port :: Int,
    host :: String,
    log :: Bool,
    tc_funcs :: Maybe Bool,
    api_conteact :: Maybe Bool
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)


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
  , fields      :: Map.Map String (StructuredTypeRep)
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

data StructuredTypeRep = StructuredTypeRep {
    raw_code :: Text
    , structure :: ComplexType
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

-- Add these constructors to ComplexType
data ComplexType = 
    AtomicType TypeComponent
    | ListType ComplexType
    | TupleType [ComplexType]
    | AppType ComplexType [ComplexType]
    | FuncType ComplexType ComplexType
    | ForallType [TypeComponent] ComplexType  -- For HsForAllTy
    | QualType [ComplexType] ComplexType      -- For HsQualTy (with context)
    | KindSigType ComplexType ComplexType     -- For HsKindSig
    | BangType ComplexType                    -- For HsBangTy
    | RecordType [(String,String, ComplexType)]      -- For HsRecTy
    | PromotedListType [ComplexType]          -- For HsExplicitListTy
    | PromotedTupleType [ComplexType]         -- For HsExplicitTupleTy
    | LiteralType String                      -- For HsTyLit
    | WildCardType                            -- For HsWildCardTy
    | StarType                                -- For HsStarTy
    | IParamType String ComplexType           -- For HsIParamTy
    | DocType ComplexType String              -- For HsDocTy
    | UnknownType Text                        -- Fallback
    deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data TypeComponent = TypeComponent 
    { moduleName' :: Text
    , typeName' :: Text
    , packageName :: Text
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)