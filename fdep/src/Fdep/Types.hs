{-# LANGUAGE DeriveAnyClass #-}
module Fdep.Types where
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text
import Data.Binary
import Control.DeepSeq

data FunctionInfo = FunctionInfo
  { package_name :: Text
  , module_name :: Text
  , name    :: Text
  , _type :: Text
  , src_Loc    :: Text
  , arguments :: [Text]
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data Function = Function
  {  function_name    :: Text
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: Text
  , stringified_code :: Text
  , function_signature :: Text
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: Text
  , typeSignature :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data PFunction = PFunction {
    parser_name :: Text
    , parser_stringified_code :: Text
    , parser_src_loc :: Text
}
    deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data PType = PType {
    typeName :: Text,
    typeDefinition :: Text,
    typeLocation :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data PClass = PClass {
    className :: Text,
    classDefinition :: Text,
    classLocation :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data PInstance = PInstance {
    instanceType :: Text,
    instanceDefinition :: Text,
    instanceLocation :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)


data QualifiedStyle = 
    NotQualified 
  | Qualified 
  | QualifiedWith Text
  deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data HidingSpec = HidingSpec {
    isHiding :: Bool,
    names :: [Text]
} deriving (Show, Eq, Generic,ToJSON,FromJSON)

data SimpleImportDecl = SimpleImportDecl {
    moduleName'      :: Text,          -- ^ Module being imported
    packageName     :: Maybe Text,    -- ^ Optional package qualifier
    isBootSource    :: Bool,            -- ^ Whether this is a SOURCE import
    isSafe         :: Bool,            -- ^ Whether this is a safe import
    qualifiedStyle  :: QualifiedStyle,  -- ^ How the import is qualified
    isImplicit     :: Bool,            -- ^ Whether this is an implicit import
    asModuleName   :: Maybe Text,    -- ^ Optional module rename
    hidingSpec     :: Maybe HidingSpec  -- ^ Optional hiding specification
} deriving (Show, Eq, Generic,ToJSON,FromJSON)
