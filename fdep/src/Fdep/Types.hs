{-# LANGUAGE DeriveAnyClass,DuplicateRecordFields #-}
module Fdep.Types where
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text

data CliOptions = CliOptions {
    path :: FilePath,
    socketPath :: Maybe FilePath,  -- New field for Unix socket path
    port :: Int,             -- Keep for backward compatibility
    host :: String,          -- Keep for backward compatibility
    log :: Bool,
    tc_funcs :: Maybe Bool
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data FunctionInfo = FunctionInfo
  { package_name :: Text
  , module_name :: Text
  , name    :: Text
  , _type :: Text
  , src_Loc    :: Text
  , arguments :: [Text]
  } deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data Function = Function
  {  function_name    :: Text
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: Text
  , stringified_code :: Text
  , function_signature :: Text
  } deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: Text
  , typeSignature :: Text
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data PFunction = PFunction {
    parser_name :: Text
    , parser_stringified_code :: Text
    , parser_src_loc :: Text
    , line_number    :: (Int,Int)
}
    deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data PType = PType {
    typeName :: Text,
    typeDefinition :: Text,
    typeLocation :: Text
    ,line_number    :: (Int,Int)
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data PClass = PClass {
    className :: Text,
    classDefinition :: Text,
    classLocation :: Text
    ,line_number    :: (Int,Int)
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data PInstance = PInstance {
    instanceType :: Text,
    instanceDefinition :: Text,
    instanceLocation :: Text
    ,line_number    :: (Int,Int)
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)


data QualifiedStyle = 
    NotQualified 
  | Qualified 
  | QualifiedWith Text
  deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

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
    ,line_number    :: (Int,Int)
} deriving (Show, Eq, Generic,ToJSON,FromJSON)
