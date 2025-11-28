{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module UnusedFieldChecker.Types where

import Data.Aeson
import Data.Binary
import Data.Text (Text)
import qualified Data.Map as Map
import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (log)

data CliOptions = CliOptions
    { path :: FilePath
    , port :: Int
    , host :: String
    , log :: Bool
    , exclusionConfigFile :: FilePath
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { path = ".juspay/unusedFieldChecker/"
    , port = 4445
    , host = "::1"
    , log = False
    , exclusionConfigFile = ".juspay/UnusedFieldChecker.yaml"
    }

data FieldDefinition = FieldDefinition
    { fieldDefName :: Text
    , fieldDefType :: Text
    , fieldDefTypeName :: Text
    , fieldDefIsMaybe :: Bool
    , fieldDefModule :: Text
    , fieldDefLocation :: Text
    , fieldDefPackageName :: Text
    , fieldDefFullyQualifiedType :: Text  -- moduleName.typeName
    , fieldDefTypeConstructor :: Text     -- Type constructor for matching
    , fieldDefIsSingleField :: Bool       -- True if this is the only field in the record (GHC optimizes away accessor)
    , fieldDefHasFieldChecker :: Bool     -- True if type derives FieldChecker
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data UsageType
    = AccessorFunction     
    | PatternMatch         
    | NamedFieldPuns        
    | RecordWildCards      
    | FunctionComposition   
    | LensesOptics         
    | HasFieldOverloaded   
    | GenericReflection    
    | TemplateHaskell       
    | DerivedInstances      
    | DataSYB               
    | RecordDotSyntax       
    | RecordConstruct       
    | RecordUpdate   
    deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data FieldUsage = FieldUsage
    { fieldUsageName :: Text 
    , fieldUsageType :: UsageType
    , fieldUsageTypeName :: Text 
    , fieldUsageModule :: Text
    , fieldUsageLocation :: Text
    , fieldUsageTypeConstructor :: Text  -- Type constructor for matching
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data ModuleFieldInfo = ModuleFieldInfo
    { moduleFieldDefs :: [FieldDefinition]
    , moduleFieldUsages :: [FieldUsage]
    , moduleName :: Text
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data AggregatedFieldInfo = AggregatedFieldInfo
    { allFieldDefs :: Map.Map (Text, Text) [FieldDefinition]   -- Key: (typeName, fieldName)
    , allFieldUsages :: Map.Map (Text, Text) [FieldUsage]      -- Key: (typeName, fieldName)
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)


data ValidationResult = ValidationResult
    { unusedNonMaybeFields :: [FieldDefinition]
    , unusedMaybeFields :: [FieldDefinition]
    , usedFields :: [FieldDefinition]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data ExclusionConfig = ExclusionConfig
    { includeFiles :: Maybe [Text]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

emptyExclusionConfig :: ExclusionConfig
emptyExclusionConfig = ExclusionConfig
    { includeFiles = Nothing
    }

data TypeUsageInModule = TypeUsageInModule
    { typeName :: Text           -- e.g., "AdyenRefundSuccessResponse"
    , typeModule :: Text         -- Module where type is defined
    , usedInModule :: Text       -- Module where type is used
    , usageLocation :: Text      -- Location where type is used
    , typeConstructor :: Text    -- Type constructor for matching
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data ServantAPIType = ServantAPIType
    { apiTypeName :: Text              -- e.g., "AuthRequest"
    , apiTypeModule :: Text            -- Module where type is defined
    , apiTypeConstructor :: Text       -- Type constructor for matching
    , apiEndpoint :: Text              -- e.g., "/v1/authentication"
    , apiLocation :: Text              -- Source location of the API definition
    , apiHasFieldChecker :: Bool       -- Whether FieldChecker instance exists (top-level)
    , apiMissingInstances :: [Text]    -- List of types in the dependency tree missing FieldChecker instances
    , apiServantCombinator :: Text     -- e.g., "ReqBody '[JSON]", "Post '[JSON]"
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)
