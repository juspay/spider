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
    , exclusionConfigFile = ".juspay/unusedFieldChecker.yaml"
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
    { allFieldDefs :: Map.Map Text [FieldDefinition] 
    , allFieldUsages :: Map.Map Text [FieldUsage] 
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)


data ValidationResult = ValidationResult
    { unusedNonMaybeFields :: [FieldDefinition]
    , unusedMaybeFields :: [FieldDefinition]
    , usedFields :: [FieldDefinition]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

-- Individual type exclusion within a module
data TypeExclusion = TypeExclusion
    { exclDataType :: Text
    , exclFields :: [Text]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData)

instance ToJSON TypeExclusion where
    toJSON (TypeExclusion dt fields) = object
        [ "dataType" .= dt
        , "fields" .= fields
        ]

instance FromJSON TypeExclusion where
    parseJSON = withObject "TypeExclusion" $ \v -> TypeExclusion
        <$> v .: "dataType"
        <*> v .: "fields"

-- Module-level exclusion with grouped types (new cleaner format)
data ModuleExclusion = ModuleExclusion
    { exclModule :: Text
    , exclTypes :: [TypeExclusion]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData)

instance ToJSON ModuleExclusion where
    toJSON (ModuleExclusion mod types) = object
        [ "module" .= mod
        , "types" .= types
        ]

instance FromJSON ModuleExclusion where
    parseJSON = withObject "ModuleExclusion" $ \v -> ModuleExclusion
        <$> v .: "module"
        <*> v .: "types"

-- Configuration with priority order:
-- 1. includeFiles (if set, ONLY these modules are checked - whitelist mode)
-- 2. excludeFiles (if includeFiles not set, these modules are skipped - blacklist mode)
-- 3. exclusions (field-level exclusions within checked modules)
data ExclusionConfig = ExclusionConfig
    { exclusions :: [ModuleExclusion]
    , excludeFiles :: [Text]
    , includeFiles :: Maybe [Text]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

emptyExclusionConfig :: ExclusionConfig
emptyExclusionConfig = ExclusionConfig
    { exclusions = []
    , excludeFiles = []
    , includeFiles = Nothing
    }

-- Phase 2: Track which types are used within configured modules
data TypeUsageInModule = TypeUsageInModule
    { typeName :: Text           -- e.g., "AdyenRefundSuccessResponse"
    , typeModule :: Text         -- Module where type is defined
    , usedInModule :: Text       -- Module where type is used
    , usageLocation :: Text      -- Location where type is used
    , typeConstructor :: Text    -- Type constructor for matching
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)
