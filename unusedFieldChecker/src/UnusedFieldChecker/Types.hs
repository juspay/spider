{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module UnusedFieldChecker.Types where

import Data.Aeson
import Data.Binary
import Data.Text (Text)
import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (log)

-- | CLI options for the plugin
data CliOptions = CliOptions
    { path :: FilePath              -- ^ Output directory for JSON files
    , exclusionConfigFile :: FilePath -- ^ Path to exclusion config YAML
    , failOnUnused :: Bool          -- ^ If True, emit compilation errors for unused fields
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { path = ".juspay/unusedFieldChecker/"
    , exclusionConfigFile = ".juspay/UnusedFieldChecker.yaml"
    , failOnUnused = True
    }

-- | A field definition extracted from a type with FieldChecker instance
data FieldDefinition = FieldDefinition
    { fieldDefName :: Text              -- ^ Field name
    , fieldDefType :: Text              -- ^ Field type as string
    , fieldDefTypeName :: Text          -- ^ Parent type name
    , fieldDefIsMaybe :: Bool           -- ^ True if field type is Maybe
    , fieldDefModule :: Text            -- ^ Module where field is defined
    , fieldDefLocation :: Text          -- ^ Source location
    , fieldDefPackageName :: Text       -- ^ Package name
    , fieldDefFullyQualifiedType :: Text -- ^ moduleName.typeName
    , fieldDefTypeConstructor :: Text   -- ^ Type constructor for matching
    , fieldDefIsSingleField :: Bool     -- ^ True if only field in record
    , fieldDefHasFieldChecker :: Bool   -- ^ True if type has FieldChecker instance
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

-- | Type of field usage detected in code
data UsageType
    = AccessorFunction     -- ^ Direct accessor function call
    | PatternMatch         -- ^ Pattern match on record
    | NamedFieldPuns       -- ^ NamedFieldPuns extension usage
    | RecordWildCards      -- ^ RecordWildCards ".." syntax
    | FunctionComposition  -- ^ Function composition with accessor
    | LensesOptics         -- ^ Lens/optics usage
    | HasFieldOverloaded   -- ^ HasField typeclass usage
    | GenericReflection    -- ^ Generic reflection field access
    | TemplateHaskell      -- ^ Template Haskell (filtered out)
    | DerivedInstances     -- ^ Derived instances (filtered out)
    | DataSYB              -- ^ SYB/Data.Data usage (filtered out)
    | RecordDotSyntax      -- ^ Record dot syntax
    | RecordConstruct      -- ^ Record construction
    | RecordUpdate         -- ^ Record update syntax
    deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

-- | A detected field usage in code
data FieldUsage = FieldUsage
    { fieldUsageName :: Text            -- ^ Field name being used
    , fieldUsageType :: UsageType       -- ^ Type of usage
    , fieldUsageTypeName :: Text        -- ^ Parent type name (may be empty)
    , fieldUsageModule :: Text          -- ^ Module where usage occurs
    , fieldUsageLocation :: Text        -- ^ Source location of usage
    , fieldUsageTypeConstructor :: Text -- ^ Type constructor for matching
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

-- | Configuration for module inclusion/exclusion
data ExclusionConfig = ExclusionConfig
    { includeFiles :: Maybe [Text]   -- ^ If Just, only these modules are checked
    , excludeFiles :: Maybe [Text]   -- ^ If Just, these modules are excluded
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

emptyExclusionConfig :: ExclusionConfig
emptyExclusionConfig = ExclusionConfig
    { includeFiles = Nothing
    , excludeFiles = Nothing
    }

-- | Type alias for the unused field log stored in JSON
-- This is a list of FieldDefinition entries that haven't been marked as used yet
type UnusedFieldLog = [FieldDefinition]
