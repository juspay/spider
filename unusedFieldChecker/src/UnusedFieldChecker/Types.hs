{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module UnusedFieldChecker.Types where

import Data.Aeson
import Data.Binary
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (log)

-- | CLI options for the plugin
data CliOptions = CliOptions
    { path :: FilePath              -- ^ Output directory for JSON files
    , failOnUnused :: Bool          -- ^ If True, emit compilation errors for unused fields
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { path = ".juspay/unusedFieldChecker/"
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

-- | Type alias for the unused field log stored in JSON
-- This is a list of FieldDefinition entries that haven't been marked as used yet
type UnusedFieldLog = [FieldDefinition]

-- | Persistent field log for a gateway (saved to .fieldLog.json)
-- Stores field definitions and usages per-module for incremental build support
data FieldLog = FieldLog
    { logModuleDefinitions :: Map.Map Text [FieldDefinition]  -- ^ Module name -> field definitions
    , logModuleUsages :: Map.Map Text [FieldUsage]            -- ^ Module name -> field usages
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Empty field log for initialization
emptyFieldLog :: FieldLog
emptyFieldLog = FieldLog Map.empty Map.empty

-- | In-memory global state for all gateways (held in MVar during GHC session)
data GlobalState = GlobalState
    { currentBuildId :: Text                              -- ^ Build ID to detect fresh builds
    , gatewayStates :: Map.Map Text GatewayInMemoryState  -- ^ Per-gateway in-memory state
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Per-gateway in-memory state (not persisted between sessions)
data GatewayInMemoryState = GatewayInMemoryState
    { moduleDefinitions :: Map.Map Text [FieldDefinition]  -- ^ Module name -> field definitions
    , moduleUsages :: Map.Map Text [FieldUsage]            -- ^ Module name -> field usages
    , pendingSinks :: Set.Set Text                         -- ^ Sink modules pending completion
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Empty global state for initialization
emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState "" Map.empty

-- | Empty gateway in-memory state for initialization
emptyGatewayInMemoryState :: GatewayInMemoryState
emptyGatewayInMemoryState = GatewayInMemoryState Map.empty Map.empty Set.empty
