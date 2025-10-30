{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    { path = "/tmp/unusedFieldChecker/"
    , port = 4445
    , host = "::1"
    , log = False
    , exclusionConfigFile = "UnusedFieldChecker.yaml"
    }

data FieldDefinition = FieldDefinition
    { fieldDefName :: Text  
    , fieldDefType :: Text
    , fieldDefTypeName :: Text
    , fieldDefIsMaybe :: Bool
    , fieldDefModule :: Text
    , fieldDefLocation :: Text
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data UsageType
    = AccessorFunction      -- Method 1: name p (auto-generated getter)
    | PatternMatch          -- Method 2: Person { name = n } (destructures record)
    | NamedFieldPuns        -- Method 3: { name } (syntactic sugar)
    | RecordWildCards       -- Method 3: {..} (syntactic sugar)
    | FunctionComposition   -- Method 4: (address . company) (chaining getters)
    | LensesOptics          -- Method 5: p ^. name (functional field view)
    | HasFieldOverloaded    -- Method 6: getField @"name" p (polymorphic field access)
    | GenericReflection     -- Method 7: p ^. field @"name" (structural reflection)
    | TemplateHaskell       -- Method 8: reify ''Person (compile-time field inspection)
    | DerivedInstances      -- Method 9: deriving (Show, Eq, Ord) (auto pattern matching)
    | DataSYB               -- Method 10: gmapQ f p (runtime structural access)
    | RecordDotSyntax       -- Method 11: p.name (sugar over getField @"name")
    | RecordConstruct       -- Record construction (legacy, kept for compatibility)
    | RecordUpdate          -- Record updates (legacy, kept for compatibility)
    deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

data FieldUsage = FieldUsage
    { fieldUsageName :: Text 
    , fieldUsageType :: UsageType
    , fieldUsageTypeName :: Text 
    , fieldUsageModule :: Text
    , fieldUsageLocation :: Text
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

data ExclusionRule = ExclusionRule
    { exclModule :: Text 
    , exclDataType :: Text 
    , exclFields :: [Text]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)


data ExclusionConfig = ExclusionConfig
    { exclusions :: [ExclusionRule]
    } deriving (Show, Eq, Ord, Binary, Generic, NFData, ToJSON, FromJSON)

emptyExclusionConfig :: ExclusionConfig
emptyExclusionConfig = ExclusionConfig { exclusions = [] }
