{-# LANGUAGE DerivingStrategies, CPP, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module DC.Types where

import qualified Data.HashMap.Strict as HM
import Data.Aeson
import GHC hiding (typeKind)
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ >= 900
import GHC.Data.FastString
#else
import GhcPlugins hiding ((<>))
#endif

data PluginOpts = PluginOpts {
    failOnFileNotFound :: Bool,
    domainConfigFile :: String,
    pathsTobeChecked :: [String]
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts =
  PluginOpts {
    failOnFileNotFound = True,
    domainConfigFile = ".juspay/domainConfig.yaml",
    pathsTobeChecked = ["euler-x/src","euler-x/src-generated","euler-x/src-extras","euler-api-decider/src", "ecPrelude/src", "ecPrelude/src-generated","ecPrelude/src-extras", "oltp/src", "oltp/src-generated","oltp/src-extras", "dbTypes/src-generated", "src/"]
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    domainConfigFile <- o .:? "domainConfigFile" .!= (domainConfigFile defaultPluginOpts)
    pathsTobeChecked <- o .:? "pathsTobeChecked" .!= (pathsTobeChecked defaultPluginOpts)
    return PluginOpts {domainConfigFile = domainConfigFile, failOnFileNotFound = failOnFileNotFound, pathsTobeChecked = pathsTobeChecked }

data EnumCheck =
  EnumCheck
    { enumList :: [String]
    , enumType :: String
    , recordType :: String
    , fieldType :: String
    , avoidedFunsByModule :: Maybe (HM.HashMap String [String])
    }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)

data FunctionCheckConfig =
  FunctionCheckConfig
    { listOfRestrictedFuns :: [String]
    , moduleNameToCheck :: String
    , funNameToCheck :: String
    }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)

data CheckerConfig = FieldsCheck EnumCheck | FunctionCheck FunctionCheckConfig
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)

data ErrorCase = Errors [CompileError] | Functions (HM.HashMap String [CompileError])
  deriving (Show, Eq)

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  , src_loc :: String
  , isFailure :: Bool
  } deriving (Show, Eq, Ord)

data UpdateInfo = UpdateInfo
  { createdRecordsFun :: [FunctionInfo]
  , updatedRecordsFun :: [FunctionInfo]
  , updatedFailurs :: [FunctionInfo]
  , createdFailurs :: [FunctionInfo]
  , allFailures :: [FunctionInfo]
  , commonError :: [FunctionInfo]
  } deriving (Show, Eq, Ord)

data UpdateInfoAsText = UpdateInfoAsText
  { createdRecords :: [String]
  , updatedRecords :: [String]
  , updatedFailures :: [String]
  , createdFailures :: [String]
  , allFailuresRecords :: [String]
  , commonErrorFuns :: [String]
  } deriving (Generic, Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

data TypeOfUpdate = Update | Create | UpdateWithFailure | CreateWithFailure | Default | NoChange
    deriving (Show, Eq, Ord)

data CompileError = CompileError
  {
    pkg_name :: String,
    mod_name :: String,
    err_msg :: String,
    src_span :: SrcSpan
  } deriving (Eq, Show, Generic)

instance ToJSON CompileError where
#if __GLASGOW_HASKELL__ < 900
  toJSON (CompileError pkg modName errMsg (RealSrcSpan srcLoc)) =
#else
  toJSON (CompileError pkg modName errMsg (RealSrcSpan srcLoc _)) =
#endif
    object [ "package_name"   .= pkg
           , "module_name"    .= modName
           , "error_message"  .= errMsg
           , "src_span_name"  .= unpackFS (srcSpanFile srcLoc)
           , "src_span_startl"  .= (srcSpanStartLine srcLoc)
           , "src_span_endl"  .= (srcSpanEndLine srcLoc)
           , "src_span_startC"  .= (srcSpanStartCol srcLoc)
           , "src_span_endC"  .= (srcSpanEndCol srcLoc)
           ]
  toJSON (CompileError pkg modName errMsg _) =
    object [ "package_name"   .= pkg
           , "module_name"    .= modName
           , "error_message"  .= errMsg
           ]


instance FromJSON CompileError where
  parseJSON = withObject "CompileError" $ \o -> do
    pkg_name <- o .: "package_name"
    mod_name <- o .: "module_name"
    err_msg <- o .: "error_message"
    src_span_name <- o .: "src_span_name"
    src_span_startl <- o .: "src_span_startl"
    src_span_endl <- o .: "src_span_endl"
    src_span_startC <- o .: "src_span_startC"
    src_span_endC <- o .: "src_span_endC"
    src_span <- pure $ mkSrcSpan (mkSrcLoc (mkFastString src_span_name) src_span_startl src_span_startC) (mkSrcLoc (mkFastString src_span_name) src_span_endl src_span_endC)
    return CompileError { .. }

