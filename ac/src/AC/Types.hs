{-# LANGUAGE DerivingStrategies, CPP, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module AC.Types where

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
    domainConfigFile = "AmountCheck.json",
    pathsTobeChecked = ["euler-x/src","euler-x/src-generated","euler-x/src-extras","euler-api-decider/src", "ecPrelude/src", "ecPrelude/src-generated","ecPrelude/src-extras", "oltp/src", "oltp/src-generated","oltp/src-extras", "dbTypes/src-generated", "src/", "test/"]
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    domainConfigFile <- o .:? "domainConfigFile" .!= (domainConfigFile defaultPluginOpts)
    pathsTobeChecked <- o .:? "pathsTobeChecked" .!= (pathsTobeChecked defaultPluginOpts)
    return PluginOpts {domainConfigFile = domainConfigFile, failOnFileNotFound = failOnFileNotFound, pathsTobeChecked = pathsTobeChecked }

data FunctionContext = FunctionContext
  { funcName :: String
  , funcSpan :: SrcSpan
  } deriving (Show)