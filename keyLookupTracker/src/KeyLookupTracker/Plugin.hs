{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KeyLookupTracker.Plugin (plugin) where

-- GHC imports

import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC hiding (exprType)
import Prelude hiding (id)
import qualified Data.ByteString as DBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List.Extra (intercalate, splitOn)
import System.Directory (createDirectoryIfMissing)
import Data.ByteString.Lazy (toStrict)
import KeyLookupTracker.Analysis (collectLookupSites, lookupsByFunction)
import KeyLookupTracker.Types (KeyLookupRules(..), Rules(..), PluginOpts(..), defaultPluginOpts)
import VariableTracer (collectModuleGraph, defaultTracerOpts, link)
import GHC.Tc.Utils.Monad

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins hiding ((<>), getHscEnv, purePlugin)
import GHC.Tc.Types
#else
import Bag
import ConLike
import DsExpr
import DsMonad
import GhcPlugins hiding ((<>), getHscEnv, purePlugin)
import TcEvidence
import TcRnMonad
import TcRnTypes
import TcType
import TyCoRep
#endif

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = keyLookupTracker
    , pluginRecompile       = purePlugin
    }

parseYAMLFile :: (FromJSON a) => FilePath -> IO (Either ParseException a)
parseYAMLFile file = decodeFileEither file

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

keyLookupTracker :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
keyLookupTracker opts modSummary tcEnv = do
  let prefixPath = "./.juspay/keyLookupTracker/"
      modulePath = prefixPath <> msHsFilePath modSummary
      path = (intercalate "/" . init . splitOn "/") modulePath
      pluginOpts = case opts of
          [] -> defaultPluginOpts
          (x : _) ->
            fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
      keyLookupRulesConfigPath = rulesConfigPath pluginOpts
  parsedKeyLookupRules <- liftIO $ parseYAMLFile keyLookupRulesConfigPath
  rule <- case parsedKeyLookupRules of
                Left _ -> pure $ Rules {additionalEligibleLookupFns = [], lookupKeyArgIndexes = []}
                Right (rules' :: KeyLookupRules) -> pure $ rules rules'
  liftIO $ createDirectoryIfMissing True path

  -- The provenance graph does the work: it knows what every binder is built
  -- from, so a key that is not a literal at the call site (a `let`, a where
  -- helper, or a parameter filled in by a caller) still resolves.
  let graph = collectModuleGraph defaultTracerOpts modSummary tcEnv
      eligible = "lookup" : map T.pack (additionalEligibleLookupFns rule)
      keyArgIndexes = Map.fromList [(T.pack fn, ix) | (fn, ix) <- lookupKeyArgIndexes rule]
      program = link [graph]
      sites = collectLookupSites eligible keyArgIndexes program
      lookupInfo = lookupsByFunction program sites

  liftIO $ DBS.writeFile (modulePath <> ".json") (toStrict $ A.encode lookupInfo)
  return tcEnv
