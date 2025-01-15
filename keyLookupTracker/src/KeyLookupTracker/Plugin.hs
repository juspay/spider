{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KeyLookupTracker.Plugin (plugin) where

-- GHC imports

import Control.Reference (biplateRef, (^?))
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (nub, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC hiding (exprType)
import Prelude hiding (id)
import Data.Generics.Uniplate.Data
import qualified Data.ByteString as DBS
import Data.List.Extra (intercalate, splitOn)
import System.Directory (createDirectoryIfMissing)
import Data.ByteString.Lazy (toStrict)
import KeyLookupTracker.Types (KeyLookupRules(..), Rules(..), PluginOpts(..), defaultPluginOpts)
import GHC.Tc.Utils.Monad

#if __GLASGOW_HASKELL__ >= 900
import GHC.Data.Bag
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
  let binds = tcg_binds tcEnv
      prefixPath = "./.juspay/lookup-finder/"
      modulePath = prefixPath <> msHsFilePath modSummary
      path = (intercalate "/" . init . splitOn "/") modulePath
      pluginOpts = case opts of
          [] -> defaultPluginOpts
          (x : _) -> 
            fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
      paymentFlowRulesConfigPath = rulesConfigPath pluginOpts
  parsedPaymentFlowRules <- liftIO $ parseYAMLFile paymentFlowRulesConfigPath
  rule <- case parsedPaymentFlowRules of
                Left _ -> pure $ Rules {additionalEligibleLookupFns=[]}
                Right (rule :: KeyLookupRules) -> pure $ rules rule
  liftIO $ createDirectoryIfMissing True path
  lookupInfo <- concat <$> mapM (checkBind rule) (bagToList binds)
  liftIO $ DBS.writeFile (modulePath <> ".json") (toStrict $ A.encode lookupInfo)
  return tcEnv

checkBind :: Rules ->  LHsBindLR GhcTc GhcTc -> TcM [(String, [String])]
checkBind rule (L _ (FunBind{..} )) = do
  let funMatches = unLoc $ mg_alts fun_matches
  concat <$> mapM (checkMatch rule (getVarNameFromIDP $ unLoc fun_id)) funMatches
checkBind rule (L _ (AbsBinds {abs_binds = binds})) =
  concat <$> (mapM (checkBind rule) $ bagToList binds)
checkBind _ _ = pure []

checkMatch :: Rules -> String -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [(String, [String])]
checkMatch rule coreFn (L _ (Match _ _ _ grhss)) = do
  let whereBinds = (grhssLocalBinds grhss) ^? biplateRef :: [HsExpr GhcTc]
      nonWhereBinds = (grhssGRHSs grhss) ^? biplateRef :: [HsExpr GhcTc]
  list <- nub <$> concat <$> mapM (loopOverExpr rule) (nonWhereBinds <> whereBinds)
  pure [(coreFn ,list)]

loopOverExpr :: Rules -> HsExpr GhcTc -> TcM [String]
loopOverExpr rule expr =
  pure $ if any (`isInfixOf` (showS expr)) (["lookup"] <> (additionalEligibleLookupFns rule))
    then
      case expr of
        (HsApp _ _ (L _ (HsOverLit _ (OverLit _ _ (HsApp _ _ (L _ (HsLit _ lit))))))) -> [showS lit]
        _ -> []
    else []

getVarNameFromIDP :: IdP GhcTc -> String
getVarNameFromIDP var = occNameString . occName $ var

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr