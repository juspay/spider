{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KeyLookupTracker.Plugin (plugin) where

-- GHC imports

import Control.Reference (biplateRef, (^?))
import Data.Aeson as A
import Data.List (nub, isInfixOf)
import GHC hiding (exprType)
import Prelude hiding (id)
import Data.Generics.Uniplate.Data
import qualified Data.ByteString as DBS
import Data.List.Extra (intercalate, splitOn)
import System.Directory (createDirectoryIfMissing)
import Data.ByteString.Lazy (toStrict)

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

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

keyLookupTracker :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
keyLookupTracker _ modSummary tcEnv = do
  let binds = tcg_binds tcEnv
      prefixPath = "./.juspay/lookup-finder/"
      modulePath = prefixPath <> msHsFilePath modSummary
      path = (intercalate "/" . init . splitOn "/") modulePath
  liftIO $ createDirectoryIfMissing True path
  lookupInfo <- concat <$> mapM checkBind (bagToList binds)
  liftIO $ DBS.writeFile (modulePath <> ".json") (toStrict $ A.encode lookupInfo)
  return tcEnv

checkBind :: LHsBindLR GhcTc GhcTc -> TcM [(String, [String])]
checkBind (L _ (FunBind{..} )) = do
  let funMatches = unLoc $ mg_alts fun_matches
  concat <$> mapM (checkMatch (getVarNameFromIDP $ unLoc fun_id)) funMatches
checkBind (L _ (AbsBinds {abs_binds = binds})) =
  concat <$> (mapM checkBind $ bagToList binds)
checkBind _ = pure []

checkMatch :: String -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [(String, [String])]
checkMatch coreFn (L _ (Match _ _ _ grhss)) = do
  let whereBinds = (grhssLocalBinds grhss) ^? biplateRef :: [HsExpr GhcTc]
      nonWhereBinds = (grhssGRHSs grhss) ^? biplateRef :: [HsExpr GhcTc]
  list <- nub <$> concat <$> mapM loopOverExpr (nonWhereBinds <> whereBinds)
  pure [(coreFn ,list)]

loopOverExpr :: HsExpr GhcTc -> TcM [String]
loopOverExpr expr =
  pure $ if "lookup" `isInfixOf` (showS expr)
    then
      case expr of
        (HsApp _ a (L _ (HsOverLit _ (OverLit _ _ (HsApp _ _ (L _ (HsLit _ lit))))))) -> [showS lit]
        _ -> []
    else []

getVarNameFromIDP :: IdP GhcTc -> String
getVarNameFromIDP var = occNameString . occName $ var

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr