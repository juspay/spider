{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module PaymentFlow.Plugin (plugin) where

import Control.Reference (biplateRef, (^?), Simple, Traversal)
import Data.Generics.Uniplate.Data (universeBi, childrenBi, contextsBi, holesBi, children)
import Data.List (nub)
import Data.Yaml
import GHC
  ( GRHS (..),
    GRHSs (..),
    GenLocated (L),
    HsValBinds (..),
    GhcTc,
    HsBindLR (..),
    HsConDetails (..),
    HsConPatDetails,
    HsExpr (..),
    HsRecField' (..),
    HsRecFields (..),
    LGRHS,
    MatchGroupTc(..),
    HsType(..),
    LHsType,
    NoGhcTc(..),
    HsTyLit(..),
    HsWildCardBndrs(..),
    LHsExpr,
    LHsRecField,
    LMatch,
    LPat,
    Match (..),
    MatchGroup (..),
    Name,
    Pat (..),
    PatSynBind (..),
    noLoc, noExtField, Module (moduleName), moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass
  )
import GHC.Hs.Binds (LHsBindLR)
import HscTypes (ModSummary (..))
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import Prelude
import Data.Aeson as A
import Control.Exception (try,SomeException)
import Outputable (showSDocUnsafe, ppr, Outputable(..))
import Control.Monad (when)
import Data.List
import Data.List.Extra (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory
import PatSyn
import Avail
import TcEnv
import Data.Bool (bool)
import qualified Outputable as OP
import FastString
import Data.Maybe (catMaybes)
import TcRnMonad (addWarn, addErrAt)
import GHC (OverLitTc(..), HsOverLit(..))
import CoreUtils (exprType)
import Control.Applicative ((<|>))
import Type (isFunTy, funResultTy, splitAppTys, dropForAlls)
import TyCoRep (Type(..), TyLit (..))
import qualified Data.ByteString.Lazy.Char8 as Char8
import ConLike (conLikeWrapId_maybe)
import GhcPlugins hiding (purePlugin, (<>))
import TcRnTypes
import Bag (bagToList)
import TcEvidence
import PaymentFlow.Types (VoilationRuleResult(..), PFRules(..), Rule(..), PluginOpts(..), defaultPluginOpts, defaultRule)
import Data.Foldable (foldl')

logWarnInfo :: Bool
logWarnInfo = True

mkInvalidYamlFileErr :: String -> OP.SDoc
mkInvalidYamlFileErr err = OP.text err

parseYAMLFile :: (FromJSON a) => FilePath -> IO (Either ParseException a)
parseYAMLFile file = decodeFileEither file

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = paymentFlow
    , pluginRecompile       = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

paymentFlow :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
paymentFlow opts modSummary tcEnv = do
  let pluginOpts = case opts of
          [] -> defaultPluginOpts
          (x : _) -> 
            fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
      moduleNm = moduleNameString $ moduleName $ ms_mod modSummary
      paymentFlowRulesConfigPath = rulesConfigPath pluginOpts
  parsedPaymentFlowRules <- liftIO $ parseYAMLFile paymentFlowRulesConfigPath
  ruleList <- case parsedPaymentFlowRules of
                Left err -> do
                  when logWarnInfo $ addWarn NoReason (mkInvalidYamlFileErr (show err))
                  pure defaultRule
                Right (rule :: PFRules) -> pure (nub $ defaultRule <> (rules rule))
  let binds = tcg_binds tcEnv
  if ("Types" `isSuffixOf` moduleNm || "Types" `isPrefixOf` moduleNm || "Types" `isInfixOf` moduleNm )
    then pure ()
    else do
      errors <- concat <$> mapM (checkBind ruleList) (bagToList binds)
      let sortedErrors = sortOn srcSpan errors
          groupedErrors = groupBy (\a b -> srcSpan a == srcSpan b) sortedErrors
          childFnFilterLogic srcGrpErrArr = do
            let srcSpn = maybe Nothing (\value -> Just $ srcSpan value) (listToMaybe srcGrpErrArr)
                srcSpanLine = case srcSpn of
                  (Just (RealSrcSpan s)) -> srcSpanStartLine s
                  _                 -> 0

                shouldThroughError = (any (\(VoilationRuleResult{..}) -> do
                        let whitelistedRules = field_access_whitelisted_fns rule
                        fnName `elem` whitelistedRules || coreFnName `elem` whitelistedRules) srcGrpErrArr) || (any (\result -> srcSpanLine `elem` (whitelisted_line_nos (rule result))) srcGrpErrArr)
            if shouldThroughError
              then Nothing
              else listToMaybe srcGrpErrArr
          filteredErrors = (\srcGrpErrArr -> childFnFilterLogic srcGrpErrArr) <$> groupedErrors
      mapM_ (\ (VoilationRuleResult {..}) ->  addErrAt srcSpan $ OP.text $ field_rule_fixes rule ) (catMaybes filteredErrors)
  return tcEnv

checkBind :: [Rule] ->  LHsBindLR GhcTc GhcTc -> TcM [VoilationRuleResult]
checkBind rule (L _ ap@(FunBind _ id matches _ _)) = do
  let funMatches = unLoc $ mg_alts matches
  concat <$> mapM (checkMatch rule (showS id)) funMatches
checkBind rule (L _ ap@(AbsBinds {abs_binds = binds})) =
  concat <$> (mapM (checkBind rule) $ bagToList binds)
checkBind _ _ = pure []

checkMatch :: [Rule] -> String ->  LMatch GhcTc (LHsExpr GhcTc) -> TcM [VoilationRuleResult]
checkMatch rule coreFn (L _ (Match _ _ _ grhss)) = do
  let whereBinds = (grhssLocalBinds grhss) ^? biplateRef :: [LHsExpr GhcTc]
      nonWhereBinds = (grhssGRHSs grhss) ^? biplateRef :: [LHsExpr GhcTc]
  loopOverExprInArgsPerFnName (nonWhereBinds <> whereBinds) rule coreFn
checkMatch _ _ _ = pure []

loopOverExprInArgsPerFnName :: [LHsExpr GhcTc] -> [Rule] -> String -> TcM [VoilationRuleResult]
loopOverExprInArgsPerFnName exprs rules coreFn = do
  let fnArgTuple = catMaybes (getFnNameWithAllArgs <$> exprs)
  nub <$> concat <$> mapM (lookOverExpr rules coreFn) fnArgTuple
loopOverExprInArgsPerFnName _ _ _ = pure []

lookOverExpr :: [Rule] -> String ->  (Located Var, [LHsExpr GhcTc]) -> TcM [VoilationRuleResult]
lookOverExpr rules funId (fnName, args) = do
  let updatedArgs = args ^? biplateRef :: [LHsExpr GhcTc]
  tupleResponse <- catMaybes <$> sequence (checkExpr' rules <$> updatedArgs)
  pure $ (\(x, y) -> VoilationRuleResult { fnName = getVarName fnName, srcSpan = x, rule = y, coreFnName = funId }) <$> tupleResponse

getVarName :: Located Var -> String
getVarName var = (getOccString . varName . unLoc) var

checkExpr' :: [Rule]-> LHsExpr GhcTc -> TcM (Maybe (SrcSpan, Rule))
checkExpr' rules expr = do
  case expr of
    L _ (HsPar _ exp) -> checkExpr' rules exp
    L loc1 (HsVar _ (L _ var)) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithRightExprAsType var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)

    L loc1 (HsApp _ (L _ (HsVar _ (L _ var))) _) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithRightExprAsType var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)
  
    L loc12 (OpApp _ (L loc1 (OpApp _ (L _ (HsVar _ (L _ leftVar))) _ (L _ (HsWrap _ _ (HsVar _ (L _ var)))))) _ _) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType  var leftVar rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)
    
    L loc1 (OpApp _ (L _ (HsVar _ (L _ leftVar))) _ (L _ (HsWrap _ _ (HsVar _ (L _ var))))) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType  var leftVar rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)

    L loc1 (HsApp _ (L loc2 (HsAppType _ (L _ (HsWrap _ (WpCompose (WpCompose (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableType))) (WpTyApp (LitTy (StrTyLit fastString)))) (WpTyApp _)) (HsVar _ opr))) _)) _) -> do
      let tblName' = case tableType of
                      AppTy ty1 _    -> showS ty1
                      TyConApp ty1 _ -> showS ty1
                      ty             -> showS ty
          filteredRule = filter (\rule -> (type_name rule) == tblName' && fastString == (mkFastString $ blocked_field rule)) rules
      case listToMaybe filteredRule of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc2, rule)

    _ -> pure Nothing

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType :: Var -> Var -> [Rule] -> [Rule]
verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType var leftVar rules =
  let name = showS $ varName var
      vType = varType leftVar
      arrTypeCon = getTypeConFromType vType
      updatedName = if "_" `isPrefixOf` name
            then fromMaybe name (stripPrefix "_" name)
            else name
  in filter (\rule -> elem (type_name rule) arrTypeCon && updatedName == blocked_field rule) rules

  where
    getTypeConFromType vType =
      case splitFunTy_maybe vType of
        Just (tyCon, _) -> [showS tyCon]
        Nothing ->
          case vType of
            (TyConApp typ tys) ->
              if null tys
                then [showS typ]
                else 
                  (\var -> do
                    case tyConAppTyCon_maybe var of
                      Just tyCon -> showS tyCon
                      Nothing -> "NA"
                    )  <$> tys
            _ -> []

verifyAndGetRuleVoilatedFnInfoWithRightExprAsType :: Var -> [Rule] -> [Rule]
verifyAndGetRuleVoilatedFnInfoWithRightExprAsType var rules =
  let name = showS $ varName var
      vType = varType var
      arrTypeCon = getTypeConFromType vType
      updatedName = if "_" `isPrefixOf` name
            then fromMaybe name (stripPrefix "_" name)
            else name
  in filter (\rule -> elem (type_name rule) arrTypeCon && updatedName == blocked_field rule) rules

  where
    getTypeConFromType vType =
      case splitFunTy_maybe vType of
        Just (tyCon, _) -> [showS tyCon]
        Nothing ->
          case vType of
            (TyConApp _ tys) ->
              (\localVar -> do
                case tyConAppTyCon_maybe localVar of
                  Just tyCon -> showS tyCon
                  Nothing -> "NA"
                )  <$> tys
            _ -> []

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v))                      = Just (v, [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl))              = (\clId -> (noLoc clId, [])) <$> conLikeWrapId_maybe cl
getFnNameWithAllArgs (L _ (HsAppType _ expr _))             = getFnNameWithAllArgs expr
getFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (v, [funr])
getFnNameWithAllArgs (L _ (HsApp _ funl funr))              = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs (L loc (OpApp _ funl op funr)) = do
  case op of
    (L _ (HsVar _ v))                -> Just (v, [funl,funr])
    (L _ (HsWrap _ _ (HsVar _ var))) -> Just (var, [funl,funr])
    _                                -> Nothing
getFnNameWithAllArgs (L loc ap@(HsWrap _ _ expr)) = getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs (L _ (HsCase _ funl exprLStmt)) = do
  let res = getFnNameWithAllArgs funl 
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> do
      let exprs = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      Just (fnName, ls <> exprs)
getFnNameWithAllArgs _                            = Nothing