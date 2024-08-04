{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PaymentFlow.Plugin (plugin) where

-- paymentFlow imports
import PaymentFlow.Types (VoilationRuleResult(..), PFRules(..), Rule(..), PluginOpts(..), defaultPluginOpts, defaultRule)
import PaymentFlow.Patterns

-- GHC imports

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?), Simple, Traversal)
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Data
import Data.Function (on)
import Data.List (nub, sortBy, groupBy, isInfixOf, isSuffixOf, isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Yaml
import GHC hiding (exprType)
import Prelude hiding (id)
import Data.Generics.Uniplate.Data

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.ConLike
import GHC.Core.TyCo.Rep
import GHC.Data.Bag
import GHC.HsToCore.Monad
import GHC.HsToCore.Expr
import GHC.Plugins hiding ((<>), getHscEnv, purePlugin)
import GHC.Tc.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Types.Annotations
import qualified GHC.Utils.Outputable as OP
#else
import Bag
import ConLike
import DsExpr
import DsMonad
import GhcPlugins hiding ((<>), getHscEnv, purePlugin)
import qualified Outputable as OP
import TcEvidence
import TcRnMonad
import TcRnTypes
import TcType
import TyCoRep
#endif

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

      let sortedErrors = sortBy (leftmost_smallest `on` srcSpan) errors
          groupedErrors = groupBy (\a b -> srcSpan a == srcSpan b) sortedErrors
          childFnFilterLogic srcGrpErrArr = do
            let srcSpn = maybe Nothing (\value -> Just $ srcSpan value) (listToMaybe srcGrpErrArr)
                srcSpanLine = getSrcSpanLine srcSpn
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
checkBind rule (L _ (FunBind{..} )) = do
  let funMatches = unLoc $ mg_alts fun_matches
  concat <$> mapM (checkMatch rule (getVarNameFromIDP $ unLoc fun_id)) funMatches
checkBind rule (L _ (AbsBinds {abs_binds = binds})) =
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
  tupleResponse <- catMaybes <$> sequence (checkExpr rules <$> updatedArgs)
  pure $ (\(x, y) -> VoilationRuleResult { fnName = getVarName fnName, srcSpan = x, rule = y, coreFnName = funId }) <$> tupleResponse

checkExpr :: [Rule]-> LHsExpr GhcTc -> TcM (Maybe (SrcSpan, Rule))
checkExpr rules expr =
  case expr of
    L _ (HsPar _ exp) -> checkExpr rules exp

#if __GLASGOW_HASKELL__ >= 900
    L loc (PatHsExpansion orig expanded) -> checkExpr rules (L loc expanded)
    
    L (SrcSpanAnn _ loc1) (HsApp _ (L _ (HsApp _ op' (L _ (HsVar _ (L _ var))))) (L _ (PatHsWrap _ (HsVar _ (L _ lens))))) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType  lens var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)

    L _ (HsApp _ (L _ (PatHsWrap _ (HsAppType _ _ (HsWC _ (L (SrcSpanAnn _ loc) (HsTyLit _ (HsStrTy _ fieldName)))) ))) (L _ (HsVar _ (L _ var)))) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithExprAndFieldAsName (showS fieldName) var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc, rule)

    L (SrcSpanAnn _ loc) (HsApp _ (L _ (HsRecFld _ (Unambiguous name _))) (L _ (HsVar _ (L _ var)))) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithExprAndFieldAsName (showS name) var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc, rule)

    _ -> pure Nothing    

#else

    L loc1 (HsApp _ (L _ (HsVar _ (L _ var))) _) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithRightExprAsType var rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)

    L _ (OpApp _ (L loc1 (OpApp _ (L _ (HsVar _ (L _ leftVar))) _ (L _ (PatHsWrap _ (HsVar _ (L _ var)))))) _ _) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType  var leftVar rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)
    
    L loc1 (OpApp _ (L _ (HsVar _ (L _ leftVar))) _ (L _ (PatHsWrap _ (HsVar _ (L _ var))))) -> do
      let voilationSatisfiedRules = verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType  var leftVar rules
      case listToMaybe voilationSatisfiedRules of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc1, rule)

    L _ (HsApp _ (L loc2 (HsAppType _ (L _ (PatHsWrap (WpCompose (WpCompose (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableType))) (WpTyApp (LitTy (StrTyLit fastString)))) (WpTyApp _)) (HsVar _ opr))) _)) _) -> do
      let tblName' = case tableType of
                      AppTy ty1 _    -> showS ty1
                      TyConApp ty1 _ -> showS ty1
                      ty             -> showS ty
          filteredRule = filter (\rule -> (type_name rule) == tblName' && fastString == (mkFastString $ blocked_field rule)) rules
      case listToMaybe filteredRule of
        Nothing -> pure Nothing
        Just rule -> pure $ Just (loc2, rule)

    _ -> pure Nothing

#endif

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType :: Var -> Var -> [Rule] -> [Rule]
verifyAndGetRuleVoilatedFnInfoWithLeftExprAsType var leftVar rules = do
  let name = showS $ varName var
      vType = varType leftVar
      arrTypeCon = getTypeConFromType vType
      updatedName = if "_" `isPrefixOf` name
            then fromMaybe name (stripPrefix "_" name)
            else name
  filter (\rule -> elem (type_name rule) arrTypeCon && updatedName == blocked_field rule) rules

verifyAndGetRuleVoilatedFnInfoWithExprAndFieldAsName :: String -> Var -> [Rule] -> [Rule]
verifyAndGetRuleVoilatedFnInfoWithExprAndFieldAsName name leftVar rules = do
  let vType = varType leftVar
      arrTypeCon = getTypeConFromType vType
      updatedName = if "_" `isPrefixOf` name
            then fromMaybe name (stripPrefix "_" name)
            else name
  filter (\rule -> elem (type_name rule) arrTypeCon && updatedName == blocked_field rule) rules

getTypeConFromType :: Type -> [String]
getTypeConFromType vType =
  case getTyConInStringFormat vType of
    Just value -> value
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
verifyAndGetRuleVoilatedFnInfoWithRightExprAsType var rules = do
  let name = showS $ varName var
      vType = varType var
      arrTypeCon = getTypeConFromType vType
      updatedName = if "_" `isPrefixOf` name
            then fromMaybe name (stripPrefix "_" name)
            else name
  filter (\rule -> elem (type_name rule) arrTypeCon && updatedName == blocked_field rule) rules

  where

    getTypeConFromType :: Type -> [String]
    getTypeConFromType vType =
      case getTyConInStringFormat vType of
        Just value -> value
        Nothing ->
          case vType of
            (TyConApp _ tys) ->
              (\localVar -> do
                case tyConAppTyCon_maybe localVar of
                  Just tyCon -> showS tyCon
                  Nothing -> "NA"
                )  <$> tys
            _ -> []

getTyConInStringFormat :: Type -> Maybe [String]
getTyConInStringFormat vType =
#if __GLASGOW_HASKELL__ >= 900
  case splitFunTy_maybe vType of
    Just (_, tyCon, _) -> Just [showS tyCon]
    Nothing -> Nothing 
#else 
  case splitFunTy_maybe vType of
    Just (tyCon, _) -> Just [showS tyCon]
    Nothing -> Nothing
#endif

conLikeWrapId :: ConLike -> Maybe Var
conLikeWrapId (RealDataCon dc) = Just (dataConWrapId dc)
conLikeWrapId _ = Nothing

#if __GLASGOW_HASKELL__ >= 900
noExtFieldOrAnn :: EpAnn a
noExtFieldOrAnn = noAnn

getLoc2 :: GenLocated (SrcSpanAnn' a) e -> SrcSpan
getLoc2 = getLocA

noExprLoc :: a -> Located a
noExprLoc = noLoc

getLocated :: GenLocated (SrcSpanAnn' a) e -> Located e
getLocated ap = L (getLocA ap) (unLoc ap)

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v))                      = Just (getLocated v, [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl))              = (\clId -> (noExprLoc clId, [])) <$> conLikeWrapId cl
getFnNameWithAllArgs (L _ (HsAppType _ expr _))             = getFnNameWithAllArgs expr
getFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (getLocated v, [funr])
getFnNameWithAllArgs (L _ (HsApp _ funl funr))              = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs (L loc (OpApp _ funl op funr)) = do
  case op of
    (L _ (HsVar _ v))                -> Just (getLocated v, [funl,funr])
    (L _ (PatHsWrap _ (HsVar _ var))) -> Just (getLocated var, [funl,funr])
    _                                -> Nothing
getFnNameWithAllArgs (L loc (PatHsWrap _ expr)) = getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs (L _ (HsCase _ funl exprLStmt)) = do
  let res = getFnNameWithAllArgs funl 
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> do
      let exprs = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      Just (fnName, ls <> exprs)
getFnNameWithAllArgs (L loc ap@(PatHsExpansion orig expanded)) = 
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> getFnNameWithAllArgs (L loc (HsApp noExtFieldOrAnn funl funr))
      _ -> getFnNameWithAllArgs (L loc expanded)
    _ -> getFnNameWithAllArgs (L loc expanded)
getFnNameWithAllArgs _                            = Nothing

#else

noExtFieldOrAnn :: NoExtField
noExtFieldOrAnn = noExtField

getLoc2 :: HasSrcSpan a => a -> SrcSpan
getLoc2 = getLoc

noExprLoc :: (HasSrcSpan a) => SrcSpanLess a -> a
noExprLoc = noLoc

getLocated :: (HasSrcSpan a) => a -> Located (SrcSpanLess a)
getLocated ap = L (getLoc ap) (unLoc ap)

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
getFnNameWithAllArgs (L loc (OpApp _ funl op funr)) =
  case showS op of
    "($)" -> getFnNameWithAllArgs $ (L loc (HsApp noExtFieldOrAnn funl funr))
    _     -> Nothing
getFnNameWithAllArgs (L loc (PatHsWrap _ expr)) = getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs (L _ (HsCase _ funl exprLStmt)) = do
  let res = getFnNameWithAllArgs funl 
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> do
      let exprs = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      Just (fnName, ls <> exprs)
getFnNameWithAllArgs _                            = Nothing

#endif

getVarNameFromIDP :: IdP GhcTc -> String
getVarNameFromIDP var = occNameString . occName $ var

getVarName :: Located Var -> String
getVarName var = (getOccString . varName . unLoc) var

getSrcSpanLine :: Maybe SrcSpan -> Int
getSrcSpanLine = \case
#if __GLASGOW_HASKELL__ >= 900
  (Just (RealSrcSpan span _)) -> srcSpanStartLine span
  _ -> 0
#else
  (Just (RealSrcSpan span)) -> srcSpanStartLine span
  _ -> 0
#endif