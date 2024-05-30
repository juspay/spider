{-# LANGUAGE DataKinds #-}

module Sheriff.Plugin (plugin) where

import Bag (bagToList,listToBag)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Data
import Control.Reference (biplateRef, (^?), Simple, Traversal)
import Data.Generics.Uniplate.Data (universeBi, childrenBi, contextsBi, holesBi, children)
import Data.List (nub)
import Debug.Trace (traceShowId, trace)
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
    Match (m_grhss, m_pats),
    MatchGroup (..),
    Name,
    Pat (..),
    PatSynBind (..),
    noLoc, Module (moduleName), moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass
  )
import GHC.Hs.Binds
import GhcPlugins (idName,Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr,elemNameSet,pprPrefixName,idType,tidyOpenType, isEnumerationTyCon)
import HscTypes (ModSummary (..), hscEPS, eps_PTE)
import Name (nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile, appendFile)
import Data.Aeson as A
import Data.ByteString.Lazy (writeFile, appendFile)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe)
import Control.Exception (try,SomeException)
import SrcLoc
import Annotations
import Outputable (showSDocUnsafe, ppr, Outputable(..))
import GhcPlugins ()
import DynFlags ()
import Control.Monad (foldM,when)
import Data.List
import Data.List.Extra (replace,splitOn)
import Data.Maybe (fromJust,isJust,mapMaybe)
import Sheriff.Types
import Sheriff.Rules
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent
import System.Directory
import PatSyn
import Avail
import TcEnv
import GHC.Hs.Utils as GHCHs
import TyCoPpr ( pprUserForAll, pprTypeApp, pprSigmaType )
import Data.Bool (bool)
import qualified Data.Map as Map
import qualified Outputable as OP
import FastString
import Data.Maybe (catMaybes)
import DsMonad (initDsTc)
import DsExpr (dsLExpr)
import TcRnMonad (failWith, addErr, addErrAt, addErrs, getEnv, env_top)
import Name (isSystemName)
import GHC (OverLitTc(..), HsOverLit(..))
import CoreUtils (exprType)
import Control.Applicative ((<|>))
import Type (isFunTy, funResultTy, splitAppTys, dropForAlls)
import TyCoRep (Type(..), TyLit (..))
import Data.ByteString.Lazy as BSL ()
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import TcType
import ConLike
import TysWiredIn
import GHC.Hs.Lit (HsLit(..))
import TcEvidence

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = sheriff
    , pluginRecompile = purePlugin
    }

logDebugInfo :: Bool
logDebugInfo = False

logTypeDebugging :: Bool
logTypeDebugging = False

logWarnInfo :: Bool
logWarnInfo = True

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

-- Parse the YAML file
parseYAMLFile :: FilePath -> IO (Either ParseException YamlTables)
parseYAMLFile file = decodeFileEither file

sheriff :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
sheriff opts modSummary tcEnv = do
  let pluginOpts = case opts of
                    []      -> defaultPluginOpts
                    (x : _) -> fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
    
      throwCompilationErrorV = throwCompilationError pluginOpts
      saveToFileV = saveToFile pluginOpts
      savePathV = savePath pluginOpts
      indexedKeysPathV = indexedKeysPath pluginOpts
      failOnFileNotFoundV = failOnFileNotFound pluginOpts 

  -- parse the yaml file from the path given
  parsedYaml <- liftIO $ parseYAMLFile indexedKeysPathV

  -- Check the parsed yaml file for indexedDbKeys and throw compilation error if configured
  rulesList <- case parsedYaml of
                  Left err -> do
                    when failOnFileNotFoundV $ addErr (mkInvalidIndexedKeysFile (show err))
                    pure badPracticeRules
                  Right (YamlTables tables) -> pure $ badPracticeRules <> (map yamlToDbRule tables)

  errors <- concat <$> (mapM (loopOverModBinds rulesList pluginOpts) $ bagToList $ tcg_binds tcEnv)

  if throwCompilationErrorV
    then addErrs $ map mkGhcCompileError errors
    else pure ()

  if saveToFileV
    then addErrToFile modSummary savePathV errors  
    else pure ()

  return tcEnv

--------------------------- Core Logic ---------------------------

-- FLOW :
-- Perform steps for each top level function binding in a module
-- 1. Extract the value bindings & fun bindings inside the definition
-- 2. Extract the function arguments
-- 3. Get all the FunApp in the definition
-- 4. Check and return if the FunApp is Logging Function and corresponding value/description argument is Text
-- 5. Now, we have function application Var and corresponding arg to be checked
-- 6. Check if the arg has any stringification function, If yes, it is `ErrorCase`
-- 7. Check if the arg uses any local binding from WhereBinds or Normal Bind, If yes, then check if that binding has any stringification output 
-- 8. Check if arg uses top level binding from any module. If yes, then check if that binding has any stringification output

-- Loop over top level function binds
loopOverModBinds :: Rules -> PluginOpts -> LHsBindLR GhcTc GhcTc -> TcM [CompileError]
loopOverModBinds rules opts (L _ ap@(FunBind _ id matches _ _)) = do
  -- liftIO $ print "FunBinds" >> showOutputable ap
  calls <- getBadFnCalls rules opts ap
  mapM mkCompileError calls
loopOverModBinds _ _ (L _ ap@(PatBind _ _ pat_rhs _)) = do
  -- liftIO $ print "PatBinds" >> showOutputable ap
  pure []
loopOverModBinds _ _ (L _ ap@(VarBind {var_rhs = rhs})) = do 
  -- liftIO $ print "VarBinds" >> showOutputable ap
  pure []
loopOverModBinds rules opts (L _ ap@(AbsBinds {abs_binds = binds})) = do
  -- liftIO $ print "AbsBinds" >> showOutputable ap
  list <- mapM (loopOverModBinds rules opts) $ bagToList binds
  pure (concat list)
loopOverModBinds _ _ _ = pure []

-- Get all the FunApps inside the top level function bind
-- This call can be anywhere in `where` clause or `regular` RHS
getBadFnCalls :: Rules -> PluginOpts -> HsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
getBadFnCalls rules opts (FunBind _ id matches _ _) = do
  let funMatches = map unLoc $ unLoc $ mg_alts matches
  concat <$> mapM getBadFnCallsHelper funMatches
  where
    getBadFnCallsHelper :: Match GhcTc (LHsExpr GhcTc) -> TcM [(LHsExpr GhcTc, Violation)]
    getBadFnCallsHelper match = do
      let whereBinds = (grhssLocalBinds $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          normalBinds = (grhssGRHSs $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          argBinds = m_pats match
          -- exprs = match ^? biplateRef :: [LHsExpr GhcTc]
          -- use childrenBi and then repeated children usage as per use case
          exprs = traverseConditionalUni (noWhereClauseExpansion) (childrenBi match :: [LHsExpr GhcTc])
      concat <$> mapM (isBadFunApp rules opts) exprs
getBadFnCalls _ _ _ = pure []

-- Takes a predicate which return true if further expansion is not required, false otherwise
traverseConditionalUni :: (Data a) => (a -> Bool) -> [a] -> [a]
traverseConditionalUni _ [] = []
traverseConditionalUni p (x : xs) = 
  if p x 
    then x : traverseConditionalUni p xs
    else (x : traverseConditionalUni p (children x)) <> traverseConditionalUni p xs

noGivenFunctionCallExpansion :: String -> LHsExpr GhcTc -> Bool
noGivenFunctionCallExpansion fnName expr = case expr of
  (L loc (HsWrap _ _ expr)) -> noWhereClauseExpansion (L loc expr)
  _ -> case getFnNameWithAllArgs expr of
        Just (lVar, _) -> (getOccString . varName . unLoc $ lVar) == fnName
        Nothing -> False

noWhereClauseExpansion :: LHsExpr GhcTc -> Bool
noWhereClauseExpansion expr = case expr of
  (L loc (HsWrap _ _ expr)) -> noWhereClauseExpansion (L loc expr)
  (L _ (ExplicitList (TyConApp ty _) _ _)) -> showS ty == "Clause"
  _ -> False

isBadFunApp :: Rules -> PluginOpts -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunApp rules opts ap@(L _ (HsVar _ v)) = isBadFunAppHelper rules opts ap
isBadFunApp rules opts ap@(L _ (HsApp _ funl funr)) = isBadFunAppHelper rules opts ap
isBadFunApp rules opts ap@(L loc (HsWrap _ _ expr)) = isBadFunApp rules opts (L loc expr)
isBadFunApp rules opts ap@(L _ (ExplicitList _ _ _)) = isBadFunAppHelper rules opts ap
isBadFunApp rules opts (L _ (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadFunAppHelper rules opts $ mkHsApp lfun rfun
    _ -> pure []
isBadFunApp _ _ _ = pure []

isBadFunAppHelper :: Rules -> PluginOpts -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunAppHelper rules opts ap = concat <$> mapM (\rule -> checkAndApplyRule rule opts ap) rules

validateFunctionRule :: FunctionRule -> PluginOpts -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateFunctionRule rule _opts fnName args expr = do
  if (arg_no rule) == 0 -- considering arg 0 as the case for blocking the whole function occurence
    then pure [(expr, FnUseBlocked rule)]
  else do
    let matches = drop ((arg_no rule) - 1) args
    if length matches == 0
      then pure []
    else do
      let arg = head matches
      argTypeGhc <- getHsExprType arg
      let argType = showS argTypeGhc
          argTypeBlocked = validateType argTypeGhc $ types_blocked_in_arg rule
          isArgTypeToCheck = validateType argTypeGhc $ types_to_check_in_arg rule

      when (logDebugInfo && fnName /= "NA") $
        liftIO $ do
          print $ (fnName, map showS args)
          print $ (fnName, showS arg)
          print $ rule
          print $ "Arg Type = " <> argType

      if argTypeBlocked
        then pure [(expr, ArgTypeBlocked argType rule)]
      else if not isArgTypeToCheck
        then pure []
      else do
        -- It's a rule function with to_be_checked type argument
        blockedFnsList <- getBlockedFnsList arg rule -- check if the expression has any stringification function
        pure $ fmap (\(lExpr, blockedFnName, blockedFnArgTyp) -> (lExpr, FnBlockedInArg (blockedFnName, blockedFnArgTyp) rule)) blockedFnsList

validateType :: Type -> TypesToCheckInArg -> Bool
validateType argTyp@(TyConApp tyCon ls) typs = 
  if showS tyCon == "(,)" && "(,)" `elem` typs 
    then (\t -> validateType t typs) `any` ls 
  else if showS tyCon == "[]" && "[]" `elem` typs 
    then (\t -> validateType t typs) `any` ls 
  else if showS tyCon == "Maybe" && "Maybe" `elem` typs 
    then (\t -> validateType t typs) `any` ls 
  else showS argTyp `elem` typs
validateType argTyp typs = showS argTyp `elem` typs

validateDBRule :: DBRule -> PluginOpts -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateDBRule rule@(DBRule ruleName ruleTableName ruleColNames _) opts tableName clauses expr = do
  simplifiedExprs <- trfWhereToSOP clauses
  let checkDBViolation = case (matchAllInsideAnd opts) of
                          True  -> checkDBViolationMatchAll
                          False -> checkDBViolationWithoutMatchAll
  violations <- catMaybes <$> mapM checkDBViolation simplifiedExprs
  pure violations
  where
    -- Since we need all columns to be indexed, we need to check for the columns in the order of composite key
    checkDBViolationMatchAll :: [SimplifiedIsClause] -> TcM (Maybe (LHsExpr GhcTc, Violation))
    checkDBViolationMatchAll sop = do
      let isDbViolation (cls, colName, tableName) = (ruleTableName == tableName) && not (doesMatchColNameInDbRuleWithComposite colName ruleColNames (map (\(_, col, _) -> col) sop))
      case find isDbViolation sop of
        Nothing  -> pure Nothing
        Just (clause, colName, tableName) -> pure $ Just (clause, NonIndexedDBColumn colName tableName rule)

    -- Since any one indexed column is sufficient, we can check only for the first column of the composite key
    checkDBViolationWithoutMatchAll :: [SimplifiedIsClause] -> TcM (Maybe (LHsExpr GhcTc, Violation))
    checkDBViolationWithoutMatchAll sop = do
      let isDbViolation (cls, colName, tableName) = (ruleTableName == tableName) && not (doesMatchColNameInDbRule colName ruleColNames)
      case any (not . isDbViolation) sop of
        True  -> pure Nothing
        False -> case sop of
                  [] -> pure Nothing
                  ((clause, colName, tableName) : _) -> pure $ Just (clause, NonIndexedDBColumn colName tableName rule)

-- Check only for the ordering of the columns of the composite key
doesMatchColNameInDbRuleWithComposite :: String -> [YamlTableKeys] -> [String] -> Bool
doesMatchColNameInDbRuleWithComposite _ [] _ = False
doesMatchColNameInDbRuleWithComposite colName (key : keys) allColsInAnd = 
  case key of
    (CompositeKey compCols)  -> checkCompositeCols compCols || (doesMatchColNameInDbRuleWithComposite colName keys allColsInAnd)
    (NonCompositeKey col)   -> (colName == col) || (doesMatchColNameInDbRuleWithComposite colName keys allColsInAnd)
  where
    checkCompositeCols :: [String] -> Bool
    checkCompositeCols [] = False
    checkCompositeCols (compCol : compCols) = 
      if compCol == colName
        then True
        else (compCol `elem` allColsInAnd) && checkCompositeCols compCols

-- Check only for the first column of the composite key
doesMatchColNameInDbRule :: String -> [YamlTableKeys] -> Bool
doesMatchColNameInDbRule _ [] = False
doesMatchColNameInDbRule colName (key : keys) = 
  case key of
    (CompositeKey (col:_))  -> (colName == col) || (doesMatchColNameInDbRule colName keys)
    (NonCompositeKey col)   -> (colName == col) || (doesMatchColNameInDbRule colName keys)

type SimplifiedIsClause = (LHsExpr GhcTc, String, String)

trfWhereToSOP :: [LHsExpr GhcTc] -> TcM [[SimplifiedIsClause]]
trfWhereToSOP [] = pure [[]]
trfWhereToSOP (clause : ls) = do
  let res = getWhereClauseFnNameWithAllArgs clause
      (fnName, args) = fromMaybe ("NA", []) res
  case (fnName, args) of
    ("And", [(L _ (ExplicitList _ _ arg))]) -> do
      curr <- trfWhereToSOP arg
      rem  <- trfWhereToSOP ls
      pure [x <> y | x <- curr, y <- rem]
    ("Or", [(L _ (ExplicitList _ _ arg))]) -> do
      curr <- foldM (\r cls -> fmap (<> r) $ trfWhereToSOP [cls]) [] arg
      rem  <- trfWhereToSOP ls
      pure [x <> y | x <- curr, y <- rem]
    ("$WIs", [arg1, arg2]) -> do
      curr <- getIsClauseData arg1 arg2 clause
      rem  <- trfWhereToSOP ls
      case curr of
        Nothing -> pure rem
        Just (tblName, colName) -> pure $ fmap (\lst -> (clause, tblName, colName) : lst) rem
    (fn, _) -> when logWarnInfo (liftIO $ print $ "Invalid/unknown clause in `where` clause : " <> fn <> " at " <> (showS . getLoc $ clause)) >> trfWhereToSOP ls

getIsClauseData :: LHsExpr GhcTc -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM (Maybe (String, String))
getIsClauseData fieldArg _comp _clause = do
  let fieldSpecType = getDBFieldSpecType fieldArg
  mbColNameAndTableName <- case fieldSpecType of
    None     -> when logWarnInfo (liftIO $ print "Can't identify the way in which DB field is specified") >> pure Nothing
    Selector -> do
      case (splitOn ":" $ showS fieldArg) of
        ("$sel" : colName : tableName : []) -> pure $ Just (colName, tableName)
        _ -> when logWarnInfo (liftIO $ print "Invalid pattern for Selector way") >> pure Nothing
    RecordDot -> do
      let tyApps = filter (\x -> case x of 
                                  (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> True
                                  (HsWrap _ (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableVar))) (HsAppType _ _ fldName)) -> True
                                  _ -> False
                          ) $ (fieldArg ^? biplateRef :: [HsExpr GhcTc])
      if length tyApps > 0 
        then 
          case head tyApps of
            (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> do
              typ <- getHsExprType tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (getStrFromHsWildCardBndrs fldName, take (length tblName' - 1) tblName')
            (HsWrap _ (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableType))) (HsAppType _ _ fldName)) ->
              let tblName' = case tableType of
                                  AppTy ty1 _    -> showS ty1
                                  TyConApp ty1 _ -> showS ty1
                                  ty             -> showS ty
              in pure $ Just (getStrFromHsWildCardBndrs fldName, take (length tblName' - 1) tblName')
            _ -> when logWarnInfo (liftIO $ putStrLn "HsAppType not present. Should never be the case as we already filtered.") >> pure Nothing
        else when logWarnInfo (liftIO $ putStrLn "HsAppType not present after filtering. Should never reach as already deduced RecordDot.") >> pure Nothing
    Lens -> do
      let opApps = filter isLensOpApp (fieldArg ^? biplateRef :: [HsExpr GhcTc])
      case opApps of
        [] -> when logWarnInfo (liftIO $ putStrLn "No lens operator application present in lens case.") >> pure Nothing
        (opExpr : _) -> do
          case opExpr of
            (OpApp _ tableVar _ fldVar) -> do
              let fldName = tail $ showS fldVar
              typ <- getHsExprType tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (fldName, take (length tblName' - 1) tblName')
            (SectionR _ _ (L _ lens)) -> do
              let tys = lens ^? biplateRef :: [Type]
                  typeForTableName = filter (\typ -> case typ of 
                                                      (TyConApp typ1 [typ2]) -> ("T" `isSuffixOf` showS typ1) && (showS typ2 == "Columnar' f")
                                                      (AppTy typ1 typ2) -> ("T" `isSuffixOf` showS typ1) && (showS typ2 == "Columnar' f")
                                                      _ -> False
                                              ) tys
              let tblName' = case head typeForTableName of
                                  AppTy ty1 _    -> showS ty1
                                  TyConApp ty1 _ -> showS ty1
                                  ty             -> showS ty
              pure $ Just (tail $ showS lens, take (length tblName' - 1) tblName')
            _ -> when logWarnInfo (liftIO $ putStrLn "OpApp not present. Should never be the case as we already filtered.") >> pure Nothing
  
  pure mbColNameAndTableName

checkAndApplyRule :: Rule -> PluginOpts -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
checkAndApplyRule ruleT opts ap = case ruleT of
  DBRuleT rule@(DBRule _ ruleTableName _ _) -> 
    case ap of
      (L _ (ExplicitList (TyConApp ty [_, tblName]) _ exprs)) -> case (showS ty == "Clause" && showS tblName == (ruleTableName <> "T")) of
        True  -> validateDBRule rule opts (showS tblName) exprs ap 
        False -> pure []
      _ -> pure []
  FunctionRuleT rule@(FunctionRule _ ruleFnName arg_no _ _ _ _) -> do
    let res = getFnNameWithAllArgs ap
    -- let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((nameStableString . varName . unLoc) x, y)) $ res
        (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
    case (fnName == ruleFnName && length args >= arg_no) of
      True  -> validateFunctionRule rule opts fnName args ap 
      False -> pure [] 

getDBFieldSpecType :: LHsExpr GhcTc -> DBFieldSpecType
getDBFieldSpecType (L _ expr)
  | isPrefixOf "$sel" (showS expr) = Selector
  | isInfixOf "^." (showS expr) = Lens
  | (\x -> isInfixOf "@" x) (showS expr) = RecordDot
  | otherwise = None

getWhereClauseFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (String, [LHsExpr GhcTc])
getWhereClauseFnNameWithAllArgs (L _ (HsVar _ v)) = Just (getVarName v, [])
getWhereClauseFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (getVarName $ noLoc clId, [])) <$> conLikeWrapId_maybe cl
getWhereClauseFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (getVarName v, [funr])
getWhereClauseFnNameWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getWhereClauseFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getWhereClauseFnNameWithAllArgs (L _ (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> getWhereClauseFnNameWithAllArgs $ mkHsApp lfun rfun
    _ -> Nothing
getWhereClauseFnNameWithAllArgs (L loc ap@(HsPar _ expr)) = getWhereClauseFnNameWithAllArgs expr
-- If condition inside the list, add dummy type
getWhereClauseFnNameWithAllArgs (L loc ap@(HsIf _ _ _pred thenCl elseCl)) = Just ("Or", [L loc (ExplicitList (LitTy (StrTyLit "Dummy")) Nothing [thenCl, elseCl])])
getWhereClauseFnNameWithAllArgs (L loc ap@(HsWrap _ _ expr)) = getWhereClauseFnNameWithAllArgs (L loc expr)
getWhereClauseFnNameWithAllArgs (L loc ap@(ExprWithTySig _ expr _)) = getWhereClauseFnNameWithAllArgs expr
getWhereClauseFnNameWithAllArgs _ = Nothing

getVarName :: Located Var -> String
getVarName var = (getOccString . varName . unLoc) var

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v)) = Just (v, [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (noLoc clId, [])) <$> conLikeWrapId_maybe cl
getFnNameWithAllArgs (L _ (HsAppType _ expr _)) = getFnNameWithAllArgs expr
getFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (v, [funr])
getFnNameWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs (L _ (OpApp _ funl op funr)) = do
  case showS op of
    "($)" -> getFnNameWithAllArgs $ mkHsApp funl funr
    _ -> Nothing
getFnNameWithAllArgs (L loc ap@(HsWrap _ _ expr)) = do
  getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs _ = Nothing

--------------------------- Utils ---------------------------
-- Check if HsExpr is Function Application
isFunApp :: LHsExpr GhcTc -> Bool
isFunApp (L _ (HsApp _ _ _)) = True
isFunApp (L _ (OpApp _ funl op funr)) = True
isFunApp _ = False

-- Check if HsExpr is Lens operator application
isLensOpApp :: HsExpr GhcTc -> Bool
isLensOpApp (OpApp _ _ op _) = showS op == "(^.)"
isLensOpApp (SectionR _ op _) = showS op == "(^.)"
isLensOpApp _ = False

-- If the type is literal type, get the string name of the literal, else return the showS verison of the type
getStrFromHsWildCardBndrs :: HsWildCardBndrs (NoGhcTc GhcTc) (LHsType (NoGhcTc GhcTc)) -> String
getStrFromHsWildCardBndrs (HsWC _ (L _ (HsTyLit _ (HsStrTy _ fs)))) = unpackFS fs
getStrFromHsWildCardBndrs typ = showS typ

-- Check if a Var is fun type
isFunVar :: Var -> Bool
isFunVar = isFunTy . dropForAlls . idType 

-- Check if a Type is Enum type
isEnumType :: Type -> Bool
isEnumType (TyConApp tyCon _) = isEnumerationTyCon tyCon
isEnumType _ = False

-- Pretty print the Internal Representations
showOutputable :: (MonadIO m, Outputable a) => a -> m ()
showOutputable = liftIO . putStrLn . showSDocUnsafe . ppr

-- Create GHC compilation error from CompileError
mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ getErrMsgWithSuggestions (err_msg err) (suggested_fixes err))

-- Make error message with suggestion
getErrMsgWithSuggestions :: String -> Suggestions -> String
getErrMsgWithSuggestions errMsg suggestions = errMsg
  <> newLine <> fourSpaces <> "Suggested fixes: "
  <> foldr (\(suggestionNo, suggestion) r -> newLine <> sixSpaces <> show suggestionNo <> ". " <> suggestion <> r) "" (zip [1..] suggestions)
  where 
    newLine = "\n"
    twoSpaces = "  "
    fourSpaces = twoSpaces <> twoSpaces
    sixSpaces = twoSpaces <> fourSpaces

-- Create invalid indexedKeys file compilation error
mkInvalidIndexedKeysFile :: String -> OP.SDoc
mkInvalidIndexedKeysFile err = OP.text err

-- Create Internal Representation of Logging Error
mkCompileError :: (LHsExpr GhcTc, Violation) -> TcM CompileError
mkCompileError (expr, violation) = pure $ CompileError "" "" (show violation) (getLoc expr) violation (getViolationSuggestions violation)

-- [DEPRECATED] Get Return type of the function application arg
getArgTypeWrapper :: LHsExpr GhcTc -> [Type]
getArgTypeWrapper expr@(L _ (HsApp _ lfun rfun)) = getArgType expr True
getArgTypeWrapper expr@(L _ (OpApp _ lfun op rfun)) = 
  case showS op of
    "($)" -> getArgType lfun True
    "(.)" -> getArgTypeWrapper lfun
    "(<>)" -> getArgTypeWrapper lfun
    _ -> getArgType op True
getArgTypeWrapper (L loc (HsWrap _ _ expr)) = getArgTypeWrapper (L loc expr)
getArgTypeWrapper (L loc (HsPar _ expr)) = getArgTypeWrapper expr
getArgTypeWrapper expr = getArgType expr False

-- [DEPRECATED] Get LHsExpr type
getArgType :: LHsExpr GhcTc -> Bool -> [Type]
getArgType (L _ (HsLit _ v)) _ = getLitType v
getArgType (L _ (HsOverLit _ (OverLit (OverLitTc _ typ) v _))) _ = [typ]
getArgType (L loc (HsWrap _ _ expr)) shouldReturnFinalType = getArgType (L loc expr) shouldReturnFinalType
getArgType (L loc (HsApp _ lfun rfun)) shouldReturnFinalType = getArgType lfun shouldReturnFinalType
getArgType arg shouldReturnFinalType = 
  let vars = filter (not . isSystemName . varName) $ arg ^? biplateRef in 
  if length vars == 0
    then []
  else
    let tys = idType $ head vars 
        (foralls, constraints, actualTyp) = tcSplitNestedSigmaTys tys
        typeReturnFn = bool (\x -> [x]) getReturnType shouldReturnFinalType
        actualReturnTyp = (trfUsingConstraints constraints $ typeReturnFn actualTyp)
    in actualReturnTyp

-- [DEPRECATED] Get HsLit literal type
getLitType :: HsLit GhcTc -> [Type]
getLitType (HsChar _ _	) = [charTy]
getLitType (HsCharPrim _ _) = [charTy]
getLitType (HsString _ _) = [stringTy]
getLitType (HsStringPrim _ _) = [stringTy]
getLitType (HsInt _ _) = [intTy]
getLitType (HsIntPrim _ _) = [intTy]
getLitType (HsWordPrim _ _) = [wordTy]
getLitType (HsInt64Prim _ _) = [intTy]
getLitType (HsWord64Prim _ _) = [wordTy]
getLitType (HsInteger _ _ _) = [intTy]
getLitType (HsRat _ _ _) = [doubleTy]
getLitType (HsFloatPrim _ _) = [floatTy]
getLitType (HsDoublePrim _ _) = [doubleTy]
getLitType _ = []

-- [DEPRECATED] Get final return type of any type/function signature
getReturnType :: Type -> [Type]
getReturnType typ 
  | isFunTy typ = getReturnType $ tcFunResultTy typ
  | otherwise = let (x, y) = tcSplitAppTys typ in x : y

-- [DECRECATED] Transform the type from the constraints
trfUsingConstraints :: [PredType] -> [Type] -> [Type]
trfUsingConstraints constraints typs =
  let replacements = catMaybes $ map constraintsToReplacements constraints
  -- in map (\typ -> ) typs
  in map (replacer replacements) typs
  where
    constraintsToReplacements :: PredType -> Maybe (Type, Type)
    constraintsToReplacements predTyp = case tcSplitTyConApp_maybe predTyp of
      Just (tycon, [typ]) -> if showS tycon == "IsString"
                              then Just (typ, stringTy)
                             else if showS tycon == "Num" || showS tycon == "GHC.Num.Num"
                              then Just (typ, intTy)
                             else Nothing
      _ -> Nothing

    replacer :: [(Type, Type)] -> Type -> Type
    replacer replacements typ@(AppTy ty1 ty2) = AppTy (replacer replacements ty1) (replacer replacements ty2) 
    replacer replacements typ@(TyConApp tyCon typOrKinds) = TyConApp tyCon $ map (replacer replacements) typOrKinds
    replacer replacements typ@(ForAllTy bndrs typ') = ForAllTy bndrs (replacer replacements typ')
    replacer replacements typ@(FunTy flag ty1 ty2) = FunTy flag (replacer replacements ty1) (replacer replacements ty2) 
    replacer replacements typ = maybe typ snd $ (\x -> eqType (fst x) typ) `find` replacements 

-- [DEPRECATED] Get List of stringification functions used inside a HsExpr; Uses `stringifierFns` 
getStringificationFns :: LHsExpr GhcTc -> TcM [String] 
getStringificationFns (L _ ap@(HsVar _ v)) = do
  liftIO $ putStrLn "Inside HsVar" >> putStrLn (showS ap) 
  pure $ [getOccString v]
  -- case (getOccString v) `elem` stringifierFns of
  --   True -> pure [getOccString v]
  --   False -> pure []
getStringificationFns (L _ ap@(HsApp _ lfun rfun)) = do
  liftIO $ putStrLn "Inside HsApp" >> putStrLn (showS ap) 
  x1 <- getStringificationFns lfun
  x2 <- getStringificationFns rfun
  pure $ x1 <> x2
getStringificationFns (L _ ap@(OpApp _ lfun op rfun)) = do
  liftIO $ putStrLn "Inside OpApp" >> putStrLn (showS ap) 
  x1 <- getStringificationFns lfun
  x2 <- getStringificationFns op
  x3 <- getStringificationFns rfun
  pure $ x1 <> x2 <> x3
getStringificationFns (L _ ap@(HsPar _ expr)) = do
  liftIO $ putStrLn "Inside HsPar" >> putStrLn (showS ap) 
  getStringificationFns expr
getStringificationFns (L loc ap@(HsWrap _ _ expr)) = do
  liftIO $ putStrLn "Inside HsWrap" >> putStrLn (showS ap) 
  getStringificationFns (L loc expr)
getStringificationFns _ = do
  liftIO $ putStrLn $ "Inside _"
  pure []

-- [DEPRECATED] Get List of stringification functions used inside a HsExpr; Uses `stringifierFns` 
getStringificationFns2 :: LHsExpr GhcTc -> FunctionRule -> [String] 
getStringificationFns2 arg rule =
  let vars = arg ^? biplateRef :: [Var]
      blockedFns = fmap (\(fname, _, _) -> fname) $ fns_blocked_in_arg rule
  in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns)) $ takeWhile isFunVar $ filter (not . isSystemName . varName) vars

-- Get List of blocked functions used inside a HsExpr; Uses `getBlockedFnsList` 
getBlockedFnsList :: LHsExpr GhcTc -> FunctionRule -> TcM [(LHsExpr GhcTc, String, String)] 
getBlockedFnsList arg rule@(FunctionRule _ _ arg_no fnsBlocked _ _ _) = do
  let argHsExprs = arg ^? biplateRef :: [LHsExpr GhcTc]
      fnApps = filter isFunApp argHsExprs
  when logDebugInfo $ liftIO $ do
    print "getBlockedFnsList"
    showOutputable arg
    showOutputable fnApps
  catMaybes <$> mapM checkFnBlockedInArg fnApps 
  --     vars = arg ^? biplateRef :: [Var]
  --     blockedFns = fmap (\(fname, _, _) -> fname) $ fns_blocked_in_arg rule
  -- in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns) && (not . isSystemName . varName) x) vars
  where 
    checkFnBlockedInArg :: LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, String, String))
    checkFnBlockedInArg expr = do
      let res = getFnNameWithAllArgs expr
      when logDebugInfo $ liftIO $ do
        print "checkFnBlockedInArg"
        showOutputable res
      case res of
        Nothing -> pure Nothing
        Just (fnName, args) -> isPresentInBlockedFnList expr fnsBlocked ((getOccString . varName . unLoc) fnName) args
    
    isPresentInBlockedFnList :: LHsExpr GhcTc -> FnsBlockedInArg -> String -> [LHsExpr GhcTc] -> TcM (Maybe (LHsExpr GhcTc, String, String))
    isPresentInBlockedFnList expr [] _ _ = pure Nothing
    isPresentInBlockedFnList expr ((ruleFnName, ruleArgNo, ruleAllowedTypes) : ls) fnName fnArgs = do
      when logDebugInfo $ liftIO $ do
        print "isPresentInBlockedFnList"
        print (ruleFnName, ruleArgNo, ruleAllowedTypes)
      case ruleFnName == fnName && length fnArgs >= ruleArgNo of
        False -> isPresentInBlockedFnList expr ls fnName fnArgs
        True  -> do
          let reqArg = head $ drop (ruleArgNo - 1) fnArgs
          argType <- getHsExprType reqArg
          when logDebugInfo $ liftIO $ do
            showOutputable reqArg
            showOutputable argType
          if validateAllowedTypes argType ruleAllowedTypes
            then isPresentInBlockedFnList expr ls fnName fnArgs
            else pure $ Just (expr, fnName, showS argType)

    validateAllowedTypes :: Type -> TypesAllowedInArg -> Bool
    validateAllowedTypes argType@(TyConApp tyCon ls) ruleAllowedTypes = 
      if showS tyCon == "(,)" && "(,)" `elem` ruleAllowedTypes
        then (\t -> validateAllowedTypes t ruleAllowedTypes) `all` ls 
      else if showS tyCon == "[]" && "[]" `elem` ruleAllowedTypes
        then (\t -> validateAllowedTypes t ruleAllowedTypes) `all` ls
      else if showS tyCon == "Maybe" && "Maybe" `elem` ruleAllowedTypes
        then (\t -> validateAllowedTypes t ruleAllowedTypes) `all` ls
      else (isEnumType argType && "EnumTypes" `elem` ruleAllowedTypes) || (showS argType) `elem` ruleAllowedTypes
    validateAllowedTypes argType ruleAllowedTypes = (isEnumType argType && "EnumTypes" `elem` ruleAllowedTypes) || (showS argType) `elem` ruleAllowedTypes
    
-- Add GHC error to a file    
addErrToFile :: ModSummary -> String -> [CompileError] -> TcM ()
addErrToFile modSummary path errs = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      -- res = (encodePretty moduleName') <> (fromString ": ") <> encodePretty errs <> (fromString ",")
      res = encodePretty errs
  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile (path <> moduleName' <> "_compilationErrors.json") res

-- Get type for a LHsExpr GhcTc
getHsExprType :: LHsExpr GhcTc -> TcM Type
getHsExprType expr = do
  coreExpr <- initDsTc $ dsLExpr expr
  when logTypeDebugging $ liftIO $ print $ "DebugType = " <> (debugPrintType $ exprType coreExpr)
  pure $ exprType coreExpr

debugPrintType :: Type -> String
debugPrintType (TyVarTy v) = "(TyVar " <> showS v <> ")"
debugPrintType (AppTy ty1 ty2) = "(AppTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
debugPrintType (TyConApp tycon tys) = "(TyCon (" <> showS tycon <> ") [" <> foldr (\x r -> debugPrintType x <> ", " <> r) "" tys <> "]"
debugPrintType (ForAllTy _ ty) = "(ForAllTy " <> debugPrintType ty <> ")"
debugPrintType (FunTy _ ty1 ty2) = "(FunTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
debugPrintType (LitTy litTy) = "(LitTy " <> showS litTy <> ")"
debugPrintType _ = ""
