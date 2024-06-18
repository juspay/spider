{-# LANGUAGE DataKinds #-}

module Sheriff.Plugin (plugin) where

import GHC.Data.Bag (bagToList,listToBag)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Data
import Control.Reference (biplateRef, (^?), Simple, Traversal)
import Data.Generics.Uniplate.Data (universeBi, childrenBi, contextsBi, holesBi, children)
import Data.List (nub)
import Debug.Trace (traceShowId)
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
    noLoc, moduleName, moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass, SrcSpan, Located,
    la2r,
    HsExpr (..),
    XXExprGhcTc(..),
    getLocA
  )
import GHC.Hs.Binds
import GHC.Plugins (isFunTy, funResultTy, splitAppTys, dropForAlls, idName,Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr,elemNameSet,pprPrefixName,idType,tidyOpenType, tcSplitTyConApp_maybe, eqType)
import GHC.Types.Name (nameStableString, isSystemName)
import GHC.Driver.Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile, appendFile)
import Data.Aeson as A
import Data.ByteString.Lazy (writeFile, appendFile)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe)
import Control.Exception (try,SomeException)
import GHC.Types.Annotations
import GHC.Utils.Outputable (showSDocUnsafe, ppr, Outputable(..))
import Control.Monad (foldM,when)
import Data.List
import Data.List.Extra (replace,splitOn)
import Data.Maybe (fromJust,isJust,mapMaybe)
import Sheriff.Types
import Sheriff.Rules
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent
import System.Directory
import GHC.Hs.Utils as GHCHs
import Data.Bool (bool)
import qualified Data.Map as Map
import qualified GHC.Utils.Outputable as OP
import Data.Maybe (catMaybes)
import GHC.Tc.Utils.Monad (failWith, addErr, addErrAt, addErrs)
import GHC (OverLitTc(..), HsOverLit(..))
import Control.Applicative ((<|>))
import Data.ByteString.Lazy as BSL ()
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import GHC.Hs.Lit (HsLit(..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Core.TyCo.Rep (Type(..), PredType)
import GHC.Tc.Types.Evidence (HsWrapper(..), EvTerm(..))
import GHC.Hs.Expr (HsWrap (..))
import GHC.Builtin.Types (stringTy, doubleTy, floatTy, intTy, wordTy, charTy)
import GHC.Tc.Utils.TcType (tcSplitAppTys, tcFunResultTy, tcSplitNestedSigmaTys)
import GHC.Data.FastString (unpackFS)
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.ConLike (ConLike(..))

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = logerr
    , pluginRecompile = purePlugin
    }

logDebugInfo :: Bool
logDebugInfo = False

logWarnInfo :: Bool
logWarnInfo = True

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

-- Parse the YAML file
parseYAMLFile :: FilePath -> IO (Either ParseException YamlTables)
parseYAMLFile file = decodeFileEither file

logerr :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
logerr opts modSummary tcEnv = do
  let pluginOpts = case opts of
                    []      -> defaultPluginOpts
                    (x : _) -> fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
    
      throwCompilationErrorV = throwCompilationError pluginOpts
      saveToFileV = saveToFile pluginOpts
      savePathV = savePath pluginOpts
      indexedKeysPathV = indexedKeysPath pluginOpts
      failOnFileNotFoundV = failOnFileNotFound pluginOpts 
      matchAllInsideAndV = matchAllInsideAnd pluginOpts

  -- parse the yaml file from the path given
  parsedYaml <- liftIO $ parseYAMLFile indexedKeysPathV

  -- Check the parsed yaml file for indexedDbKeys and throw compilation error if configured
  rulesList <- case parsedYaml of
                  Left err -> do
                    when failOnFileNotFoundV $ addErr (mkInvalidIndexedKeysFile (show err))
                    pure badPracticeRules
                  Right (YamlTables tables) -> pure $ badPracticeRules <> (map yamlToDbRule tables)

  errors <- concat <$> (mapM (loopOverModBinds rulesList) $ bagToList $ tcg_binds tcEnv)

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
loopOverModBinds :: Rules -> LHsBindLR GhcTc GhcTc -> TcM [CompileError]
loopOverModBinds rules (L _ ap@(FunBind _ id matches _)) = do
  -- liftIO $ print "FunBinds" >> showOutputable ap
  calls <- getBadFnCalls rules ap
  mapM mkCompileError calls
loopOverModBinds _ (L _ ap@(PatBind _ _ pat_rhs _)) = do
  -- liftIO $ print "PatBinds" >> showOutputable ap
  pure []
loopOverModBinds _ (L _ ap@(VarBind {var_rhs = rhs})) = do 
  -- liftIO $ print "VarBinds" >> showOutputable ap
  pure []
loopOverModBinds rules (L _ ap@(AbsBinds {abs_binds = binds})) = do
  -- liftIO $ print "AbsBinds" >> showOutputable ap
  list <- mapM (loopOverModBinds rules) $ bagToList binds
  pure (concat list)
loopOverModBinds _ _ = pure []

-- Get all the FunApps inside the top level function bind
-- This call can be anywhere in `where` clause or `regular` RHS
getBadFnCalls :: Rules -> HsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
getBadFnCalls rules (FunBind _ id matches _) = do
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
      concat <$> mapM (isBadFunApp rules) exprs
getBadFnCalls _ _ = pure []

-- Takes a predicate which return true if further expansion is not required, false otherwise
traverseConditionalUni :: (Data a) => (a -> Bool) -> [a] -> [a]
traverseConditionalUni _ [] = []
traverseConditionalUni p (x : xs) = 
  if p x 
    then x : traverseConditionalUni p xs
    else (x : traverseConditionalUni p (children x)) <> traverseConditionalUni p xs

noGivenFunctionCallExpansion :: String -> LHsExpr GhcTc -> Bool
noGivenFunctionCallExpansion fnName expr = case expr of
  (L loc (XExpr (WrapExpr (HsWrap _ expr)))) -> noWhereClauseExpansion (L loc expr)
  _ -> case getFnNameWithAllArgs expr of
        Just (lVar, _) -> (getOccString . varName . unLoc $ lVar) == fnName
        Nothing -> False

noWhereClauseExpansion :: LHsExpr GhcTc -> Bool
noWhereClauseExpansion expr = case expr of
  (L loc (XExpr (WrapExpr (HsWrap _ expr)))) -> noWhereClauseExpansion (L loc expr)
  (L _ (ExplicitList (TyConApp ty _) _)) -> showS ty == "Clause"
  _ -> False

isBadFunApp :: Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunApp rules ap@(L _ (HsVar _ v)) = isBadFunAppHelper rules ap
isBadFunApp rules ap@(L _ (HsApp _ funl funr)) = isBadFunAppHelper rules ap
isBadFunApp rules ap@(L loc (XExpr (WrapExpr (HsWrap _ expr)))) = isBadFunApp rules (L loc expr)
isBadFunApp rules ap@(L _ (ExplicitList _ _)) = isBadFunAppHelper rules ap
isBadFunApp rules (L _ (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadFunAppHelper rules $ mkHsApp lfun rfun
    _ -> pure []
isBadFunApp _ _ = pure []

isBadFunAppHelper :: Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunAppHelper rules ap = catMaybes <$> mapM (\rule -> checkAndApplyRule rule ap) rules

validateFunctionRule :: FunctionRule -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
validateFunctionRule rule fnName args expr = do
  if (arg_no rule) == 0 -- considering arg 0 as the case for blocking the whole function occurence
    then pure $ Just (expr, FnUseBlocked rule)
  else do
    let matches = drop ((arg_no rule) - 1) args
    if length matches == 0
      then pure Nothing
    else do
      let arg = head matches
          argTypes = (map showS $ getArgTypeWrapper arg)
          argTypeBlocked = fromMaybe "NA" $ (`elem` types_blocked_in_arg rule) `find` argTypes
          isArgTypeToCheck = (`elem` types_to_check_in_arg rule) `any` argTypes 

      when (logDebugInfo && fnName /= "NA") $
        liftIO $ do
          print $ (fnName, map showS args)
          print $ (fnName, showS arg)
          print $ rule
          putStr "Arg Types = "
          let vars = filter (not . isSystemName . varName) $ arg ^? biplateRef
              tys = map (showS . idType) vars
          print $ map showS vars
          print $ tys
          print $ argTypes

      if argTypeBlocked /= "NA"
        then pure $ Just (expr, ArgTypeBlocked argTypeBlocked rule)
      else if not isArgTypeToCheck
        then pure Nothing
      else do
        -- It's a rule function with to_be_checked type argument
        -- stringificationFns1 <- getStringificationFns arg -- check if the expression has any stringification function
        let blockedFnsList = getBlockedFnsList arg rule -- check if the expression has any stringification function
            vars = filter (not . isSystemName . varName) $ arg ^? biplateRef
            tys = map (map showS . (getReturnType . dropForAlls . idType)) vars
        if length blockedFnsList > 0
          then do
            pure $ Just (expr, FnBlockedInArg (head blockedFnsList) rule)
          else pure Nothing

validateDBRule :: DBRule -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
validateDBRule rule@(DBRule ruleName ruleTableName ruleColNames) tableName clauses expr = do
  -- let checkerFn = if True then all else any -- TODO: Make it user input based decision
  processClauses clauses rule

processClauses :: [LHsExpr GhcTc] -> DBRule -> TcM (Maybe (LHsExpr GhcTc, Violation))
processClauses [] _               = pure Nothing
processClauses (clause : ls) rule = do
  let res = getFnNameWithAllArgs clause
      (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
  case (fnName, args) of
    ("And", [(L _ (ExplicitList _ arg))]) -> processAndClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("Or", [(L _ (ExplicitList _ arg))])  -> processOrClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("$WIs", [arg1, arg2])                  -> processIsClause arg1 arg2 clause rule >>= (maybe (processClauses ls rule) (pure . Just))
    _      -> when logWarnInfo (liftIO $ print "Invalid clause in `where` clause") >> processClauses ls rule

processAndClause :: [LHsExpr GhcTc] -> DBRule -> TcM (Maybe (LHsExpr GhcTc, Violation))
processAndClause [] _               = pure Nothing
processAndClause (clause : ls) rule = do
  let res = getFnNameWithAllArgs clause
      (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
  case (fnName, args) of
    ("And", [(L _ (ExplicitList _ arg))]) -> processAndClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("Or", [(L _ (ExplicitList _ arg))])  -> processOrClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("$WIs", [arg1, arg2])                  -> processIsClause arg1 arg2 clause rule >>= (maybe (processClauses ls rule) (pure . Just))
    _      -> when logWarnInfo (liftIO $ print "Invalid clause in `And` clause") >> processClauses ls rule

processOrClause :: [LHsExpr GhcTc] -> DBRule -> TcM (Maybe (LHsExpr GhcTc, Violation))
processOrClause [] _               = pure Nothing
processOrClause (clause : ls) rule = do
  let res = getFnNameWithAllArgs clause
      (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
  case (fnName, args) of
    ("And", [(L _ (ExplicitList _ arg))]) -> processAndClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("Or", [(L _ (ExplicitList _ arg))])  -> processOrClause arg rule >>= (maybe (processClauses ls rule) (pure . Just))
    ("$WIs", [arg1, arg2])                  -> processIsClause arg1 arg2 clause rule >>= (maybe (processClauses ls rule) (pure . Just))
    _      -> when logWarnInfo (liftIO $ print "Invalid clause in `Or` clause") >> processClauses ls rule

processIsClause :: LHsExpr GhcTc -> LHsExpr GhcTc -> LHsExpr GhcTc -> DBRule -> TcM (Maybe (LHsExpr GhcTc, Violation))
processIsClause fieldArg _ clause rule@(DBRule ruleName ruleTableName ruleColNamesComposite) = do
  let fieldSpecType = getDBFieldSpecType fieldArg
      ruleColNames = map head ruleColNamesComposite -- Unsafe, keeping it so that we need to change the yaml file
  mbColNameAndTableName <- case fieldSpecType of
    None     -> when logWarnInfo (liftIO $ print "Can't identify the way in which DB field is specified") >> pure Nothing
    Selector -> do
      case (splitOn ":" $ showS fieldArg) of
        ("$sel" : colName : tableName : []) -> pure $ Just (colName, tableName)
        _ -> when logWarnInfo (liftIO $ print "Invalid pattern for Selector way") >> pure Nothing
    RecordDot -> do
      let tyApps = filter (\x -> case x of 
                                  (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> True
                                  (XExpr (WrapExpr (HsWrap (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableVar))) (HsAppType _ _ fldName)))) -> True
                                  _ -> False
                          ) $ (fieldArg ^? biplateRef :: [HsExpr GhcTc])
      if length tyApps > 0 
        then 
          case head tyApps of
            (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> do
              let tys = getArgTypeWrapper tableVar
              if length tys >= 1
                then 
                  let tblName' = case head tys of
                                  AppTy ty1 _    -> showS ty1
                                  TyConApp ty1 _ -> showS ty1
                                  ty             -> showS ty
                  in pure $ Just (getStrFromHsWildCardBndrs fldName, take (length tblName' - 1) tblName')
                else pure Nothing
            (XExpr (WrapExpr (HsWrap (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableType))) (HsAppType _ _ fldName)))) ->
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
                  tys = getArgTypeWrapper tableVar
              if length tys >= 1
                then 
                  let tblName' = case head tys of
                                  AppTy ty1 _    -> showS ty1
                                  TyConApp ty1 _ -> showS ty1
                                  ty             -> showS ty
                  in pure $ Just (fldName, take (length tblName' - 1) tblName')
                else pure Nothing
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
  
  when logDebugInfo $ liftIO $ putStrLn $ "DBRule - " <> show mbColNameAndTableName

  case mbColNameAndTableName of
    Nothing -> pure Nothing
    Just (colName, tableName) -> 
      if tableName == ruleTableName && colName `notElem` ruleColNames
        then pure $ Just (clause, NonIndexedDBColumn colName tableName rule)
        else pure Nothing

checkAndApplyRule :: Rule -> LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
checkAndApplyRule ruleT ap = case ruleT of
  DBRuleT rule@(DBRule _ ruleTableName _) -> 
    case ap of
      (L _ (ExplicitList (TyConApp ty [_, tblName]) exprs)) -> case (showS ty == "Clause" && showS tblName == (ruleTableName <> "T")) of
        True  -> validateDBRule rule (showS tblName) exprs ap 
        False -> pure Nothing
      _ -> pure Nothing
  FunctionRuleT rule@(FunctionRule _ ruleFnName arg_no _ _ _) -> do
    let res = getFnNameWithAllArgs ap
    -- let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((nameStableString . varName . unLoc) x, y)) $ res
        (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
    case (fnName == ruleFnName && length args >= arg_no) of
      True  -> validateFunctionRule rule fnName args ap 
      False -> pure Nothing 

getDBFieldSpecType :: LHsExpr GhcTc -> DBFieldSpecType
getDBFieldSpecType (L _ expr)
  | isPrefixOf "$sel" (showS expr) = Selector
  | isInfixOf "^." (showS expr) = Lens
  | (\x -> isInfixOf "@" x) (showS expr) = RecordDot
  | otherwise = None

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v)) = Just (L (getLocA v) (unLoc v), [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (noLoc clId, [])) <$> conLikeWrapId cl
getFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (L (getLocA v) (unLoc v), [funr])
getFnNameWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
-- getFnNameWithAllArgs (L _ (OpApp _ funl op funr)) = do
--   let res = getFnNameWithAllArgs funl
--   case res of
--     Nothing -> Nothing
--     Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs (L loc ap@(XExpr (WrapExpr (HsWrap _ expr)))) = do
  getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs _ = Nothing

conLikeWrapId :: ConLike -> Maybe Var
conLikeWrapId (RealDataCon dc) = Just (dataConWrapId dc)
conLikeWrapId _ = Nothing
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

-- Pretty print the Internal Representations
showOutputable :: (MonadIO m, Outputable a) => a -> m ()
showOutputable = liftIO . putStrLn . showSDocUnsafe . ppr

-- Create GHC compilation error from CompileError
mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ err_msg err)

-- Create invalid indexedKeys file compilation error
mkInvalidIndexedKeysFile :: String -> OP.SDoc
mkInvalidIndexedKeysFile err = OP.text err

-- Create Internal Representation of Logging Error
mkCompileError :: (LHsExpr GhcTc, Violation) -> TcM CompileError
mkCompileError (expr, violation) = pure $ CompileError "" "" (show violation) ( (getLocA (expr))) violation

-- Get Return type of the function application arg
getArgTypeWrapper :: LHsExpr GhcTc -> [Type]
getArgTypeWrapper expr@(L _ (HsApp _ lfun rfun)) = getArgType expr True
getArgTypeWrapper expr@(L _ (OpApp _ lfun op rfun)) = 
  case showS op of
    "($)" -> getArgType lfun True
    "(.)" -> getArgTypeWrapper lfun
    "(<>)" -> getArgTypeWrapper lfun
    _ -> getArgType op True
getArgTypeWrapper (L loc (XExpr (WrapExpr (HsWrap _ expr)))) = getArgTypeWrapper (L loc expr)
getArgTypeWrapper (L loc (HsPar _ expr)) = getArgTypeWrapper expr
getArgTypeWrapper expr = getArgType expr False

getArgType :: LHsExpr GhcTc -> Bool -> [Type]
getArgType (L _ (HsLit _ v)) _ = getLitType v
getArgType (L _ (HsOverLit _ (OverLit (OverLitTc _ typ) v _))) _ = [typ]
getArgType (L loc (XExpr (WrapExpr (HsWrap _ expr)))) shouldReturnFinalType = getArgType (L loc expr) shouldReturnFinalType
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

-- Get final return type of any type/function signature
getReturnType :: Type -> [Type]
getReturnType typ 
  | isFunTy typ = getReturnType $ tcFunResultTy typ
  | otherwise = let (x, y) = tcSplitAppTys typ in x : y

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
                              else Nothing
      _ -> Nothing

    replacer :: [(Type, Type)] -> Type -> Type
    replacer replacements typ@(AppTy ty1 ty2) = AppTy (replacer replacements ty1) (replacer replacements ty2) 
    replacer replacements typ@(TyConApp tyCon typOrKinds) = TyConApp tyCon $ map (replacer replacements) typOrKinds
    replacer replacements typ@(ForAllTy bndrs typ') = ForAllTy bndrs (replacer replacements typ')
    replacer replacements typ@(FunTy flag mult ty1 ty2) = FunTy flag mult (replacer replacements ty1) (replacer replacements ty2) 
    replacer replacements typ = maybe typ snd $ (\x -> eqType (fst x) typ) `find` replacements 

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
getStringificationFns (L loc ap@(XExpr (WrapExpr (HsWrap _ expr)))) = do
  liftIO $ putStrLn "Inside HsWrap" >> putStrLn (showS ap) 
  getStringificationFns (L loc expr)
getStringificationFns _ = do
  liftIO $ putStrLn $ "Inside _"
  pure []

-- Get List of stringification functions used inside a HsExpr; Uses `stringifierFns` 
getStringificationFns2 :: LHsExpr GhcTc -> FunctionRule -> [String] 
getStringificationFns2 arg rule =
  let vars = arg ^? biplateRef :: [Var]
      blockedFns = fns_blocked_in_arg rule
  in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns)) $ takeWhile isFunVar $ filter (not . isSystemName . varName) vars

-- Get List of blocked functions used inside a HsExpr; Uses `getBlockedFnsList` 
getBlockedFnsList :: LHsExpr GhcTc -> FunctionRule -> [String] 
getBlockedFnsList arg rule =
  let vars = arg ^? biplateRef :: [Var]
      blockedFns = fns_blocked_in_arg rule
  in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns) && (not . isSystemName . varName) x) vars

addErrToFile :: ModSummary -> String -> [CompileError] -> TcM ()
addErrToFile modSummary path errs = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      -- res = (encodePretty moduleName') <> (fromString ": ") <> encodePretty errs <> (fromString ",")
      res = encodePretty errs
  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile (path <> moduleName' <> "_compilationErrors.json") res