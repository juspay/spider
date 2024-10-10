{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sheriff.Plugin (plugin) where

-- Sheriff imports
import Sheriff.Types
import Sheriff.Rules
import Sheriff.Utils
import Sheriff.Patterns

-- GHC imports
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (writeFile, appendFile)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Data
import Data.Function (on)
import Data.List (nub, sortBy, groupBy, find, isInfixOf, isSuffixOf, isPrefixOf)
import Data.List.Extra (splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Yaml
import Debug.Trace (traceShowId, trace)
import GHC hiding (exprType)
import Prelude hiding (id, writeFile, appendFile)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)

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

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = sheriff
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

--------------------------- Core Logic ---------------------------

{-

Stage - 1 SETUP 
  1. Parse the following -
    1.1 plugin options
    1.2 Rules yaml file
    1.3 Exceptions yaml file
    1.4 DB indexed keys file
  2. Filter out rules based on module level exceptions
  3. Separate out individual rule level exception rules

Stage - 2 EXECUTION
  1. Repeat steps 2 to 4 for all function binds
  2. Extract all `LHsExpr` type i.e. all expressions
  3. Perform some simplifications
  4. For each rule, check if that rule is applicable or not. If applicable call, corresponding validation function.
  5. Validation function will return violation found along with other info required

Stage - 3 ERRORS AND IO
  1. Convert raw error information to high level error 
  2. Sort & group errors on basis of src_span
  3. Filter out rules for rule level exceptions -- If current rule in the error group has any exception rule coinciding with any other rule in the error group, then eliminate current rule
  4. Filter out rules for global level exceptions -- if any rule in the error group is part of globalExceptions, then eliminate the group
  5. Throw filtered errors, if configured
  6. Write errors to file, if configured

-}


sheriff :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
sheriff opts modSummary tcEnv = do
  -- STAGE-1
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      pluginOpts@PluginOpts{..} = case opts of
                                    []      -> defaultPluginOpts
                                    (x : _) -> fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)

  -- parse the yaml file from the path given
  parsedYaml <- liftIO $ parseYAMLFile indexedKeysPath

  -- parse the yaml file from the path given for sheriff general rules
  parsedRulesYaml <- liftIO $ parseYAMLFile rulesConfigPath

  -- parse the yaml file from the path given for sheriff general exception rules
  parsedExceptionsYaml <- liftIO $ parseYAMLFile exceptionsConfigPath

  -- Check the parsed yaml file for indexedDbKeys and generate DB rules. If failed, throw file error if configured.
  dbRules <- case parsedYaml of
              Left err -> do
                when failOnFileNotFound $ addErr (mkInvalidYamlFileErr (show err))
                pure []
              Right (YamlTables tables) -> pure $ (map yamlToDbRule tables)
  
  -- Check the parsed rules yaml file.  If failed, throw file error if configured.
  configuredRules <- case parsedRulesYaml of
                Left err -> do
                  when failOnFileNotFound $ addErr (mkInvalidYamlFileErr (show err))
                  pure []
                Right (SheriffRules rules) -> pure rules

  -- Check the parsed exception rules yaml file.  If failed, throw file error if configured.
  configuredExceptionRules <- case parsedExceptionsYaml of
                                Left err -> do
                                  when failOnFileNotFound $ addErr (mkInvalidYamlFileErr (show err))
                                  pure []
                                Right (SheriffRules exceptionRules) -> pure exceptionRules
  
  let rawGlobalRules          = defaultSheriffRules <> dbRules <> configuredRules
      globalRules             = filter (isAllowedOnCurrentModule moduleName') rawGlobalRules
      rawExceptionRules       = defaultSheriffExceptionsRules <> configuredExceptionRules 
      globalExceptionRules    = filter (isAllowedOnCurrentModule moduleName') rawExceptionRules 
      ruleLevelExceptionRules = concat $ fmap getRuleExceptions globalRules
      finalSheriffRules       = nub $ globalRules <> globalExceptionRules <> ruleLevelExceptionRules

  when logDebugInfo $ liftIO $ print globalRules
  when logDebugInfo $ liftIO $ print globalExceptionRules

  -- STAGE-2
  rawErrors <- concat <$> (mapM (loopOverModBinds finalSheriffRules pluginOpts) $ bagToList $ tcg_binds tcEnv)

  -- STAGE-3
  errors <- mapM (mkCompileError moduleName') rawErrors

  let sortedErrors = sortBy (leftmost_smallest `on` src_span) errors
      groupedErrors = groupBy (\a b -> src_span a == src_span b) sortedErrors
      filteredErrorsForRuleLevelExceptions = fmap (\x -> let errorRulesInCurrentGroup = fmap getRuleFromCompileError x in filter (\err -> not $ (getRuleExceptionsFromCompileError err) `hasAny` errorRulesInCurrentGroup) x) groupedErrors
      filteredErrorsForGlobalExceptions = concat $ filter (\x -> not $ (\err -> (getRuleFromCompileError err) `elem` globalExceptionRules) `any` x) filteredErrorsForRuleLevelExceptions
      filteredErrors = nub $ filter (\x -> getRuleFromCompileError x `elem` globalRules) filteredErrorsForGlobalExceptions -- Filter errors to take only rules since we might have some individual rule level errors in this list

  if throwCompilationError
    then addErrs $ map mkGhcCompileError filteredErrors
    else pure ()

  if saveToFile
    then addErrToFile modSummary savePath filteredErrors
    else pure ()

  return tcEnv

-- Loop over top level function binds
loopOverModBinds :: Rules -> PluginOpts -> LHsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
loopOverModBinds rules opts (L _ ap@(FunBind{})) = do
  -- liftIO $ print "FunBinds" >> showOutputable ap
  badCalls <- getBadFnCalls rules opts ap
  pure badCalls
loopOverModBinds _ _ (L _ ap@(PatBind{})) = do
  -- liftIO $ print "PatBinds" >> showOutputable ap
  pure []
loopOverModBinds _ _ (L _ ap@(VarBind{})) = do 
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
getBadFnCalls rules opts (FunBind{fun_matches = matches}) = do
  let funMatches = map unLoc $ unLoc $ mg_alts matches
  concat <$> mapM getBadFnCallsHelper funMatches
  where
    getBadFnCallsHelper :: Match GhcTc (LHsExpr GhcTc) -> TcM [(LHsExpr GhcTc, Violation)]
    getBadFnCallsHelper match = do
      let whereBinds = traverseAst (grhssLocalBinds $ m_grhss match) :: [LHsBinds GhcTc]
          normalBinds = traverseAst (grhssGRHSs $ m_grhss match) :: [LHsBinds GhcTc]
          argBinds = m_pats match
          -- exprs = match ^? biplateRef :: [LHsExpr GhcTc]
          -- use childrenBi and then repeated children usage as per use case
          -- (exprs :: [LHsExpr GhcTc]) = traverseConditionalUni (noWhereClauseExpansion) (childrenBi match :: [LHsExpr GhcTc])
          (exprs :: [LHsExpr GhcTc]) = traverseAstConditionally match noWhereClauseExpansion
      concat <$> mapM (isBadExpr rules opts) exprs
getBadFnCalls _ _ _ = pure []

-- Do not expand sequelize `where` clause further
noWhereClauseExpansion :: LHsExpr GhcTc -> Bool
noWhereClauseExpansion expr = case expr of
  (L loc (PatHsWrap _ expr)) -> noWhereClauseExpansion (L loc expr)
  (L _ (PatExplicitList (TyConApp ty _) _)) -> showS ty == "Clause"
  _ -> False

-- Takes a function name which should not be expanded further while traversing AST
noGivenFunctionCallExpansion :: String -> LHsExpr GhcTc -> Bool
noGivenFunctionCallExpansion fnName expr = case expr of
  (L loc (PatHsWrap _ expr)) -> noGivenFunctionCallExpansion fnName (L loc expr)
  _ -> case getFnNameWithAllArgs expr of
        Just (lVar, _) -> matchNamesWithModuleName (getLocatedVarNameWithModuleName lVar) fnName AsteriskInSecond -- (getOccString . varName . unLoc $ lVar) == fnName
        Nothing -> False

-- Simplifies few things and handles some final transformations
isBadExpr :: Rules -> PluginOpts -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadExpr rules opts ap@(L _ (HsVar _ v)) = isBadExprHelper rules opts ap
isBadExpr rules opts ap@(L _ (HsApp _ funl funr)) = isBadExprHelper rules opts ap
isBadExpr rules opts ap@(L _ (PatExplicitList _ _)) = isBadExprHelper rules opts ap
isBadExpr rules opts ap@(L loc (PatHsWrap _ expr)) = isBadExpr rules opts (L loc expr) >>= mapM (\(x, y) -> trfViolationErrorInfo opts y ap x >>= \z -> pure (x, z))
isBadExpr rules opts ap@(L loc (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadExpr rules opts (L loc (HsApp noExtFieldOrAnn lfun rfun)) >>= mapM (\(x, y) -> trfViolationErrorInfo opts y ap x >>= \z -> pure (x, z))
    _ -> isBadExprHelper rules opts ap
#if __GLASGOW_HASKELL__ >= 900
isBadExpr rules opts ap@(L loc (PatHsExpansion orig expanded)) = do
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> isBadExpr rules opts (L loc (HsApp noExtFieldOrAnn funl funr)) >>= mapM (\(x, y) -> trfViolationErrorInfo opts y ap x >>= \z -> pure (x, z))
      _ -> isBadExpr rules opts (L loc expanded)
    _ -> isBadExpr rules opts (L loc expanded)
#endif
isBadExpr rules opts ap = pure []

-- Calls checkAndApplyRule, can be used to directly call without simplifier if needed
isBadExprHelper :: Rules -> PluginOpts -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadExprHelper rules opts ap = concat <$> mapM (\rule -> checkAndApplyRule rule opts ap) rules

-- Check if a particular rule applies to given expr
checkAndApplyRule :: Rule -> PluginOpts -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
checkAndApplyRule ruleT opts ap = case ruleT of
  DBRuleT rule@(DBRule _ ruleTableName _ _ _) ->
    case ap of
      (L _ (PatExplicitList (TyConApp ty [_, tblName]) exprs)) -> do
        case (showS ty == "Clause" && showS tblName == (ruleTableName <> "T")) of
          True  -> validateDBRule rule opts (showS tblName) exprs ap
          False -> pure []
      _ -> pure []
  FunctionRuleT rule@(FunctionRule _ ruleFnNames arg_no _ _ _ _ _ _ _ _) -> do
    let res = getFnNameWithAllArgs ap
    case res of
      Nothing                   -> pure []
      Just (fnLocatedVar, args) -> do
        let fnName    = getLocatedVarNameWithModuleName fnLocatedVar
            fnLHsExpr = mkLHsVar fnLocatedVar
        case (find (\ruleFnName -> matchNamesWithModuleName fnName ruleFnName AsteriskInSecond && length args >= arg_no) ruleFnNames) of
          Just ruleFnName  -> validateFunctionRule rule opts ruleFnName fnName fnLHsExpr args ap 
          Nothing -> pure []
  GeneralRuleT rule -> pure [] --TODO: Add handling of general rule

--------------------------- Function Rule Validation Logic ---------------------------
{-

Part-1 Checking Applicability
  1. Get function name and arguments list
  2. Check if function name matches with required name
  3. Check if function has more than or equal number of arguments than required as per rule

Part-2 Validation
  1. Check if argument number in rule is 0, then the use of function is not allowed in code.
  2. Extract the required argument as per rule from the argument list.
  3. Get the type of required argument
  4. Check if argument type is in the blocked types list as per rule, then the use of this argument type is not allowed
  5. Check if argument type is in the to_be_checked types list as per rule, then check for function type blocked
    5.1 Extract the list of all function application inside the argument
    5.2 For each function application, get the function name and list of arguments
    5.3 For each function name match, if the type of required argument is not in exception list, then it is a Function Blocked in argument violation.

-}

-- Function to check if given function rule is violated or not
validateFunctionRule :: FunctionRule -> PluginOpts -> String -> String -> LHsExpr GhcTc -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateFunctionRule rule opts ruleFnName fnName fnNameExpr args expr = do
  if arg_no rule == 0 && fn_sigs_blocked rule == [] -- considering arg 0 as the case for blocking the whole function occurence
    then pure [(fnNameExpr, FnUseBlocked ruleFnName rule)]
  else if arg_no rule == 0
    then do
      -- Check argument types for functions with polymorphic signature
      argTyps <- concat <$> mapM (\arg -> getHsExprTypeAsTypeDataList <$> getHsExprTypeWithResolver (logTypeDebugging opts) arg) args
      fnReturnType <- getHsExprTypeAsTypeDataList <$> getHsExprTypeWithResolver (logTypeDebugging opts) expr
      let fnSigFromArg = argTyps <> fnReturnType

      -- Given function signature
      fnExprTyp <- getHsExprTypeWithResolver (logTypeDebugging opts) fnNameExpr
      let fnSigTypList = getHsExprTypeAsTypeDataList fnExprTyp

      pure . concat $ fmap (\ruleFnSig -> if matchFnSignatures fnSigTypList ruleFnSig || matchFnSignatures fnSigFromArg ruleFnSig then [(fnNameExpr, FnSigBlocked fnName ruleFnSig rule)] else []) (fn_sigs_blocked rule)
  else do
    let matches = drop ((arg_no rule) - 1) args
    if length matches == 0
      then pure []
    else do
      let arg = head matches
      argTypeGhc <- getHsExprTypeWithResolver (logTypeDebugging opts) arg
      let argType = showS argTypeGhc
          argTypeBlocked = validateType argTypeGhc $ types_blocked_in_arg rule
          isArgTypeToCheck = validateType argTypeGhc $ types_to_check_in_arg rule

      when (logDebugInfo opts && fnName /= "NA") $
        liftIO $ do
          print $ (fnName, map showS args)
          print $ (fnName, showS arg)
          print $ fn_rule_name rule
          print $ "Arg Type = " <> argType

      if argTypeBlocked
        then do
          exprType <- getHsExprTypeWithResolver (logTypeDebugging opts) expr
          pure [(expr, ArgTypeBlocked argType (showS exprType) ruleFnName rule)]
      else if isArgTypeToCheck
        then do
          blockedFnsList <- getBlockedFnsList opts arg rule -- check if the expression has any stringification function
          mapM (\(lExpr, blockedFnName, blockedFnArgTyp) -> mkFnBlockedInArgErrorInfo opts expr lExpr >>= \errorInfo -> pure (lExpr, FnBlockedInArg (blockedFnName, blockedFnArgTyp) ruleFnName errorInfo rule)) blockedFnsList
      else pure []

-- Helper to validate types based on custom types present in the rules -- tuples, list, maybe
validateType :: Type -> TypesToCheckInArg -> Bool
validateType argTyp@(TyConApp tyCon ls) typs = 
  let tyConStr = showS tyCon in
  if tyConStr `elem` typs
    then case tyConStr of
      "(,)"   -> (\t -> validateType t typs) `any` ls 
      "[]"    -> (\t -> validateType t typs) `any` ls 
      "Maybe" -> (\t -> validateType t typs) `any` ls 
      _       -> showS argTyp `elem` typs
  else showS argTyp `elem` typs
validateType argTyp typs = showS argTyp `elem` typs

-- Get List of blocked functions used inside a HsExpr; Uses `getBlockedFnsList` 
getBlockedFnsList :: PluginOpts -> LHsExpr GhcTc -> FunctionRule -> TcM [(LHsExpr GhcTc, String, String)] 
getBlockedFnsList opts arg rule@(FunctionRule _ _ arg_no _ fnsBlocked _ _ _ _ _ _) = do
  let argHsExprs = traverseAst arg :: [LHsExpr GhcTc]
      fnApps = filter isFunApp argHsExprs
  when (logDebugInfo opts) $ liftIO $ do
    print "getBlockedFnsList"
    showOutputable arg
    showOutputable fnApps
  catMaybes <$> mapM checkFnBlockedInArg fnApps 
  where 
    checkFnBlockedInArg :: LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, String, String))
    checkFnBlockedInArg expr = do
      let res = getFnNameWithAllArgs expr
      when (logDebugInfo opts) $ liftIO $ do
        print "checkFnBlockedInArg"
        showOutputable res
      case res of
        Nothing -> pure Nothing
        Just (fnNameVar, args) -> isPresentInBlockedFnList expr fnsBlocked (getLocatedVarNameWithModuleName fnNameVar) args
    
    isPresentInBlockedFnList :: LHsExpr GhcTc -> FnsBlockedInArg -> String -> [LHsExpr GhcTc] -> TcM (Maybe (LHsExpr GhcTc, String, String))
    isPresentInBlockedFnList expr [] _ _ = pure Nothing
    isPresentInBlockedFnList expr ((ruleFnName, ruleArgNo, ruleAllowedTypes) : ls) fnName fnArgs = do
      when (logDebugInfo opts) $ liftIO $ do
        print "isPresentInBlockedFnList"
        print (ruleFnName, ruleArgNo, ruleAllowedTypes)
      case matchNamesWithModuleName fnName ruleFnName AsteriskInSecond && length fnArgs >= ruleArgNo of
        False -> isPresentInBlockedFnList expr ls fnName fnArgs
        True  -> do
          let reqArg = head $ drop (ruleArgNo - 1) fnArgs
          argType <- getHsExprType (logTypeDebugging opts) reqArg
          when (logDebugInfo opts) $ liftIO $ do
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

--------------------------- DB Rule Validation Logic ---------------------------
{-

Part-1 Checking Applicability
  1. Check if the expression is an explicit list, basically a hard coded list
  2. Extract the type of the explicit list and from it further extract actual type (i.e. `Clause`) and table name
  3. Check if extracted table name matches the given table name

Part-2 Validation
  1. Simplify the given list as SOP form (OR of AND)
    1.1 For each non breakable clause (i.e. `Se.Is`), get the field name and table name
      1.1.1 Get DB field specifier, i.e. how field is being extracted - Lens, Selector, RecordDot
      1.1.2 Extract data according to the way we have field written
    1.2 For each OR, cross product the result of current list and remaining result, creating individual lists for each clause in current list
    1.3 For each AND, cross product the list of current list and remaining result, simplifying each element of the current list
  2. Check whether we want to match only 1st column in composite key or all columns of composite key
  3. For each AND clause in SOP, perform 4 or 5, if all AND clause are indexed, then only this query is indexed
  4. Case when we want to check only 1st column of composite key
    3.1 If any of the field in AND clause does not violate the rule, then we mark this AND clause as Indexed
    3.2 For each field of AND clause, perform following for all rule keys,:
      3.2.1 If it is a composite key, we check if the 1st column of composite key matches the current field, then consider this current field as indexed
      3.2.2 If it is non-composite key, then we directly compare and if it matches, then consider this current field as indexed
  5. Case when we want to check all columns of composite key
    5.1 If any of the field in AND clause violate the rule, then we mark this AND clause as Non-Indexed
    5.2 For each field of AND clause, perform following for all rule keys,:
      3.2.1 If it is a composite key, and if it current column of composite key and it matches the current field of AND, then consider this current field as indexed
      3.2.2 If it is a composite key, and if it does not match, then if it is present in overall AND clause fields, then we skip this and check next composite key column. 
      3.2.2 If it is non-composite key, then we directly compare and if it matches, then consider this current field as indexed
  
-}
-- Function to check if given DB rules is violated or not
-- TODO: Fix this, keep two separate options for - 1. Match All Fields in AND   2. Use 1st column matching or all columns matching for composite key 
validateDBRule :: DBRule -> PluginOpts -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateDBRule rule@(DBRule ruleName ruleTableName ruleColNames _ _) opts tableName clauses expr = do
  simplifiedExprs <- trfWhereToSOP opts clauses
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
    _                       -> doesMatchColNameInDbRule colName keys

type SimplifiedIsClause = (LHsExpr GhcTc, String, String)

-- Simplify the complex `where` clause of SQL queries as OR queries at top (i.e. ((C1 and C2 and C3) OR (C1 AND C5) OR (C6)))
trfWhereToSOP :: PluginOpts -> [LHsExpr GhcTc] -> TcM [[SimplifiedIsClause]]
trfWhereToSOP _ [] = pure [[]]
trfWhereToSOP opts (clause : ls) = do
  let res = getWhereClauseFnNameWithAllArgs clause
      (fnName, args) = fromMaybe ("NA", []) res
  case (fnName, args) of
    ("And", [(L _ (PatExplicitList _ arg))]) -> do
      curr <- trfWhereToSOP opts arg
      rem  <- trfWhereToSOP opts ls
      pure [x <> y | x <- curr, y <- rem]
    ("Or", [(L _ (PatExplicitList _ arg))]) -> do
      curr <- foldM (\r cls -> fmap (<> r) $ trfWhereToSOP opts [cls]) [] arg
      rem  <- trfWhereToSOP opts ls
      pure [x <> y | x <- curr, y <- rem]
    ("$WIs", [arg1, arg2]) -> do
      curr <- getIsClauseData opts arg1 arg2 clause
      rem  <- trfWhereToSOP opts ls
      case curr of
        Nothing -> pure rem
        Just (tblName, colName) -> pure $ fmap (\lst -> (clause, tblName, colName) : lst) rem
    (fn, _) -> when (logWarnInfo opts) (liftIO $ print $ "Invalid/unknown clause in `where` clause : " <> fn <> " at " <> (showS . getLoc2 $ clause)) >> trfWhereToSOP opts ls

-- Get table field name and table name for the `Se.Is` clause
-- Patterns to match 'getField`, `recordDot`, `overloadedRecordDot` (ghc > 9), selector (duplicate record fields), rec fields (ghc 9), lens
-- TODO: Refactor this to use HasField instance if possible
getIsClauseData :: PluginOpts -> LHsExpr GhcTc -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM (Maybe (String, String))
getIsClauseData opts fieldArg _comp _clause = do
  let fieldSpecType = getDBFieldSpecType fieldArg
  mbColNameAndTableName <- case fieldSpecType of
    None     -> when (logWarnInfo opts) (liftIO $ print $ "Can't identify the way in which DB field is specified: " <> showS fieldArg) >> pure Nothing
    Selector -> do
      let modFieldArg arg = case arg of
                              (L _ (HsRecFld _ fldOcc))   -> showS $ selectorAmbiguousFieldOcc fldOcc
                              (L loc (PatHsWrap _ wExpr)) -> modFieldArg (L loc wExpr)
                              (L _ expr)                  -> showS expr
      case (splitOn ":" $ modFieldArg fieldArg) of
        ("$sel" : colName : tableName : []) -> pure $ Just (colName, tableName)
        _ -> when (logWarnInfo opts) (liftIO $ print "Invalid pattern for Selector way") >> pure Nothing
    RecordDot -> do
      let tyApps = filter (\x -> case x of 
                                  (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> True
                                  (PatHsWrap (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableVar))) (HsAppType _ _ fldName)) -> True
                                  _ -> False
                          ) $ (traverseAst fieldArg :: [HsExpr GhcTc])
      if length tyApps > 0 
        then 
          case head tyApps of
            (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> do
              typ <- getHsExprType (logTypeDebugging opts) tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (getStrFromHsWildCardBndrs fldName, take (length tblName' - 1) tblName')
            (PatHsWrap (WpCompose (WpEvApp (EvExpr _hasFld)) (WpCompose (WpTyApp _fldType) (WpTyApp tableType))) (HsAppType _ _ fldName)) ->
              let tblName' = case tableType of
                                  AppTy ty1 _    -> showS ty1
                                  TyConApp ty1 _ -> showS ty1
                                  ty             -> showS ty
              in pure $ Just (getStrFromHsWildCardBndrs fldName, take (length tblName' - 1) tblName')
            _ -> when (logWarnInfo opts) (liftIO $ putStrLn "HsAppType not present. Should never be the case as we already filtered.") >> pure Nothing
        else when (logWarnInfo opts) (liftIO $ putStrLn "HsAppType not present after filtering. Should never reach as already deduced RecordDot.") >> pure Nothing
    Lens -> do
      let opApps = filter isLensOpApp (traverseAst fieldArg :: [HsExpr GhcTc])
      case opApps of
        [] -> when (logWarnInfo opts) (liftIO $ putStrLn "No lens operator application present in lens case.") >> pure Nothing
        (opExpr : _) -> do
          case opExpr of
            (OpApp _ tableVar _ fldVar) -> do
              let fldName = tail $ showS fldVar
              typ <- getHsExprType (logTypeDebugging opts) tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (fldName, take (length tblName' - 1) tblName')
            (SectionR _ _ (L _ lens)) -> do
              let tys = traverseAst lens :: [Type]
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
#if __GLASGOW_HASKELL__ >= 900
            (PatHsExpansion orig (HsApp _ (L _ (HsApp _ _ tableVar)) fldVar)) -> do
              let fldName = tail $ showS fldVar
              typ <- getHsExprType (logTypeDebugging opts) tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (fldName, take (length tblName' - 1) tblName')
#endif              
            _ -> when (logWarnInfo opts) (liftIO $ putStrLn "OpApp not present. Should never be the case as we already filtered.") >> pure Nothing
  
  pure mbColNameAndTableName

-- Get how DB field is being extracted in sequelize
getDBFieldSpecType :: LHsExpr GhcTc -> DBFieldSpecType
getDBFieldSpecType (L loc expr)
  | (PatHsWrap _ wExpr) <- expr = getDBFieldSpecType (L loc wExpr)
  | (HsRecFld _ fldOcc) <- expr = checkExprString . showS $ selectorAmbiguousFieldOcc fldOcc
  | otherwise                   = checkExprString $ showS expr
  where
    checkExprString exprStr
      | isPrefixOf "$sel" exprStr       = Selector
      | isInfixOf "^." exprStr          = Lens
      | (\x -> isInfixOf "@" x) exprStr = RecordDot
      | otherwise                       = None

-- Get function name for the where clause for db rules cases
getWhereClauseFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (String, [LHsExpr GhcTc])
getWhereClauseFnNameWithAllArgs (L _ (HsVar _ v)) = Just (getVarName $ unLoc v, [])
getWhereClauseFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (getVarName clId, [])) <$> conLikeWrapId cl
getWhereClauseFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (getVarName $ unLoc v, [funr])
getWhereClauseFnNameWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getWhereClauseFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getWhereClauseFnNameWithAllArgs (L loc (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> getWhereClauseFnNameWithAllArgs $ (L loc (HsApp noExtFieldOrAnn lfun rfun))
    _ -> Nothing
getWhereClauseFnNameWithAllArgs (L loc ap@(HsPar _ expr)) = getWhereClauseFnNameWithAllArgs expr
-- If condition inside the list, add dummy type
getWhereClauseFnNameWithAllArgs (L loc ap@(PatHsIf _pred thenCl elseCl)) = Just ("Or", [L loc (PatExplicitList (LitTy (StrTyLit "Dummy")) [thenCl, elseCl])])
getWhereClauseFnNameWithAllArgs (L loc ap@(PatHsWrap _ expr)) = getWhereClauseFnNameWithAllArgs (L loc expr)
#if __GLASGOW_HASKELL__ >= 900
getWhereClauseFnNameWithAllArgs (L loc ap@(PatHsExpansion orig expanded)) = 
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> getWhereClauseFnNameWithAllArgs (L loc (HsApp noExtFieldOrAnn funl funr))
      _ -> getWhereClauseFnNameWithAllArgs (L loc expanded)
    _ -> getWhereClauseFnNameWithAllArgs (L loc expanded)
#endif
getWhereClauseFnNameWithAllArgs (L loc ap@(ExprWithTySig _ expr _)) = getWhereClauseFnNameWithAllArgs expr
getWhereClauseFnNameWithAllArgs _ = Nothing

-- TODO: Verify the correctness of this function before moving it to utils
-- Get function name with all it's arguments
getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L loc (HsVar _ v)) = Just (getLocated v loc, [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (noExprLoc clId, [])) <$> conLikeWrapId cl
getFnNameWithAllArgs (L _ (HsAppType _ expr _)) = getFnNameWithAllArgs expr
getFnNameWithAllArgs (L _ (HsApp _ (L loc (HsVar _ v)) funr)) = Just (getLocated v loc, [funr])
getFnNameWithAllArgs (L _ (HsPar _ expr)) = getFnNameWithAllArgs expr
getFnNameWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs (L loc (OpApp _ funl op funr)) = do
  case showS op of
    "($)" -> getFnNameWithAllArgs $ (L loc (HsApp noExtFieldOrAnn funl funr))
    _ -> Nothing
getFnNameWithAllArgs (L loc ap@(PatHsWrap _ expr)) = getFnNameWithAllArgs (L loc expr)
#if __GLASGOW_HASKELL__ >= 900
getFnNameWithAllArgs (L loc ap@(PatHsExpansion orig expanded)) = 
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> getFnNameWithAllArgs (L loc (HsApp noExtFieldOrAnn funl funr))
      _ -> getFnNameWithAllArgs (L loc expanded)
    _ -> getFnNameWithAllArgs (L loc expanded)
#endif
getFnNameWithAllArgs _ = Nothing

--------------------------- Sheriff Plugin Utils ---------------------------
-- Transform the FnBlockedInArg Violation with correct expression 
trfViolationErrorInfo :: PluginOpts -> Violation -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM Violation
trfViolationErrorInfo opts violation@(FnBlockedInArg p1 ruleFnName _ rule) outsideExpr insideExpr = do
  errorInfo <- mkFnBlockedInArgErrorInfo opts outsideExpr insideExpr
  pure $ FnBlockedInArg p1 ruleFnName errorInfo rule
trfViolationErrorInfo _ violation _ _ = pure violation

-- Create Error Info for FnBlockedInArg Violation
mkFnBlockedInArgErrorInfo :: PluginOpts -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM Value
mkFnBlockedInArgErrorInfo opts lOutsideExpr@(L _ outsideExpr) lInsideExpr@(L _ insideExpr) = do
  let loc1 = getLoc2 lOutsideExpr
      loc2 = getLoc2 lInsideExpr
  filePath <- unpackFS . srcSpanFile . tcg_top_loc . env_gbl <$> getEnv
  let overall_src_span = showS loc1
      overall_err_line_orig = showS lOutsideExpr
      err_fn_src_span = showS loc2
      err_fn_err_line_orig = showS lInsideExpr
  overall_err_line <- 
    if useIOForSourceCode opts
      then liftIO $ extractSrcSpanSegment loc1 filePath overall_err_line_orig
      else pure overall_err_line_orig
  err_fn_err_line <- 
    if useIOForSourceCode opts
      then liftIO $ extractSrcSpanSegment loc2 filePath err_fn_err_line_orig
      else pure err_fn_err_line_orig
  pure $ A.object [
      ("overall_src_span", A.toJSON overall_src_span),
      ("overall_err_line", A.toJSON overall_err_line),
      ("err_fn_src_span", A.toJSON err_fn_src_span),
      ("err_fn_err_line", A.toJSON err_fn_err_line)
    ]

-- Check if a rule is allowed on current module
isAllowedOnCurrentModule :: String -> Rule -> Bool
isAllowedOnCurrentModule moduleName rule = 
  let ignoredModules = getRuleIgnoreModules rule
      allowedModules = getRuleCheckModules rule
      isCurrentModuleAllowed = any (matchNamesWithAsterisk AsteriskInBoth moduleName) allowedModules
      isCurrentModuleIgnored = any (matchNamesWithAsterisk AsteriskInBoth moduleName) ignoredModules
  in isCurrentModuleAllowed && not isCurrentModuleIgnored

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

-- Create invalid yaml file compilation error
mkInvalidYamlFileErr :: String -> OP.SDoc
mkInvalidYamlFileErr err = OP.text err

-- Create Internal Representation of Logging Error
mkCompileError :: String -> (LHsExpr GhcTc, Violation) -> TcM CompileError
mkCompileError modName (expr, violation) = pure $ CompileError "" modName (show violation) (getLoc2 expr) violation (getViolationSuggestions violation) (getErrorInfoFromViolation violation)
    
-- Add GHC error to a file
addErrToFile :: ModSummary -> String -> [CompileError] -> TcM ()
addErrToFile modSummary path errs = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      res = encodePretty errs
  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile (path <> moduleName' <> "_compilationErrors.json") res

-- Check if HsExpr is Function Application
isFunApp :: LHsExpr GhcTc -> Bool
isFunApp (L _ (HsApp _ _ _)) = True
isFunApp (L _ (OpApp _ funl op funr)) = True
isFunApp (L loc (PatHsWrap _ expr)) = isFunApp (L loc expr)
#if __GLASGOW_HASKELL__ >= 900
isFunApp (L _ (PatHsExpansion orig expanded)) = 
  case orig of
    (OpApp{}) -> True
    _ -> False
#endif
isFunApp _ = False

-- Check if HsExpr is Lens operator application
isLensOpApp :: HsExpr GhcTc -> Bool
isLensOpApp (OpApp _ _ op _) = showS op == "(^.)"
isLensOpApp (SectionR _ op _) = showS op == "(^.)"
#if __GLASGOW_HASKELL__ >= 900
isLensOpApp (PatHsExpansion (OpApp _ _ op _) expanded) = showS op == "(^.)"
#endif
isLensOpApp _ = False

-- Get Var for the data constructor
conLikeWrapId :: ConLike -> Maybe Var
conLikeWrapId (RealDataCon dc) = Just (dataConWrapId dc)
conLikeWrapId _ = Nothing

-- If the type is literal type, get the string name of the literal, else return the showS verison of the type
getStrFromHsWildCardBndrs :: HsWildCardBndrs (NoGhcTc GhcTc) (LHsType (NoGhcTc GhcTc)) -> String
getStrFromHsWildCardBndrs (HsWC _ (L _ (HsTyLit _ (HsStrTy _ fs)))) = unpackFS fs
getStrFromHsWildCardBndrs typ = showS typ

-- -------------------------------- DEPRECATED CODE (Might be useful for some other use cases or some other plugin) -------------------------------- --
-- [DEPRECATED] Get Return type of the function application arg
getArgTypeWrapper :: LHsExpr GhcTc -> [Type]
getArgTypeWrapper expr@(L _ (HsApp _ lfun rfun)) = getArgType expr True
getArgTypeWrapper expr@(L _ (OpApp _ lfun op rfun)) = 
  case showS op of
    "($)" -> getArgType lfun True
    "(.)" -> getArgTypeWrapper lfun
    "(<>)" -> getArgTypeWrapper lfun
    _ -> getArgType op True
getArgTypeWrapper (L loc (PatHsWrap _ expr)) = getArgTypeWrapper (L loc expr)
getArgTypeWrapper (L loc (HsPar _ expr)) = getArgTypeWrapper expr
getArgTypeWrapper expr = getArgType expr False

-- [DEPRECATED] Get LHsExpr type
getArgType :: LHsExpr GhcTc -> Bool -> [Type]
getArgType (L _ (HsLit _ v)) _ = getLitType v
getArgType (L _ (HsOverLit _ (OverLit (OverLitTc _ typ) v _))) _ = [typ]
getArgType (L loc (PatHsWrap _ expr)) shouldReturnFinalType = getArgType (L loc expr) shouldReturnFinalType
getArgType (L loc (HsApp _ lfun rfun)) shouldReturnFinalType = getArgType lfun shouldReturnFinalType
getArgType arg shouldReturnFinalType = 
  let vars = filter (not . isSystemName . varName) $ traverseAst arg in 
  if length vars == 0
    then []
  else
    let tys = idType $ head vars 
        (foralls, constraints, actualTyp) = tcSplitNestedSigmaTys tys
        typeReturnFn = bool (\x -> [x]) getReturnType shouldReturnFinalType
        actualReturnTyp = (trfUsingConstraints constraints $ typeReturnFn actualTyp)
    in actualReturnTyp

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
#if __GLASGOW_HASKELL__ >= 900
    replacer replacements typ@(FunTy flag mult ty1 ty2) = FunTy flag mult (replacer replacements ty1) (replacer replacements ty2) 
#else
    replacer replacements typ@(FunTy flag ty1 ty2) = FunTy flag (replacer replacements ty1) (replacer replacements ty2) 
#endif
    replacer replacements typ = maybe typ snd $ (\x -> eqType (fst x) typ) `find` replacements
