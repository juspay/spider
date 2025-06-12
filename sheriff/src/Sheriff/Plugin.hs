{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sheriff.Plugin (plugin) where

-- Sheriff imports
import Control.Exception (IOException, catch) 
import Sheriff.CommonTypes
import Sheriff.Patterns
import Sheriff.Rules
import Sheriff.Types
import Sheriff.TypesUtils
import Sheriff.Utils
import Text.Regex.TDFA ((=~))
-- GHC imports
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State
import Control.Reference (biplateRef, (^?))
import Data.Aeson as A 
import qualified Data.Aeson.KeyMap as KM
import Data.Function (on, id)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (writeFile, appendFile, readFile)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Data
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortBy, groupBy, find, isInfixOf, isSuffixOf, isPrefixOf)
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Typeable (typeOf)
import Data.Yaml hiding (decode)
import qualified Data.Text as T
import Debug.Trace (traceShowId, trace, traceM)
import GHC hiding (exprType)
import GHC.Types.TypeEnv (typeEnvElts)
import Prelude hiding (id, writeFile, appendFile, readFile)
import Text.Show.Pretty (ppShow) 
import Data.Data (Data, toConstr, gmapQ)
import Data.Generics (everything, mkQ)
import Language.Haskell.Exts (parseFile, prettyPrint, ParseResult(..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Class
import GHC.Core.ConLike
import GHC.Core.InstEnv
import GHC.Core.TyCo.Rep
import GHC.Data.Bag
import GHC.HsToCore.Monad
import GHC.Hs.Expr (HsExpr(..))
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
import Class
import ConLike
import DsExpr
import DsMonad
import GhcPlugins hiding ((<>), getHscEnv, purePlugin)
import InstEnv
import qualified Outputable as OP
import TcEvidence
import TcRnMonad
import TcRnTypes
import TcType
import TyCoRep
#endif

-- type VarBindingMap = Map.Map String (LHsExpr GhcTc)

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
  6. Detect infinite recursion errors

Stage - 3 ERRORS AND IO
  1. Convert raw error information to high level error 
  2. Sort & group errors on basis of src_span
  3. Filter out rules for rule level exceptions -- If current rule in the error group has any exception rule coinciding with any other rule in the error group, then eliminate current rule
  4. Filter out rules for global level exceptions -- if any rule in the error group is part of globalExceptions, then eliminate the group
  6. Throw errors, if configured
  7. Write errors to file, if configured

-}

{-
  TODO: 
    1. Generalize the custom state monad and run things with context available to all functions
    2. Reuse the same type for implicit param and state (for interusability)
    3. Add helper functions to set in implicit params from state
    4. Change module name matching to direct variable matching by means of transforming to top most level
-}

type SheriffTcM = StateT (HM.HashMap NameModuleValue NameModuleValue) TcM

sheriff :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
sheriff opts modSummary tcEnv = do
  
  -- STAGE-1
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      pluginOpts@PluginOpts{..} = decodeAndUpdateOpts opts defaultPluginOpts

  let ?pluginOpts = PluginCommonOpts moduleName' HM.empty pluginOpts

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
      isInfiniteRecursionRule r = case r of
                                    (InfiniteRecursionRuleT rule) -> True
                                    _ -> False
      infRule = case find isInfiniteRecursionRule globalRules of
                  Just (InfiniteRecursionRuleT r) -> r
                  _ -> defaultInfiniteRecursionRuleT

  when logDebugInfo $ liftIO $ print globalRules
  when logDebugInfo $ liftIO $ print globalExceptionRules

  -- STAGE-2
  -- Get Instance declarations and add class name to module name binding in initial state
  insts <- tcg_insts . env_gbl <$> getEnv
  let namesModTuple = concatMap (\inst -> let clsName = className (is_cls inst) in (is_dfun_name inst, clsName) : fmap (\clsMethod -> (varName clsMethod, clsName)) (classMethods $ is_cls inst)) insts
      nameModMap = foldr (\(name, clsName) r -> HM.insert (NMV_Name name) (NMV_ClassModule clsName (getModuleName clsName)) r) HM.empty namesModTuple
  -- let binds = bagToList $ tcg_binds tcEnv
  -- liftIO $ putStrLn ("AST of binds:\n" ++ ppShow binds)
  -- liftIO $ putStrLn ("Extracted bind names: " ++ OP.showSDocUnsafe (OP.ppr binds))

  -- let mymAction = do
  --       results <- forM binds extractExprFromBind
  --       return (results)
  
  -- let (extractedAll, finalState) = runMyM mymAction

  -- let jsonFilePath = "/Users/sailaja.b/spider/tree.json"

  -- Read the JSON file
  -- jsonData <- liftIO (Char8.readFile jsonFilePath `catch` handleReadError)
  -- let parsedData = eitherDecode jsonData :: Either String FunctionMap

  -- case parsedData of
  --     Right functionMap -> do
  --       let filteredEntries = filterEntries functionMap
  --       forM_ (HM.toList filteredEntries) $ \(key, functionEntry) -> do
  --         let codeContent = code functionEntry
  --         -- let codeContent = "res <- findOneRow\n                dbConf meshConfig\n      $          [Is DB.filterGroupId (Eq groupId),\n                 Is DB.dimensionValue (Eq dimension)]\n       toDomainAll res parseTenantConfigFilter\n         ! #function_name \"getTenantConfigFilterByGroupIdAndDimensionValue\"\n         ! #parser_name \"parseTenantConfigFilter\"" :: String
          
  --         -- let regex1 = "findOneRow" :: String
  --         -- let regex2 = "findOneRow[[:space:]]+([^[:space:]]+)" :: String
  --         -- let regex3 = "findOneRow[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+)" :: String
  --         -- let regex4 = "findOneRow[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+)[[:space:]]+\\[([[:print:][:space:]]*)\\]" :: String
  --         let regexFlexible = "(findOneRow|findAllRows)[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+)[[:space:]]+.*[[:space:]]*\\[([[:print:][:space:]]*)\\]" :: String
  --         -- Test each regex pattern
  --         -- let matches1 = codeContent =~ regex1 :: [[String]]
  --         -- let matches2 = codeContent =~ regex2 :: [[String]]
  --         -- let matches3 = codeContent =~ regex3 :: [[String]]
  --         -- let matches4 = codeContent =~ regex4 :: [[String]]
  --         let matches5 = codeContent =~ regexFlexible :: [[String]]
          
  --         -- Print results
  --         -- liftIO $ putStrLn $ "Matches with Regex 1: " ++ show matches1
  --         -- liftIO $ putStrLn $ "Matches with Regex 2: " ++ show matches2
  --         -- liftIO $ putStrLn $ "Matches with Regex 3: " ++ show matches3
  --         -- liftIO $ putStrLn $ "Matches with Regex 4: " ++ show matches4
  --         let firstElements = map (\innerList -> case innerList of
  --                                                 (a:_) -> a
  --                                                 [] -> "No match") matches5
  --         liftIO $ putStrLn $ "Function Name: " ++ key ++ " ,First elements: " ++ show firstElements
    
  --     Left err -> liftIO $ putStrLn $ "Failed to parse JSON: " ++ err
    
  -- Example: Extract specific data from the FunctionMap
  -- let extractedData = fromMaybe HM.empty (decode jsonData :: Maybe FunctionMap)
  -- liftIO $ putStrLn $ "Extracted data: " ++ show extractedData


  -- liftIO $ putStrLn $ "📍 Final state: " ++ show finalState
  -- liftIO $ putStrLn $ "📌 Extracted expressions: " ++ OP.showSDocUnsafe (OP.ppr extractedAll)

  rawErrors <- concat <$> (mapM (loopOverModBinds finalSheriffRules) $ bagToList $ tcg_binds tcEnv)
  (rawInfiniteRecursionErrors, _) <- flip runStateT nameModMap $ concat <$> (mapM (checkInfiniteRecursion True infRule) $ bagToList $ tcg_binds tcEnv)
  
  -- STAGE-3
  errors <- mapM (mkCompileError moduleName') (rawErrors <> rawInfiniteRecursionErrors)

  let sortedErrors = sortBy (leftmost_smallest `on` src_span) errors
      groupedErrors = groupBy (\a b -> src_span a == src_span b) sortedErrors
      filteredErrorsForRuleLevelExceptions = fmap (\x -> let errorRulesInCurrentGroup = fmap getRuleFromCompileError x in filter (\err -> not $ (getRuleExceptionsFromCompileError err) `hasAny` errorRulesInCurrentGroup) x) groupedErrors
      filteredErrorsForGlobalExceptions = concat $ filter (\x -> not $ (\err -> (getRuleFromCompileError err) `elem` globalExceptionRules) `any` x) filteredErrorsForRuleLevelExceptions
      filteredErrors = nub $ filter (\x -> getRuleFromCompileError x `elem` (InfiniteRecursionRuleT infRule : globalRules)) filteredErrorsForGlobalExceptions -- Filter errors to take only rules since we might have some individual rule level errors in this list

  if throwCompilationError
    then addErrs $ map mkGhcCompileError filteredErrors
    else pure ()

  if saveToFile
    then addErrToFile modSummary savePath filteredErrors
    else pure ()

  return tcEnv

handleReadError :: IOException -> IO Char8.ByteString
handleReadError e = do
  putStrLn $ "c " ++ show e
  return Char8.empty

-- filterEntries :: HM.HashMap String FunctionEntry -> HM.HashMap String FunctionEntry
-- filterEntries functionMap =
--   HM.filter containsFindFunctions functionMap
--   where
--     containsFindFunctions :: FunctionEntry -> Bool
--     containsFindFunctions entry =
--       case toJSON entry of
--         Object obj ->  -- Ensure the JSON value is an Object
--           case KM.lookup "functions_it_is_calling" obj of
--             Just (Array functions) ->
--               any (\func -> func == "findOneRow" || func == "findAllRows") functions
--             _ -> False
--         _ -> False  -- Handle cases where the JSON value is not an Object

type TableMap = Map.Map String String
type ClauseMap = Map.Map String String


data MyState = MyState
  { tableMap  :: TableMap
  , clauseMap :: ClauseMap
  } deriving (Show)

type MyM = State MyState

runMyM :: MyM a -> (a, MyState)
runMyM action = runState action initialMyState

initialMyState :: MyState
initialMyState = MyState Map.empty Map.empty

extractExprFromBind :: LHsBindLR GhcTc GhcTc -> MyM (Maybe (String, String, String))
extractExprFromBind (L loc bind) = do
  traceM "extractExprFromBind called"
  myMap <- get
  case bind of
    FunBind { fun_matches = MG { mg_alts = L _ matches } } -> do
      traceM "Matched FunBind"
      results <- forM matches $ \(L _ match) -> case match of
        Match { m_grhss = GRHSs _ grhss _ } -> do
          innerResults <- forM grhss $ \(L _ grhs) -> case grhs of
            GRHS _ _ body -> do
              let exprs = body ^? biplateRef :: [LHsExpr GhcTc]
              extracted <- mapM (\expr -> extractQueryInfo expr (L loc bind)) exprs
              traceM $ "body from FunBind: " ++ OP.showSDocUnsafe (OP.ppr body) ++ "exprs from FunBind: " ++ OP.showSDocUnsafe (OP.ppr exprs) ++ "\n📌 Query Info:\n" ++ unlines (map showTriple (catMaybes extracted))
              pure (catMaybes extracted)
          pure (concat innerResults)
      pure $ listToMaybe (concat results)

    PatBind { pat_rhs = GRHSs _ grhss _ } -> do
      traceM "Matched PatBind" 
      results <- forM grhss $ \(L _ grhs) -> case grhs of
        GRHS _ _ body -> do
          let exprs = body ^? biplateRef :: [LHsExpr GhcTc]
          extracted <- mapM (\expr -> extractQueryInfo expr (L loc bind)) exprs
          traceM $ "body from FunBind: " ++ OP.showSDocUnsafe (OP.ppr body) ++ "exprs from PatBind: " ++ OP.showSDocUnsafe (OP.ppr exprs) ++ "\n📌 Query Info:\n" ++ unlines (map showTriple (catMaybes extracted))
          pure (catMaybes extracted)
      pure $ listToMaybe (concat results)

    AbsBinds { abs_binds = binds } -> do
      traceM "Matched AbsBinds"
      results <- mapM extractExprFromBind (bagToList binds)
      pure $ listToMaybe (catMaybes results)

    _ -> do
      traceM "No matching bind constructor (not FunBind or PatBind)"
      pure Nothing

extractQueryInfo :: LHsExpr GhcTc -> LHsBindLR GhcTc GhcTc -> MyM (Maybe (String, String, String))
extractQueryInfo expr bindings = do
  traceM ("🔍 Called extractQueryInfo with expr: " ++ OP.showSDocUnsafe (OP.ppr expr))
  (fn, args) <- flattenHsAppM expr
  let fnName = OP.showSDocUnsafe (OP.ppr fn)
  if fnName `elem` ["findOneRow", "findAllRows"] && length args == 3
    then do
      let clause = OP.showSDocUnsafe (OP.ppr (args !! 2))
      let typeStr = show (typeOf (unLoc (args !! 2)))
      let unlocatedBindings = [unLoc bindings]
      let isWhereClause = hasIsOrEmptyList (args !! 2)
      clauseMapVal <- gets clauseMap
      let whereClause = if isWhereClause
                        then clause
                        else fromMaybe "<unknown_clause" (Map.lookup clause clauseMapVal)
      traceM ("clausee: " ++ clause ++ " :: whereClause: " ++ show whereClause)
      tableNameMap <- gets tableMap
      let key = OP.showSDocUnsafe (OP.ppr (args !! 0))
          tableName = fromMaybe "<unknown_table" (Map.lookup key tableNameMap)
      traceM ("✅ Matched query function.\nFunction: " ++ fnName ++ "\nTable Name: " ++ tableName ++ "\nClause: " ++ whereClause ++ "\n tableNameMap: " ++ show tableNameMap ++ "\n clauseMapVal: " ++ show clauseMapVal)
      pure $ Just (fnName, tableName, whereClause)
    else do
      traceM ("⚠️ Skipping: fnName = " ++ fnName ++ ", args = " ++ show (length args))
      pure Nothing

hasIsOrEmptyList :: LHsExpr GhcTc -> Bool
hasIsOrEmptyList expr =
  trace "📍 Entering hasIsOrEmptyList" $
  let expr' = stripExpr expr
      core = unLoc expr'
  in trace ("🔍 unLoc expr': " ++ showSDocUnsafe (ppr core) ++ " , Tyoe of expr :" ++ show (typeOf expr)) $
     case core of
       ExplicitList _ [] -> trace "Matched: Empty list" True
       ExplicitList _ xs -> trace ("Checking list of length " ++ show (length xs)) $ any hasIsOrEmptyList xs

       HsApp _ fun arg ->
         let funStr = showSDocUnsafe (ppr (unLoc fun))
             matches = any (`isPrefixOf` funStr) ["Is", "And", "Or"]
             isListCons = funStr == ":" || funStr == "[]"
         in trace ("Matched: HsApp, head string: " ++ funStr ++ ", matches? " ++ show matches ++ ", isListCons? " ++ show isListCons) $
              matches || isListCons || hasIsOrEmptyList fun || hasIsOrEmptyList arg
       OpApp _ fun op arg ->
         let funStr = showSDocUnsafe (ppr (unLoc fun))
             opStr  = showSDocUnsafe (ppr (unLoc op))
             argStr = showSDocUnsafe (ppr (unLoc arg))
             matches = any (`isPrefixOf` funStr) ["Is", "And", "Or"]
             isListCons = funStr == ":" || funStr == "[]"
         in trace ("Matched: OpApp, fun: " ++ funStr ++ ", op: " ++ opStr ++ ", arg: " ++ argStr ++
                   ", matches? " ++ show matches ++ ", isListCons? " ++ show isListCons) $
              matches || isListCons || hasIsOrEmptyList fun || hasIsOrEmptyList op || hasIsOrEmptyList arg

       HsPar _ inner -> trace "Matched: HsPar" $ hasIsOrEmptyList inner

       XExpr (WrapExpr innerExpr) ->
         case innerExpr of
           HsWrap _ exprInner ->
             trace ("Matched: XExpr WrapExpr with location " ++ showSDocUnsafe (ppr exprInner)) $
               hasIsOrEmptyList (noLocA exprInner)

       XExpr _ -> trace "XExpr case: cannot handle yet" False

       other -> trace ("🛑 we came to other case: " ++ showSDocUnsafe (ppr other)) False
-- unwrapExpr :: LHsExpr GhcTc -> LHsExpr GhcTc
-- unwrapExpr lexpr =
--   case unLoc lexpr of
--     HsWrap _ inner -> noLocA inner -- inner :: HsExpr GhcTc
--     other          -> lexpr

-- unwrapExprFromHsExpr :: HsExpr GhcTc -> HsExpr GhcTc
-- unwrapExprFromHsExpr (HsWrap _ inner) = unwrapExprFromHsExpr inner
-- unwrapExprFromHsExpr expr             = expr



flattenHsAppM :: LHsExpr GhcTc -> MyM (HsExpr GhcTc, [LHsExpr GhcTc])
flattenHsAppM expr = do
  traceM $ "🧾 Received expr: " ++ OP.showSDocUnsafe (OP.ppr expr)
  let doStmts = case expr of
        L _ (HsDo _ _ (L _ stmts)) -> stmts
        _                          -> []

  forM_ doStmts $ \stmt -> case stmt of
    L _ (BindStmt _ (L _ (VarPat _ (L _ varName))) rhsExpr) -> do   -- To get dbConfig (Table Name)
      traceM $ "📦 RHS Expr: " ++ OP.showSDocUnsafe (OP.ppr rhsExpr)
      let normalizedExpr = stripExpr rhsExpr
      case normalizedExpr of
        L _ (HsAppType _ (L _ (HsVar _ (L _ fnName))) (HsWC _ innerType))
          | occNameString (occName fnName) `elem` ["getEulerDbConf", "getEulerPsqlDbConf"] -> do
              let lhsVarStr = OP.showSDocUnsafe (OP.ppr varName)
                  typeStr = case innerType of
                              L _ (HsTyVar _ _ (L _ name)) -> occNameString (occName name)
                              _                            -> "unknown_type"
              traceM $ "📥 Inserting into map: " ++ lhsVarStr ++ " -> " ++ typeStr
              modify $ \s -> s { tableMap = Map.insert lhsVarStr typeStr (tableMap s) } -- Map Insertion (if dbcongig1 <- getEulerDbConf @orderReferenceT) , ["dbcongig1", "orderReferenceT"] get inserted
        _ -> pure ()
    -- To get where clause => 
    -- 1. Written in let statement => let whereclause = [Is..]
    -- 2. Directly Passed to select functions => findonerow tablename [Is..]
    -- 3. Passed as a parameter to the function => fun1 whereClause = do
    --                                             findonerow tableame whereClause
    -- 4. A local function or local value defined using a where clause  
    L _ (LetStmt _ localBindsL) ->  
      (case localBindsL of
        HsValBinds _ valBinds -> 
          case valBinds of
            XValBindsLR (NValBinds bindList _) -> do
              traceM "🔍 Processing LetStmt -> HsValBinds -> XValBindsLR"
              forM_ bindList $ \(recFlag, bindBag) -> do
                traceM $ "🌀 Processing bind list with RecFlag: " ++ showSDocUnsafe (ppr recFlag) ++ " ,bindBag: " ++ showSDocUnsafe (ppr bindBag)
                let binds = bagToList bindBag
                traceM $ "🔍 Number of binds in bindBag: " ++ show (length binds)
                if null binds
                  then traceM "⚠️ bindBag is empty – nothing to process"
                  else forM_ (bagToList bindBag) $ \(L _ bind) -> do
                    traceM $ "🔍 bind constructor: " ++ showConstr (toConstr bind)
                    case bind of
                         FunBind { fun_id = L _ varName
                                 , fun_matches = MG _ (L _ [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ [] body)] _) )]) _
                                 } -> do
                           traceM $ "📦 Found FunBind with var: " ++ showSDocUnsafe (ppr varName)
                           traceM $ "📦 Let RHS: " ++ showSDocUnsafe (ppr body)
                           let normalizedExpr = stripExpr body
                               varStr = showSDocUnsafe (ppr varName)
                           traceM $ "🧹 Normalized RHS: " ++ showSDocUnsafe (ppr normalizedExpr)
                           let result = hasIsOrEmptyList normalizedExpr
                           traceM $ "🧪 hasIsOrEmptyList result: " ++ show result
                           when result $ do 
                             traceM $ "✅ Match: RHS has 'is' or '[]', recording clause for: " ++ varStr
                             modify $ \s -> s { clauseMap = Map.insert varStr (showSDocUnsafe (ppr normalizedExpr)) (clauseMap s) }
                         _ -> traceM "⛔ Skipping non-PatBind or unhandled bind pattern"
            _ -> traceM "⚠️ valBinds is not XValBindsLR -> skipping"
        _ -> pure ())

    _ -> pure ()

  go expr []
  where
    go :: LHsExpr GhcTc -> [LHsExpr GhcTc] -> MyM (HsExpr GhcTc, [LHsExpr GhcTc])
    go (L _ (HsApp _ f x)) args = go f (x : args)
    go (L _ f) args = pure (f, args)


stripExpr :: LHsExpr GhcTc -> LHsExpr GhcTc
stripExpr (L l (HsPar _ e))               = stripExpr e
stripExpr (L l (HsAppType x e t))         = L l (HsAppType x (stripExpr e) t)
stripExpr (L l (XExpr (WrapExpr (HsWrap _ e)))) = stripExpr (L l e)
stripExpr other                           = other

showTriple :: (String, String, String) -> String
showTriple (a, b, c) = "(" ++ a ++ ", " ++ b ++ ", " ++ c ++ ")" 

{-

  1. Check if bind is AbsBind, add a mapping from mono to poly Var and recurse for binds
  2. Check if bind is VarBind, add mappings for child HsVar to VarId and update state
  3. Check if bind is FunBind, then get the function `var`
  4. Get all the match groups from the match (One match group is single definition for a function, a function may have multiple match groups)
  5. For each match group, perform below steps:
    5.1 Get the Pattern matches
    5.2 Transform pattern matches into common type `SimpleTcExpr`
    5.3 Append function name var to the beginning to complete the transformation
    5.4 Fetch all the HsExpr from the match group's guarded rhs (includes where clause)
    5.5 Filter out FunApp from all the HsExpr
    5.6 Simplify for ($) operator and transform to `SimpleTcExpr`
    5.7 Check if any of the `SimpleTcExpr` representation of HsExpr is same as `SimpleTcExpr` representation of pattern matches
    5.8 Fetch all the FunBinds from the guarded rhs
    5.9 Recur for each fun bind and repeat from step 1 

TODO: (Optimizations)
  1. Traverse a functions's body HsExpr once only and traverse the list/tree manually filtering based on location for local function binds
  2. Traverse AST manually, passing down all the required info as required
  3. Avoid duplicate recursion

TODO: (Extending Patterns)
  1. Match on VarBind. PatBind and other binds
  2. Support all the Pat and HsExpr types
  3. Check if infinite recursion possible in lambda function??
  4. Validate and handle SectionL and SectionR and partial functions and tuple sections
  5. Handle infinite lists traversals like map, fmap, etc. (represented by `ArithSeq`)
  6. Handle function renaming before enabling partial functions
-}


-- Function to check if the AST has deterministic infinite recursion
checkInfiniteRecursion :: (HasPluginOpts PluginOpts) => Bool -> InfiniteRecursionRule -> LHsBindLR GhcTc GhcTc -> SheriffTcM [(LHsExpr GhcTc, Violation)]
checkInfiniteRecursion recurseForBinds rule (L _ ap@(FunBind{fun_id = funVar, fun_matches = matches})) = do
  currNameModMap <- get
  let ?pluginOpts = ?pluginOpts {nameModuleMap = currNameModMap}
  errs <- mapM (checkAndVerifyAlt recurseForBinds rule funVar) (fmap unLoc . unLoc $ mg_alts matches)
  pure $ concat errs
checkInfiniteRecursion recurseForBinds rule (L _ ap@(AbsBinds{abs_binds = binds, abs_exports = bindVars})) = do
  let mbVar = case bindVars of
                x : _ -> Just $ (varName $ abe_poly x, varName $ abe_mono x)
                _ -> Nothing
  currNameModMap <- get
  let updatedNameModMap = maybe currNameModMap (\(poly, mono) -> HM.insert (NMV_Name mono) (NMV_Name poly) currNameModMap) mbVar
  put updatedNameModMap
  list <- mapM (\x -> checkInfiniteRecursion recurseForBinds rule x) $ bagToList binds
  pure $ concat list
checkInfiniteRecursion recurseForBinds rule (L loc ap@(VarBind{var_id = varId, var_rhs = rhs})) = do
  let currVarName = varName varId
      childHsVar = fmap varName (traverseAst rhs)
  currNameModMap <- get
  let updatedNameModMap = foldr (\childVar r -> HM.insert (NMV_Name childVar) (NMV_Name currVarName) r) currNameModMap childHsVar
  put updatedNameModMap
  pure []
checkInfiniteRecursion _ _ _ = pure []

-- Helper function to verify if any of the body HsExpr results in infinite recursion
checkAndVerifyAlt :: (HasPluginOpts PluginOpts) => Bool -> InfiniteRecursionRule -> LIdP GhcTc -> Match GhcTc (LHsExpr GhcTc) -> SheriffTcM [(LHsExpr GhcTc, Violation)]
checkAndVerifyAlt recurseForBinds rule ap@(L loc fnVar) match = do
  let currentFnNameWithModule = getVarNameWithModuleName fnVar
      ignoredFunctions = infinite_recursion_rule_ignore_functions rule
      skipCurrentFn = any (\ignoredFnName -> matchNamesWithModuleName currentFnNameWithModule ignoredFnName AsteriskInSecond) ignoredFunctions
  currentFnErrors <- case skipCurrentFn of
    True -> pure []
    False -> do
      let argsInFnDefn = m_pats match
          trfArgsInFnDefn = fmap (trfPatToSimpleTcExpr . unLoc) argsInFnDefn
      fnVarTyp <- lift $ getHsExprTypeWithResolver False (mkLHsVar $ getLocated ap loc)
      let finalTrfArgsInFnDefn = (SimpleFnNameVar fnVar fnVarTyp) : trfArgsInFnDefn
          argLenByTy = length (getHsExprTypeAsTypeDataList fnVarTyp) - 1
          argLenByFnDefn = length argsInFnDefn
          grhssList = grhssGRHSs (m_grhss match)
      when (logDebugInfo . pluginOpts $ ?pluginOpts) $ 
        liftIO $ do
          putStrLn (showS loc <> " :: " <> showS fnVar) >> putStrLn "***"
          print (getHsExprTypeAsTypeDataList fnVarTyp) >> putStrLn "***"
          putStrLn "******"
      concatMapM (checkGrhSS finalTrfArgsInFnDefn argLenByTy argLenByFnDefn) grhssList

  -- Process sub binds if further recursion allowed
  subBindsErrors <- 
    if recurseForBinds
      then do
        let (subBinds :: [LHsBindLR GhcTc GhcTc]) = traverseAst (m_grhss match)
        concat <$> mapM (checkInfiniteRecursion False rule) subBinds
      else pure []
  
  pure (currentFnErrors <> subBindsErrors)
  where
    checkGrhSS :: HasPluginOpts PluginOpts => [SimpleTcExpr] -> Int -> Int -> LGRHS GhcTc (LHsExpr GhcTc) -> SheriffTcM [(LHsExpr GhcTc, Violation)]
    checkGrhSS finalTrfArgsInFnDefn argLenByTy argLenByFnDefn grhss = do 
      let lenDiff = argLenByTy - argLenByFnDefn
          isPartialFn = lenDiff > 0 
      if isPartialFn then case getMaybeLambdaCaseOrLambdaMG grhss of
        Just mg -> do
          let matches = map unLoc . unLoc $ mg_alts mg
          flip concatMapM matches $ \match -> do
            let argsInFnDefn = m_pats match
                trfArgsInFnDefn = fmap (trfPatToSimpleTcExpr . unLoc) argsInFnDefn
                updatedFinalTrfArgsInFnDefn = finalTrfArgsInFnDefn <> trfArgsInFnDefn
                updatedArgLenByFnDefn = argLenByFnDefn + length argsInFnDefn
                grhssList = grhssGRHSs (m_grhss match)
            concatMapM (checkGrhSS updatedFinalTrfArgsInFnDefn argLenByTy updatedArgLenByFnDefn) grhssList
        Nothing -> processLHsExprInGrhs (getLastStmt grhss) finalTrfArgsInFnDefn
      else processLHsExprInGrhs (traverseAst grhss) finalTrfArgsInFnDefn
    
    processLHsExprInGrhs :: HasPluginOpts PluginOpts => [LHsExpr GhcTc] -> [SimpleTcExpr] -> SheriffTcM [(LHsExpr GhcTc, Violation)]
    processLHsExprInGrhs hsExprs finalTrfArgsInFnDefn = do
      let (funApps :: [LHsExpr GhcTc]) = filter (isFunApp True) hsExprs
          (simplifiedFnApps :: [(Located Var, (LHsExpr GhcTc, LHsExpr GhcTc, [LHsExpr GhcTc]))]) = HM.toList $ foldr (\x r -> maybe r (\(lVar, typ, args) -> HM.insertWith (\(x1, e1, newArgs) (x2, e2, oldArgs) -> if length newArgs >= length oldArgs then (x1, e1, newArgs) else (x2, e2, oldArgs)) lVar (x, typ, args) r) $ getFnNameAndTypeableExprWithAllArgs x) HM.empty funApps
      (trfSimplifiedFunApps :: [(LHsExpr GhcTc, [SimpleTcExpr])]) <- mapM trfSimplifiedFunApp simplifiedFnApps 
      let currentGrhssErrors = map (\(lhsExpr, _) -> (lhsExpr, InfiniteRecursionDetected rule)) $ filter (\x -> (snd x === finalTrfArgsInFnDefn)) trfSimplifiedFunApps 
      when (logDebugInfo . pluginOpts $ ?pluginOpts) $ 
        liftIO $ do
          let tyL = concat $ fmap (foldr (\x r -> case x of; SimpleFnNameVar v ty -> (v, ty) : r; _ -> r;) [] . snd) trfSimplifiedFunApps
          mapM (\(v, t) -> putStrLn $ showS v <> " ::: " <> show (getHsExprTypeAsTypeDataList t)) tyL
          putStrLn "******"
      pure currentGrhssErrors

    {-
    Assumption for infinite recursion in partial function case:
    1. Function composition , e.g. - fn a = fn1 x . fn2 y . fn a
    2. Let-in statement, and infinite recursion is inside `in` statement
    3. Do statement, infinite recursion can be in last statement
    4. Straight HsApp case or HsVar case

    TODO: 
    1. Correct the assumption and handle more possible cases
    -} 
    getLastStmt :: LGRHS GhcTc (LHsExpr GhcTc) -> [LHsExpr GhcTc]
    getLastStmt (L _ (GRHS _ _ lExpr)) = checkAndGetExpr lExpr
#if __GLASGOW_HASKELL__ < 900
    getLastStmt (L _ (XGRHS _)) = []
#endif

    checkAndGetExpr :: LHsExpr GhcTc -> [LHsExpr GhcTc]
    checkAndGetExpr (L loc expr) = case expr of
      HsLet _ _ inStmt -> checkAndGetExpr inStmt
      HsApp _ _ _ -> [L loc expr]
      HsVar _ _ -> [L loc expr]
      HsDo _ _ doStmts -> concatMap checkAndGetExpr $ foldr isLastStmt [] (traverseAst doStmts)
      PatHsWrap wrapper wrapExpr -> fmap (\(L _ trfExpr) -> L loc (PatHsWrap wrapper trfExpr)) $ checkAndGetExpr (L loc wrapExpr)
      OpApp _ lApp op rApp -> case showS op of
        "(.)" -> checkAndGetExpr rApp
        _ -> []
#if __GLASGOW_HASKELL__ >= 900
      PatHsExpansion (OpApp _ _ op _) (HsApp _ lApp rApp) -> case showS op of
        "(.)" -> checkAndGetExpr rApp
        _ -> []
#endif
      _ -> []

    isLastStmt :: ExprLStmt GhcTc -> [LHsExpr GhcTc] -> [LHsExpr GhcTc]
    isLastStmt (L _ (LastStmt _ lexpr _ _)) rem = lexpr : rem
    isLastStmt _                            rem = rem

    getMaybeLambdaCaseOrLambdaMG :: LGRHS GhcTc (LHsExpr GhcTc) -> Maybe (MatchGroup GhcTc (LHsExpr GhcTc))
    getMaybeLambdaCaseOrLambdaMG grhs = case grhs of
      (L _ (GRHS _ _ lExpr)) -> checkAndGetMaybeLambdaCaseOrLambdaMG lExpr
#if __GLASGOW_HASKELL__ < 900
      (L _ (XGRHS _))        -> Nothing
#endif

    checkAndGetMaybeLambdaCaseOrLambdaMG :: LHsExpr GhcTc -> Maybe (MatchGroup GhcTc (LHsExpr GhcTc))
    checkAndGetMaybeLambdaCaseOrLambdaMG (L loc expr) = case expr of
      (HsLamCase _ mg)   -> Just mg -- LambdaCase
      (HsLam _ mg)       -> Just mg -- Lambda Function
      (HsLet _ _ inStmt) -> checkAndGetMaybeLambdaCaseOrLambdaMG inStmt
      PatHsWrap _ wrapExpr -> checkAndGetMaybeLambdaCaseOrLambdaMG (L loc wrapExpr)
      OpApp _ _ op rApp -> case showS op of
        "(.)" -> checkAndGetMaybeLambdaCaseOrLambdaMG rApp
        _ -> Nothing
#if __GLASGOW_HASKELL__ >= 900
      PatHsExpansion (OpApp _ _ op _) (HsApp _ _ rApp) -> case showS op of
        "(.)" -> checkAndGetMaybeLambdaCaseOrLambdaMG rApp
        _ -> Nothing
#endif
      _ -> Nothing

    trfSimplifiedFunApp :: (Located Var, (LHsExpr GhcTc, LHsExpr GhcTc, [LHsExpr GhcTc])) -> SheriffTcM (LHsExpr GhcTc, [SimpleTcExpr])
    trfSimplifiedFunApp (lVar, (lHsExpr, typLHsExpr, lHsExprArgsLs)) = do
      typ <- lift $ dropForAlls <$> (getHsExprTypeWithResolver False) typLHsExpr
      pure (lHsExpr, SimpleFnNameVar (unLoc lVar) typ : (fmap trfLHsExprToSimpleTcExpr lHsExprArgsLs))

-- Loop over top level function binds
loopOverModBinds :: (HasPluginOpts PluginOpts) => Rules -> LHsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
loopOverModBinds rules (L _ ap@(FunBind{fun_id = (L _ funVar)})) = do
  -- liftIO $ print "FunBinds" >> showOutputable ap
  let currentFnNameWithModule = getVarNameWithModuleName funVar
      filteredRulesForFunction = filter (isAllowedOnCurrentFunction currentFnNameWithModule) rules
  badCalls <- getBadFnCalls filteredRulesForFunction ap
  pure badCalls
loopOverModBinds _ (L _ ap@(PatBind{})) = do
  -- liftIO $ print "PatBinds" >> showOutputable ap
  pure []
loopOverModBinds _ (L _ ap@(VarBind{})) = do 
  -- liftIO $ print "VarBinds" >> showOutputable ap
  pure []
loopOverModBinds rules (L _ ap@(AbsBinds {abs_binds = binds})) = do
  -- liftIO $ print "AbsBinds" >> showOutputable ap
  list <- mapM (loopOverModBinds rules) $ bagToList binds
  pure (concat list)
loopOverModBinds _ _ = pure []

-- Get all the FunApps inside the top level function bind
-- This call can be anywhere in `where` clause or `regular` RHS
getBadFnCalls :: (HasPluginOpts PluginOpts) => Rules -> HsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
getBadFnCalls rules (FunBind{fun_matches = matches}) = do
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
      concat <$> mapM (isBadExpr rules) exprs
getBadFnCalls _ _ = pure []

-- Do not expand sequelize `where` clause further
noWhereClauseExpansion :: LHsExpr GhcTc -> Bool
noWhereClauseExpansion expr = case expr of
  (L loc (PatHsWrap _ expr)) -> noWhereClauseExpansion (L loc expr)
  (L _ (PatExplicitList (TyConApp ty _) _)) -> showS ty == "Clause"
  _ -> False

-- Takes a function name which should not be expanded further while traversing AST
noGivenFunctionCallExpansion :: (HasPluginOpts a) => String -> LHsExpr GhcTc -> Bool
noGivenFunctionCallExpansion fnName expr = case expr of
  (L loc (PatHsWrap _ expr)) -> noGivenFunctionCallExpansion fnName (L loc expr)
  _ -> case getFnNameWithAllArgs expr of
        Just (lVar, _) -> matchNamesWithModuleName (getLocatedVarNameWithModuleName lVar) fnName AsteriskInSecond -- (getOccString . varName . unLoc $ lVar) == fnName
        Nothing -> False

-- Simplifies few things and handles some final transformations
isBadExpr :: (HasPluginOpts PluginOpts) => Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadExpr rules ap@(L _ (HsVar _ v)) = isBadExprHelper rules ap
isBadExpr rules ap@(L _ (HsApp _ funl funr)) = isBadExprHelper rules ap
isBadExpr rules ap@(L _ (PatExplicitList _ _)) = isBadExprHelper rules ap
isBadExpr rules ap@(L loc (PatHsWrap _ expr)) = isBadExpr rules (L loc expr) >>= mapM (\(x, y) -> trfViolationErrorInfo y ap x >>= \z -> pure (x, z))
isBadExpr rules ap@(L loc (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadExpr rules (L loc (HsApp noExtFieldOrAnn lfun rfun)) >>= mapM (\(x, y) -> trfViolationErrorInfo y ap x >>= \z -> pure (x, z))
    _ -> isBadExprHelper rules ap
#if __GLASGOW_HASKELL__ >= 900
isBadExpr rules ap@(L loc (PatHsExpansion orig expanded)) = do
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> isBadExpr rules (L loc (HsApp noExtFieldOrAnn funl funr)) >>= mapM (\(x, y) -> trfViolationErrorInfo y ap x >>= \z -> pure (x, z))
      _ -> isBadExpr rules (L loc expanded)
    _ -> isBadExpr rules (L loc expanded)
#endif
isBadExpr rules ap = pure []

-- Calls checkAndApplyRule, can be used to directly call without simplifier if needed
isBadExprHelper :: (HasPluginOpts PluginOpts) => Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadExprHelper rules ap = concat <$> mapM (\rule -> checkAndApplyRule rule ap) rules

-- Check if a particular rule applies to given expr
checkAndApplyRule :: (HasPluginOpts PluginOpts) => Rule -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
checkAndApplyRule ruleT ap = case ruleT of
  DBRuleT rule@(DBRule {table_name = ruleTableName}) ->
    case ap of
      (L _ (PatExplicitList (TyConApp ty [_, tblName]) exprs)) -> do
        case (showS ty == "Clause") of
          True  -> do
            simplifiedExprs <- trfWhereToSOP exprs
            checkWhereClauseRule <- mapM (validateWhereClauseRule (showS tblName)) simplifiedExprs
            liftIO $ putStrLn $ "Checking where clause rule for table: " <> (showS tblName) <> " ,clauses: " <> showS exprs <> " ,checkWhereClauseRule: " <> show checkWhereClauseRule
            if (showS tblName == (ruleTableName <> "T")) then validateDBRule simplifiedExprs rule (showS tblName) exprs ap
            else pure []
          False -> pure []
      _ -> pure []
  FunctionRuleT rule@(FunctionRule {fn_name = ruleFnNames, arg_no}) -> do
    let res = getFnNameWithAllArgs ap
    case res of
      Nothing                   -> pure []
      Just (fnLocatedVar, args) -> do
        let fnName    = getLocatedVarNameWithModuleName fnLocatedVar
            fnLHsExpr = mkLHsVar fnLocatedVar
        case (find (\ruleFnName -> matchNamesWithModuleName fnName ruleFnName AsteriskInSecond && length args >= arg_no) ruleFnNames) of
          Just ruleFnName  -> validateFunctionRule rule ruleFnName fnName fnLHsExpr args ap 
          Nothing -> pure []
  InfiniteRecursionRuleT rule -> pure [] --TODO: Add handling of infinite recursion rule
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
validateFunctionRule :: (HasPluginOpts PluginOpts) => FunctionRule -> String -> String -> LHsExpr GhcTc -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateFunctionRule rule ruleFnName fnName fnNameExpr args expr = do
  if arg_no rule == 0 && fn_sigs_blocked rule == [] -- considering arg 0 as the case for blocking the whole function occurence
    then pure [(fnNameExpr, FnUseBlocked ruleFnName rule)]
  else if arg_no rule == 0
    then do
      -- Check argument types for functions with polymorphic signature
      argTyps <- concat <$> mapM (\arg -> getHsExprTypeAsTypeDataList <$> getHsExprTypeWithResolver (logTypeDebugging . pluginOpts $ ?pluginOpts) arg) args
      fnReturnType <- getHsExprTypeAsTypeDataList <$> getHsExprTypeWithResolver (logTypeDebugging . pluginOpts $ ?pluginOpts) expr
      let fnSigFromArg = argTyps <> fnReturnType

      -- Given function signature
      fnExprTyp <- getHsExprTypeWithResolver (logTypeDebugging . pluginOpts $ ?pluginOpts) fnNameExpr
      let fnSigTypList = getHsExprTypeAsTypeDataList fnExprTyp

      pure . concat $ fmap (\ruleFnSig -> if matchFnSignatures fnSigTypList ruleFnSig || matchFnSignatures fnSigFromArg ruleFnSig then [(fnNameExpr, FnSigBlocked fnName ruleFnSig rule)] else []) (fn_sigs_blocked rule)
  else do
    let matches = drop ((arg_no rule) - 1) args
    if length matches == 0
      then pure []
    else do
      let arg = head matches
      argTypeGhc <- getHsExprTypeWithResolver (logTypeDebugging . pluginOpts $ ?pluginOpts) arg
      let argType = showS argTypeGhc
          argTypeBlocked = validateType argTypeGhc $ types_blocked_in_arg rule
          isArgTypeToCheck = validateType argTypeGhc $ types_to_check_in_arg rule

      when ((logDebugInfo . pluginOpts $ ?pluginOpts) && fnName /= "NA") $
        liftIO $ do
          print $ (fnName, map showS args)
          print $ (fnName, showS arg)
          print $ fn_rule_name rule
          print $ "Arg Type = " <> argType

      if argTypeBlocked
        then do
          exprType <- getHsExprTypeWithResolver (logTypeDebugging . pluginOpts $ ?pluginOpts) expr
          pure [(expr, ArgTypeBlocked argType (showS exprType) ruleFnName rule)]
      else if isArgTypeToCheck
        then do
          blockedFnsList <- getBlockedFnsList arg rule -- check if the expression has any stringification function
          mapM (\(lExpr, blockedFnName, blockedFnArgTyp) -> mkFnBlockedInArgErrorInfo expr lExpr >>= \errorInfo -> pure (lExpr, FnBlockedInArg (blockedFnName, blockedFnArgTyp) ruleFnName errorInfo rule)) blockedFnsList
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
getBlockedFnsList :: (HasPluginOpts PluginOpts) => LHsExpr GhcTc -> FunctionRule -> TcM [(LHsExpr GhcTc, String, String)] 
getBlockedFnsList arg rule@(FunctionRule { arg_no, fns_blocked_in_arg = fnsBlocked }) = do
  let argHsExprs = traverseAst arg :: [LHsExpr GhcTc]
      fnApps = filter (isFunApp False) argHsExprs
  when ((logDebugInfo . pluginOpts $ ?pluginOpts)) $ liftIO $ do
    print "getBlockedFnsList"
    showOutputable arg
    showOutputable fnApps
  catMaybes <$> mapM checkFnBlockedInArg fnApps 
  where 
    checkFnBlockedInArg :: LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, String, String))
    checkFnBlockedInArg expr = do
      let res = getFnNameWithAllArgs expr
      when ((logDebugInfo . pluginOpts $ ?pluginOpts)) $ liftIO $ do
        print "checkFnBlockedInArg"
        showOutputable res
      case res of
        Nothing -> pure Nothing
        Just (fnNameVar, args) -> isPresentInBlockedFnList expr fnsBlocked (getLocatedVarNameWithModuleName fnNameVar) args
    
    isPresentInBlockedFnList :: LHsExpr GhcTc -> FnsBlockedInArg -> String -> [LHsExpr GhcTc] -> TcM (Maybe (LHsExpr GhcTc, String, String))
    isPresentInBlockedFnList expr [] _ _ = pure Nothing
    isPresentInBlockedFnList expr ((ruleFnName, ruleArgNo, ruleAllowedTypes) : ls) fnName fnArgs = do
      when ((logDebugInfo . pluginOpts $ ?pluginOpts)) $ liftIO $ do
        print "isPresentInBlockedFnList"
        print (ruleFnName, ruleArgNo, ruleAllowedTypes)
      case matchNamesWithModuleName fnName ruleFnName AsteriskInSecond && length fnArgs >= ruleArgNo of
        False -> isPresentInBlockedFnList expr ls fnName fnArgs
        True  -> do
          let reqArg = head $ drop (ruleArgNo - 1) fnArgs
          argType <- getHsExprType (logTypeDebugging . pluginOpts $ ?pluginOpts) reqArg
          when ((logDebugInfo . pluginOpts $ ?pluginOpts)) $ liftIO $ do
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
validateDBRule :: (HasPluginOpts PluginOpts) => [SimplifiedIsClause] -> DBRule -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM ([(LHsExpr GhcTc, Violation)])
validateDBRule simplifiedExprs rule@(DBRule {db_rule_name = ruleName, table_name = ruleTableName, indexed_cols_names = ruleColNames}) tableName clauses expr = do 

  let checkDBViolation = case (matchAllInsideAnd . pluginOpts $ ?pluginOpts) of
                          True  -> checkDBViolationMatchAll
                          False -> checkDBViolationWithoutMatchAll
  violations <- catMaybes <$> mapM checkDBViolation simplifiedExprs
  pure violations
  where
    -- Since we need all columns to be indexed, we need to check for the columns in the order of composite ke
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

validateWhereClauseRule :: String -> [SimplifiedIsClause] -> TcM Bool
validateWhereClauseRule tableName simplifiedExprs = do
  -- Read JSON file
  jsonData <- liftIO $ readFile "/Users/sailaja.b/euler-db/tables_and_fields_with_types.json"
  case decode jsonData :: Maybe (HM.HashMap String (HM.HashMap String String)) of
    Just jsonMap -> do
      let tableKey = tableName
      case HM.lookup tableKey jsonMap of
        Just fieldsMap -> do
          -- Extract field names from the nested structure
          let fieldNames = HM.keys fieldsMap
          let matchingFields = filter (\field -> "disable" `isInfixOf` field || "enable" `isInfixOf` field) fieldNames
          liftIO $ putStrLn $ "Matching fields for " <> tableKey <> ": " <> show matchingFields
          -- Check if any matching field exists in simplifiedExprs
          let fieldExists = any (\(_, colName, _) -> any (\field -> field == colName) matchingFields) simplifiedExprs
          pure fieldExists
        Nothing -> do
          liftIO $ putStrLn $ "No fields found for table: " <> tableKey
          pure False
    Nothing -> do
      liftIO $ putStrLn "Failed to parse JSON file."
      pure False

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
trfWhereToSOP :: (HasPluginOpts PluginOpts) => [LHsExpr GhcTc] -> TcM [[SimplifiedIsClause]]
trfWhereToSOP [] = pure [[]]
trfWhereToSOP (clause : ls) = do
  let res = getWhereClauseFnNameWithAllArgs clause
      (fnName, args) = fromMaybe ("NA", []) res
  case (fnName, args) of
    ("And", [(L _ (PatExplicitList _ arg))]) -> do
      curr <- trfWhereToSOP arg
      rem  <- trfWhereToSOP ls
      pure [x <> y | x <- curr, y <- rem]
    ("Or", [(L _ (PatExplicitList _ arg))]) -> do
      curr <- foldM (\r cls -> fmap (<> r) $ trfWhereToSOP [cls]) [] arg
      rem  <- trfWhereToSOP ls
      pure [x <> y | x <- curr, y <- rem]
    ("$WIs", [arg1, arg2]) -> do
      curr <- getIsClauseData arg1 arg2 clause
      rem  <- trfWhereToSOP ls
      case curr of
        Nothing -> pure rem
        Just (tblName, colName) -> pure $ fmap (\lst -> (clause, tblName, colName) : lst) rem
    (fn, _) -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ print $ "Invalid/unknown clause in `where` clause : " <> fn <> " at " <> (showS . getLoc2 $ clause)) >> trfWhereToSOP ls

-- Get table field name and table name for the `Se.Is` clause
-- Patterns to match 'getField`, `recordDot`, `overloadedRecordDot` (ghc > 9), selector (duplicate record fields), rec fields (ghc 9), lens
-- TODO: Refactor this to use HasField instance if possible
getIsClauseData :: (HasPluginOpts PluginOpts) => LHsExpr GhcTc -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM (Maybe (String, String))
getIsClauseData fieldArg _comp _clause = do
  let fieldSpecType = getDBFieldSpecType fieldArg
  mbColNameAndTableName <- case fieldSpecType of
    None     -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ print $ "Can't identify the way in which DB field is specified: " <> showS fieldArg) >> pure Nothing
    Selector -> do
      let modFieldArg arg = case arg of
                        (L _ (HsRecFld _ fldOcc))   -> showS $ selectorAmbiguousFieldOcc fldOcc
                        (L loc (PatHsWrap _ wExpr)) -> modFieldArg (L loc wExpr)
                        (L _ expr)                  -> showS expr
      case (splitOn ":" $ modFieldArg fieldArg) of
        ("$sel" : colName : tableName : []) -> pure $ Just (colName, tableName)
        _ -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ print "Invalid pattern for Selector way") >> pure Nothing
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
              typ <- getHsExprType (logTypeDebugging . pluginOpts $ ?pluginOpts) tableVar
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
            _ -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ putStrLn "HsAppType not present. Should never be the case as we already filtered.") >> pure Nothing
        else when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ putStrLn "HsAppType not present after filtering. Should never reach as already deduced RecordDot.") >> pure Nothing
    Lens -> do
      let opApps = filter isLensOpApp (traverseAst fieldArg :: [HsExpr GhcTc])
      case opApps of
        [] -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ putStrLn "No lens operator application present in lens case.") >> pure Nothing
        (opExpr : _) -> do
          case opExpr of
            (OpApp _ tableVar _ fldVar) -> do
              let fldName = tail $ showS fldVar
              typ <- getHsExprType (logTypeDebugging . pluginOpts $ ?pluginOpts) tableVar
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
              typ <- getHsExprType (logTypeDebugging . pluginOpts $ ?pluginOpts) tableVar
              let tblName' = case typ of
                              AppTy ty1 _    -> showS ty1
                              TyConApp ty1 _ -> showS ty1
                              ty             -> showS ty
              pure $ Just (fldName, take (length tblName' - 1) tblName')
#endif                            
            _ -> when ((logWarnInfo . pluginOpts $ ?pluginOpts)) (liftIO $ putStrLn "OpApp not present. Should never be the case as we already filtered.") >> pure Nothing
  
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
-- Get function name & LHsExpr which gives resolved type with all it's arguments
getFnNameAndTypeableExprWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, LHsExpr GhcTc, [LHsExpr GhcTc])
getFnNameAndTypeableExprWithAllArgs ap@(L loc (HsVar _ v)) = Just (getLocated v loc, ap, [])
getFnNameAndTypeableExprWithAllArgs ap@(L _ (HsConLikeOut _ cl)) = (\clId -> (noExprLoc clId, ap, [])) <$> conLikeWrapId cl
getFnNameAndTypeableExprWithAllArgs (L _ (HsAppType _ expr _)) = getFnNameAndTypeableExprWithAllArgs expr
getFnNameAndTypeableExprWithAllArgs (L _ (HsApp _ ap@(L loc (HsVar _ v)) funr)) = Just (getLocated v loc, ap, [funr])
getFnNameAndTypeableExprWithAllArgs (L _ (HsPar _ expr)) = getFnNameAndTypeableExprWithAllArgs expr
getFnNameAndTypeableExprWithAllArgs (L _ (HsApp _ funl funr)) = do
  let res = getFnNameAndTypeableExprWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, typLHsExpr, ls) -> Just (fnName, typLHsExpr, ls ++ [funr])
getFnNameAndTypeableExprWithAllArgs (L loc (OpApp _ funl op funr)) = do
  case showS op of
    "($)" -> getFnNameAndTypeableExprWithAllArgs $ (L loc (HsApp noExtFieldOrAnn funl funr))
    _ -> Nothing
getFnNameAndTypeableExprWithAllArgs ap@(L loc (PatHsWrap _ (HsVar _ v))) = Just (getLocated v loc, ap, [])
getFnNameAndTypeableExprWithAllArgs (L loc ap@(PatHsWrap _ expr)) = getFnNameAndTypeableExprWithAllArgs (L loc expr)
#if __GLASGOW_HASKELL__ >= 900
getFnNameAndTypeableExprWithAllArgs (L loc ap@(PatHsExpansion orig expanded)) = 
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> getFnNameAndTypeableExprWithAllArgs (L loc (HsApp noExtFieldOrAnn funl funr))
      _ -> getFnNameAndTypeableExprWithAllArgs (L loc expanded)
    _ -> getFnNameAndTypeableExprWithAllArgs (L loc expanded)
#endif
getFnNameAndTypeableExprWithAllArgs _ = Nothing


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
trfViolationErrorInfo :: (HasPluginOpts PluginOpts) => Violation -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM Violation
trfViolationErrorInfo violation@(FnBlockedInArg p1 ruleFnName _ rule) outsideExpr insideExpr = do
  errorInfo <- mkFnBlockedInArgErrorInfo outsideExpr insideExpr
  pure $ FnBlockedInArg p1 ruleFnName errorInfo rule
trfViolationErrorInfo violation _ _ = pure violation

-- Create Error Info for FnBlockedInArg Violation
mkFnBlockedInArgErrorInfo :: (HasPluginOpts PluginOpts) => LHsExpr GhcTc -> LHsExpr GhcTc -> TcM Value
mkFnBlockedInArgErrorInfo lOutsideExpr@(L _ outsideExpr) lInsideExpr@(L _ insideExpr) = do
  let loc1 = getLoc2 lOutsideExpr
      loc2 = getLoc2 lInsideExpr
  filePath <- unpackFS . srcSpanFile . tcg_top_loc . env_gbl <$> getEnv
  let overall_src_span = showS loc1
      overall_err_line_orig = showS lOutsideExpr
      err_fn_src_span = showS loc2
      err_fn_err_line_orig = showS lInsideExpr
  overall_err_line <- 
    if (useIOForSourceCode . pluginOpts $ ?pluginOpts)
      then liftIO $ extractSrcSpanSegment loc1 filePath overall_err_line_orig
      else pure overall_err_line_orig
  err_fn_err_line <- 
    if (useIOForSourceCode . pluginOpts $ ?pluginOpts)
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

-- Check if a rule is allowed on current Function
isAllowedOnCurrentFunction :: String -> Rule -> Bool
isAllowedOnCurrentFunction currentFnNameWithModule rule = 
  let ignoredFunctions = getRuleIgnoreFunctions rule
  in not $ any (matchNamesWithAsterisk AsteriskInSecond currentFnNameWithModule) ignoredFunctions

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

-- TODO: Verify the correctness of function
-- Check if HsExpr is HsVar which can be simple variable or function application
isHsVar :: LHsExpr GhcTc -> Bool
isHsVar (L _ (HsVar _ _)) = True
isHsVar _ = False

-- TODO: Verify the correctness of function
-- Check if HsExpr is Function Application
isFunApp :: Bool -> LHsExpr GhcTc -> Bool
isFunApp hsVarAsFunApp (L _ (HsVar _ _)) = hsVarAsFunApp
isFunApp _ (L _ (HsApp _ _ _)) = True
isFunApp _ (L _ (OpApp _ funl op funr)) = True
isFunApp hsVarAsFunApp (L loc (PatHsWrap _ expr)) = isFunApp hsVarAsFunApp (L loc expr)
#if __GLASGOW_HASKELL__ >= 900
isFunApp _ (L _ (PatHsExpansion orig expanded)) = 
  case orig of
    (OpApp{}) -> True
    _ -> False
#endif
isFunApp _ _ = False

-- Check if HsExpr is Lens operator application
isLensOpApp :: HsExpr GhcTc -> Bool
isLensOpApp (OpApp _ _ op _) = showS op == "(^.)"
isLensOpApp (SectionR _ op _) = showS op == "(^.)"
#if __GLASGOW_HASKELL__ >= 900
isLensOpApp (PatHsExpansion (OpApp _ _ op _) expanded) = showS op == "(^.)"
#endif
isLensOpApp _ = False

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
