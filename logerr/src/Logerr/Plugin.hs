{-# LANGUAGE DataKinds #-}

module Logerr.Plugin (plugin) where

import Bag (bagToList,listToBag)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import Data.List (nub)
import Debug.Trace (traceShowId)
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
import GhcPlugins (idName,Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr,elemNameSet,pprPrefixName,idType,tidyOpenType)
import HscTypes (ModSummary (..))
import Name (nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile, appendFile)
import Data.Aeson
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
import Logerr.Types
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
import TcRnMonad (failWith, addErr, addErrAt, addErrs)
import Name (isSystemName)
import GHC (OverLitTc(..), HsOverLit(..))
import Control.Applicative ((<|>))
import Type (isFunTy, funResultTy, splitAppTys, dropForAlls)
import TyCoRep (Type(..))
import Data.ByteString.Lazy as BSL ()
import Data.String (fromString)
import TcType
import TysWiredIn
import GHC.Hs.Lit (HsLit(..))

logDebugInfo :: Bool
logDebugInfo = True

throwCompilationError :: Bool
throwCompilationError = False

plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = logerr
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

logerr :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
logerr opts modSummary tcEnv = do
  let prefixPath = case opts of
                        []    -> "test/output"
                        local : _ -> local
      moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      modulePath = prefixPath <> ms_hspp_file modSummary
  -- liftIO $ forkIO $ do
      -- depsMapList <- mapM loopOverLHsBindLR $ bagToList $ tcg_binds tcEnv
      -- let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
      -- print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path)
      -- createDirectoryIfMissing True path
      -- writeFile ((modulePath) <> ".json") (encodePretty $ concat depsMapList)

  errors <- concat <$> (mapM loopOverModBinds $ bagToList $ tcg_binds tcEnv)
  -- if length errors > 0
    -- then liftIO $ print ("More than 0 errors: " <> show errors)
    -- else liftIO $ print "No errors"
  if throwCompilationError
    then addErrs $ map mkGhcCompileError errors
    else pure ()
  addErrToFile modSummary errors  

  return tcEnv

--------------------------- Core Logic Hardcoded Data ---------------------------
badPracticeRules :: Rules
badPracticeRules = [
    defaultRule
  , logRule1 
  , logRule2 
  , logRule3 
  , logRule4 
  , logRule5 
  , logRule6
  , showRule
  ]

logArgNo :: ArgNo
logArgNo = 2

logRule1 :: Rule
logRule1 = Rule "LogRule" "logErrorT" logArgNo stringifierFns [] textTypesToCheck

logRule2 :: Rule
logRule2 = Rule "LogRule" "logErrorV" logArgNo stringifierFns [] textTypesToCheck

logRule3 :: Rule
logRule3 = Rule "LogRule" "logInfoT" logArgNo stringifierFns [] textTypesToCheck

logRule4 :: Rule
logRule4 = Rule "LogRule" "logInfoV" logArgNo stringifierFns [] textTypesToCheck

logRule5 :: Rule
logRule5 = Rule "LogRule" "logInfo" logArgNo stringifierFns [] textTypesToCheck

logRule6 :: Rule
logRule6 = Rule "LogRule" "logError" logArgNo stringifierFns [] textTypesToCheck

showRule :: Rule
showRule = Rule "ShowRule" "show" 1 stringifierFns textTypesBlocked textTypesToCheck

noUseRule :: Rule
noUseRule = Rule "NoDecodeUtf8Rule" "$text-1.2.4.1$Data.Text.Encoding$decodeUtf8" 0 [] [] []

stringifierFns :: FnsBlockedInArg
stringifierFns = ["show", "encode", "encodeJSON"]

textTypesBlocked :: TypesBlockedInArg
textTypesBlocked = ["Text", "String", "Char", "[Char]"]

textTypesToCheck :: TypesToCheckInArg
textTypesToCheck = ["Text", "String", "Char", "[Char]"]

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
loopOverModBinds :: LHsBindLR GhcTc GhcTc -> TcM [CompileError]
loopOverModBinds (L _ ap@(FunBind _ id matches _ _)) = do
  -- liftIO $ print "FunBinds" >> showOutputable ap
  calls <- getBadFnCalls ap
  mapM mkCompileError calls
loopOverModBinds (L _ ap@(PatBind _ _ pat_rhs _)) = do
  -- liftIO $ print "PatBinds" >> showOutputable ap
  pure []
loopOverModBinds (L _ ap@(VarBind {var_rhs = rhs})) = do 
  -- liftIO $ print "VarBinds" >> showOutputable ap
  pure []
loopOverModBinds (L _ ap@(AbsBinds {abs_binds = binds})) = do
  -- liftIO $ print "AbsBinds" >> showOutputable ap
  list <- mapM loopOverModBinds $ bagToList binds
  pure (concat list)
loopOverModBinds _ = pure []

-- Get all the FunApps inside the top level function bind
-- This call can be anywhere in `where` clause or `regular` RHS
getBadFnCalls :: HsBindLR GhcTc GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
getBadFnCalls (FunBind _ id matches _ _) = do
  let funMatches = map unLoc $ unLoc $ mg_alts matches
  concat <$> mapM getBadFnCallsHelper funMatches
  where
    getBadFnCallsHelper :: Match GhcTc (LHsExpr GhcTc) -> TcM [(LHsExpr GhcTc, Violation)]
    getBadFnCallsHelper match = do
      let whereBinds = (grhssLocalBinds $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          normalBinds = (grhssGRHSs $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          argBinds = m_pats match
          exprs = match ^? biplateRef :: [LHsExpr GhcTc]
      catMaybes <$> mapM isBadFunApp exprs
getLoggerFnCall _ = pure []

isBadFunApp :: LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
isBadFunApp ap@(L _ (HsVar _ v)) = isBadFunAppHelper ap
isBadFunApp ap@(L _ (HsApp _ funl funr)) = isBadFunAppHelper ap
-- **** Not adding these because of biplateRef ****
-- isBadFunApp ap@(L loc (HsWrap _ expr)) = isBadFunApp (L loc expr)
-- isBadFunApp ap@(L _ (Hspar _ expr)) = isBadFunApp expr
isBadFunApp (L _ (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadFunAppHelper $ mkHsApp lfun rfun
    _ -> pure Nothing
isBadFunApp _ = pure Nothing

isBadFunAppHelper :: LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
isBadFunAppHelper ap = do
  let res = getFnNameWithAllArgs ap
  -- let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((nameStableString . varName . unLoc) x, y)) $ res
  let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
      rule = fromMaybe defaultRule $ (\x -> fnName == fn_name x) `find` badPracticeRules
  -- if logDebugInfo && fnName /= "NA"
  --   then liftIO $ do
  --     print $ (fnName, map showS args)
  --     print $ rule
  --   else pure ()
  if rule == defaultRule
    then pure Nothing
  else if (arg_no rule) == 0 -- considering arg 0 as the case for blocking the whole function occurence
    then pure $ Just (ap, FnUseBlocked rule)
  else do
    let matches = drop ((arg_no rule) - 1) args
    if length matches == 0
      then pure Nothing
    else do
      let arg = head matches
          argTypes = (map showS $ getArgTypeWrapper arg)
          argTypeBlocked = fromMaybe "NA" $ (`elem` types_blocked_in_arg rule) `find` argTypes
          isArgTypeToCheck = (`elem` types_to_check_in_arg rule) `any` argTypes 
      if logDebugInfo && fnName /= "NA" then
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
      else pure ()
      if argTypeBlocked /= "NA"
        then pure $ Just (ap, ArgTypeBlocked argTypeBlocked rule)
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
            pure $ Just (ap, FnBlockedInArg (head blockedFnsList) rule)
          else pure Nothing

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v)) = Just (v, [])
getFnNameWithAllArgs (L _ (HsApp _ (L _ (HsVar _ v)) funr)) = Just (v, [funr])
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
getFnNameWithAllArgs (L loc ap@(HsWrap _ _ expr)) = do
  getFnNameWithAllArgs (L loc expr)
getFnNameWithAllArgs _ = Nothing

-- TODO: Implement this
isLogArgInFunArg :: LHsBindLR GhcTc GhcTc -> TcM Bool
isLogArgInFunArg _ = pure False

-- TODO: Implement this function to update the list of functions which accept the string to log as function argt
findAndUpdateFunctionsWithArgLogger :: LHsBindLR GhcTc GhcTc -> TcM [LHsBindLR GhcTc GhcTc]
findAndUpdateFunctionsWithArgLogger _ = pure []

--------------------------- Utils ---------------------------
-- Check if HsExpr is Function Application
isFunApp :: LHsExpr GhcTc -> Bool
isFunApp (L _ (HsApp _ _ _)) = True
isFunApp (L _ (OpApp _ funl op funr)) = True
isFunApp _ = False

-- Check if a Var is fun type
isFunVar :: Var -> Bool
isFunVar = isFunTy . dropForAlls . idType 

-- Pretty print the Internal Representations
showOutputable :: (MonadIO m, Outputable a) => a -> m ()
showOutputable = liftIO . putStr . showSDocUnsafe . ppr

-- Create GHC compilation error from CompileError
mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ err_msg err)

-- Create Internal Representation of Logging Error
mkCompileError :: (LHsExpr GhcTc, Violation) -> TcM CompileError
mkCompileError (expr, violation) = pure $ CompileError "" "" (show violation) (getLoc expr) violation

-- Get Return type of the function application arg
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

getArgType :: LHsExpr GhcTc -> Bool -> [Type]
getArgType (L _ (HsLit _ v)) _ = getLitType v
getArgType (L _ (HsOverLit _ (OverLit (OverLitTc _ typ) v _))) _ = [typ]
getArgType (L loc (HsWrap _ _ expr)) shouldReturnFinalType = getArgType (L loc expr) shouldReturnFinalType
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
    replacer replacements typ@(FunTy flag ty1 ty2) = FunTy flag (replacer replacements ty1) (replacer replacements ty2) 
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
getStringificationFns (L loc ap@(HsWrap _ _ expr)) = do
  liftIO $ putStrLn "Inside HsWrap" >> putStrLn (showS ap) 
  getStringificationFns (L loc expr)
getStringificationFns _ = do
  liftIO $ putStrLn $ "Inside _"
  pure []

-- Get List of stringification functions used inside a HsExpr; Uses `stringifierFns` 
getStringificationFns2 :: LHsExpr GhcTc -> Rule -> [String] 
getStringificationFns2 arg rule =
  let vars = arg ^? biplateRef :: [Var]
      blockedFns = fns_blocked_in_arg rule
  in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns)) $ takeWhile isFunVar $ filter (not . isSystemName . varName) vars

-- Get List of blocked functions used inside a HsExpr; Uses `getBlockedFnsList` 
getBlockedFnsList :: LHsExpr GhcTc -> Rule -> [String] 
getBlockedFnsList arg rule =
  let vars = arg ^? biplateRef :: [Var]
      blockedFns = fns_blocked_in_arg rule
  in map getOccString $ filter (\x -> ((getOccString x) `elem` blockedFns) && (not . isSystemName . varName) x) vars

addErrToFile :: ModSummary -> [CompileError] -> TcM ()
addErrToFile modSummary errs = do
  let path = "test/output/"
      moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      res = (encodePretty moduleName') <> (fromString ": ") <> encodePretty errs <> (fromString ",")
  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile (path <> moduleName' <> "_compilationErrors.js") res

-- Fdep plugin -------------------------------------------------------------------

transformFromNameStableString :: (Maybe String,Maybe String) -> Maybe FunctionInfo
transformFromNameStableString (Just str,Just loc) =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in Just $ if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) loc else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) loc

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ (FunBind _ id matches _ _)) = do
  let funName = getOccString $ unLoc id
      matchList = mg_alts matches
  (list,funcs) <- foldM (\(x,y) xx -> do
                                  (l,f) <- processMatch xx
                                  pure $ (x <> l,y <> f)
                        ) ([],[]) (unLoc matchList)
  let listTransformed = map transformFromNameStableString $ nub $ list
  pure [(Function funName listTransformed (nub funcs) (showSDocUnsafe $ ppr $ getLoc id))]
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = do
  let l = map transformFromNameStableString $ concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [(Function "" l [] "")]
loopOverLHsBindLR (L _ VarBind {var_rhs = rhs}) = do
  pure [(Function "" (map transformFromNameStableString $ processExpr rhs) [] "")]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  let list = map transformFromNameStableString $ map (\(n,srcLoc) -> (Just $ traceShowId $ nameStableString n, srcLoc)) $ processPat def
  pure [(Function "" list [] "")]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  pure []

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([(Maybe String,Maybe String)],[Function])
processMatch (L _ match) = do
  whereClause <- processHsLocalBinds $ unLoc $ grhssLocalBinds (m_grhss match)
  pure $ (concatMap processGRHS $ grhssGRHSs (m_grhss match),whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [(Maybe String,Maybe String)]
processGRHS (L _ (GRHS _ _ body)) = processExpr body
processGRHS _ = []

processHsLocalBinds :: HsLocalBindsLR GhcTc GhcTc -> IO [Function]
processHsLocalBinds (HsValBinds _ (ValBinds _ x y)) = do
  res <- mapM loopOverLHsBindLR $ bagToList $ x
  pure $ concat res
processHsLocalBinds (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
  res <- foldM (\acc (recFlag,binds) -> do 
                  funcs <- mapM loopOverLHsBindLR $ bagToList binds
                  pure (acc <> funcs)
                ) [] x
  pure $ concat res
processHsLocalBinds x =
  pure []

processExpr :: LHsExpr GhcTc -> [(Maybe String,Maybe String)]
processExpr x@(L _ (HsVar _ (L _ var))) =
  let name = nameStableString $ varName var
  in [(Just name,Just $ showSDocUnsafe $ ppr $ getLoc $ x)]
processExpr (L _ (HsUnboundVar _ _)) = []
processExpr (L _ (HsApp _ funl funr)) =
  processExpr funl <> processExpr funr
processExpr (L _ (OpApp _ funl funm funr)) =
  processExpr funl <> processExpr funm <> processExpr funr
processExpr (L _ (NegApp _ funl _)) =
  processExpr funl
processExpr (L _ (HsTick _ _ fun)) =
  processExpr fun
processExpr (L _ (HsStatic _ fun)) =
  processExpr fun
processExpr (L _ (HsWrap _ _ fun)) =
  processExpr (noLoc fun)
processExpr (L _ (HsBinTick _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExplicitList _ _ funList)) =
  concatMap processExpr funList
processExpr (L _ (HsTickPragma _ _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsSCC _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsCoreAnn _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExprWithTySig _ fun _)) =
  processExpr fun
processExpr (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap processExpr stmts
processExpr (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExpr func <> nub (concatMap processExpr stmts)
processExpr (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl] <> stmts)
processExpr (L _ (ExplicitSum _ _ _ fun)) = processExpr fun
processExpr (L _ (SectionR _ funl funr)) = processExpr funl <> processExpr funr
processExpr (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
processExpr (L _ (HsPar _ fun)) = processExpr fun
processExpr (L _ (HsAppType _ fun _)) = processExpr fun
processExpr (L _ (HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmtsR)
processExpr (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(Maybe String,Maybe String)]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr fun

processPat :: LPat GhcTc -> [(Name,Maybe String)]
processPat (L _ pat) = case pat of
  ConPatIn _ details -> processDetails details
  VarPat _ x@(L _ var) -> [(varName var,Just $ showSDocUnsafe $ ppr $ getLoc $ x)]
  ParPat _ pat' -> processPat pat'
  _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name,Maybe String)]
processDetails (PrefixCon args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name,Maybe String)]
processPatField (L _ HsRecField {hsRecFieldArg = arg}) = processPat arg