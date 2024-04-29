{-# LANGUAGE DataKinds #-}

module Sheriff.Plugin (plugin) where

import Bag (bagToList,listToBag)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
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
    noLoc, Module (moduleName), moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass
  )
import GHC.Hs.Binds
import GhcPlugins (idName,Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr,elemNameSet,pprPrefixName,idType,tidyOpenType)
import HscTypes (ModSummary (..))
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
import TcRnMonad (failWith, addErr, addErrAt, addErrs)
import Name (isSystemName)
import GHC (OverLitTc(..), HsOverLit(..))
import Control.Applicative ((<|>))
import Type (isFunTy, funResultTy, splitAppTys, dropForAlls)
import TyCoRep (Type(..))
import Data.ByteString.Lazy as BSL ()
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import TcType
import ConLike
import TysWiredIn
import GHC.Hs.Lit (HsLit(..))

plugin :: Plugin
plugin = defaultPlugin {
      typeCheckResultAction = logerr
    , pluginRecompile = purePlugin
    }

logDebugInfo :: Bool
logDebugInfo = False

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
loopOverModBinds rules (L _ ap@(FunBind _ id matches _ _)) = do
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
getBadFnCalls rules (FunBind _ id matches _ _) = do
  let funMatches = map unLoc $ unLoc $ mg_alts matches
  concat <$> mapM getBadFnCallsHelper funMatches
  where
    getBadFnCallsHelper :: Match GhcTc (LHsExpr GhcTc) -> TcM [(LHsExpr GhcTc, Violation)]
    getBadFnCallsHelper match = do
      let whereBinds = (grhssLocalBinds $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          normalBinds = (grhssGRHSs $ m_grhss match) ^? biplateRef :: [LHsBinds GhcTc]
          argBinds = m_pats match
          exprs = match ^? biplateRef :: [LHsExpr GhcTc]
      concat <$> mapM (isBadFunApp rules) exprs
getBadFnCalls _ _ = pure []

isBadFunApp :: Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunApp rules ap@(L _ (HsVar _ v)) = isBadFunAppHelper rules ap
isBadFunApp rules ap@(L _ (HsApp _ funl funr)) = isBadFunAppHelper rules ap
isBadFunApp rules ap@(L loc (HsWrap _ _ expr)) = isBadFunApp rules (L loc expr)
-- **** Not adding these because of biplateRef ****
-- isBadFunApp ap@(L _ (HsPar _ expr)) = isBadFunApp expr
isBadFunApp rules (L _ (OpApp _ lfun op rfun)) = do
  case showS op of
    "($)" -> isBadFunAppHelper rules $ mkHsApp lfun rfun
    _ -> pure []
isBadFunApp _ _ = pure []

isBadFunAppHelper :: Rules -> LHsExpr GhcTc -> TcM [(LHsExpr GhcTc, Violation)]
isBadFunAppHelper rules ap = do
  let res = getFnNameWithAllArgs ap
  -- let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((nameStableString . varName . unLoc) x, y)) $ res
  let (fnName, args) = maybe ("NA", []) (\(x, y) -> ((getOccString . varName . unLoc) x, y)) $ res
      applicableRules = filter (\x -> doesRuleApply x fnName args) rules
  catMaybes <$> mapM (\rule -> validateRule rule fnName args ap) applicableRules

validateRule :: Rule -> String -> [LHsExpr GhcTc] -> LHsExpr GhcTc -> TcM (Maybe (LHsExpr GhcTc, Violation))
validateRule rule fnName args expr = case rule of
  DBRule ruleName ruleTableName ruleColNames -> if length args < 0 then pure Nothing else do
    let fieldArg = head args
        fieldSpecType = getDBFieldSpecType fieldArg
    mbColNameAndTableName <- case fieldSpecType of
      None     -> when logDebugInfo (liftIO $ print "Can't identify the way in which DB field is specified") >> pure Nothing
      Selector -> do
        case (splitOn ":" . showS $ head args) of
          ("$sel" : colName : tableName : []) -> pure $ Just (colName, tableName)
          _ -> when logDebugInfo (liftIO $ print "Invalid pattern for Selector way") >> pure Nothing
      RecordDot -> do
        let tyApps = filter (\x -> case x of 
                                    (HsApp _ (L _ (HsAppType _ _ fldName)) tableVar) -> True
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
              _ -> when logDebugInfo (liftIO $ putStrLn "HsAppType not present. Should never be the case as we already filtered.") >> pure Nothing
          else pure Nothing
      Lens -> do
        let opApps = filter isLensOpApp (fieldArg ^? biplateRef :: [HsExpr GhcTc])
        case opApps of
          [] -> when logDebugInfo (liftIO $ putStrLn "No lens operator application present in lens case.") >> pure Nothing
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
              _ -> when logDebugInfo (liftIO $ putStrLn "OpApp not present. Should never be the case as we already filtered.") >> pure Nothing
    
    when logDebugInfo $ liftIO $ putStrLn $ "DBRule - " <> show mbColNameAndTableName

    case mbColNameAndTableName of
      Nothing -> pure Nothing
      Just (colName, tableName) -> 
        if tableName == ruleTableName && colName `notElem` ruleColNames
          then pure $ Just (expr, NonIndexedDBColumn colName tableName rule)
          else pure Nothing

  FunctionRule {} -> do
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

doesRuleApply :: Rule -> String -> [LHsExpr GhcTc] -> Bool
doesRuleApply rule fnName args = case rule of
  DBRule _ _ _                           -> fnName == "$WIs" && length args == 2
  FunctionRule _ ruleFnName arg_no _ _ _ -> fnName == ruleFnName && length args >= arg_no

getDBFieldSpecType :: LHsExpr GhcTc -> DBFieldSpecType
getDBFieldSpecType (L _ expr)
  | isPrefixOf "$sel" (showS expr) = Selector
  | isInfixOf "^." (showS expr) = Lens
  | (\x -> isInfixOf "->" x && isInfixOf "@" x) (showS expr) = RecordDot
  | otherwise = None

getFnNameWithAllArgs :: LHsExpr GhcTc -> Maybe (Located Var, [LHsExpr GhcTc])
getFnNameWithAllArgs (L _ (HsVar _ v)) = Just (v, [])
getFnNameWithAllArgs (L _ (HsConLikeOut _ cl)) = (\clId -> (noLoc clId, [])) <$> conLikeWrapId_maybe cl
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

--------------------------- Utils ---------------------------
-- Check if HsExpr is Function Application
isFunApp :: LHsExpr GhcTc -> Bool
isFunApp (L _ (HsApp _ _ _)) = True
isFunApp (L _ (OpApp _ funl op funr)) = True
isFunApp _ = False

-- Check if HsExpr is Lens operator application
isLensOpApp :: HsExpr GhcTc -> Bool
isLensOpApp (OpApp _ _ op _) = showS op == "(^.)"
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
showOutputable = liftIO . putStr . showSDocUnsafe . ppr

-- Create GHC compilation error from CompileError
mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ err_msg err)

-- Create invalid indexedKeys file compilation error
mkInvalidIndexedKeysFile :: String -> OP.SDoc
mkInvalidIndexedKeysFile err = OP.text err

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

addErrToFile :: ModSummary -> String -> [CompileError] -> TcM ()
addErrToFile modSummary path errs = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      -- res = (encodePretty moduleName') <> (fromString ": ") <> encodePretty errs <> (fromString ",")
      res = encodePretty errs
  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile (path <> moduleName' <> "_compilationErrors.json") res