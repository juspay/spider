{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass, CPP, TypeApplications #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, RecordWildCards, PartialTypeSignatures #-}

module AC.AmountCheck where

import GHC hiding (typeKind)
import Data.Aeson
import Data.Aeson as A
import Data.Maybe (fromMaybe)
import Data.Generics.Uniplate.Data ()
import Data.List as DL
import qualified Data.HashMap.Strict as HM
import Control.Monad (when)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson as Aeson
import System.Directory (createDirectoryIfMissing, canonicalizePath, getCurrentDirectory )
import Control.Exception (try, IOException)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import qualified Data.ByteString.Lazy.Char8 as Char8
import AC.Types
import AC.Constants
import GHC.Unit.Module
#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.Monad (addErr)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types
import GHC.Driver.Plugins
import GHC.Types.Var
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.Name hiding (varName)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BL
#if __GLASGOW_HASKELL__ < 902
import ConLike
import qualified Outputable as OP
import TcRnMonad (addErrs)
import Bag (bagToList)
import TcRnTypes (TcGblEnv (..), TcM)
import GhcPlugins hiding ((<>))
#endif
#endif
import           Data.IORef (readIORef, IORef, newIORef, modifyIORef)

plugin :: Plugin
plugin =
    defaultPlugin {
      typeCheckResultAction = checkIntegrity
    , pluginRecompile = purePlugin
    }

getShouldLog :: IO Bool
getShouldLog = do
    envVal <- lookupEnv "SHOULD_LOG"
    return $ case envVal of
        Just "TRUE"  -> True
        _            -> False

getFuncWithModule :: Name -> Maybe String
getFuncWithModule name =
  let occStr = occNameString (nameOccName name)
  in case nameModule_maybe name of
        Just m  ->
            let modStr = moduleNameString (moduleName m)
            in Just $ modStr ++ "." ++ occStr
        Nothing -> Nothing

-- export KEYWORDS="[\"surchargeAmount\"]"
getKeywords :: IO [String]
getKeywords = do
    envVal <- lookupEnv "KEYWORDS"
    return $ case envVal of
        Just keywords ->
            case Aeson.decode (Char8.pack keywords) :: Maybe [String] of
                Just parsed -> parsed
                Nothing     -> []
        Nothing       -> []

checkIntegrity :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
checkIntegrity opts modSummary tcEnv = do
    let PluginOpts{..} = case opts of
            []      -> defaultPluginOpts
            (x : _) -> fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
    mPath <- case ml_hs_file (ms_location modSummary) of
        Nothing   -> pure Nothing
        Just path -> Just <$> liftIO (canonicalizePath path)
    logIt 0 "PATH" [fromMaybe "NOTFOUND" mPath]
    cwd <- liftIO getCurrentDirectory
    logIt 0 "CWD_PATH" [cwd]
    let shouldCheckPath = maybe False (\path -> any (\pathToCheck -> pathToCheck `isInfixOf` path) pathsTobeChecked) mPath
    if shouldCheckPath then do
        let binds = bagToList $ tcg_binds tcEnv
        let modName = moduleNameString $ moduleName $ tcg_mod tcEnv

        newErrorsRef <- liftIO $ newIORef (HM.empty :: HM.HashMap String (HM.HashMap String [String]))
        eOldErrors <- liftIO $ readAmountCheckFile (prefixPath ++ domainConfigFile)
        oldErrors <- case eOldErrors of
            Right oldErrors -> pure $ oldErrors
            Left err        -> do
                addErr (text ("Check AmountCheck.json File - Decode Failed " ++ err))
                pure $ (HM.empty :: HM.HashMap String (HM.HashMap String [String]))
        _ <- mapM (myLookOverLHsBindLR 0 modName newErrorsRef oldErrors) binds
        
        newErrors <- liftIO $ readIORef newErrorsRef
        liftIO $ do
            let combinedErrors = unionDeep oldErrors newErrors
            createDirectoryIfMissing True (takeDirectory ((prefixPath ++ domainConfigFile)))
            BL.writeFile ((prefixPath ++ domainConfigFile)) (encodePretty combinedErrors)
        pure tcEnv
    else pure tcEnv

myLookOverLHsBindLR :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> LHsBindLR GhcTc GhcTc -> TcM ()
myLookOverLHsBindLR level moduleName errorsRef oldErrors (L _ hsBindLR) = do
    logIt level "myLookOverLHsBindLR: Processing binding" ([])
    handleHsBindLR level moduleName errorsRef oldErrors Nothing hsBindLR

handleHsBindLR :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> HsBindLR GhcTc GhcTc -> TcM ()
handleHsBindLR level moduleName errorsRef oldErrors maybeFuncCtx hsBindLR = do
    case hsBindLR of
#if __GLASGOW_HASKELL__ < 900
        (FunBind _ fun_id fun_matches _ _) -> do
            let funName = getOccString fun_id
                funSpan = getLoc fun_id
                funcCtx = FunctionContext funName funSpan
            logIt level "FunBind:" [funName]
            handleFundBind (level + 2) funcCtx fun_matches
#else
        (FunBind _ fun_id fun_matches _) -> do
            let funName = getOccString fun_id
                funSpan = RealSrcSpan (la2r (getLoc fun_id)) Nothing
                funcCtx = FunctionContext funName funSpan
            logIt level "FunBind:" [funName]
            handleFundBind (level + 2) funcCtx fun_matches
#endif
        (PatBind _ _ _ _) -> handlePatBind level ()
        (VarBind _ var_id var_rhs) -> do
            logIt level "VarBind:" [showSDocUnsafe $ ppr var_id]
            handleVarBind (level + 2) maybeFuncCtx var_rhs
        (AbsBinds _ _ _ _ _ abs_binds _) -> handleAbsBind level abs_binds
        (PatSynBind _ _) -> handlePatSyncBind level ()
        _                     -> pure ()
    where
        handleFundBind :: Int -> FunctionContext -> MatchGroup GhcTc (LHsExpr GhcTc) -> TcM ()
        handleFundBind lvl funcCtx fun_matches = do
            case fun_matches of
                MG _ mg_alts _ ->
                    case mg_alts of
                        (L _ lMatchList) -> mapM_ (handleLMatchList lvl moduleName errorsRef oldErrors (Just funcCtx)) lMatchList
                        _                -> pure ()
                _                           -> pure ()
        handlePatBind lvl patBind = do
            logIt lvl "HANDLE_PATBIND" ([])
            pure ()
        handleVarBind lvl mFuncCtx var_rhs = do
            processExpr lvl moduleName errorsRef oldErrors mFuncCtx var_rhs
        handleAbsBind :: Int -> LHsBinds GhcTc -> TcM ()
        handleAbsBind lvl binds = do
            logIt lvl "HANDLE_ABSBIND" ([])
            mapM_ (myLookOverLHsBindLR lvl moduleName errorsRef oldErrors) (bagToList binds)
        handlePatSyncBind lvl _ = do
            logIt lvl "HANDLE_PATSYNBIND" ([])
            pure ()

handleLMatchList :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> LMatch GhcTc (LHsExpr GhcTc) -> TcM ()
handleLMatchList level moduleName errorsRef oldErrors maybeFuncCtx (L _ match) = do
    logIt level "handleLMatchList: Processing match" ([])
    case match of
        Match _ _ _ m_grhss -> do
            logIt level "Match: Processing GRHSs" ([])
            case m_grhss of
                GRHSs _ grhssGRHSs _ -> mapM_ (handleGrhssGRHSs (level + 6)) grhssGRHSs
                _                                         -> pure ()
        _                                 -> pure ()
    where
        handleGrhssGRHSs :: Int -> LGRHS GhcTc (LHsExpr GhcTc) -> TcM ()
        handleGrhssGRHSs lvl (L _ grhs) = do
            logIt lvl "handleGrhssGRHSs: Processing GRHS body" ([])
            case grhs of
                GRHS _ _ body -> processExpr (lvl + 8) moduleName errorsRef oldErrors maybeFuncCtx body
                _             -> pure ()

giveWithIndent :: Int -> String -> String
giveWithIndent level msg = (replicate level ' ') ++ msg

logIt :: Int -> String -> [String] -> TcM ()
logIt level msg [] = do
    shouldLog <- liftIO $ getShouldLog
    when shouldLog $ liftIO $ print (giveWithIndent level msg)
logIt level msg values = do
    shouldLog <- liftIO $ getShouldLog
    when shouldLog $ liftIO $ print (giveWithIndent level msg, values)

checkModuleFunctionInOldErrors :: HM.HashMap String (HM.HashMap String [String]) -> String -> String -> Bool
checkModuleFunctionInOldErrors oldErrors moduleName funcName =
    case HM.lookup moduleName oldErrors of
        Just funcMap -> 
            case HM.lookup funcName funcMap of
                Just _  -> True
                Nothing -> False
        Nothing      -> False

checkAndAddAmountFieldError :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> String -> TcM ()
checkAndAddAmountFieldError level moduleName errorsRef _ maybeFuncCtx fieldInfo = do
    keywords <- liftIO $ getKeywords
    let mFound = containsAmountFields keywords fieldInfo
    when (isJust mFound) $ do
        let funcNameStr = case maybeFuncCtx of
                Just funcCtx -> funcName funcCtx
                Nothing -> "<unknown function>"
        logIt level "->WARNING: Amount field detected:" [fieldInfo, "in function:", funcNameStr]
        liftIO $ modifyIORef errorsRef (insertFieldIntoErrors moduleName funcNameStr [fromMaybe "" mFound])

containsAmountFields :: [String] -> String -> Maybe String
containsAmountFields keywords str = find (`isInfixOf` str) keywords

insertFieldIntoErrors
  :: String
  -> String
  -> [String]
  -> HM.HashMap String (HM.HashMap String [String])
  -> HM.HashMap String (HM.HashMap String [String])
insertFieldIntoErrors moduleName functionName amountFields =
    HM.insertWith mergeModule moduleName (HM.singleton functionName amountFields)
  where
    mergeModule new old =
        HM.unionWith (++) old new

processRecField :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM ()
processRecField level moduleName errorsRef oldErrors maybeFuncCtx (L _ recField) = do
    let fieldName = hsRecFieldId recField
        fieldNameStr = showSDocUnsafe $ ppr fieldName
        fieldArg = hsRecFieldArg recField
        fieldArgStr = showSDocUnsafe $ ppr fieldArg
    logIt level "processRecField" [fieldNameStr, "=", fieldArgStr]
    processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx fieldArg

processRecUpdField :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> LHsRecUpdField GhcTc -> TcM ()
processRecUpdField level moduleName errorsRef oldErrors maybeFuncCtx (L _ recUpdField) = do
    let fieldOcc = hsRecUpdFieldOcc recUpdField
        fieldNameStr = showSDocUnsafe $ ppr fieldOcc
        fieldArg = hsRecFieldArg recUpdField
        fieldArgStr = showSDocUnsafe $ ppr fieldArg
    logIt level "processRecUpdField" [fieldNameStr, "=", fieldArgStr]
    processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx fieldArg

processLocalBinds :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> HsLocalBinds GhcTc -> TcM ()
processLocalBinds level moduleName errorsRef oldErrors _ binds = do
    case binds of
#if __GLASGOW_HASKELL__ >= 900
        HsValBinds _ (ValBinds _ valBinds _sigs) -> do
            logIt level "Processing value bindings" ([])
            mapM_ (myLookOverLHsBindLR (level + 2) moduleName errorsRef oldErrors) (bagToList valBinds)
        HsValBinds _ (XValBindsLR (NValBinds binds_ _sigs)) -> do
            logIt level "Processing NValBinds" ([])
            mapM_ (mapM_ (myLookOverLHsBindLR (level + 2) moduleName errorsRef oldErrors) . bagToList . snd) binds_
#else
        HsValBinds (ValBindsIn valBinds _sigs) -> do
            logIt level "Processing value bindings" ([])
            mapM_ (myLookOverLHsBindLR (level + 2) moduleName errorsRef oldErrors) (bagToList valBinds)
        HsValBinds (ValBindsOut binds_ _sigs) -> do
            logIt level "Processing ValBindsOut" ([])
            mapM_ (mapM_ (myLookOverLHsBindLR (level + 2) moduleName errorsRef oldErrors) . bagToList . snd) binds_
#endif
        HsIPBinds _ _ -> do
            logIt level "Implicit parameter bindings (not processing)" ([])
        _ -> pure ()

-- Process a statement (used in HsDo, ListComp, ParComp, etc.)
processStmt :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> LStmt GhcTc (LHsExpr GhcTc) -> TcM ()
processStmt level moduleName errorsRef oldErrors maybeFuncCtx (L _ stmt) = do
    case stmt of
        LastStmt _ body _ _ -> do
            logIt level "LastStmt" ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx body
        BindStmt _ _pat expr_ -> do
            logIt level "BindStmt (pat <- expr)" ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        BodyStmt _ expr_ _ _ -> do
            logIt level "BodyStmt" ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        LetStmt _ binds -> do
            logIt level "LetStmt" ([])
            processLocalBinds (level + 2) moduleName errorsRef oldErrors maybeFuncCtx binds
        ApplicativeStmt _ args _ -> do
            logIt level "ApplicativeStmt" ([])
            mapM_ processApplicativeArg args
          where
            processApplicativeArg (_, arg) = do
                case arg of
                    ApplicativeArgOne _ _pat expr_ _ -> processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
                    ApplicativeArgMany _ stmts_ _ _pat _ -> mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) stmts_
                    XApplicativeArg{} -> pure ()
        ParStmt _ blocks _ _ -> do
            logIt level "ParStmt" ([])
            mapM_ processParStmtBlock blocks
          where
            processParStmtBlock (ParStmtBlock _ stmts_ _ _) = mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) stmts_
#if __GLASGOW_HASKELL__ < 900
            processParStmtBlock (XParStmtBlock{}) = pure ()
#endif
        TransStmt{} -> do
            logIt level "TransStmt - list comprehension transformer" ([])
        RecStmt _ stmts_ _ _ _ _ _ -> do
            logIt level "RecStmt - recursive do" ([])
            mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (unLoc stmts_)
        XStmtLR{} -> do
            logIt level "XStmtLR - extension" ([])

processExpr :: Int -> String -> IORef (HM.HashMap String (HM.HashMap String [String])) -> (HM.HashMap String (HM.HashMap String [String])) -> Maybe FunctionContext -> LHsExpr GhcTc -> TcM ()
processExpr level moduleName errorsRef oldErrors maybeFuncCtx expr = do
    logIt level "processExpr: Analyzing expression" ([])
    case unLoc expr of
        HsVar _ (L _ var) -> do
            case maybeFuncCtx of
                Just funcCtx -> do
                    let name = varName var
                    let funcNameStr = funcName funcCtx
                    let occStr = occNameString (nameOccName name)
                    case nameModule_maybe name of
                        Just modl  -> do
                            let modStr = moduleNameString (GHC.Unit.Module.moduleName modl)
                            if checkModuleFunctionInOldErrors oldErrors modStr occStr
                                then case getFuncWithModule name of
                                        Just errFuncName -> liftIO $ modifyIORef errorsRef (insertFieldIntoErrors moduleName funcNameStr [errFuncName])
                                        Nothing          -> pure ()
                            else pure ()
                        Nothing -> pure ()
                Nothing -> pure ()
            logIt level "HsVar:" [fromMaybe "" $ getFuncWithModule (varName var), "used here", (show $ funcName <$> maybeFuncCtx)]
        HsUnboundVar _xUnbound unboundVar -> do
            logIt level "HsUnboundVar:" [showSDocUnsafe $ ppr unboundVar]
#if __GLASGOW_HASKELL__ >= 900
        HsConLikeOut _xConLike conLike -> do
            logIt level "HsConLikeOut >= 900:" [showSDocUnsafe $ ppr conLike]
#else
        HsConLikeOut conLike -> do
            logIt level "HsConLikeOut < 900:" [showSDocUnsafe $ ppr conLike] 
#endif
        HsRecFld _xRecFld ambiguousFieldOcc -> do
            logIt level "HsRecFld:" [showSDocUnsafe $ ppr ambiguousFieldOcc]
            case ambiguousFieldOcc of
                Unambiguous _ fieldName -> do
                    let fieldNameStr = showSDocUnsafe $ ppr fieldName
                    checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx fieldNameStr
                Ambiguous _ fieldName -> do
                    let fieldNameStr = showSDocUnsafe $ ppr fieldName
                    checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx fieldNameStr
                XAmbiguousFieldOcc _ ->
                    logIt level "->XAmbiguousFieldOcc (extension)" ([])
        HsOverLit _xOverLit overLit -> do 
            logIt level "HsOverLit:" [showSDocUnsafe $ ppr overLit] 
        HsLit _xLit lit -> do 
            logIt level "HsLit:" [showSDocUnsafe $ ppr lit] 
        HsApp _xApp func arg -> do
            logIt level "HsApp: func =" [showSDocUnsafe $ ppr func, ", arg =", showSDocUnsafe $ ppr arg] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx func
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx arg
        HsAppType _xAppType expr_ typ -> do
            let typStr = showSDocUnsafe $ ppr typ
            logIt level "HsAppType:" [showSDocUnsafe $ ppr expr_, "@", typStr]
            checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx ("@" ++ typStr)
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#if __GLASGOW_HASKELL__ < 900
        OpApp _xOpApp left op right -> do 
            logIt level "OpApp < 900:" [showSDocUnsafe $ ppr left, showSDocUnsafe $ ppr op, showSDocUnsafe $ ppr right] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx left
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx op
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx right
#endif
#if __GLASGOW_HASKELL__ >= 900
        HsLam _xLam matchGroup -> do 
            logIt level "HsLam >= 900:" [] 
#else
        HsLam _xLam matchGroup -> do 
            logIt level "HsLam < 900:" [showSDocUnsafe $ ppr matchGroup] 
#endif
        HsLamCase _xLamCase matchGroup -> do
            logIt level "HsLamCase - lambda case expression" ([])
            let matches = mg_alts matchGroup
            mapM_ (handleLMatchList (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (unLoc matches)
        HsLet _xLet binds expr -> do
            logIt level "HsLet - let expression with local bindings" ([])
            
            processLocalBinds (level + 2) moduleName errorsRef oldErrors maybeFuncCtx binds
            
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr
        HsIf _xIf condExpr thenExpr elseExpr -> do
            logIt level "HsIf - if-then-else expression" ([])
            
            logIt level "Processing then branch..." ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx thenExpr
            logIt level "Processing else branch..." ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx elseExpr
        HsMultiIf _xMultiIf guards -> do
            logIt level "HsMultiIf - multi-way if expression" ([])
            mapM_ processGuardedRHS guards
          where
            processGuardedRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> TcM ()
            processGuardedRHS (L _ (GRHS _ guardStmts body)) = do
                logIt level "Processing guard alternative..." ([])
                
                processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx body
        HsCase _xCase expr_ matchGroup -> do
            logIt level "HsCase - case expression" ([])
            logIt level "Processing scrutinee expression..." ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
            logIt level "Processing case alternatives..." ([])
            let matches = mg_alts matchGroup
            mapM_ (handleLMatchList (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (unLoc matches)
        HsDo _xDo context stmts -> do
            logIt level "HsDo - do notation:" [showSDocUnsafe $ ppr context]
            mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (unLoc stmts)
        ExplicitList _xList exprs -> do
            logIt level "ExplicitList - explicit list literal" ([])
            mapM_ (processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) exprs
        ExplicitTuple _xTuple args boxity -> do
            logIt level "ExplicitTuple - tuple literal:" ([])
            mapM_ processTupleArg args
          where
            processTupleArg :: HsTupArg GhcTc -> TcM ()
            processTupleArg arg = do
                case arg of
                    Present _ expr -> processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr
                    Missing _ -> pure ()
                    XTupArg{} -> pure ()
#if __GLASGOW_HASKELL__ >= 900
        ExplicitSum _xSum alt arity expr_ -> do
            logIt level "ExplicitSum - unboxed sum type: alternative" [show alt, "of", show arity]
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#else
        ExplicitSum _xSum alt arity expr_ types -> do
            logIt level "ExplicitSum - unboxed sum type: alternative" [show alt, "of", show arity]
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        ListComp _xListComp expr_ stmts -> do
            logIt level "ListComp - list comprehension (GHC < 900)" ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
            mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) stmts
        ParComp _xParComp expr_ stmts -> do
            logIt level "ParComp - parallel list comprehension (GHC < 900)" ([])
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
            mapM_ (mapM_ (processStmt (level + 2) moduleName errorsRef oldErrors maybeFuncCtx)) stmts
#endif
#if __GLASGOW_HASKELL__ >= 900
        RecordCon _xRecordCon constructor fields -> do
            logIt level "RecordCon >= 900:" [showSDocUnsafe $ ppr constructor, showSDocUnsafe $ ppr fields]
            mapM_ (processRecField (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (rec_flds fields)
        RecordUpd _xRecordUpd expr_ fields -> do
            logIt level "RecordUpd >= 900:" [showSDocUnsafe $ ppr expr_, showSDocUnsafe $ ppr fields]
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
            case fields of
                Left regularFields -> mapM_ (processRecUpdField (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (reverse regularFields)
                Right _projFields -> logIt level "RecordUpd with projection syntax (not yet supported)" ([])
#else
        RecordCon _xRecordCon constructor fields -> do
            logIt level "RecordCon < 900:" [showSDocUnsafe $ ppr constructor, showSDocUnsafe $ ppr fields]
            mapM_ (processRecField (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (rec_flds fields)
        RecordUpd _xRecordUpd expr_ fields _cons _inTypes _outTypes -> do
            logIt level "RecordUpd < 900:" [showSDocUnsafe $ ppr expr_, showSDocUnsafe $ ppr fields]
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
            mapM_ (processRecUpdField (level + 2) moduleName errorsRef oldErrors maybeFuncCtx) (reverse fields)
#endif
        SectionL _xSectionL expr_ op -> do 
            logIt level "SectionL:" [showSDocUnsafe $ ppr expr_, showSDocUnsafe $ ppr op] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        SectionR _xSectionR op expr_ -> do 
            logIt level "SectionR:" [showSDocUnsafe $ ppr op, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        NegApp _xNegApp expr_ _ -> do 
            logIt level "NegApp:" [showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        HsPar _xPar expr_ -> do
            logIt level "HsPar:" [showSDocUnsafe $ ppr expr_]
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#if __GLASGOW_HASKELL__ >= 900
        ExprWithTySig _xExprWithTySig expr_ sig -> do 
            logIt level "ExprWithTySig >= 900:" [showSDocUnsafe $ ppr expr_, "::", showSDocUnsafe $ ppr sig] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#else
        ExprWithTySig _xExprWithTySig expr_ sig -> do 
            logIt level "ExprWithTySig < 900:" [showSDocUnsafe $ ppr expr_, "::", showSDocUnsafe $ ppr sig] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#endif
#if __GLASGOW_HASKELL__ >= 900
        HsStatic _xStatic expr_ -> do 
            logIt level "HsStatic >= 900:" [showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#else
        HsStatic _xStatic _fvs expr_ -> do 
            logIt level "HsStatic < 900:" [showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        HsProc _xProc pat cmd -> do 
            logIt level "HsProc < 900:" [showSDocUnsafe $ ppr pat, showSDocUnsafe $ ppr cmd] 
        HsArrApp _xArrApp expr1 expr2 _type arrApp -> do 
            logIt level "HsArrApp < 900:" [showSDocUnsafe $ ppr expr1, showSDocUnsafe $ ppr expr2] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr1
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr2
        HsArrForm _xArrForm expr_ _fixity cmds -> do 
            logIt level "HsArrForm < 900:" [showSDocUnsafe $ ppr expr_, showSDocUnsafe $ ppr cmds] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#endif
        HsTick _xTick tickish expr_ -> do 
            logIt level "HsTick:" [showSDocUnsafe $ ppr tickish, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        HsBinTick _xBinTick int1 int2 expr_ -> do
            logIt level "HsBinTick:" [show int1, show int2, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#if __GLASGOW_HASKELL__ >= 900
        HsPragE _xPragE pragma expr_ -> do 
            logIt level "HsPragE >= 900:" [showSDocUnsafe $ ppr pragma, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#else
        HsTickPragma _xTickPragma source triple triple2 expr_ -> do 
            logIt level "HsTickPragma < 900:" [showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        HsWrap _xWrap wrapper expr_ -> do 
            logIt level "HsWrap < 900:" [showSDocUnsafe $ ppr wrapper, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#endif
#if __GLASGOW_HASKELL__ >= 900
        HsBracket _xBracket bracket -> do 
            logIt level "HsBracket >= 900:" [showSDocUnsafe $ ppr bracket] 
        HsRnBracketOut _xRnBracket bracket _ -> do 
            logIt level "HsRnBracketOut >= 900:" [showSDocUnsafe $ ppr bracket] 
        HsTcBracketOut _xTcBracket maybe_pending bracket _ -> do 
            logIt level "HsTcBracketOut >= 900:" [showSDocUnsafe $ ppr bracket] 
        HsSpliceE _xSplice splice -> do 
            logIt level "HsSpliceE >= 900:" [showSDocUnsafe $ ppr splice]
#else
        HsBracket _xBracket bracket -> do 
            logIt level "HsBracket < 900:" [showSDocUnsafe $ ppr bracket] 
        HsRnBracketOut _xRnBracket bracket _ -> do 
            logIt level "HsRnBracketOut < 900:" [showSDocUnsafe $ ppr bracket] 
        HsTcBracketOut _xTcBracket bracket _ -> do 
            logIt level "HsTcBracketOut < 900:" [showSDocUnsafe $ ppr bracket] 
        HsSpliceE _xSplice splice -> do 
            logIt level "HsSpliceE < 900:" [showSDocUnsafe $ ppr splice] 
#endif
#if __GLASGOW_HASKELL__ < 900
        HsPragE _xPragE pragma expr_ -> do
            logIt level "HsPragE < 900:" [showSDocUnsafe $ ppr pragma, showSDocUnsafe $ ppr expr_] 
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#endif
        HsOverLabel _xOverLabel fastString -> do
            logIt level "HsOverLabel:" [showSDocUnsafe $ ppr fastString] 
        HsIPVar _xIPVar ipName -> do
            logIt level "HsIPVar:" [showSDocUnsafe $ ppr ipName] 
#if __GLASGOW_HASKELL__ >= 904
        HsGetField _xGetField expr_ fieldLbl -> do
            let fieldLblStr = showSDocUnsafe $ ppr fieldLbl
            logIt level "HsGetField:" [showSDocUnsafe $ ppr expr_, ".", fieldLblStr] 
            checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx fieldLblStr
            processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
        HsProjection _xProjection fieldLbls -> do
            let fieldLblsStr = showSDocUnsafe $ ppr fieldLbls
            logIt level "HsProjection:" [fieldLblsStr] 
            checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx fieldLblsStr
#endif
        ArithSeq _xArithSeq _ arithSeqInfo -> do
            logIt level "ArithSeq:" [showSDocUnsafe $ ppr arithSeqInfo] 
#if __GLASGOW_HASKELL__ >= 902
        XExpr xxExpr -> do
            case xxExpr of
                WrapExpr (HsWrap _ expr_) -> do
                    logIt level "XExpr (WrapExpr) - type coercion wrapper" ([])
                    processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx (noLocA expr_)
                ExpansionExpr (HsExpanded _original desugared) -> do
                    let desugaredStr = showSDocUnsafe $ ppr desugared
                    logIt level "XExpr (ExpansionExpr - HsExpanded):" [desugaredStr]
                    keywords <- liftIO $ getKeywords
                    let matchingKeywords = filter (`isInfixOf` desugaredStr) keywords
                    when (not (null matchingKeywords)) $ do
                        mapM_ (\keywrd -> checkAndAddAmountFieldError level moduleName errorsRef oldErrors maybeFuncCtx keywrd) matchingKeywords
                    pure ()
#endif
#if __GLASGOW_HASKELL__ >= 904
                ConLikeTc conLike _ _ -> do
                    logIt level "XExpr (ConLikeTc):" [showSDocUnsafe $ ppr conLike]
                HsTick tickish expr_ -> do
                    logIt level "XExpr (HsTick) - code coverage tick" ([])
                    processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
                HsBinTick int1 int2 expr_ -> do
                    logIt level "XExpr (HsBinTick) - binary code coverage tick:" [show int1, show int2]
                    processExpr (level + 2) moduleName errorsRef oldErrors maybeFuncCtx expr_
#endif
        _ -> do
            logIt level "Unknown expression type:" [showSDocUnsafe $ ppr expr]

readAmountCheckFile :: FilePath -> IO (Either String (HM.HashMap String (HM.HashMap String [String])))
readAmountCheckFile path = do
    (eBytes :: Either IOException BL.ByteString) <- try (BL.readFile path)
    case eBytes of
        Left err -> pure . Left $ (show err)
        Right bytes -> pure (eitherDecode bytes)

unionDeep :: HM.HashMap String (HM.HashMap String [String]) -> HM.HashMap String (HM.HashMap String [String]) -> HM.HashMap String (HM.HashMap String [String])
unionDeep =
  HM.unionWith (HM.unionWith unionList)
  where
    unionList xs ys = nub (xs ++ ys)