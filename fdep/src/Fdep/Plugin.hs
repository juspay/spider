{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts,NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns,DeriveAnyClass #-}

module Fdep.Plugin (plugin) where

import qualified Data.Aeson as A
import Bag (bagToList, listToBag)
import BasicTypes (FractionalLit (..), IntegralLit (..))
import Control.Concurrent
import Control.Exception (SomeException, try,evaluate)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (writeFile,toStrict)
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data ()
import Data.List
import Data.List (nub)
import Data.List.Extra (replace, splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import DynFlags ()
import Fdep.Types
import GHC (
    GRHS (..),
    hsmodDecls,
    GhcPs(..),
    FieldOcc(..),
    rdrNameAmbiguousFieldOcc,
    GRHSs (..),
    GenLocated (L),
    GhcPass,
    GhcTc,
    HsBindLR (..),
    HsConDetails (..),
    HsConPatDetails,
    HsExpr (..),
    HsRecField' (..),
    HsRecFields (..),
    HsValBinds (..),
    Id (..),
    IdP (..),
    LGRHS,
    LHsCmd (..),
    LHsExpr,
    LHsRecField,
    LHsRecUpdField (..),
    LMatch,
    LPat,
    Match (m_grhss),
    MatchGroup (..),
    Module (moduleName),
    Name,
    OutputableBndrId,
    OverLitVal (..),
    Pat (..),
    PatSynBind (..),
    StmtLR (..),
    TyCon,
    getName,
    moduleNameString,
    nameSrcSpan,
    noLoc,
    ol_val,
    ol_witness,
 )
import GHC.Hs.Binds
import GHC.Hs.Expr (unboundVarOcc)
import GHC.Hs.Utils as GHCHs
import GhcPlugins (hpm_module,Hsc,HsParsedModule,RdrName(..),rdrNameOcc,Plugin (..), PluginRecompile (..), Var (..), binderArgFlag, binderType, binderVars, elemNameSet, getOccString, idName, idType, nameSetElemsStable, ppr, pprPrefixName, pprPrefixOcc, showSDocUnsafe, tidyOpenType, tyConBinders, unLoc, unpackFS)
import HscTypes (ModSummary (..), typeEnvIds)
import Name (nameStableString,occName,occNameString,occNameSpace,occNameFS,pprNameSpaceBrief)
import Outputable ()
import PatSyn
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import SrcLoc
import Streamly
import Streamly.Prelude (drain, fromList, mapM, mapM_, toList)
import System.Directory
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import TcEnv
import TcRnTypes (TcGblEnv (..), TcM)
import TyCoPpr (pprSigmaType, pprTypeApp, pprUserForAll)
import TyCon
import Prelude hiding (id, mapM, mapM_, writeFile)
import GHC.Hs.Decls
import qualified Data.ByteString as DBS
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BL
import System.Environment (lookupEnv)
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.IO (unsafePerformIO)

plugin :: Plugin
plugin =
    defaultPlugin
        { typeCheckResultAction = fDep
        , pluginRecompile = purePlugin
        , parsedResultAction = collectDecls
        }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

filterList :: [Text]
filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
    , "showList"
    , "toConstr"
    , "toDomResAcc"
    , "toEncoding"
    , "toEncodingList"
    , "toEnum"
    , "toForm"
    , "toHaskellString"
    , "toInt"
    , "toJSON"
    , "toJSONList"
    , "toJSONWithOptions"
    , "encodeJSON"
    , "gfoldl"
    , "ghmParser"
    , "gmapM"
    , "gmapMo"
    , "gmapMp"
    , "gmapQ"
    , "gmapQi"
    , "gmapQl"
    , "gmapQr"
    , "gmapT"
    , "parseField"
    , "parseJSON"
    , "parseJSONList"
    , "parseJSONWithOptions"
    , "hasField"
    , "gunfold"
    , "getField"
    , "_mapObjectDeep'"
    , "_mapObjectDeep"
    , "_mapObjectDeepForSnakeCase"
    , "!!"
    , "/="
    , "<"
    , "<="
    , "<>"
    , "<$"
    , "=="
    , ">"
    , ">="
    , "readsPrec"
    , "readPrec"
    , "toDyn"
    , "fromDyn"
    , "fromDynamic"
    , "compare"
    , "readListPrec"
    , "toXml"
    , "fromXml"
    ]

collectDecls :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectDecls opts modSummary hsParsedModule =  do
    liftIO $
        forkIO $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fdep/"
                    local : _ -> local
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> ms_hspp_file modSummary
                path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                declsList = hsmodDecls $ unLoc $ hpm_module $ hsParsedModule
            createDirectoryIfMissing True path
            functionsVsCodeString <- toList $ parallely $ mapM (getDecls) $ fromList $ declsList
            writeFile ((modulePath) <> ".function_code.json") (encodePretty $ Map.fromList $ concat functionsVsCodeString)
    pure hsParsedModule

getDecls :: LHsDecl GhcPs -> IO [(Text,PFunction)]
getDecls x = do
    case x of
        (L _ (TyClD  _ _))   -> pure $ mempty
        (L _ (InstD  _ _))   -> pure $ mempty
        (L _ (DerivD _ _))   -> pure $ mempty
        (L _ (ValD   _ bind))-> pure $ getFunBind bind
        (L _ (SigD   _ _))   -> pure $ mempty
        _                    -> pure $ mempty
    where
        getFunBind f@(FunBind {fun_id=funId}) = [(((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ showSDocUnsafe $ ppr $ getLoc funId)),PFunction ((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ showSDocUnsafe $ ppr $ getLoc funId)) (T.pack $ showSDocUnsafe $ ppr f) (T.pack $ showSDocUnsafe $ ppr $ getLoc funId))]
        getFunBind _ = mempty

shouldLog :: Bool
shouldLog = readBool $ unsafePerformIO $ lookupEnv "ENABLE_LOGS"
    where
        readBool :: (Maybe String) -> Bool
        readBool (Just "true") = True
        readBool (Just "True") = True
        readBool (Just "TRUE") = True
        readBool _ = False

decodeBlacklistedFunctions :: IO [Text]
decodeBlacklistedFunctions  = do
    mBlackListedFunctions <- lookupEnv "BLACKLIST_FUNCTIONS_FDEP"
    pure $ case mBlackListedFunctions of
        Just val ->
            case A.decode $ BL.fromStrict $ encodeUtf8 $ (T.pack val) of
                Just (val) -> filterList <> val
                _ -> filterList
        _ -> filterList

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    liftIO $ do
        mEnableLogsEnvVar <- lookupEnv "ENABLE_LOGS"
        let prefixPath = case opts of
                [] -> "/tmp/fdep/"
                local : _ -> local
            moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
            modulePath = prefixPath <> ms_hspp_file modSummary
        let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
        when (shouldLog) $ print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
        let binds = bagToList $ tcg_binds tcEnv
        t1 <- getCurrentTime
        depsMapList <- runConcurrently $ traverse (Concurrently . loopOverLHsBindLR) binds
        createDirectoryIfMissing True path
        BL.writeFile ((modulePath) <> ".json") $ encodePretty $ HM.fromList $ map (\x -> (function_name x,x)) $ concat depsMapList
        -- functionVsUpdates <- getAllTypeManipulations binds
        -- writeFile ((modulePath) <> ".typeUpdates.json") (encodePretty $ functionVsUpdates)
        -- DBS.writeFile ((modulePath) <> ".json") =<< evaluate (toStrict $ encodePretty $ map (\(x,y) -> y) $ HM.toList $ HM.fromList $ map (\x -> (function_name x,x)) $ concat depsMapList)
        -- writeFile ((modulePath) <> ".missing.signatures.json")
        --     (encodePretty $
        --             Map.fromList $
        --                 map (\element -> (\(x, y) -> (x, typeSignature y)) $ filterForMaxLenTypSig element) $
        --                     groupBy (\a b -> (srcSpan a) == (srcSpan b)) $
        --                         dumpMissingTypeSignatures tcEnv)
        t2 <- getCurrentTime
        when (shouldLog) $ print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path <> " timetaken: " <> (show $ diffUTCTime t2 t1))
    return tcEnv
  where
    filterForMaxLenTypSig :: [MissingTopLevelBindsSignature] -> (Text, MissingTopLevelBindsSignature)
    filterForMaxLenTypSig x =
        case x of
            [el] -> (srcSpan $ el, el)
            [el1, el2] -> (srcSpan el1, bool (el2) (el1) ((T.length $ typeSignature $ el1) > (T.length $ typeSignature $ el2)))
            (xx : xs) -> (\(y, yy) -> (srcSpan xx, bool (yy) (xx) ((T.length $ typeSignature $ xx) > (T.length $ typeSignature $ yy)))) $ filterForMaxLenTypSig xs

getAllTypeManipulations :: [LHsBindLR GhcTc GhcTc] -> IO [DataTypeUC]
getAllTypeManipulations binds = do
    bindWiseUpdates <-
        toList $
            parallely $
                mapM
                    ( \x -> do
                        let functionName = getFunctionName x
                            filterRecordUpdateAndCon = filter (\x -> ((show $ toConstr x) `elem` ["RecordCon", "RecordUpd"])) (x ^? biplateRef :: [HsExpr GhcTc])
                        pure $ bool (Nothing) (Just (DataTypeUC functionName (mapMaybe getDataTypeDetails filterRecordUpdateAndCon))) (length filterRecordUpdateAndCon > 0)
                    )
                    (fromList binds)
    pure $ catMaybes bindWiseUpdates
  where
    getDataTypeDetails :: HsExpr GhcTc -> Maybe TypeVsFields
    getDataTypeDetails (RecordCon _ (L _ (iD)) rcon_flds) = Just (TypeVsFields (T.pack $ nameStableString $ getName $ idName iD) (extractRecordBinds (rcon_flds)))
    getDataTypeDetails (RecordUpd _ rupd_expr rupd_flds) = Just (TypeVsFields (T.pack $ showSDocUnsafe $ ppr rupd_expr) (getFieldUpdates rupd_flds))

    -- inferFieldType :: Name -> String
    inferFieldTypeFieldOcc (L _ (FieldOcc _ (L _ rdrName))) = handleRdrName rdrName
    inferFieldTypeAFieldOcc = (handleRdrName . rdrNameAmbiguousFieldOcc . unLoc)

    handleRdrName :: RdrName -> String
    handleRdrName x = 
        case x of 
            Unqual occName          ->               ("$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Qual moduleName occName -> ((moduleNameString moduleName) <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Orig module' occName    -> ((moduleNameString $ moduleName module') <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Exact name              -> nameStableString name

    getFieldUpdates :: [LHsRecUpdField GhcTc] -> [FieldRep]
    getFieldUpdates fields = map extractField fields
      where
        extractField :: LHsRecUpdField GhcTc -> FieldRep
        extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =
            if pun
                then (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ inferFieldTypeAFieldOcc lbl))
                else (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr (unLoc expr)) (T.pack $ inferFieldTypeAFieldOcc lbl))

    extractRecordBinds :: HsRecFields GhcTc (LHsExpr GhcTc) -> [FieldRep]
    extractRecordBinds (HsRecFields{rec_flds = fields}) =
        map extractField fields
      where
        extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> FieldRep
        extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =
            if pun
                then (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ inferFieldTypeFieldOcc lbl))
                else (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr $ unLoc expr) (T.pack $ inferFieldTypeFieldOcc lbl))

    getFunctionName :: LHsBindLR GhcTc GhcTc -> [Text]
    getFunctionName (L _ x@(FunBind fun_ext id matches _ _)) = [T.pack $ nameStableString $ getName id]
    getFunctionName (L _ (VarBind{var_id = var, var_rhs = expr, var_inline = inline})) = [T.pack $ nameStableString $ getName var]
    getFunctionName (L _ (PatBind{pat_lhs = pat, pat_rhs = expr})) = [""]
    getFunctionName (L _ (AbsBinds{abs_binds = binds})) = Prelude.concatMap getFunctionName $ bagToList binds

dumpMissingTypeSignatures :: TcGblEnv -> [MissingTopLevelBindsSignature]
dumpMissingTypeSignatures gbl_env =
    let binds = (collectHsBindsBinders $ tcg_binds $ gbl_env)
        whereBinds = Prelude.concatMap (\x -> ((Prelude.concatMap collectHsBindsBinders $ processHsLocalBindsForWhereFunctions $ unLoc $ processMatchForWhereFunctions x))) ((bagToList $ tcg_binds $ gbl_env) ^? biplateRef :: [LMatch GhcTc (LHsExpr GhcTc)])
     in nub $ mapMaybe add_bind_warn (binds <> whereBinds)
  where
    add_bind_warn :: Id -> Maybe MissingTopLevelBindsSignature
    add_bind_warn id =
        let name = idName id
            ty = (idType id)
            ty_msg = pprSigmaType ty
         in add_warn (T.pack $ showSDocUnsafe $ ppr $ nameSrcSpan $ getName name) (T.pack $ showSDocUnsafe $ pprPrefixName name) (showSDocUnsafe $ ppr $ ty_msg)

    add_warn "<no location info>" msg ty_msg = Nothing
    add_warn "<wired into compiler>" msg ty_msg = Nothing
    add_warn _ msg "*" = Nothing
    add_warn _ msg "* -> *" = Nothing
    add_warn _ msg ('_' : xs) = Nothing
    add_warn name msg ty_msg =
        if "$" `T.isPrefixOf` msg
            then Nothing
            else Just $ MissingTopLevelBindsSignature{srcSpan = (name), typeSignature = ((msg) <> " :: " <> (T.pack ty_msg))}

    processMatchForWhereFunctions :: LMatch GhcTc (LHsExpr GhcTc) -> LHsLocalBinds GhcTc
    processMatchForWhereFunctions (L _ match) = (grhssLocalBinds (m_grhss match))

    processHsLocalBindsForWhereFunctions :: HsLocalBindsLR GhcTc GhcTc -> [LHsBindsLR GhcTc GhcTc]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (ValBinds _ x _)) = [x]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (XValBindsLR (NValBinds x _))) = map (\(_, binds) -> binds) $ x
    processHsLocalBindsForWhereFunctions x = []

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args

filterFunctionInfos :: [Maybe FunctionInfo] -> IO [FunctionInfo]
filterFunctionInfos infos = do
    let grouped = groupBy (\info1 info2 -> src_Loc info1 == src_Loc info2 && name info1 == name info2) $ catMaybes infos
    pure $ Prelude.concatMap (filter (not . Prelude.null . arguments)) grouped

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ x@(FunBind fun_ext id matches _ _)) = do
    let funName = T.pack $ getOccString $ unLoc id
        matchList = mg_alts matches
        fName = T.pack $ nameStableString $ getName id
    if funName `elem` (unsafePerformIO $ decodeBlacklistedFunctions) || ("$_in$$" `T.isPrefixOf` fName)
        then pure mempty
        else do
            when (shouldLog) $ print ("processing function: " <> fName)
            t1 <- getCurrentTime
            (list, funcs) <- foldM
                (\(x, y) xx -> do
                    (l, f) <- processMatch xx
                    pure (x <> l, y <> f))
                ([], [])
                (unLoc matchList)
            listTransformed <- filterFunctionInfos $ map transformFromNameStableString list
            t2 <- getCurrentTime
            when (shouldLog) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
            pure [Function (funName <> "**" <> (T.pack $ showSDocUnsafe (ppr (getLoc id)))) (map Just listTransformed) (nub funcs) (T.pack $ showSDocUnsafe (ppr (getLoc id))) "" (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))]
loopOverLHsBindLR (L _ AbsBinds{abs_binds = binds}) = do
    list <- runConcurrently $ traverse (Concurrently . loopOverLHsBindLR) $ bagToList binds
    pure (concat list)
loopOverLHsBindLR (L _ VarBind{var_rhs = rhs}) = pure mempty
loopOverLHsBindLR (L _ (PatSynBind _ PSB{psb_def = def})) = pure mempty
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = pure mempty
loopOverLHsBindLR (L _ (XHsBindsLR _)) = pure mempty
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = pure mempty

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([(Maybe Text, Maybe Text, Maybe Text, [Text])], [Function])
processMatch (L _ match) = do
    whereClause <- processHsLocalBinds $ unLoc $ grhssLocalBinds (m_grhss match)
    r <- runConcurrently $ traverse (Concurrently . processGRHS) $ grhssGRHSs (m_grhss match)
    pure (Prelude.concat r, whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> IO [(Maybe Text, Maybe Text, Maybe Text, [Text])]
processGRHS (L _ (GRHS _ _ body)) =
    pure $ processExpr [] body
processGRHS _ = pure $ []

processHsLocalBinds :: HsLocalBindsLR GhcTc GhcTc -> IO [Function]
processHsLocalBinds (HsValBinds _ (ValBinds _ x y)) = do
    res <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ bagToList $ x
    pure $ Prelude.concat res
processHsLocalBinds (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
    res <-
        foldM
            ( \acc (recFlag, binds) -> do
                funcs <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ bagToList binds
                pure (acc <> funcs)
            )
            []
            x
    pure $ Prelude.concat res
processHsLocalBinds x =
    pure []

processArgs (funr) = case funr of
    (HsUnboundVar _ uv) -> [T.pack $ showSDocUnsafe $ pprPrefixOcc (unboundVarOcc uv)]
    (HsConLikeOut _ c) -> [T.pack $ showSDocUnsafe $ pprPrefixOcc c]
    (HsIPVar _ v) -> [T.pack $ showSDocUnsafe $ ppr v]
    (HsOverLabel _ _ l) -> [T.pack $ showSDocUnsafe $ ppr l]
    (HsLit _ lit) -> [T.pack $ showSDocUnsafe $ ppr lit]
    (HsOverLit _ lit) -> [T.pack $ showSDocUnsafe $ ppr lit]
    (HsPar _ e) -> [T.pack $ showSDocUnsafe $ ppr e]
    (HsApp _ funl funr) -> processArgs (unLoc funr) <> processArgs (unLoc funl)
    _ -> []

processExpr :: [Text] -> LHsExpr GhcTc -> [(Maybe Text, Maybe Text, Maybe Text, [Text])]
processExpr arguments x@(L _ (HsVar _ (L _ var))) =
    let name = T.pack $ nameStableString $ varName var
        _type = T.pack $ showSDocUnsafe $ ppr $ varType var
     in [(Just name, Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ x, Just _type, arguments)]
processExpr arguments (L _ (HsUnboundVar _ _)) = []
processExpr arguments (L _ (HsApp _ funl funr)) =
    let processedArgs = nub $ processArgs (unLoc funr) <> arguments
        l = processExpr processedArgs funl
        r = processExpr arguments funr
     in l <> r
processExpr arguments (L _ (OpApp _ funl funm funr)) =
    let l = processExpr arguments funl
        m = processExpr arguments funm
        r = processExpr arguments funr
     in nub $ l <> m <> r
processExpr arguments (L _ (NegApp _ funl _)) =
    processExpr arguments funl
processExpr arguments (L _ (HsTick _ _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ (HsStatic _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ x@(HsWrap _ _ fun)) =
    let r = processArgs x
     in processExpr (arguments <> r) (noLoc fun)
processExpr arguments (L _ (HsBinTick _ _ _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ (ExplicitList _ _ funList)) =
    Prelude.concatMap (processExpr arguments) funList
processExpr arguments (L _ (HsTickPragma _ _ _ _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ (HsSCC _ _ _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ (HsCoreAnn _ _ _ fun)) =
    processExpr arguments fun
processExpr arguments (L _ (ExprWithTySig _ fun _)) =
    processExpr arguments fun
processExpr arguments (L _ (HsDo _ _ exprLStmt)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in nub $
            Prelude.concatMap
                ( \x ->
                    let processedArgs = processArgs (unLoc x)
                     in processExpr (processedArgs) x
                )
                stmts
processExpr arguments (L _ (HsLet _ exprLStmt func)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in processExpr arguments func <> nub (Prelude.concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsMultiIf _ exprLStmt)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in nub (Prelude.concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsIf _ exprLStmt funl funm funr)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) $ [funl, funm, funr] <> stmts)
processExpr arguments (L _ (HsCase _ funl exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) $ [funl] <> stmts)
processExpr arguments (L _ (ExplicitSum _ _ _ fun)) = processExpr arguments fun
processExpr arguments (L _ (SectionR _ funl funr)) = processExpr arguments funl <> processExpr arguments funr
processExpr arguments (L _ (ExplicitTuple _ exprLStmt _)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsPar _ fun)) =
    let processedArgs = processArgs (unLoc fun)
     in processExpr (processedArgs) fun
processExpr arguments (L _ (HsAppType _ fun _)) = processExpr arguments fun
processExpr arguments (L _ x@(HsLamCase _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
        processedArgs = processArgs (x)
        res =
            nub
                ( Prelude.concatMap
                    ( \x ->
                        let processedArgs = processArgs (unLoc x)
                         in processExpr (processedArgs) x
                    )
                    stmts
                )
     in case res of
            [(x, y, t, [])] -> [(x, y, t, processedArgs)]
            _ -> res
processExpr arguments (L _ x@(HsLam _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
        processedArgs = processArgs (x)
        res =
            nub
                ( Prelude.concatMap
                    ( \x ->
                        let processedArgs = processArgs (unLoc x)
                         in processExpr (processedArgs <> arguments) x
                    )
                    stmts
                )
     in case res of
            [(x, y, t, [])] -> [(x, y, t, processedArgs)]
            _ -> res
processExpr arguments y@(L _ x@(HsLit _ hsLit)) =
    [(Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr hsLit), [])]
processExpr arguments y@(L _ x@(HsOverLit _ overLitVal)) =
    [(Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr overLitVal), [])]
processExpr arguments (L _ (HsRecFld _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ Nothing exprLStmtR)) =
    let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) stmtsR)
processExpr arguments (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (Prelude.concatMap (processExpr arguments) (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr arguments (L _ (RecordCon _ (L _ (iD)) rcon_flds)) =
    let stmts = (rcon_flds ^? biplateRef :: [LHsExpr GhcTc])
    in nub (Prelude.concatMap (processExpr arguments) stmts)
    -- extractRecordBinds (rcon_flds)
-- processExpr arguments (L _ (RecordUpd _ rupd_expr rupd_flds)) = (processExpr arguments rupd_expr) <> Prelude.concatMap extractLHsRecUpdField rupd_flds
processExpr arguments (L _ (RecordUpd _ rupd_expr rupd_flds)) =
    let stmts = (rupd_flds ^? biplateRef :: [LHsExpr GhcTc])
    in nub (Prelude.concatMap (processExpr arguments) stmts)
    -- Just (TypeVsFields (showSDocUnsafe $ ppr rupd_expr) (getFieldUpdates rupd_flds))
processExpr arguments _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(Maybe Text, Maybe Text, Maybe Text, [Text])]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr [] fun

processPat :: LPat GhcTc -> [(Name, Maybe Text)]
processPat (L _ pat) = case pat of
    ConPatIn _ details -> processDetails details
    VarPat _ x@(L _ var) -> [(varName var, Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ x)]
    ParPat _ pat' -> processPat pat'
    _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name, Maybe Text)]
processDetails (PrefixCon args) = Prelude.concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = Prelude.concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name, Maybe Text)]
processPatField (L _ HsRecField{hsRecFieldArg = arg}) = processPat arg
