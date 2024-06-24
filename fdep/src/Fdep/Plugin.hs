{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Fdep.Plugin (plugin) where

import Annotations
import Avail
import Bag (bagToList, listToBag)
import BasicTypes (FractionalLit (..), IntegralLit (..))
import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (writeFile)
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
import GhcPlugins (RdrName(..),rdrNameOcc,Plugin (pluginRecompile), PluginRecompile (..), Var (..), binderArgFlag, binderType, binderVars, elemNameSet, getOccString, idName, idType, nameSetElemsStable, ppr, pprPrefixName, pprPrefixOcc, showSDocUnsafe, tidyOpenType, tyConBinders, unLoc, unpackFS)
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

plugin :: Plugin
plugin =
    defaultPlugin
        { typeCheckResultAction = fDep
        , pluginRecompile = purePlugin
        }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
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

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    liftIO $
        forkIO $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fdep/"
                    local : _ -> local
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> ms_hspp_file modSummary
            when True $ do
                let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
                let binds = bagToList $ tcg_binds tcEnv
                depsMapList <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ binds
                functionVsUpdates <- getAllTypeManipulations binds
                createDirectoryIfMissing True path
                writeFile ((modulePath) <> ".typeUpdates.json") $ (encodePretty $ functionVsUpdates)
                writeFile ((modulePath) <> ".json") (encodePretty $ concat depsMapList)
                writeFile ((modulePath) <> ".missing.signatures.json") $
                    encodePretty $
                        Map.fromList $
                            map (\element -> (\(x, y) -> (x, typeSignature y)) $ filterForMaxLenTypSig element) $
                                groupBy (\a b -> (srcSpan a) == (srcSpan b)) $
                                    dumpMissingTypeSignatures tcEnv
                print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path)
    return tcEnv
  where
    filterForMaxLenTypSig :: [MissingTopLevelBindsSignature] -> (String, MissingTopLevelBindsSignature)
    filterForMaxLenTypSig x =
        case x of
            [el] -> (srcSpan $ el, el)
            [el1, el2] -> (srcSpan el1, bool (el2) (el1) ((length $ typeSignature $ el1) > (length $ typeSignature $ el2)))
            (xx : xs) -> (\(y, yy) -> (srcSpan xx, bool (yy) (xx) ((length $ typeSignature $ xx) > (length $ typeSignature $ yy)))) $ filterForMaxLenTypSig xs

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
    getDataTypeDetails (RecordCon _ (L _ (iD)) rcon_flds) = Just (TypeVsFields (nameStableString $ getName $ idName iD) (extractRecordBinds (rcon_flds)))
    getDataTypeDetails (RecordUpd _ rupd_expr rupd_flds) = Just (TypeVsFields (showSDocUnsafe $ ppr rupd_expr) (getFieldUpdates rupd_flds))

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
                then (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr lbl) (inferFieldTypeAFieldOcc lbl))
                else (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr (unLoc expr)) (inferFieldTypeAFieldOcc lbl))

    extractRecordBinds :: HsRecFields GhcTc (LHsExpr GhcTc) -> [FieldRep]
    extractRecordBinds (HsRecFields{rec_flds = fields}) =
        map extractField fields
      where
        extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> FieldRep
        extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =
            if pun
                then (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr lbl) (inferFieldTypeFieldOcc lbl))
                else (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr $ unLoc expr) (inferFieldTypeFieldOcc lbl))

    getFunctionName :: LHsBindLR GhcTc GhcTc -> [String]
    getFunctionName (L _ x@(FunBind fun_ext id matches _ _)) = [nameStableString $ getName id]
    getFunctionName (L _ (VarBind{var_id = var, var_rhs = expr, var_inline = inline})) = [nameStableString $ getName var]
    getFunctionName (L _ (PatBind{pat_lhs = pat, pat_rhs = expr})) = [""]
    getFunctionName (L _ (AbsBinds{abs_binds = binds})) = concatMap getFunctionName $ bagToList binds

dumpMissingTypeSignatures :: TcGblEnv -> [MissingTopLevelBindsSignature]
dumpMissingTypeSignatures gbl_env =
    let binds = (collectHsBindsBinders $ tcg_binds $ gbl_env)
        whereBinds = concatMap (\x -> ((concatMap collectHsBindsBinders $ processHsLocalBindsForWhereFunctions $ unLoc $ processMatchForWhereFunctions x))) ((bagToList $ tcg_binds $ gbl_env) ^? biplateRef :: [LMatch GhcTc (LHsExpr GhcTc)])
     in nub $ mapMaybe add_bind_warn (binds <> whereBinds)
  where
    add_bind_warn :: Id -> Maybe MissingTopLevelBindsSignature
    add_bind_warn id =
        let name = idName id
            ty = (idType id)
            ty_msg = pprSigmaType ty
         in add_warn (showSDocUnsafe $ ppr $ nameSrcSpan $ getName name) (showSDocUnsafe $ pprPrefixName name) (showSDocUnsafe $ ppr $ ty_msg)

    add_warn "<no location info>" msg ty_msg = Nothing
    add_warn "<wired into compiler>" msg ty_msg = Nothing
    add_warn _ msg "*" = Nothing
    add_warn _ msg "* -> *" = Nothing
    add_warn _ msg ('_' : xs) = Nothing
    add_warn name msg ty_msg =
        if "$" `isPrefixOf` msg
            then Nothing
            else Just $ MissingTopLevelBindsSignature{srcSpan = (name), typeSignature = (msg <> " :: " <> ty_msg)}

    processMatchForWhereFunctions :: LMatch GhcTc (LHsExpr GhcTc) -> LHsLocalBinds GhcTc
    processMatchForWhereFunctions (L _ match) = (grhssLocalBinds (m_grhss match))

    processHsLocalBindsForWhereFunctions :: HsLocalBindsLR GhcTc GhcTc -> [LHsBindsLR GhcTc GhcTc]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (ValBinds _ x _)) = [x]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (XValBindsLR (NValBinds x _))) = map (\(_, binds) -> binds) $ x
    processHsLocalBindsForWhereFunctions x = []

transformFromNameStableString :: (Maybe String, Maybe String, Maybe String, [String]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args

filterFunctionInfos :: [Maybe FunctionInfo] -> IO [Maybe FunctionInfo]
filterFunctionInfos infos = do
    let grouped = groupBy (\info1 info2 -> src_Loc info1 == src_Loc info2 && name info1 == name info2) $ catMaybes infos
    pure $
        map (Just) $
            concat $
                map
                    ( \group ->
                        if length group == 1
                            then group
                            else concat $ map (\x -> if (null $ arguments x) then [] else [x]) group
                    )
                    $ grouped

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ x@(FunBind fun_ext id matches _ _)) = do
    let funName = getOccString $ unLoc id
        matchList = mg_alts matches
        fName = nameStableString $ getName id
    if ((funName) `elem` filterList || (("$_in$$" `isPrefixOf` fName) && (not $ "$_in$$sel:" `isPrefixOf` fName )))
        then pure []
        else do
            (list, funcs) <-
                foldM
                    ( \(x, y) xx -> do
                        (l, f) <- processMatch xx
                        pure $ (x <> l, y <> f)
                    )
                    ([], [])
                    (unLoc matchList)
            listTransformed <- filterFunctionInfos $ map transformFromNameStableString list
            pure [(Function funName listTransformed (nub funcs) (showSDocUnsafe $ ppr $ getLoc id) (showSDocUnsafe $ ppr x) (showSDocUnsafe $ ppr $ varType $ unLoc id))]
loopOverLHsBindLR x@(L _ VarBind{var_rhs = rhs}) = do
    pure [(Function "" (map transformFromNameStableString $ processExpr [] rhs) [] "" (showSDocUnsafe $ ppr x) "")]
loopOverLHsBindLR x@(L _ AbsBinds{abs_binds = binds}) = do
    list <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ bagToList binds
    pure (concat list)
loopOverLHsBindLR x@(L _ (PatSynBind _ PSB{psb_def = def})) = do
    let list = map transformFromNameStableString $ map (\(n, srcLoc) -> (Just $ nameStableString n, srcLoc,Nothing, [])) $ processPat def
    pure [(Function "" list [] "" (showSDocUnsafe $ ppr x) "")]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
    pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
    pure []
loopOverLHsBindLR x@(L _ (PatBind _ _ pat_rhs _)) = do
    r <- toList $ parallely $ mapM processGRHS $ fromList $ grhssGRHSs pat_rhs
    let l = map transformFromNameStableString $ concat r
    pure [(Function "" l [] "" (showSDocUnsafe $ ppr x) "")]

-- checkIfCreateOrUpdtingDataTypes binds = mapM_ (go) (fromList $ bagToList binds)
--     where
--         go (L _ (FunBind fun_ext id matches _ _)) = do

--             pure ()
--         go (L _ VarBind{var_rhs = rhs}) = pure ()
--         go (L _ AbsBinds{abs_binds = binds}) = pure ()
--         go (L _ (PatSynBind _ PSB{psb_def = def})) = pure ()
--         go (L _ (PatSynBind _ (XPatSynBind _))) = pure ()

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([(Maybe String, Maybe String, Maybe String, [String])], [Function])
processMatch (L _ match) = do
    -- let stmts = match ^? biplateRef :: [StmtLR GhcTc GhcTc (LHsExpr GhcTc)]
    -- mapM (print . showSDocUnsafe . ppr) stmts
    -- let stmtsCMD = match ^? biplateRef :: [StmtLR GhcTc GhcTc (LHsCmd GhcTc)]
    -- mapM (print . showSDocUnsafe . ppr) stmtsCMD
    whereClause <- processHsLocalBinds $ unLoc $ grhssLocalBinds (m_grhss match)
    -- let names = map (\x -> (Just (nameStableString x), Just $ showSDocUnsafe $ ppr $ getLoc $ x, mempty)) $ (match ^? biplateRef :: [Name])
    r <- toList $ parallely $ (mapM processGRHS (fromList $ grhssGRHSs (m_grhss match)))
    pure $ (concat r, whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> IO [(Maybe String, Maybe String, Maybe String, [String])]
processGRHS (L _ (GRHS _ _ body)) = do
    pure $ processExpr [] body
processGRHS _ = pure $ []

processHsLocalBinds :: HsLocalBindsLR GhcTc GhcTc -> IO [Function]
processHsLocalBinds (HsValBinds _ (ValBinds _ x y)) = do
    res <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ bagToList $ x
    pure $ concat res
processHsLocalBinds (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
    res <-
        foldM
            ( \acc (recFlag, binds) -> do
                funcs <- toList $ parallely $ mapM loopOverLHsBindLR $ fromList $ bagToList binds
                pure (acc <> funcs)
            )
            []
            x
    pure $ concat res
processHsLocalBinds x =
    pure []

processArgs (funr) = case funr of
    (HsUnboundVar _ uv) -> [showSDocUnsafe $ pprPrefixOcc (unboundVarOcc uv)]
    (HsConLikeOut _ c) -> [showSDocUnsafe $ pprPrefixOcc c]
    (HsIPVar _ v) -> [showSDocUnsafe $ ppr v]
    (HsOverLabel _ _ l) -> [showSDocUnsafe $ ppr l]
    (HsLit _ lit) -> [showSDocUnsafe $ ppr lit]
    (HsOverLit _ lit) -> [showSDocUnsafe $ ppr lit]
    (HsPar _ e) -> [showSDocUnsafe $ ppr e]
    (HsApp _ funl funr) -> processArgs (unLoc funr) <> processArgs (unLoc funl)
    _ -> []

processExpr :: [String] -> LHsExpr GhcTc -> [(Maybe String, Maybe String, Maybe String, [String])]
processExpr arguments x@(L _ (HsVar _ (L _ var))) =
    let name = nameStableString $ varName var
        _type = showSDocUnsafe $ ppr $ varType var
     in [(Just name, Just $ showSDocUnsafe $ ppr $ getLoc $ x, Just _type, arguments)]
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
    concatMap (processExpr arguments) funList
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
            concatMap
                ( \x ->
                    let processedArgs = processArgs (unLoc x)
                     in processExpr (processedArgs) x
                )
                stmts
processExpr arguments (L _ (HsLet _ exprLStmt func)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in processExpr arguments func <> nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsMultiIf _ exprLStmt)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsIf _ exprLStmt funl funm funr)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) $ [funl, funm, funr] <> stmts)
processExpr arguments (L _ (HsCase _ funl exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) $ [funl] <> stmts)
processExpr arguments (L _ (ExplicitSum _ _ _ fun)) = processExpr arguments fun
processExpr arguments (L _ (SectionR _ funl funr)) = processExpr arguments funl <> processExpr arguments funr
processExpr arguments (L _ (ExplicitTuple _ exprLStmt _)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsPar _ fun)) =
    let processedArgs = processArgs (unLoc fun)
     in processExpr (processedArgs) fun
processExpr arguments (L _ (HsAppType _ fun _)) = processExpr arguments fun
processExpr arguments (L _ x@(HsLamCase _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
        processedArgs = processArgs (x)
        res =
            nub
                ( concatMap
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
                ( concatMap
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
    [(Just $ ("$_lit$" <> (showSDocUnsafe $ ppr hsLit)), (Just $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ show $ toConstr hsLit), [])]
processExpr arguments y@(L _ x@(HsOverLit _ overLitVal)) =
    [(Just $ ("$_lit$" <> (showSDocUnsafe $ ppr overLitVal)), (Just $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ show $ toConstr overLitVal), [])]
processExpr arguments (L _ (HsRecFld _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ Nothing exprLStmtR)) =
    let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) stmtsR)
processExpr arguments (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr arguments (L _ (RecordCon _ (L _ (iD)) rcon_flds)) =
    let stmts = (rcon_flds ^? biplateRef :: [LHsExpr GhcTc])
    in nub (concatMap (processExpr arguments) stmts)
    -- extractRecordBinds (rcon_flds)
-- processExpr arguments (L _ (RecordUpd _ rupd_expr rupd_flds)) = (processExpr arguments rupd_expr) <> concatMap extractLHsRecUpdField rupd_flds
processExpr arguments (L _ (RecordUpd _ rupd_expr rupd_flds)) =
    let stmts = (rupd_flds ^? biplateRef :: [LHsExpr GhcTc])
    in nub (concatMap (processExpr arguments) stmts)
    -- Just (TypeVsFields (showSDocUnsafe $ ppr rupd_expr) (getFieldUpdates rupd_flds))
processExpr arguments _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(Maybe String, Maybe String, Maybe String, [String])]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr [] fun

processPat :: LPat GhcTc -> [(Name, Maybe String)]
processPat (L _ pat) = case pat of
    ConPatIn _ details -> processDetails details
    VarPat _ x@(L _ var) -> [(varName var, Just $ showSDocUnsafe $ ppr $ getLoc $ x)]
    ParPat _ pat' -> processPat pat'
    _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name, Maybe String)]
processDetails (PrefixCon args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name, Maybe String)]
processPatField (L _ HsRecField{hsRecFieldArg = arg}) = processPat arg
