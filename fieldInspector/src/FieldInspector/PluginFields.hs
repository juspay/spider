{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FieldInspector.PluginFields (plugin) where

import Bag (bagToList)
import Control.Exception (evaluate)
import Control.Reference (biplateRef, (^?))
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass))
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (toStrict)
import Data.Data (Data (toConstr))
import Data.Generics.Uniplate.Data ()
import Data.List (sortBy)
import Data.List.Extra (groupBy, intercalate, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import FieldInspector.Types (
    DataConInfo (..),
    DataTypeUC (DataTypeUC),
    FieldRep (FieldRep),
    FieldUsage (FieldUsage),
    TypeInfo (..),
    TypeVsFields (TypeVsFields),
 )
import GHC.Hs (
    ConDecl (
        ConDeclGADT,
        ConDeclH98,
        con_args,
        con_name,
        con_names,
        con_res_ty
    ),
    ConDeclField (ConDeclField),
    FieldOcc (FieldOcc),
    GhcPs,
    GhcTc,
    HsBindLR (
        AbsBinds,
        FunBind,
        PatBind,
        VarBind,
        abs_binds,
        pat_lhs,
        pat_rhs,
        var_id,
        var_inline,
        var_rhs
    ),
    HsConDeclDetails,
    HsConDetails (InfixCon, PrefixCon, RecCon),
    HsConPatDetails,
    HsDataDefn (dd_cons),
    HsDecl (TyClD),
    HsExpr (RecordCon, RecordUpd),
    HsModule (hsmodDecls),
    HsRecField' (HsRecField, hsRecFieldArg, hsRecFieldLbl, hsRecPun),
    HsRecFields (HsRecFields, rec_flds),
    LConDecl,
    LHsBindLR,
    LHsDecl,
    LHsExpr,
    LHsRecField,
    LHsRecUpdField,
    LPat,
    Pat (ConPatIn, ParPat, VarPat),
    TyClDecl (DataDecl, SynDecl),
    rdrNameAmbiguousFieldOcc,
 )
import GhcPlugins (
    CommandLineOption,
    HsParsedModule (..),
    Hsc,
    ModGuts (mg_binds, mg_loc),
    ModSummary (..),
    Module (moduleName),
    Name,
    NamedThing (getName),
    Outputable (..),
    Plugin (..),
    RdrName (Exact, Orig, Qual, Unqual),
    SDoc,
    Var,
    dataConName,
    dataConTyCon,
    defaultPlugin,
    idName,
    moduleNameString,
    nameStableString,
    purePlugin,
    showSDocUnsafe,
    tyConKind,
    tyConName,
    tyVarKind,
    unpackFS,
 )
import Name (OccName (occNameFS, occNameSpace), occNameString, pprNameSpaceBrief)
import SrcLoc (
    GenLocated (L),
    RealSrcSpan (srcSpanFile),
    SrcSpan (..),
    getLoc,
    unLoc,
 )
import Streamly (parallely)
import Streamly.Prelude (fromList, mapM, toList)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude (
    catMaybes,
    catch,
    forkIO,
    isDoesNotExistError,
    on,
    throwIO,
 )
import TcRnMonad (MonadIO (liftIO))
import TcRnTypes (TcGblEnv (tcg_binds), TcM)
import TyCoRep (Type (AppTy, FunTy, TyConApp, TyVarTy))
import Var (varName, varType)
import Prelude hiding (id, mapM, mapM_)

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = GhcPlugins.purePlugin
        , typeCheckResultAction = collectTypesTC
        }
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "FieldInspector" (buildCfgPass args) : todos)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcEnv = do
    _ <- liftIO $
        forkIO $
            do
                let prefixPath = case opts of
                        [] -> "/tmp/fieldInspector/"
                        local : _ -> local
                    modulePath = prefixPath <> ms_hspp_file modSummary
                    path = (intercalate "/" . init . splitOn "/") modulePath
                    binds = bagToList $ tcg_binds tcEnv
                createDirectoryIfMissing True path
                functionVsUpdates <- getAllTypeManipulations binds
                DBS.writeFile ((modulePath) <> ".typeUpdates.json") (toStrict $ encodePretty functionVsUpdates)
    return tcEnv

buildCfgPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
            [] -> "./tmp/fieldInspector/"
            [local] -> local
            _ -> error "unexpected no of arguments"
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
        createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
        removeIfExists (moduleLoc Prelude.<> ".fieldUsage.json")
        l <- toList $ parallely $ mapM (liftIO . toLBind) (fromList binds)
        DBS.writeFile (moduleLoc Prelude.<> ".fieldUsage.json") =<< (evaluate $ toStrict $ encodePretty $ Map.fromList $ groupByFunction $ Prelude.concat l)
    return guts

getAllTypeManipulations :: [LHsBindLR GhcTc GhcTc] -> IO [DataTypeUC]
getAllTypeManipulations binds = do
    bindWiseUpdates <-
        toList $
            parallely $
                mapM
                    ( \x -> do
                        let functionName = getFunctionName x
                            filterRecordUpdateAndCon = Prelude.filter (\x -> ((show $ toConstr x) `Prelude.elem` ["RecordCon", "RecordUpd"])) (x ^? biplateRef :: [HsExpr GhcTc])
                        pure $ bool (Nothing) (Just (DataTypeUC functionName (Data.Maybe.mapMaybe getDataTypeDetails filterRecordUpdateAndCon))) (not (Prelude.null filterRecordUpdateAndCon))
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
            Unqual occName -> ("$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Qual moduleName occName -> ((moduleNameString moduleName) <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Orig module' occName -> ((moduleNameString $ moduleName module') <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
            Exact name -> nameStableString name

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

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs

--   1. `HasField _ r _` where r is a variable

--   2. `HasField _ (T ...) _` if T is a data family
--      (because it might have fields introduced later)

--   3. `HasField x (T ...) _` where x is a variable,
--       if T has any fields at all

--   4. `HasField "foo" (T ...) _` if T has a "foo" field
processHasField :: Text -> Expr Var -> Expr Var -> IO [(Text, [FieldUsage])]
processHasField functionName b@(App (App (App getField (Type fieldName)) (Type haskellType@(TyConApp haskellTypeT _))) (Type finalFieldType)) hasField =
    pure [(functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr haskellType) (pack $ showSDocUnsafe $ ppr fieldName) (pack $ showSDocUnsafe $ ppr finalFieldType) (pack $ nameStableString $ GhcPlugins.tyConName haskellTypeT) (pack $ showSDocUnsafe $ ppr b)])]
processHasField functionName b@(App (App (App getField (Type fieldName)) (Type haskellType)) (Type finalFieldType)) hasField =
    pure [(functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr haskellType) (pack $ showSDocUnsafe $ ppr fieldName) (pack $ showSDocUnsafe $ ppr finalFieldType) (pack $ show $ toConstr haskellType) (pack $ showSDocUnsafe $ ppr b)])]
processHasField functionName (Var x) (Var hasField) = do
    res <- pure mempty
    let b = pack $ showSDocUnsafe $ ppr x
        lensString = T.replace "\n" "" $ pack $ showSDocUnsafe $ ppr x
        parts =
            if ((Prelude.length (T.splitOn " @ " lensString)) >= 2)
                then []
                else words $ T.unpack $ T.replace "\t" "" $ T.replace "\n" "" $ T.strip (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField)
    case tyVarKind hasField of
        (TyConApp haskellTypeT z) -> do
            let y = map (\(zz) -> (pack $ showSDocUnsafe $ ppr zz, pack $ extractVarFromType zz)) z
            if length y == 4
                then
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (T.strip $ fst $ y Prelude.!! 2)
                                            (T.strip $ fst $ y Prelude.!! 1)
                                            (T.strip $ fst $ y Prelude.!! 3)
                                            (T.strip $ snd $ y Prelude.!! 2)
                                            lensString
                                       ]
                                   )
                               ]
                else
                    if length y == 3
                        then
                            pure $
                                res
                                    <> [
                                           ( functionName
                                           ,
                                               [ FieldUsage
                                                    (T.strip $ fst $ y Prelude.!! 1)
                                                    (T.strip $ fst $ y Prelude.!! 0)
                                                    (T.strip $ fst $ y Prelude.!! 2)
                                                    (T.strip $ snd $ y Prelude.!! 1)
                                                    lensString
                                               ]
                                           )
                                       ]
                        else do
                            print y
                            pure res
        (FunTy _ a _) -> do
            let fieldType = T.strip $ Prelude.last $ T.splitOn "->" $ pack $ showSDocUnsafe $ ppr $ varType hasField
            pure $
                res
                    <> [
                           ( functionName
                           ,
                               [ FieldUsage
                                    (pack $ showSDocUnsafe $ ppr $ varType x)
                                    (pack $ showSDocUnsafe $ ppr hasField)
                                    fieldType
                                    (pack $ extractVarFromType $ varType x)
                                    b
                               ]
                           )
                       ]
        (TyVarTy a) -> do
            let fieldType = T.strip $ Prelude.last $ T.splitOn "->" $ pack $ showSDocUnsafe $ ppr $ varType hasField
            pure $
                res
                    <> [
                           ( functionName
                           ,
                               [ FieldUsage
                                    (pack $ showSDocUnsafe $ ppr $ varType x)
                                    (pack $ showSDocUnsafe $ ppr hasField)
                                    fieldType
                                    (pack $ extractVarFromType $ varType x)
                                    b
                               ]
                           )
                       ]
        _ -> do
            case parts of
                ["HasField", fieldName, dataType, fieldType] ->
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (pack dataType)
                                            (pack $ init (Prelude.tail fieldName))
                                            (pack fieldType)
                                            "" --(pack $ show $ toConstr $ haskellType)
                                            b
                                       ]
                                   )
                               ]
                ("HasField" : fieldName : dataType : fieldTypeRest) ->
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (pack dataType)
                                            (pack fieldName)
                                            (pack $ unwords fieldTypeRest)
                                            "" --(pack $ show $ toConstr $ haskellType)
                                            b
                                       ]
                                   )
                               ]
                _ -> do
                    print (T.strip (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField))
                    pure res
processHasField functionName x (Var hasField) = do
    res <- toLexpr functionName x
    let b = pack $ showSDocUnsafe $ ppr x
        lensString = T.replace "\n" "" $ pack $ showSDocUnsafe $ ppr x
        parts =
            if ((Prelude.length (T.splitOn " @ " lensString)) >= 2)
                then []
                else words $ T.unpack $ T.replace "\t" "" $ T.replace "\n" "" $ T.strip (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField)
    case tyVarKind hasField of
        (TyConApp haskellTypeT z) -> do
            let y = map (\(zz) -> (pack $ showSDocUnsafe $ ppr zz, pack $ extractVarFromType zz)) z
            if length y == 4
                then
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (T.strip $ fst $ y Prelude.!! 2)
                                            (T.strip $ fst $ y Prelude.!! 1)
                                            (T.strip $ fst $ y Prelude.!! 3)
                                            (T.strip $ snd $ y Prelude.!! 2)
                                            lensString
                                       ]
                                   )
                               ]
                else
                    if length y == 3
                        then
                            pure $
                                res
                                    <> [
                                           ( functionName
                                           ,
                                               [ FieldUsage
                                                    (T.strip $ fst $ y Prelude.!! 1)
                                                    (T.strip $ fst $ y Prelude.!! 0)
                                                    (T.strip $ fst $ y Prelude.!! 2)
                                                    (T.strip $ snd $ y Prelude.!! 1)
                                                    lensString
                                               ]
                                           )
                                       ]
                        else do
                            print y
                            pure res
        _ -> do
            case parts of
                ["HasField", fieldName, dataType, fieldType] ->
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (pack dataType)
                                            (pack $ init (Prelude.tail fieldName))
                                            (pack fieldType)
                                            "" --(pack $ show $ toConstr $ haskellType)
                                            b
                                       ]
                                   )
                               ]
                ("HasField" : fieldName : dataType : fieldTypeRest) ->
                    pure $
                        res
                            <> [
                                   ( functionName
                                   ,
                                       [ FieldUsage
                                            (pack dataType)
                                            (pack fieldName)
                                            (pack $ unwords fieldTypeRest)
                                            "" --(pack $ show $ toConstr $ haskellType)
                                            b
                                       ]
                                   )
                               ]
                _ -> do
                    print (T.strip (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField))
                    pure res

groupByFunction :: [(Text, [FieldUsage])] -> [(Text, [FieldUsage])]
groupByFunction = map mergeGroups . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
  where
    mergeGroups :: [(Text, [FieldUsage])] -> (Text, [FieldUsage])
    mergeGroups xs = (fst (Prelude.head xs), concatMap snd xs)

toLBind :: CoreBind -> IO [(Text, [FieldUsage])]
toLBind (NonRec binder expr) = do
    res <- toLexpr (pack $ nameStableString $ idName binder) expr
    pure $ groupByFunction res
toLBind (Rec binds) = do
    r <-
        toList $
            parallely $
                mapM
                    ( \(b, e) -> do
                        toLexpr (pack $ nameStableString (idName b)) e
                    )
                    (fromList binds)
    pure $ groupByFunction $ Prelude.concat r

processFieldExtraction :: Text -> Var -> Var -> Text -> IO [(Text, [FieldUsage])]
processFieldExtraction functionName _field _type b = do
    res <- case (varType _field) of
        (FunTy _ a _) -> do
            let fieldType = T.strip $ Prelude.last $ T.splitOn "->" $ pack $ showSDocUnsafe $ ppr $ varType _field
            pure
                [
                    ( functionName
                    ,
                        [ FieldUsage
                            (pack $ showSDocUnsafe $ ppr $ varType _type)
                            (pack $ showSDocUnsafe $ ppr _field)
                            fieldType
                            (pack $ extractVarFromType $ varType _type)
                            b
                        ]
                    )
                ]
        (TyConApp haskellTypeT z) -> do
            let y = map (\(zz) -> (pack $ showSDocUnsafe $ ppr zz, pack $ extractVarFromType zz)) z
            if length y == 4
                then
                    pure $
                        [
                            ( functionName
                            ,
                                [ FieldUsage
                                    (T.strip $ fst $ y Prelude.!! 2)
                                    (T.strip $ fst $ y Prelude.!! 1)
                                    (T.strip $ fst $ y Prelude.!! 3)
                                    (T.strip $ snd $ y Prelude.!! 2)
                                    b
                                ]
                            )
                        ]
                else
                    if length y == 3
                        then
                            pure $
                                [
                                    ( functionName
                                    ,
                                        [ FieldUsage
                                            (T.strip $ fst $ y Prelude.!! 1)
                                            (T.strip $ fst $ y Prelude.!! 0)
                                            (T.strip $ fst $ y Prelude.!! 2)
                                            (T.strip $ snd $ y Prelude.!! 1)
                                            b
                                        ]
                                    )
                                ]
                        else do
                            print y
                            pure mempty
        _ -> pure mempty
    pure $ res

extractVarFromType :: Type -> String
extractVarFromType = go
  where
    go :: Type -> String
    go (TyVarTy v) = (nameStableString $ varName v)
    go (TyConApp haskellTypeT z) = (nameStableString $ GhcPlugins.tyConName haskellTypeT)
    go (AppTy a b) = go a <> "," <> go b
    go _ = mempty

toLexpr :: Text -> Expr Var -> IO [(Text, [FieldUsage])]
toLexpr functionName (Var x) = pure mempty
toLexpr functionName (Lit x) = pure mempty
toLexpr functionName (Type _id) = pure mempty
toLexpr functionName x@(App func@(App _ _) args@(Var isHasField))
    | "$_sys$$dHasField" == pack (nameStableString $ idName isHasField) =
        processHasField functionName func args
    | otherwise = do
        processApp functionName x
toLexpr functionName x@(App func@(Var _field) args@(Var _type)) = do
    processFieldExtraction functionName _field _type (pack $ showSDocUnsafe $ ppr x)
toLexpr functionName x@(App _ _) = processApp functionName x
toLexpr functionName (Lam func args) =
    toLexpr functionName args
toLexpr functionName (Let func args) = do
    a <- toLexpr functionName args
    f <- toLBind func
    pure $ map (\(x, y) -> (functionName, y)) f <> a
toLexpr functionName (Case condition bind _type alts) = do
    c <- toLexpr functionName condition
    a <- toList $ parallely $ mapM (toLAlt functionName) (fromList alts)
    pure $ c <> Prelude.concat a
toLexpr functionName (Tick _ expr) = toLexpr functionName expr
toLexpr functionName (Cast expr _) = toLexpr functionName expr
toLexpr functionName _ = pure mempty

processApp functionName x@(App func args) = do
    f <- toLexpr functionName func
    a <- toLexpr functionName args
    pure $ f <> a

toLAlt :: Text -> (AltCon, [Var], CoreExpr) -> IO [(Text, [FieldUsage])]
toLAlt functionName (DataAlt dataCon, val, e) = do
    let typeName = GhcPlugins.tyConName $ GhcPlugins.dataConTyCon dataCon
        extractingConstruct = showSDocUnsafe $ ppr $ GhcPlugins.dataConName dataCon
        kindStr = showSDocUnsafe $ ppr $ tyConKind $ GhcPlugins.dataConTyCon dataCon
    res <- toLexpr functionName e
    pure $ ((map (\x -> (functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr $ typeName) (pack $ extractingConstruct) (pack $ showSDocUnsafe $ ppr $ varType x) (pack $ nameStableString $ typeName) (pack $ showSDocUnsafe $ ppr x)])) val)) <> res
toLAlt functionName (LitAlt lit, val, e) =
    toLexpr functionName e
toLAlt functionName (DEFAULT, val, e) =
    toLexpr functionName e

pprTyCon :: Name -> SDoc
pprTyCon = ppr

pprDataCon :: Name -> SDoc
pprDataCon = ppr

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    _ <- liftIO $
        do
            let prefixPath = case opts of
                    [] -> "/tmp/fieldInspector/"
                    local : _ -> local
                moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                modulePath = prefixPath <> ms_hspp_file modSummary
                hm_module = unLoc $ hpm_module hpm
                path = (intercalate "/" . init . splitOn "/") modulePath
            -- print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
            types <- toList $ parallely $ mapM (pure . getTypeInfo) (fromList $ hsmodDecls hm_module)
            createDirectoryIfMissing True path
            DBS.writeFile (modulePath <> ".types.parser.json") (toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
    -- print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    return hpm

getTypeInfo :: LHsDecl GhcPs -> [(String, TypeInfo)]
getTypeInfo (L _ (TyClD _ (DataDecl _ lname _ _ defn))) =
    [
        ( showSDocUnsafe (ppr lname)
        , TypeInfo
            { name = showSDocUnsafe (ppr lname)
            , typeKind = "data"
            , dataConstructors = map getDataConInfo (dd_cons defn)
            }
        )
    ]
getTypeInfo (L _ (TyClD _ (SynDecl _ lname _ _ rhs))) =
    [
        ( showSDocUnsafe (ppr lname)
        , TypeInfo
            { name = showSDocUnsafe (ppr lname)
            , typeKind = "type"
            , dataConstructors = [DataConInfo (showSDocUnsafe (ppr lname)) (Map.singleton "synonym" (showSDocUnsafe (ppr rhs))) []]
            }
        )
    ]
getTypeInfo _ = []

getDataConInfo :: LConDecl GhcPs -> DataConInfo
getDataConInfo (L _ ConDeclH98{con_name = lname, con_args = args}) =
    DataConInfo
        { dataConNames = showSDocUnsafe (ppr lname)
        , fields = getFieldMap args
        , sumTypes = [] -- For H98-style data constructors, sum types are not applicable
        }
getDataConInfo (L _ ConDeclGADT{con_names = lnames, con_res_ty = ty}) =
    DataConInfo
        { dataConNames = intercalate ", " (map (showSDocUnsafe . ppr) lnames)
        , fields = Map.singleton "gadt" (showSDocUnsafe (ppr ty))
        , sumTypes = [] -- For GADT-style data constructors, sum types can be represented by the type itself
        }

getFieldMap :: HsConDeclDetails GhcPs -> Map String String
getFieldMap (PrefixCon args) = Map.fromList $ Prelude.zipWith (\i t -> (show i, showSDocUnsafe (ppr t))) [1 ..] args
getFieldMap (RecCon (L _ fields)) = Map.fromList $ concatMap getRecField fields
  where
    getRecField (L _ (ConDeclField _ fnames t _)) = [(showSDocUnsafe (ppr fname), showSDocUnsafe (ppr t)) | L _ fname <- fnames]
getFieldMap (InfixCon t1 t2) = Map.fromList [("field1", showSDocUnsafe (ppr t1)), ("field2", showSDocUnsafe (ppr t2))]
