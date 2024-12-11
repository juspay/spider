{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}

module FieldInspector.PluginFields (plugin) where


#if __GLASGOW_HASKELL__ >= 900
import qualified Data.IntMap.Internal as IntMap
import Streamly.Internal.Data.Stream (fromList,mapM_,mapM,toList)
import GHC
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..))
import GHC as GhcPlugins
import GHC.Core.DataCon as GhcPlugins
import GHC.Core.TyCon as GhcPlugins
import GHC.Core.TyCo.Rep
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Unit.Module.ModGuts
import GHC.Types.Name.Reader
import GHC.Types.Id
import GHC.Data.FastString

#else
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
    mkStringLit
 )
import TyCoRep
import GHC.IO (unsafePerformIO)
import GHC.Hs
import GHC.Hs.Decls
import GhcPlugins (
    CommandLineOption,Arg (..),
    HsParsedModule(..),
    Hsc,
    Name,SDoc,DataCon,DynFlags,ModSummary(..),TyCon,
    Literal (..),typeEnvElts,
    ModGuts (mg_binds, mg_loc, mg_module),showSDoc,
    Module (moduleName),tyConKind,
    NamedThing (getName),getDynFlags,tyConDataCons,dataConOrigArgTys,dataConName,
    Outputable (..),dataConFieldLabels,
    Plugin (..),
    Var,flLabel,dataConRepType,
    coVarDetails,
    defaultPlugin,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDocUnsafe,
    tyVarKind,
    unpackFS,
    tyConName,
    msHsFilePath
 )
import Id (isExportedId,idType)
import Name (getSrcSpan)
import SrcLoc
import Unique (mkUnique)
import Var (isLocalId,varType)
import FieldInspector.Types
import TcRnTypes
import TcRnMonad
import DataCon
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass))
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
 )
import Bag (bagToList)
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
    PluginRecompile(..),
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
    msHsFilePath,
 )
import Name (OccName (occNameFS, occNameSpace), occNameString, pprNameSpaceBrief)
import SrcLoc (
    GenLocated (L),
    RealSrcSpan (srcSpanFile),
    SrcSpan (..),
    getLoc,
    unLoc,
 )
import TcRnMonad (MonadIO (liftIO))
import TcRnTypes (TcGblEnv (tcg_binds), TcM)
import TyCoRep (Type (AppTy, FunTy, TyConApp, TyVarTy))
import Var (varName, varType)
#endif

import FieldInspector.Types
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import Data.List ( sortBy, intercalate ,foldl')
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Map (Map)
import Data.Data
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)
-- import System.IO (writeFile)
import Control.Monad (forM)
import Streamly.Internal.Data.Stream hiding (concatMap, init, length, map, splitOn,foldl',intercalate)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_,log)
import Prelude hiding (id, mapM, mapM_,log)
import Control.Exception (evaluate)
import Control.Exception (evaluate)
import Control.Reference (biplateRef, (^?))
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
import Streamly.Internal.Data.Stream (fromList, mapM, toList)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude (
    catMaybes,
    catch,
    forkIO,
    isDoesNotExistError,
    on,
    throwIO,
 )
import Prelude hiding (id, mapM, mapM_,log)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Network.WebSockets as WS
import Network.Socket (withSocketsDo)
import Text.Read (readMaybe)
import GHC.IO (unsafePerformIO)



plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = (\_ -> return NoForceRecompile)
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

websocketPort :: Maybe Int
websocketPort = maybe Nothing (readMaybe) $ unsafePerformIO $ lookupEnv "SERVER_PORT"

websocketHost :: Maybe String
websocketHost = unsafePerformIO $ lookupEnv "SERVER_HOST"


sendFileToWebSocketServer :: CliOptions -> Text -> _ -> IO ()
sendFileToWebSocketServer cliOptions path data_ =
    withSocketsDo $ do
        eres <- try $
            WS.runClient
                (fromMaybe (host cliOptions) websocketHost)
                (fromMaybe (port cliOptions) websocketPort)
                (T.unpack path)
                (\conn -> do
                    res <- try $ WS.sendTextData conn data_
                    case res of
                        Left (err :: SomeException) ->
                            when (log cliOptions) $ print err
                        Right _ -> pure ()
                )
        case eres of
            Left (err :: SomeException) ->
                when (log cliOptions) $ print err
            Right _ -> pure ()

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcEnv = do
    _ <- liftIO $
            forkIO $
                do
                    let cliOptions = case opts of
                                        [] ->  defaultCliOptions
                                        (local : _) ->
                                                    case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                                        Just (val :: CliOptions) -> val
                                                        Nothing -> defaultCliOptions
                        modulePath = (path cliOptions) <> msHsFilePath modSummary
                        -- path = (intercalate "/" . init . splitOn "/") modulePath
                        binds = bagToList $ tcg_binds tcEnv
                    -- createDirectoryIfMissing True path
                    functionVsUpdates <- getAllTypeManipulations binds
                    sendFileToWebSocketServer cliOptions (T.pack $ "/" <> (modulePath) <> ".typeUpdates.json") (decodeUtf8 $ toStrict $ encodePretty functionVsUpdates)
                    -- DBS.writeFile ((modulePath) <> ".typeUpdates.json") (toStrict $ encodePretty functionVsUpdates)
    return tcEnv

-- default options
-- "{\"path\":\"/tmp/fdep/\",\"port\":9898,\"host\":\"localhost\",\"log\":true}"
defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {path="/tmp/fieldInspector/",port=9898,host="localhost",log=False,tc_funcs=Just False}

buildCfgPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) ->
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleLoc = (path cliOptions) Prelude.<> getFilePath (mg_loc guts)
        -- createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
        -- removeIfExists (moduleLoc Prelude.<> ".fieldUsage.json")
        l <- toList $ mapM (liftIO . toLBind) (fromList binds)
        res <- pure $ Prelude.filter (\(x,y) -> (Prelude.not $ Prelude.null y) && (Prelude.not $ ("$$" :: Text) `T.isInfixOf` x)) $ groupByFunction $ Prelude.concat l
        when (Prelude.not $ Prelude.null res) $
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> moduleLoc Prelude.<> ".fieldUsage.json") (decodeUtf8 $ toStrict $ encodePretty $ Map.fromList $ res)
            -- DBS.writeFile (moduleLoc Prelude.<> ".fieldUsage.json") =<< (evaluate $ toStrict $ encodePretty $ Map.fromList $ res)
    return guts

getAllTypeManipulations :: [LHsBindLR GhcTc GhcTc] -> IO [DataTypeUC]
getAllTypeManipulations binds = do
    bindWiseUpdates <-
        toList $
                mapM
                    ( \x -> do
                        let functionName = getFunctionName x
                            filterRecordUpdateAndCon = Prelude.filter (\x -> ((show $ toConstr x) `Prelude.elem` ["HsGetField","RecordCon", "RecordUpd"])) (x ^? biplateRef :: [HsExpr GhcTc])
                        pure $ bool (Nothing) (Just (DataTypeUC functionName (Data.Maybe.mapMaybe getDataTypeDetails filterRecordUpdateAndCon))) (not (Prelude.null filterRecordUpdateAndCon))
                    )
                    (fromList binds)
    pure $ System.Directory.Internal.Prelude.catMaybes bindWiseUpdates
  where
    getDataTypeDetails :: HsExpr GhcTc -> Maybe TypeVsFields
#if __GLASGOW_HASKELL__ >= 900
    getDataTypeDetails (RecordCon _ (iD) rcon_flds) = Just (TypeVsFields (T.pack $ nameStableString $ getName (GHC.unXRec @(GhcTc) iD)) (extractRecordBinds (rcon_flds)))
#else
    getDataTypeDetails (RecordCon _ (iD) rcon_flds) = Just (TypeVsFields (T.pack $ nameStableString $ getName $ idName $ unLoc $ iD) (extractRecordBinds (rcon_flds)))
#endif
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

#if __GLASGOW_HASKELL__ >= 900
    getFieldUpdates :: Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> Either [FieldRep] [Text]
    getFieldUpdates fields = 
        case fields of
           Left x -> (Left . map (extractField . unLoc)) x 
           Right x -> (Right . map (T.pack . showSDocUnsafe . ppr)) x
      where
        extractField :: HsRecUpdField GhcTc -> FieldRep
        extractField (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun}) =
            if pun
                then (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ inferFieldTypeAFieldOcc lbl))
                else (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr (unLoc expr)) (T.pack $ inferFieldTypeAFieldOcc lbl))
#else
    getFieldUpdates :: [LHsRecUpdField GhcTc]-> Either [FieldRep] [Text]
    getFieldUpdates fields = Left $ map extractField fields
      where
        extractField :: LHsRecUpdField GhcTc -> FieldRep
        extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =
            if pun
                then (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ inferFieldTypeAFieldOcc lbl))
                else (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr (unLoc expr)) (T.pack $ inferFieldTypeAFieldOcc lbl))
#endif

    extractRecordBinds :: HsRecFields GhcTc (LHsExpr GhcTc) -> Either [FieldRep] [Text]
    extractRecordBinds (HsRecFields{rec_flds = fields}) =
        Left $ map extractField fields
      where
        extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> FieldRep
        extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =
            if pun
                then (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ inferFieldTypeFieldOcc lbl))
                else (FieldRep (T.pack $ showSDocUnsafe $ ppr lbl) (T.pack $ showSDocUnsafe $ ppr $ unLoc expr) (T.pack $ inferFieldTypeFieldOcc lbl))

    getFunctionName :: LHsBindLR GhcTc GhcTc -> [Text]
#if __GLASGOW_HASKELL__ >= 900
    getFunctionName (L _ x@(FunBind fun_ext id matches _)) = [T.pack $ nameStableString $ getName id]
#else
    getFunctionName (L _ x@(FunBind fun_ext id matches _ _)) = [T.pack $ nameStableString $ getName id]
#endif
    getFunctionName (L _ (VarBind{var_id = var, var_rhs = expr})) = [T.pack $ nameStableString $ getName var]
    getFunctionName (L _ (PatBind{pat_lhs = pat, pat_rhs = expr})) = [""]
    getFunctionName (L _ (AbsBinds{abs_binds = binds})) = Prelude.concatMap getFunctionName $ bagToList binds

processPat :: LPat GhcTc -> [(Name, Maybe Text)]
processPat (L _ pat) = case pat of
#if __GLASGOW_HASKELL__ >= 900
    ConPat _ _ details -> processDetails details
#else
    ConPatIn _ details -> processDetails details
#endif
    VarPat _ x@(L _ var) -> [(varName var, Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ x)]
    ParPat _ pat' -> processPat pat'
    _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name, Maybe Text)]
#if __GLASGOW_HASKELL__ >= 900
processDetails (PrefixCon _ args) = Prelude.concatMap processPat args
#else
processDetails (PrefixCon args) = Prelude.concatMap processPat args
#endif
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = Prelude.concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name, Maybe Text)]
processPatField (L _ HsRecField{hsRecFieldArg = arg}) = processPat arg

#if __GLASGOW_HASKELL__ >= 900
getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan _) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = showSDocUnsafe $ ppr $ fs
#else
getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs
#endif

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
                            pure res
#if __GLASGOW_HASKELL__ >= 900
        (FunTy _ _ a _) -> do
#else
        (FunTy _ a _) -> do
#endif
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
                    pure res

groupByFunction :: [(Text, [FieldUsage])] -> [(Text, [FieldUsage])]
groupByFunction = filter' . map mergeGroups . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
  where
    mergeGroups :: [(Text, [FieldUsage])] -> (Text, [FieldUsage])
    mergeGroups xs = (fst (Prelude.head xs), concatMap snd xs)

primitivePackages = ["aeson","aeson-better-errors","aeson-casing","aeson-diff","aeson-pretty","amazonka","amazonka-kms","async","attoparsec","authenticate-oauth","base","base16","base16-bytestring","base64-bytestring","basement","beam-core","beam-mysql","binary","blaze-builder","byteable","bytestring","case-insensitive","cassava","cipher-aes","containers","country","cryptohash","cryptonite","cryptonite-openssl","cryptostore","currency-codes","data-default","digest","dns","double-conversion","errors","extra","generic-arbitrary","generic-random","hedis","HsOpenSSL","HTTP","http-api-data","http-client","http-client-tls","http-media","http-types","iso8601-time","jose-jwt","json","jwt","lucid","memory","mtl","mysql","neat-interpolation","network-uri","newtype-generics","optics-core","QuickCheck","quickcheck-text","random","record-dot-preprocessor","record-hasfield","reflection","regex-compat","regex-pcre","regex-pcre","relude","resource-pool","RSA","safe","safe-exceptions","scientific","sequelize","servant","servant-client","servant-client-core","servant-server","split","streamly-core","streamly-serialize-instances","string-conversions","tagsoup","text","time","transformers","unix","unix-time","unordered-containers","utf8-string","uuid","vector","wai","wai-extra","warp","x509","x509-store","xeno","xml-conduit","xmlbf","xmlbf-xeno","ghc-prim","reflection","time","base","servant-client-core","reflection","servant-server","http-types","containers","unordered-containers"]

filter' :: [(Text, [FieldUsage])] -> [(Text, [FieldUsage])]
filter' [] = []
filter' li =
    map (\(funName,fieldsList) ->
            (funName,Prelude.filter (\(FieldUsage typeName fieldName fieldType typeSrcLoc beautifiedCode) ->
                (Prelude.not (Prelude.any (\x -> x `T.isInfixOf` typeSrcLoc) primitivePackages) && (typeName /= fieldName) && (fieldName /= fieldType))
                ) fieldsList)
        ) li

toLBind :: CoreBind -> IO [(Text, [FieldUsage])]
toLBind (NonRec binder expr) = do
    res <- toLexpr (pack $ nameStableString $ idName binder) expr
    pure $ filter' $ groupByFunction res
toLBind (Rec binds) = do
    r <-
        toList $
                mapM
                    ( \(b, e) -> do
                        toLexpr (pack $ nameStableString (idName b)) e
                    )
                    (fromList binds)
    pure $ filter' $ groupByFunction $ Prelude.concat r

processFieldExtraction :: Text -> Var -> Var -> Text -> IO [(Text, [FieldUsage])]
processFieldExtraction functionName _field _type b = do
    res <- case (varType _field) of
#if __GLASGOW_HASKELL__ >= 900
        (FunTy _ _ a _) -> do
#else
        (FunTy _ a _) -> do
#endif
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
    | "$_sys$$dHasField" == pack (nameStableString $ idName isHasField) = do
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
    a <- toList $ mapM (toLAlt functionName) (fromList alts)
    pure $ c <> Prelude.concat a
toLexpr functionName (Tick _ expr) = toLexpr functionName expr
toLexpr functionName (Cast expr _) = toLexpr functionName expr
toLexpr functionName _ = pure mempty

processApp functionName x@(App func args) = do
    f <- toLexpr functionName func
    a <- toLexpr functionName args
    pure $ f <> a

#if __GLASGOW_HASKELL__ >= 900
toLAlt :: Text -> Alt Var -> IO [(Text, [FieldUsage])]
toLAlt x (Alt a b c) = toLAlt' x (a,b,c)
    where
        toLAlt' :: Text -> (AltCon, [Var], CoreExpr) -> IO [(Text, [FieldUsage])]
        toLAlt' functionName (DataAlt dataCon, val, e) = do
            let typeName = GhcPlugins.tyConName $ GhcPlugins.dataConTyCon dataCon
                extractingConstruct = showSDocUnsafe $ ppr $ GhcPlugins.dataConName dataCon
                kindStr = showSDocUnsafe $ ppr $ tyConKind $ GhcPlugins.dataConTyCon dataCon
            res <- toLexpr functionName e
            pure $ ((map (\x -> (functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr $ typeName) (pack $ extractingConstruct) (pack $ showSDocUnsafe $ ppr $ varType x) (pack $ nameStableString $ typeName) (pack $ showSDocUnsafe $ ppr x)])) val)) <> res
        toLAlt' functionName (LitAlt lit, val, e) =
            toLexpr functionName e
        toLAlt' functionName (DEFAULT, val, e) =
            toLexpr functionName e
#else
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
#endif

pprTyCon :: Name -> SDoc
pprTyCon = ppr

pprDataCon :: Name -> SDoc
pprDataCon = ppr