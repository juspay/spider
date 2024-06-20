{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FieldInspector.Plugin (plugin) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad (forM)
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
    mkStringLit,
 )
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (toStrict)
import Data.Data (Data (toConstr))
import Data.Int (Int64)
import Data.List (sortBy)
import Data.List.Extra (groupBy, intercalate, isSuffixOf, replace, splitOn)
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import DataCon
import Debug.Trace (trace, traceShowId)
import FieldInspector.Types
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GhcPlugins (
    CommandLineOption,
    DataCon,
    DynFlags,
    Literal (..),
    ModGuts (mg_binds, mg_loc, mg_module),
    ModSummary (..),
    Module (moduleName),
    Name,
    NamedThing (getName),
    Outputable (..),
    Plugin (installCoreToDos, pluginRecompile, typeCheckResultAction),
    SDoc,
    TyCon,
    Var,
    coVarDetails,
    dataConFieldLabels,
    dataConName,
    dataConOrigArgTys,
    dataConRepType,
    defaultPlugin,
    flLabel,
    getDynFlags,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDoc,
    showSDocUnsafe,
    tyConDataCons,
    tyConKind,
    tyConName,
    tyVarKind,
    typeEnvElts,
    unpackFS,
 )
import Id (isExportedId)
import Name (getSrcSpan)
import SrcLoc
import Streamly (parallely, serially)
import Streamly.Prelude hiding (concatMap, init, length, map, splitOn)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import TcRnMonad
import TcRnTypes
import TyCoRep
import Unique (mkUnique)
import Var (isLocalId)
import Prelude hiding (id, mapM, mapM_)

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = GhcPlugins.purePlugin
        , typeCheckResultAction = collectTypes
        }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "FieldInspector" (buildCfgPass args) : todos)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

buildCfgPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
            [] -> "/tmp/fieldInspector/"
            [local] -> local
            _ -> error "unexpected no of arguments"
    _ <- liftIO $
        forkIO $ do
            let binds = mg_binds guts
                moduleN = moduleNameString $ moduleName $ mg_module guts
                moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
            createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
            removeIfExists (moduleLoc Prelude.<> ".fieldUsage.json")
            print ("start generating fieldUsage for module: " <> moduleN <> " at path: " <> moduleLoc, length binds)
            t1 <- getCurrentTime
            l <- toList $ parallely $ mapM (liftIO . toLBind) (fromList binds)
            print ("started writing to file fieldUsage for module: " <> moduleN <> " at path: " <> moduleLoc, length l)
            DBS.writeFile (moduleLoc Prelude.<> ".fieldUsage.json") $ toStrict $ encodePretty $ Map.fromList $ groupByFunction $ Prelude.concat $ l
            t2 <- getCurrentTime
            print $ diffUTCTime t2 t1
            print ("generated fieldUsage for module: " <> moduleN <> " at path: " <> moduleLoc, length binds)
    return guts

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
    pure [(functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr $ haskellType) (pack $ showSDocUnsafe $ ppr $ fieldName) (pack $ showSDocUnsafe $ ppr $ finalFieldType) (pack $ nameStableString $ tyConName $ haskellTypeT) (pack $ showSDocUnsafe $ ppr $ b)])]
processHasField functionName b@(App (App (App getField (Type fieldName)) (Type haskellType)) (Type finalFieldType)) hasField =
    pure [(functionName, [FieldUsage (pack $ showSDocUnsafe $ ppr $ haskellType) (pack $ showSDocUnsafe $ ppr $ fieldName) (pack $ showSDocUnsafe $ ppr $ finalFieldType) (pack $ show $ toConstr $ haskellType) (pack $ showSDocUnsafe $ ppr $ b)])]
processHasField functionName x (Var hasField) = do
    res <- toLexpr functionName x
    let b = (pack $ showSDocUnsafe $ ppr x)
        parts = words $ T.unpack $ T.replace "\t" "" $ T.replace "\n" "" $ T.strip $ (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField)
    case parts of
        ["HasField", fieldName, dataType, fieldType] ->
            pure $
                (res)
                    <> [
                           ( functionName
                           ,
                               [ FieldUsage
                                    (pack dataType)
                                    (pack $ init (Prelude.tail fieldName))
                                    (pack fieldType)
                                    "" --(pack $ show $ toConstr $ haskellType)
                                    (b)
                               ]
                           )
                       ]
        ("HasField" : fieldName : dataType : fieldTypeRest) ->
            pure $
                (res)
                    <> [
                           ( functionName
                           ,
                               [ FieldUsage
                                    (pack dataType)
                                    (pack $ fieldName)
                                    (pack $ intercalate " " fieldTypeRest)
                                    "" --(pack $ show $ toConstr $ haskellType)
                                    (b)
                               ]
                           )
                       ]
        _ -> do
            print (showSDocUnsafe $ ppr $ tyVarKind hasField)
            pure res

test x =
    case words $ T.unpack $ T.replace "\t" "" $ T.replace "\n" "" $ T.strip $ pack x of
        x@["HasField", fieldName, dataType, fieldType] -> fieldType
        x@("HasField" : fieldName : dataType : fieldTypeRest) -> intercalate " " fieldTypeRest
        y -> ""

groupByFunction :: [(Text, [FieldUsage])] -> [(Text, [FieldUsage])]
groupByFunction = map mergeGroups . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
  where
    mergeGroups :: [(Text, [FieldUsage])] -> (Text, [FieldUsage])
    mergeGroups xs = (fst (Prelude.head xs), Prelude.concat (map snd xs))

toLBind :: CoreBind -> IO (([(Text, [FieldUsage])]))
toLBind (NonRec binder expr) = do
    res <- toLexpr (pack $ nameStableString $ idName binder) expr
    pure $ groupByFunction res
toLBind (Rec binds) = do
    r <-
        toList $
            serially $
                mapM
                    ( \(b, e) ->
                        toLexpr (pack $ nameStableString (idName b)) e
                    )
                    (fromList binds)
    pure $ groupByFunction $ Prelude.concat r

toLexpr :: Text -> Expr Var -> IO ([(Text, [FieldUsage])])
toLexpr functionName (Var x) = pure mempty
toLexpr functionName (Lit x) = pure $ mempty
toLexpr functionName (Type _id) = pure $ mempty
toLexpr functionName (App func@(App _ _) args@(Var isHasField)) = do
    if ("$_sys$$dHasField" == (pack $ nameStableString $ idName isHasField))
        then processHasField functionName func args
        else do
            f <- toLexpr functionName func
            a <- toLexpr functionName args
            pure $ f <> a
toLexpr functionName (App func args) = do
    f <- toLexpr functionName func
    a <- toLexpr functionName args
    pure $ f <> a
toLexpr functionName (Lam func args) = do
    toLexpr functionName args
toLexpr functionName (Let func args) = do
    a <- toLexpr functionName args
    f <- toLBind func
    pure $ (map (\(x, y) -> (functionName, y)) f) <> a
toLexpr functionName (Case condition bind _type alts) = do
    c <- toLexpr functionName condition
    a <- toList $ serially $ mapM (toLAlt functionName) (fromList alts)
    pure $ c <> Prelude.concat a
toLexpr functionName (Tick _ expr) = toLexpr functionName expr
toLexpr functionName (Cast expr _) = toLexpr functionName expr
toLexpr functionName _ = pure mempty

toLAlt :: Text -> (AltCon, [Var], CoreExpr) -> IO ([(Text, [FieldUsage])])
toLAlt functionName (DataAlt dataCon, val, e) = do
    a <- toLexpr functionName e
    pure $ a
toLAlt functionName (LitAlt lit, val, e) = do
    a <- toLexpr functionName e
    pure a
toLAlt functionName (DEFAULT, val, e) = do
    a <- toLexpr functionName e
    pure a

collectTypes :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypes opts modSummary tcg = do
    dflags <- getDynFlags
    _ <- liftIO $
        forkIO $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fieldInspector/"
                    local : _ -> local
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> ms_hspp_file modSummary
                typeEnv = tcg_type_env tcg
                path = (intercalate "/" . Prelude.reverse . Prelude.tail . Prelude.reverse . splitOn "/") modulePath
            print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
            types <-
                toList $
                    parallely $
                        mapM
                            ( \tyThing ->
                                case tyThing of
                                    ATyCon tyCon -> collectTyCon dflags tyCon
                                    _ -> return []
                            )
                            (fromList $ typeEnvElts typeEnv)
            createDirectoryIfMissing True path
            DBS.writeFile ((modulePath) <> ".types.json") $ (toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
            print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    return tcg

collectTyCon :: DynFlags -> TyCon -> IO [(String, TypeInfo)]
collectTyCon dflags tyCon = do
    let name = tyConName tyCon
        tyConStr = showSDoc dflags (pprTyCon name)
        tyConKind' = tyConKind tyCon
        kindStr = showSDoc dflags (ppr tyConKind')
        dataCons = tyConDataCons tyCon
    dataConInfos <- toList $ parallely $ mapM (collectDataCon dflags) (fromList dataCons)
    return $
        [
            ( tyConStr
            , TypeInfo
                { name = tyConStr
                , typeKind = kindStr
                , dataConstructors = dataConInfos
                }
            )
        ]

collectDataCon :: DynFlags -> DataCon -> IO (DataConInfo)
collectDataCon dflags dataCon = do
    let name = GhcPlugins.dataConName dataCon
        dataConStr = showSDoc dflags (pprDataCon name)
        fields = map (unpackFS . flLabel) $ dataConFieldLabels dataCon
        fieldTypes = map (showSDoc dflags . ppr) (dataConOrigArgTys dataCon)
        fieldInfo = Map.fromList $ zip fields fieldTypes
    return
        DataConInfo
            { dataConName = dataConStr
            , fields = fieldInfo
            , sumTypes = getAllFunTy $ dataConRepType $ dataCon
            }
  where
    getAllFunTy (FunTy _ ftArg ftRes) = [showSDoc dflags $ ppr $ ftArg] <> (getAllFunTy ftRes)
    getAllFunTy _ = mempty

pprTyCon :: Name -> SDoc
pprTyCon name = ppr name

pprDataCon :: Name -> SDoc
pprDataCon name = ppr name
