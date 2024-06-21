
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FieldInspector.Plugin (plugin) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
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
import Data.Int (Int64)
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import Data.List ( sortBy, intercalate ,foldl')
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import TyCoRep
import GHC.IO (unsafePerformIO)
import GHC.Hs
import Data.Map (Map)
import Data.Data
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)
import System.IO (writeFile)
import GHC.Hs.Decls
import GhcPlugins (
    CommandLineOption,
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
    tyConName
 )
import Id (isExportedId)
import Name (getSrcSpan)
import Control.Monad (forM)
import SrcLoc
import Streamly (parallely, serially)
import Streamly.Prelude hiding (concatMap, init, length, map, splitOn,foldl')
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import Unique (mkUnique)
import Var (isLocalId)
import Prelude hiding (id, mapM, mapM_)
import FieldInspector.Types
import TcRnTypes
import TcRnMonad
import DataCon

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = GhcPlugins.purePlugin
        , typeCheckResultAction = collectTypesTC
        , parsedResultAction = collectTypeInfoParser
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
    _ <- liftIO $ forkIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ GhcPlugins.moduleName $ mg_module guts
            moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
        createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
        removeIfExists (moduleLoc Prelude.<> ".fieldUsage.json")
        print ("start generating fieldUsage for module: " <> moduleN <> " at path: " <> moduleLoc, length binds)
        t1 <- getCurrentTime
        l <- toList $ parallely $ mapM (liftIO . toLBind) (fromList binds)
        print ("started writing to file fieldUsage for module: " <> moduleN <> " at path: " <> moduleLoc, length l)
        DBS.writeFile (moduleLoc Prelude.<> ".fieldUsage.json") $ toStrict $ encodePretty $ Map.fromList $ groupByFunction $ Prelude.concat l
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
processHasField :: Text -> Expr Var -> Expr Var -> IO [(Text,[FieldUsage])]
processHasField functionName b@(App (App (App getField (Type fieldName)) (Type haskellType@(TyConApp haskellTypeT _))) (Type finalFieldType)) hasField =
    pure [(functionName,[FieldUsage (pack $ showSDocUnsafe $ ppr haskellType) (pack $ showSDocUnsafe $ ppr fieldName) (pack $ showSDocUnsafe $ ppr finalFieldType) (pack $ nameStableString $ GhcPlugins.tyConName haskellTypeT) (pack $ showSDocUnsafe $ ppr b)])]
processHasField functionName b@(App (App (App getField (Type fieldName)) (Type haskellType)) (Type finalFieldType)) hasField =
    pure [(functionName,[FieldUsage (pack $ showSDocUnsafe $ ppr haskellType) (pack $ showSDocUnsafe $ ppr fieldName) (pack $ showSDocUnsafe $ ppr finalFieldType) (pack $ show $ toConstr haskellType) (pack $ showSDocUnsafe $ ppr b)])]
processHasField functionName x (Var hasField) = do
    res <- toLexpr functionName x
    let b = pack $ showSDocUnsafe $ ppr x
        parts = words $ T.unpack $ T.replace "\t" "" $ T.replace "\n" "" $ T.strip (pack $ showSDocUnsafe $ ppr $ tyVarKind hasField)
    case parts of
        ["HasField", fieldName, dataType, fieldType] ->
            pure $ res <> [(functionName,[
                    FieldUsage
                        (pack dataType)
                        (pack $ init (Prelude.tail fieldName))
                        (pack fieldType)
                        ""--(pack $ show $ toConstr $ haskellType)
                        b
            ])]
        ("HasField":fieldName:dataType:fieldTypeRest) ->
            pure $ res <> [(functionName,[
                    FieldUsage
                        (pack dataType)
                        (pack fieldName)
                        (pack $ unwords fieldTypeRest)
                        ""--(pack $ show $ toConstr $ haskellType)
                        b
            ])]
        ("Field":fieldName:dataType:fieldTypeRest) ->
            pure $ res <> [(functionName,[
                    FieldUsage
                        (pack dataType)
                        (pack fieldName)
                        (pack $ unwords fieldTypeRest)
                        ""--(pack $ show $ toConstr $ haskellType)
                        b
            ])]
        ("Field'":fieldName:dataType:fieldTypeRest) ->
            pure $ res <> [(functionName,[
                    FieldUsage
                        (pack dataType)
                        (pack fieldName)
                        (pack $ unwords fieldTypeRest)
                        ""--(pack $ show $ toConstr $ haskellType)
                        b
            ])]
        _ -> do
            print (showSDocUnsafe $ ppr $ tyVarKind hasField)
            pure res

groupByFunction :: [(Text, [FieldUsage])] -> [(Text, [FieldUsage])]
groupByFunction = map mergeGroups . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
  where
    mergeGroups :: [(Text, [FieldUsage])] -> (Text, [FieldUsage])
    mergeGroups xs = (fst (Prelude.head xs), concatMap snd xs)

toLBind :: CoreBind -> IO [(Text,[FieldUsage])]
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

toLexpr :: Text -> Expr Var -> IO [(Text,[FieldUsage])]
toLexpr functionName (Var x) = pure mempty
toLexpr functionName (Lit x) = pure mempty
toLexpr functionName (Type _id) = pure mempty
toLexpr functionName (App func@(App _ _) args@(Var isHasField)) =
    if "$_sys$$dHasField" == pack (nameStableString $ idName isHasField)
        then processHasField functionName func args
        else do
            f <- toLexpr functionName func
            a <- toLexpr functionName args
            pure $ f <> a
toLexpr functionName (App func args) = do
    f <- toLexpr functionName func
    a <- toLexpr functionName args
    pure $ f <> a
toLexpr functionName (Lam func args) =
    toLexpr functionName args
toLexpr functionName (Let func args) = do
    a <- toLexpr functionName args
    f <- toLBind func
    pure $ map (\(x,y) -> (functionName,y)) f <> a
toLexpr functionName (Case condition bind _type alts) = do
    c <- toLexpr functionName condition
    a <- toList $ serially $ mapM (toLAlt functionName) (fromList alts)
    pure $ c <> Prelude.concat a
toLexpr functionName (Tick _ expr) = toLexpr functionName expr
toLexpr functionName (Cast expr _) = toLexpr functionName expr
toLexpr functionName _ = pure mempty

toLAlt :: Text -> (AltCon, [Var], CoreExpr) -> IO [(Text,[FieldUsage])]
toLAlt functionName (DataAlt dataCon, val, e) =
    toLexpr functionName e
toLAlt functionName (LitAlt lit, val, e) =
    toLexpr functionName e
toLAlt functionName (DEFAULT, val, e) =
    toLexpr functionName e

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcg = do
    dflags <- getDynFlags
    _ <- liftIO $
            forkIO $ do
                let prefixPath = case opts of
                        [] -> "/tmp/fieldInspector/"
                        local : _ -> local
                    moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> ms_hspp_file modSummary
                    typeEnv = tcg_type_env tcg
                    path = (intercalate "/" . init . splitOn "/") modulePath
                print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
                types <- toList $ parallely $ mapM (\tyThing ->
                            case tyThing of
                                ATyCon tyCon -> collectTyCon dflags tyCon
                                _            -> return []) (fromList $ typeEnvElts typeEnv)
                createDirectoryIfMissing True path
                DBS.writeFile (modulePath <> ".types.json") (toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
                print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    return tcg

collectTyCon :: DynFlags -> GhcPlugins.TyCon -> IO [(String,TypeInfo)]
collectTyCon dflags tyCon' = do
  let name = GhcPlugins.tyConName tyCon'
      tyConStr = showSDoc dflags (pprTyCon name)
      tyConKind' = tyConKind tyCon'
      kindStr = showSDoc dflags (ppr tyConKind')
      dataCons = tyConDataCons tyCon'
  dataConInfos <- toList $ parallely $ mapM (collectDataCon dflags) (fromList dataCons)
  return [(tyConStr,TypeInfo
    { name = tyConStr
    , typeKind = kindStr
    , dataConstructors = dataConInfos
    })]

collectDataCon :: DynFlags -> DataCon -> IO DataConInfo
collectDataCon dflags dataCon = do
  let name = GhcPlugins.dataConName dataCon
      dataConStr = showSDoc dflags (pprDataCon name)
      fields = map (unpackFS . flLabel) $ dataConFieldLabels dataCon
      fieldTypes = map (showSDoc dflags . ppr) (dataConOrigArgTys dataCon)
      fieldInfo = Map.fromList $ zip fields fieldTypes
  return DataConInfo
    { dataConName = dataConStr
    , fields = fieldInfo
    , sumTypes = getAllFunTy $ dataConRepType dataCon
    }
    where
        getAllFunTy (FunTy _ ftArg ftRes) = [showSDoc dflags $ ppr ftArg] <> getAllFunTy ftRes
        getAllFunTy _ = mempty

pprTyCon :: Name -> SDoc
pprTyCon = ppr

pprDataCon :: Name -> SDoc
pprDataCon = ppr

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    _ <- liftIO $
            forkIO $ do
                let prefixPath = case opts of
                        [] -> "/tmp/fieldInspector/"
                        local : _ -> local
                    moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> ms_hspp_file modSummary
                    hm_module = unLoc $ hpm_module hpm
                    path = (intercalate "/" . init . splitOn "/") modulePath
                print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
                types <- toList $ parallely $ mapM (pure . getTypeInfo) (fromList $ hsmodDecls hm_module)
                createDirectoryIfMissing True path
                DBS.writeFile (modulePath <> ".types.parser.json") (toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
                print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    return hpm

getTypeInfo :: LHsDecl GhcPs -> [(String,TypeInfo)]
getTypeInfo (L _ (TyClD _ (DataDecl _ lname _ _ defn))) =
  [(showSDocUnsafe (ppr lname) ,TypeInfo
    { name = showSDocUnsafe (ppr lname)
    , typeKind = "data"
    , dataConstructors = map getDataConInfo (dd_cons defn)
    })]
getTypeInfo (L _ (TyClD _ (SynDecl _ lname _ _ rhs))) =
    [(showSDocUnsafe (ppr lname),TypeInfo
    { name = showSDocUnsafe (ppr lname)
    , typeKind = "type"
    , dataConstructors = [DataConInfo (showSDocUnsafe (ppr lname)) (Map.singleton "synonym" (showSDocUnsafe (ppr rhs))) []]
    })]
getTypeInfo _ = []

getDataConInfo :: LConDecl GhcPs -> DataConInfo
getDataConInfo (L _ ConDeclH98{ con_name = lname, con_args = args }) =
  DataConInfo
    { dataConName = showSDocUnsafe (ppr lname)
    , fields = getFieldMap args
    , sumTypes = [] -- For H98-style data constructors, sum types are not applicable
    }
getDataConInfo (L _ ConDeclGADT{ con_names = lnames, con_res_ty = ty }) =
  DataConInfo
    { dataConName = intercalate ", " (map (showSDocUnsafe . ppr) lnames)
    , fields = Map.singleton "gadt" (showSDocUnsafe (ppr ty))
    , sumTypes = [] -- For GADT-style data constructors, sum types can be represented by the type itself
    }

getFieldMap :: HsConDeclDetails GhcPs -> Map String String
getFieldMap (PrefixCon args) = Map.fromList $ Prelude.zipWith (\i t -> (show i, showSDocUnsafe (ppr t))) [1..] args
getFieldMap (RecCon (L _ fields)) = Map.fromList $ concatMap getRecField fields
  where
    getRecField (L _ (ConDeclField _ fnames t _)) = [(showSDocUnsafe (ppr fname), showSDocUnsafe (ppr t)) | L _ fname <- fnames]
getFieldMap (InfixCon t1 t2) = Map.fromList [("field1", showSDocUnsafe (ppr t1)), ("field2", showSDocUnsafe (ppr t2))]
