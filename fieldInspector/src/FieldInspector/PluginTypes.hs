{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module FieldInspector.PluginTypes (plugin) where

#if __GLASGOW_HASKELL__ >= 900
import Language.Haskell.Syntax.Type
import GHC.Hs.Extension ()
import GHC.Parser.Annotation ()
import GHC.Utils.Outputable ()
import qualified Data.IntMap.Internal as IntMap
import Streamly.Internal.Data.Stream (fromList,mapM_,mapM,toList)
import GHC
import GHC.Unit.Types
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..))
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc,Outputable)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import GHC.Core.Opt.Monad
import GHC.Rename.HsType
-- import GHC.HsToCore.Docs
import GHC.Types.Name.Reader


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
    rdrNameOcc,
    unitIdString,
    fsLit,
    moduleUnitId,
    CommandLineOption,Arg (..),
    HsParsedModule(..),
    Hsc,
    RdrName(..),
    Name,SDoc,DataCon,DynFlags,ModSummary(..),TyCon,
    Literal (..),typeEnvElts,
    ModGuts (mg_binds, mg_loc, mg_module),showSDoc,
    Module (moduleName),tyConKind,
    NamedThing (getName),getDynFlags,tyConDataCons,dataConOrigArgTys,dataConName,
    Outputable (..),dataConFieldLabels,PluginRecompile(..),
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
import Name
import SrcLoc
import Unique (mkUnique)
import Var (isLocalId,varType)
import FieldInspector.Types
import TcRnTypes
import TcRnMonad
import DataCon
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
import System.IO (writeFile)
import Control.Monad (forM)
import Streamly.Internal.Data.Stream hiding (concatMap, init, length, map, splitOn,foldl',intercalate)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_,log)
import Prelude hiding (id, mapM, mapM_,log)
import Control.Exception (evaluate)
import qualified Data.Record.Plugin as DRP
import qualified Data.Record.Anon.Plugin as DRAP
import qualified Data.Record.Plugin.HasFieldPattern as DRPH
import qualified RecordDotPreprocessor as RDP
#if defined(ENABLE_API_CONTRACT_PLUGINS)
import qualified ApiContract.Plugin as ApiContract
#endif
-- import qualified Fdep.Plugin as Fdep
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Network.WebSockets as WS
import Network.Socket (withSocketsDo)
import Text.Read (readMaybe)
import GHC.IO (unsafePerformIO)
import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)

plugin :: Plugin
plugin = (defaultPlugin{
            -- installCoreToDos = install
        pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectTypeInfoParser
        })
#if defined(ENABLE_API_CONTRACT_PLUGINS)
        <> ApiContract.plugin
#endif
#if defined(ENABLE_LR_PLUGINS)
        <> DRP.plugin
        <> DRAP.plugin
        <> DRPH.plugin
#endif
        <> RDP.plugin

instance Semigroup Plugin where
  p <> q = defaultPlugin {
      parsedResultAction = \args summary ->
            parsedResultAction p args summary
        >=> parsedResultAction q args summary
    ,  typeCheckResultAction = \args summary ->
            typeCheckResultAction p args summary
        >=> typeCheckResultAction q args summary
    , pluginRecompile = \args ->
            (<>)
        <$> pluginRecompile p args
        <*> pluginRecompile q args
    , tcPlugin = \args -> 
        case (tcPlugin p args, tcPlugin q args) of
          (Nothing, Nothing) -> Nothing
          (Just tp, Nothing) -> Just tp
          (Nothing, Just tq) -> Just tq
          (Just (TcPlugin tcPluginInit1 tcPluginSolve1 tcPluginStop1), Just (TcPlugin tcPluginInit2 tcPluginSolve2 tcPluginStop2)) -> Just $ TcPlugin 
            { tcPluginInit = do
                ip <- tcPluginInit1
                iq <- tcPluginInit2
                return (ip, iq)
            , tcPluginSolve = \(sp,sq) given derived wanted -> do
                solveP <- tcPluginSolve1 sp given derived wanted
                solveQ <- tcPluginSolve2 sq given derived wanted
                return $ combineTcPluginResults solveP solveQ
            , tcPluginStop = \(solveP,solveQ) -> do
                tcPluginStop1 solveP
                tcPluginStop2 solveQ
            }
    }

combineTcPluginResults :: TcPluginResult -> TcPluginResult -> TcPluginResult
combineTcPluginResults resP resQ =
  case (resP, resQ) of
    (TcPluginContradiction ctsP, TcPluginContradiction ctsQ) ->
      TcPluginContradiction (ctsP ++ ctsQ)
 
    (TcPluginContradiction ctsP, TcPluginOk _ _) ->
      TcPluginContradiction ctsP
 
    (TcPluginOk _ _, TcPluginContradiction ctsQ) ->
      TcPluginContradiction ctsQ
 
    (TcPluginOk solvedP newP, TcPluginOk solvedQ newQ) ->
      TcPluginOk (solvedP ++ solvedQ) (newP ++ newQ)
 

instance Monoid Plugin where
  mempty = defaultPlugin

pprTyCon :: Name -> SDoc
pprTyCon = ppr

pprDataCon :: Name -> SDoc
pprDataCon = ppr

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

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {path="/tmp/fieldInspector/",port=4444,host="::1",log=False,tc_funcs=Just False,api_conteact=Just True}

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    -- _ <- Fdep.collectDecls opts modSummary hpm
    _ <- liftIO $ forkIO $
            do
                -- let prefixPath = case opts of
                --         [] -> "/tmp/fieldInspector/"
                --         local : _ -> local
                let cliOptions = case opts of
                                [] ->  defaultCliOptions
                                (local : _) ->
                                            case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                                Just (val :: CliOptions) -> val
                                                Nothing -> defaultCliOptions
                let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                    modulePath = (path cliOptions) <> msHsFilePath modSummary
                    hm_module = unLoc $ hpm_module hpm
                    -- path = (intercalate "/" . init . splitOn "/") modulePath
                -- print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
                types <- toList $ mapM (pure . getTypeInfo moduleName') (fromList $ hsmodDecls hm_module)
                -- createDirectoryIfMissing True path
                sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".types.parser.json") (decodeUtf8 $ toStrict $ A.encode $ Map.fromList $ Prelude.concat types)
                -- DBS.writeFile (modulePath <> ".types.parser.json") =<< (evaluate $ toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
                -- print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    pure hpm

getTypeInfo :: String -> LHsDecl GhcPs -> [(String, TypeInfo)]
getTypeInfo modName (L _ decl) = case decl of
#if __GLASGOW_HASKELL__ >= 900
    TyClD _ (DataDecl _ lname _ _ defn) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "data"
            , dataConstructors = map (getDataConInfo modName) (dd_cons defn)
            })]
    TyClD _ (SynDecl _ lname _ _ rhs) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "type"
            , dataConstructors = [DataConInfo 
                (showSDocUnsafe' lname) 
                (Map.singleton "synonym" (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc rhs) (parseTypeToComplexType $ unLoc rhs))) 
                []]
            })]
#elif __GLASGOW_HASKELL__ >= 810
    TyClD _ (DataDecl _ lname _ _ defn) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "data"
            , dataConstructors = map (getDataConInfo modName) (dd_cons defn)
            })]
    TyClD _ (SynDecl _ lname _ _ rhs) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "type"
            , dataConstructors = [DataConInfo 
                (showSDocUnsafe' lname) 
                (Map.singleton "synonym" (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc rhs) (parseTypeToComplexType $ unLoc rhs))) 
                []]
            })]
#else
    TyClD (DataDecl _ lname _ defn) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "data"
            , dataConstructors = map (getDataConInfo modName) (con_decls defn)
            })]
    TyClD (SynDecl lname _ rhs) ->
        [(showSDocUnsafe' lname, TypeInfo
            { name = showSDocUnsafe' lname
            , typeKind = "type"
            , dataConstructors = [DataConInfo 
                (showSDocUnsafe' lname) 
                (Map.singleton "synonym" ((StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc rhs) (parseTypeToComplexType $ unLoc rhs)))) 
                []]
            })]
#endif
    _ -> []

-- getTypeInfo :: LHsDecl GhcPs -> [(String,TypeInfo)]
-- getTypeInfo (L _ (TyClD _ (DataDecl _ lname _ _ defn))) =
--   [((showSDocUnsafe' lname) ,TypeInfo
--     { name = showSDocUnsafe' lname
--     , typeKind = "data"
--     -- , dataConstructors = map getDataConInfo (dd_cons defn)
--     , dataConstructors = map (getDataConInfo modName) (dd_cons defn)
--     })]
-- getTypeInfo (L _ (TyClD _ (SynDecl _ lname _ _ rhs))) =
--     [((showSDocUnsafe' lname),TypeInfo
--     { name = showSDocUnsafe' lname
--     , typeKind = "type"
-- #if __GLASGOW_HASKELL__ >= 900
--     , dataConstructors = [DataConInfo (showSDocUnsafe' lname) (Map.singleton "synonym" (parseTypeToComplexType $ unLoc rhs)) []]
-- #else
--     , dataConstructors = [DataConInfo (showSDocUnsafe' lname) (Map.singleton "synonym" (parseTypeToComplexType $ unLoc rhs)) []]
-- #endif
--     })]
-- getTypeInfo _ = []

instance Outputable Void where

getDataConInfo :: String -> LConDecl GhcPs -> DataConInfo
#if __GLASGOW_HASKELL__ >= 900
getDataConInfo modName (L _ decl) = case decl of
    ConDeclH98{con_name = lname, con_args = args} ->
        DataConInfo
            { dataConNames = showSDocUnsafe' lname
            , fields = getFieldMap args
            , sumTypes = []
            }
    ConDeclGADT{con_names = lnames, con_res_ty = ty} ->
        DataConInfo
            { dataConNames = intercalate ", " (map showSDocUnsafe' lnames)
            , fields = Map.singleton "gadt" (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc ty) (parseTypeToComplexType $ unLoc ty))
            , sumTypes = []
            }
#elif __GLASGOW_HASKELL__ >= 810
getDataConInfo modName (L _ decl) = case decl of
    ConDeclH98{con_name = lname, con_args = args} ->
        DataConInfo
            { dataConNames = showSDocUnsafe' lname
            , fields = getFieldMap args
            , sumTypes = []
            }
    ConDeclGADT{con_names = lnames, con_res_ty = ty} ->
        DataConInfo
            { dataConNames = intercalate ", " (map showSDocUnsafe' lnames)
            , fields = Map.singleton "gadt" (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc ty) (parseTypeToComplexType $ unLoc ty))
            , sumTypes = []
            }
#else
getDataConInfo modName (L _ decl) = case decl of
    ConDecl{con_name = lname, con_args = args} ->
        DataConInfo
            { dataConNames = showSDocUnsafe' lname
            , fields = getFieldMap args
            , sumTypes = []
            }
    -- Note: GHC 8.8.3 handles GADTs differently
    ConDeclGADT{con_names = lnames, con_res_ty = ty} ->
        DataConInfo
            { dataConNames = intercalate ", " (map showSDocUnsafe' lnames)
            , fields = Map.singleton "gadt" (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc ty) (parseTypeToComplexType $ unLoc ty))
            , sumTypes = []
            }
#endif

#if __GLASGOW_HASKELL__ >= 900
getFieldMap :: HsConDeclH98Details GhcPs -> Map.Map String StructuredTypeRep
getFieldMap con_args = case con_args of
    PrefixCon _ args -> 
        Map.fromList $ Prelude.zipWith (\i arg -> (show i, (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc $ hsScaledThing arg) (parseTypeToComplexType $ unLoc $ hsScaledThing arg)))) 
                              [0..] args
    InfixCon arg1 arg2 ->
        Map.fromList $ Prelude.zipWith (\i arg -> (show i, (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc $ hsScaledThing arg) (parseTypeToComplexType $ unLoc $ hsScaledThing arg)))) 
                              [0..] [arg1, arg2]
    RecCon fields ->
        Map.fromList $ concatMap extractRecField $ map unLoc $ unXRec @(GhcPs) fields
  where
    extractRecField (ConDeclField _ names typ _) =
        let fieldNames = map (showSDocUnsafe . ppr . unLoc) names
            typeInfo = (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc typ) (parseTypeToComplexType $ unLoc typ))
        in map (\name -> (name, typeInfo)) fieldNames

#elif __GLASGOW_HASKELL__ >= 810
getFieldMap :: HsConDetails (LHsType GhcPs) (Located [LConDeclField GhcPs]) -> Map.Map String StructuredTypeRep
getFieldMap con_args = case con_args of
    PrefixCon args -> 
        Map.fromList $ Prelude.zipWith (\i arg -> (show i, (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc arg) (parseTypeToComplexType $ unLoc arg)))) 
                              [1..] args
    InfixCon arg1 arg2 ->
        Map.fromList [("1", (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc arg1) (parseTypeToComplexType $ unLoc arg1))),
                     ("2", (StructuredTypeRep (pack $ showSDocUnsafe $ ppr $ unLoc arg2) (parseTypeToComplexType $ unLoc arg2)))]
    RecCon (L _ fields) ->
        Map.fromList $ concatMap extractRecField fields
  where
    extractRecField x@(L _ (ConDeclField _ names typ _)) =
        let fieldNames = map (showSDocUnsafe . ppr . unLoc) names
            typeInfo = (StructuredTypeRep (pack $ "") (parseTypeToComplexType $ unLoc typ))
        in map (\name -> (name, typeInfo)) fieldNames

#else
getFieldMap :: HsConDetails (LHsType GhcPs) [LConDeclField GhcPs] -> Map.Map String StructuredTypeRep
getFieldMap con_args = case con_args of
    PrefixCon args -> 
        Map.fromList $ Prelude.zipWith (\i arg -> (show i, (StructuredTypeRep (showSDocUnsafe $ ppr $ unLoc arg) (parseTypeToComplexType $ unLoc arg)))) 
                              [1..] args
    InfixCon arg1 arg2 ->
        Map.fromList [("1", (StructuredTypeRep (showSDocUnsafe $ ppr $ unLoc arg1) (parseTypeToComplexType $ unLoc arg1))),
                     ("2", (StructuredTypeRep (showSDocUnsafe $ ppr $ unLoc arg2) (parseTypeToComplexType $ unLoc arg2)))]
    RecCon fields ->
        Map.fromList $ concatMap extractRecField fields
  where
    extractRecField (L _ (ConDeclField names typ _)) =
        let fieldNames = map (showSDocUnsafe . ppr . unLoc) names
            typeInfo = (StructuredTypeRep (showSDocUnsafe $ ppr $ unLoc typ) (parseTypeToComplexType $ unLoc typ))
        in map (\name -> (name, typeInfo)) fieldNames
#endif

#if __GLASGOW_HASKELL__ >= 900
showSDocUnsafe' = showSDocUnsafe . ppr . GHC.unXRec @(GhcPs)
#else
showSDocUnsafe' = showSDocUnsafe . ppr
#endif

getModuleName :: RdrName -> Text
getModuleName name = case name of
#if __GLASGOW_HASKELL__ >= 900
    Qual modName _ -> pack $ moduleNameString $ modName  -- Direct use of ModuleName
    Orig m _ -> pack $ showSDocUnsafe $ ppr m
#else
    Qual modName _ -> pack $ moduleNameString modName    -- No need for extra moduleName call
    Orig m _ -> pack $ showSDocUnsafe $ ppr m
#endif
    Unqual _ -> pack "Main"  -- Default for unqualified names
    Exact n -> pack $ moduleNameString $ moduleName $ nameModule n
    _ -> pack "Unknown"

getTypeName :: RdrName -> Text
getTypeName name = pack $ case name of
#if __GLASGOW_HASKELL__ >= 900
    Qual _ n -> occNameString $ occName n
    Unqual n -> occNameString $ occName n
#else
    Qual _ n -> occNameString $ occName n
    Unqual n -> occNameString $ occName n
#endif
    Exact n -> occNameString $ nameOccName n
    _ -> "UnknownType"

getPackageName :: RdrName -> Text
getPackageName name = case name of
#if __GLASGOW_HASKELL__ >= 900
    Exact n -> pack $ unitIdString $ toUnitId $ moduleUnit $ nameModule n  -- Convert Unit to UnitId

#else
    Exact n -> pack $ unitIdString $ moduleUnitId $ nameModule n           -- Direct UnitId access
#endif
    _ -> pack "this"  -- Default package name for non-exact names

#if __GLASGOW_HASKELL__ >= 900
extractTypeComponent :: Located RdrName -> TypeComponent
extractTypeComponent (L _ name) = 
    TypeComponent {
        moduleName' = getModuleName name,
        typeName' = getTypeName name,
        packageName = getPackageName name
    }

extractFieldType :: ConDeclField GhcPs -> (String,String,ComplexType)
extractFieldType (ConDeclField _ names ty _) =
    let fieldName = intercalate "," $ map (showSDocUnsafe . ppr . unLoc) names
        fieldType = parseTypeToComplexType $ unLoc ty
    in (fieldName, (showSDocUnsafe $ ppr $ unLoc ty) ,fieldType)
#else
extractTypeComponent :: Located RdrName -> TypeComponent
extractTypeComponent (L _ name) = 
    TypeComponent {
        moduleName' = getModuleName name,
        typeName' = getTypeName name,
        packageName = getPackageName name
    }

extractFieldType :: LConDeclField GhcPs -> (String,String,ComplexType)
extractFieldType (L _ (ConDeclField _ names ty _)) =
    let fieldName = intercalate "," $ map (showSDocUnsafe . ppr . unLoc) names
        fieldType = ((parseTypeToComplexType $ unLoc ty))
    in (fieldName,(showSDocUnsafe $ ppr $ unLoc ty) ,fieldType)
#endif

#if __GLASGOW_HASKELL__ >= 900
parseTypeToComplexType :: HsType GhcPs -> ComplexType
parseTypeToComplexType typ = case typ of
    HsForAllTy _ tele body -> 
        let binders = extractForallBinders tele
            bodyType = parseTypeToComplexType $ unLoc body
        in ForallType binders bodyType
    
    HsQualTy _ mContext body -> 
        let contextTypes = case mContext of
                            Nothing -> []
                            Just (L _ ctx) -> map (parseTypeToComplexType . unLoc) ctx
            bodyType = parseTypeToComplexType $ unLoc body
        in QualType contextTypes bodyType
    
    HsTyVar _ _ name -> 
        AtomicType $ extractTypeComponent $ convertLIdP name
    
    HsAppTy _ f x ->
        let baseType = parseTypeToComplexType (unLoc f)
            argType = parseTypeToComplexType (unLoc x)
        in AppType baseType [argType]
    
    HsAppKindTy _ ty kind ->
        let baseType = parseTypeToComplexType (unLoc ty)
            kindType = parseTypeToComplexType (unLoc kind)
        in KindSigType baseType kindType
    
    HsFunTy _ _ arg res ->
        FuncType (parseTypeToComplexType $ unLoc arg) (parseTypeToComplexType $ unLoc res)
    
    HsListTy _ elemType -> 
        ListType (parseTypeToComplexType $ unLoc elemType)
    
    HsTupleTy _ _ types -> 
        TupleType (map (parseTypeToComplexType . unLoc) types)
    
    HsSumTy _ types ->
        TupleType (map (parseTypeToComplexType . unLoc) types)
    
    HsOpTy _ ty1 op ty2 ->
        let left = parseTypeToComplexType $ unLoc ty1
            right = parseTypeToComplexType $ unLoc ty2
            opComp = AtomicType $ extractTypeComponent $ convertLIdP op
        in AppType opComp [left, right]
    
    HsParTy _ ty ->
        parseTypeToComplexType (unLoc ty)
    
    HsIParamTy _ (L _ name) ty ->
        IParamType (showSDocUnsafe $ ppr name) (parseTypeToComplexType $ unLoc ty)
    
    HsStarTy _ _ ->
        StarType
    
    HsKindSig _ ty kind ->
        KindSigType (parseTypeToComplexType $ unLoc ty) (parseTypeToComplexType $ unLoc kind)
    
    HsSpliceTy _ splice ->
        UnknownType $ pack $ "Splice: " ++ showSDocUnsafe (ppr splice)
    
    HsDocTy _ ty (L _ doc) ->
        DocType (parseTypeToComplexType $ unLoc ty) (unpackHDS doc)
    
    HsBangTy _ _ ty ->
        BangType (parseTypeToComplexType $ unLoc ty)
    
    HsRecTy _ fields ->
        RecordType $ map extractFieldType $ map unLoc fields
    
    HsExplicitListTy _ _ types ->
        PromotedListType (map (parseTypeToComplexType . unLoc) types)
    
    HsExplicitTupleTy _ types ->
        PromotedTupleType (map (parseTypeToComplexType . unLoc) types)
    
    HsTyLit _ lit ->
        LiteralType $ showSDocUnsafe $ ppr lit
    
    HsWildCardTy _ ->
        WildCardType
    
    XHsType _ ->
        UnknownType "XHsType extension"

extractForallBinders :: HsForAllTelescope GhcPs -> [TypeComponent]
extractForallBinders telescope = case telescope of
    HsForAllVis _ bndrs -> map ((extractTypeComponent . reLocN . hsLTyVarLocName)) bndrs
    HsForAllInvis _ bndrs -> map ((extractTypeComponent . reLocN . hsLTyVarLocName)) bndrs

#else
-- GHC 8.8.3 and 8.10.7 version
parseTypeToComplexType :: HsType GhcPs -> ComplexType
parseTypeToComplexType typ = case typ of
    -- HsForAllTy _ bndrs ctx ty -> 
    --     let binders = map (extractTypeComponent . hsLTyVarName . unLoc) bndrs
    --         contextTypes = case unLoc ctx of
    --                         (cs) -> map (parseTypeToComplexType) cs
    --                         _ -> []
    --         bodyType = parseTypeToComplexType $ unLoc ty
    --     in ForallType binders (QualType contextTypes bodyType)
    
    HsQualTy _ ctx ty -> 
        let contextTypes = map (parseTypeToComplexType . unLoc) $ unLoc ctx
            bodyType = parseTypeToComplexType $ unLoc ty
        in QualType contextTypes bodyType
    
    HsTyVar _ _ name -> 
        AtomicType $ extractTypeComponent $ convertLIdP name
    
    HsAppTy _ f x ->
        let baseType = parseTypeToComplexType (unLoc f)
            argType = parseTypeToComplexType (unLoc x)
        in AppType baseType [argType]
    
    HsFunTy _ arg res ->
        FuncType (parseTypeToComplexType $ unLoc arg) (parseTypeToComplexType $ unLoc res)
    
    HsListTy _ elemType -> 
        ListType (parseTypeToComplexType $ unLoc elemType)
    
    HsTupleTy _ tupleSort types -> 
        TupleType (map (parseTypeToComplexType . unLoc) types)
    
    HsOpTy _ ty1 op ty2 ->
        let left = parseTypeToComplexType $ unLoc ty1
            right = parseTypeToComplexType $ unLoc ty2
            opComp = AtomicType $ extractTypeComponent $ convertLIdP op
        in AppType opComp [left, right]
    
    HsParTy _ ty ->
        parseTypeToComplexType (unLoc ty)
    
    HsIParamTy _ name ty ->
        IParamType (unpackFS $ fsLit $ showSDocUnsafe $ ppr name) 
                  (parseTypeToComplexType $ unLoc ty)
    
    HsStarTy _ _ ->
        StarType
    
    HsKindSig _ ty kind ->
        KindSigType (parseTypeToComplexType $ unLoc ty) (parseTypeToComplexType $ unLoc kind)
    
    HsSpliceTy _ splice ->
        UnknownType $ pack $ "Splice: " ++ showSDocUnsafe (ppr splice)
    
#if __GLASGOW_HASKELL__ >= 810
    HsDocTy _ ty doc ->
        DocType (parseTypeToComplexType $ unLoc ty) (unpackFS $ fsLit $ showSDocUnsafe $ ppr doc)
#else
    HsDocTy ty doc ->
        DocType (parseTypeToComplexType $ unLoc ty) (show doc)
#endif
    
    HsBangTy _ bang ty ->
        BangType (parseTypeToComplexType $ unLoc ty)
    
    HsRecTy _ fields ->
        RecordType $ map extractFieldType fields
    
    HsExplicitListTy _ _ types ->
        PromotedListType (map (parseTypeToComplexType . unLoc) types)
    
    HsExplicitTupleTy _ types ->
        PromotedTupleType (map (parseTypeToComplexType . unLoc) types)
    
    HsTyLit _ lit ->
        LiteralType $ showSDocUnsafe $ ppr lit
    
    HsWildCardTy _ ->
        WildCardType
    
    _ ->
        UnknownType "Unknown type"
#endif

#if __GLASGOW_HASKELL__ >= 900
convertLIdP :: LIdP GhcPs -> Located RdrName
convertLIdP x = noLoc (GHC.unXRec @(GhcPs) x)

unwrapRdrName :: XRec GhcPs RdrName -> RdrName
unwrapRdrName (L _ name) = name

unwrapLocated :: Located (XRec GhcPs RdrName) -> RdrName
unwrapLocated (L _ wrapped) = unwrapRdrName wrapped
#else
convertLIdP :: Located RdrName -> Located RdrName
convertLIdP = id                                      -- No conversion needed
unwrapRdrName :: RdrName -> RdrName
unwrapRdrName = id

unwrapLocated :: Located RdrName -> RdrName
unwrapLocated (L _ name) = name
#endif