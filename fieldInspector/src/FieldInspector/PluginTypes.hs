{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module FieldInspector.PluginTypes where

#if __GLASGOW_HASKELL__ >= 900
import Language.Haskell.Syntax.Type
import GHC.Hs.Extension ()
import GHC.Parser.Annotation ()
import GHC.Utils.Outputable ()
import qualified Data.IntMap.Internal as IntMap
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
import GHC.Data.FastString
import GHC.Types.TypeEnv
import GHC.Types.FieldLabel
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
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
import GhcPlugins hiding ((<>))
import Class
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
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy,nubBy)
import Data.List ( sortBy, intercalate ,foldl')
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Map (Map)
import Data.Data (Data,toConstr)
import Data.Maybe (catMaybes,isJust)
import Control.Monad.IO.Class (liftIO)
import System.IO (writeFile)
import Control.Monad (forM,zipWithM)
-- import Streamly.Internal.Data.Stream hiding (concatMap, init, length, map, splitOn,foldl',intercalate)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_,log)
import Prelude hiding (id, mapM_,log)
import Control.Exception (evaluate)
-- import qualified Data.Record.Plugin as DRP
-- import qualified Data.Record.Anon.Plugin as DRAP
-- import qualified Data.Record.Plugin.HasFieldPattern as DRPH
import qualified RecordDotPreprocessor as RDP
-- import qualified ApiContract.Plugin as ApiContract
-- import qualified Fdep.Plugin as Fdep
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Text.Read (readMaybe)
import GHC.IO (unsafePerformIO)
import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import System.Directory (createDirectoryIfMissing)
import Socket

plugin :: Plugin
plugin = (defaultPlugin{
            -- installCoreToDos = install
        pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectTypeInfoParser
        , typeCheckResultAction = collectTypesTC
        })
-- #if defined(ENABLE_API_CONTRACT_PLUGINS)
--         <> ApiContract.plugin
-- #endif
-- #if defined(ENABLE_LR_PLUGINS)
--         <> DRP.plugin
--         <> DRAP.plugin
--         <> DRPH.plugin
-- #endif
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

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {path="./tmp/fdep/",port=4444,host="::1",log=False,tc_funcs=Just False,api_conteact=Just True}

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    _ <- liftIO $ forkIO $
            do
                let cliOptions = case opts of
                                [] ->  defaultCliOptions
                                (local : _) ->
                                            case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                                Just (val :: CliOptions) -> val
                                                Nothing -> defaultCliOptions
                let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                    modulePath = (path cliOptions) <> msHsFilePath modSummary
                    hm_module = unLoc $ hpm_module hpm
                    path_ = (intercalate "/" . init . splitOn "/") modulePath
                -- createDirectoryIfMissing True path_
                types <- mapM (pure . getTypeInfo moduleName') (hsmodDecls hm_module)
                -- DBS.writeFile (modulePath <> ".type.parser.json") (toStrict $ A.encode $ Map.fromList $ Prelude.concat types)
                sendViaUnixSocket (path cliOptions) (T.pack $ "/" <> modulePath <> ".types.parser.json") (decodeUtf8 $ toStrict $ A.encode $ Map.fromList $ Prelude.concat types)
    pure hpm

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcg = do
    _ <- do
        let cliOptions = case opts of
                        [] ->  defaultCliOptions
                        (local : _) ->
                                    case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                        Just (val :: CliOptions) -> val
                                        Nothing -> defaultCliOptions
        let tcEnv = (tcg_rn_decls tcg)
        let tcEnv' = (tcg_rn_exports tcg)
            modulePath = (path cliOptions) <> msHsFilePath modSummary
            path_ = (intercalate "/" . init . splitOn "/") modulePath
        -- liftIO $ createDirectoryIfMissing True path_
        typeDefs <- extractTypeInfo tcg
        -- liftIO $ forkIO $ DBS.writeFile (modulePath <> ".type.typechecker.json") =<< (pure $ DBS.toStrict $ A.encode $ Map.fromList typeDefs)
        liftIO $ sendViaUnixSocket (path cliOptions) (T.pack $ "/" <> modulePath <> ".type.typechecker.json") =<< (pure $ decodeUtf8 $ BL.toStrict $ A.encode $ Map.fromList typeDefs)
    pure tcg

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

extractTypeInfo :: TcGblEnv -> TcM [(String,TypeInfo)]
extractTypeInfo tcg = do
    dflags <- getDynFlags
    let tcs = extractTyCons tcg
    mapM (tyConToTypeInfo dflags) tcs

typeToStructuredTypeRep :: DynFlags -> Type -> TcM StructuredTypeRep
typeToStructuredTypeRep dflags ty = do
    let rawCode = pack (showSDocUnsafe (ppr ty))
    structType <- typeToComplexType dflags ty
    return $ StructuredTypeRep {
        raw_code = rawCode,
        structure = structType
    }

typeToComplexType :: DynFlags -> Type -> TcM ComplexType
typeToComplexType dflags ty = case ty of
    TyVarTy var -> return $ AtomicType $ nameToTypeComponent dflags $ getName var
    AppTy t1 t2 -> do
        c1 <- typeToComplexType dflags t1
        c2 <- typeToComplexType dflags t2
        return $ AppType c1 [c2]
    
    TyConApp tc args ->
        let tc_name = getName tc
            tc_string = showSDocUnsafe $ ppr tc_name
            occ = getOccString tc_name
        in
            if occ == "~" then do
                -- In GHC, equality constraints can have different numbers of arguments
                -- Traditionally it's kind, lhs, rhs but we need to be careful
                -- For clarity, let's process all args to see what we're dealing with
                processedArgs <- mapM (typeToComplexType dflags) args
                
                -- If there are at least 2 args, the last one is typically the RHS type
                if length args >= 2 then do
                    rhs <- typeToComplexType dflags (last args)
                    return rhs
                else if (length processedArgs == 0)
                    then return $ (AtomicType $ nameToTypeComponent dflags tc_name)
                else do
                    -- Default case: create the AppType normally
                    return $ AppType (AtomicType $ nameToTypeComponent dflags tc_name) processedArgs
            
            else if getOccString (getName tc) == "[]" && length args == 1 then do
                res <- typeToComplexType dflags (head args)
                pure $ ListType res
            -- Check for tuples using proper GHC API functions
            else if isBoxedTupleTyCon tc || isUnboxedTupleTyCon tc 
                then do
                    res <- mapM (typeToComplexType dflags) args
                    pure $ TupleType res
            else do
                res <- mapM (typeToComplexType dflags) args
                if length res == 0
                    then pure $ (AtomicType $ nameToTypeComponent dflags tc_name)
                    else pure $ AppType (AtomicType $ nameToTypeComponent dflags tc_name) (res)
#if __GLASGOW_HASKELL__ >= 900
    (FunTy _ _ argTy resTy) -> do
#else
    (FunTy _ argTy resTy) -> do
#endif
        cArg <- typeToComplexType dflags argTy
        cRes <- typeToComplexType dflags resTy
        return $ FuncType cArg cRes
    
    ForAllTy bndr ty -> do
        let var = binderVar bndr
        varComp <- varToTypeComponent dflags var
        innerType <- typeToComplexType dflags ty
        return $ ForallType [varComp] innerType
    
    LitTy lit -> 
        return $ LiteralType (showSDocUnsafe (ppr lit))
    
    CastTy t _ -> 
        typeToComplexType dflags t
    
    CoercionTy _ -> 
        return $ UnknownType "Coercion"
    
    _ -> return $ UnknownType (pack $ showSDocUnsafe (ppr ty))

tyConToAtomicType :: DynFlags -> TyCon -> TcM TypeComponent
tyConToAtomicType dflags tc =
    pure $ nameToTypeComponent dflags $ tyConName tc

nameToTypeComponent :: DynFlags -> Name -> TypeComponent
nameToTypeComponent dflags name = TypeComponent 
    { moduleName' = getModuleName name
    , typeName' = getTypeName name
    , packageName = getPackageName name
    }
  where
    getModuleName :: Name -> Text
    getModuleName nm = case nameModule_maybe nm of
        Just mod -> pack $ showSDocUnsafe (ppr (moduleName mod))
        Nothing  -> pack "" -- For internal/system names with no associated module

    getTypeName :: Name -> Text
    getTypeName nm = pack $ showSDocUnsafe (ppr (nameOccName nm))

    getPackageName :: Name -> Text
    getPackageName nm = case nameModule_maybe nm of
#if __GLASGOW_HASKELL__ >= 900
        Just mod -> pack $ showSDocUnsafe (ppr (moduleUnit mod))
#else
        Just mod -> pack $ showSDocUnsafe (ppr (moduleUnitId mod))
#endif
        Nothing  -> pack "" -- For internal/system names with no associated module

-- -- | For use in the GHC plugin
varToTypeComponent :: DynFlags -> Var -> TcM TypeComponent
varToTypeComponent dflags var = do
    return $ nameToTypeComponent dflags (getName var)

           
tyConToTypeInfo :: DynFlags -> TyCon -> TcM (String,TypeInfo)
tyConToTypeInfo dflags tc = do
    let tcName = showSDocUnsafe (ppr (tyConName tc))
    
    -- Determine what type of declaration this is - use the same format as in parser
    let typeCategory = if isTypeSynonymTyCon tc
                      then "type"             -- Use "type" instead of "type synonym"
                      else if isAlgTyCon tc && isNewTyCon tc
                           then "newtype"
                           else if isAlgTyCon tc
                                then "data"
                                else if isClassTyCon tc
                                     then "class"
                                     else if isFamilyTyCon tc
                                          then "type family"
                                          else "other"

    
    -- Get data constructors based on the type constructor's kind
    dataConsInfo <- case () of
        -- Algebraic data types (regular data and newtype)
        _ | isAlgTyCon tc && not (isClassTyCon tc) -> do
            -- liftIO $ putStrLn $ "  Processing as algebraic type"
            let dcons = tyConDataCons tc
            -- liftIO $ putStrLn $ "  Found " ++ show (length dcons) ++ " data constructors"
            mapM (dataConToDataConInfo dflags) dcons
            
        -- Type synonyms: extract the expanded type - format to match parser
        _ | isTypeSynonymTyCon tc -> do
            -- liftIO $ putStrLn $ "  Processing as type synonym"
            case synTyConRhs_maybe tc of
                Just ty -> do
                    -- liftIO $ putStrLn $ "  Synonym expands to: " ++ showSDocUnsafe (ppr ty)
                    structType <- typeToStructuredTypeRep dflags ty
                    return [DataConInfo {
                        dataConNames = tcName,  -- Use the type name instead of "type_synonym"
                        fields = Map.singleton "synonym" structType,  -- Use "synonym" key as in parser
                        sumTypes = []
                    }]
                Nothing -> do
                    -- liftIO $ putStrLn $ "  Could not get expansion"
                    return []
                
        -- Type families (open and closed)
        _ | isFamilyTyCon tc -> do
            -- liftIO $ putStrLn $ "  Processing as type family"
            -- Get the type parameters
            let tyVars = tyConTyVars tc
            let tyVarStrings = map (showSDocUnsafe . ppr . getName) tyVars
            
            -- Check if it's a closed family
            let isClosed = isJust $ isClosedSynFamilyTyConWithAxiom_maybe tc
            
            -- Format to match parser style
            return [DataConInfo {
                dataConNames = tcName,  -- Use the type name
                fields = Map.singleton "family_kind" (StructuredTypeRep {
                    raw_code = pack $ if isClosed then "closed" else "open",
                    structure = LiteralType $ if isClosed then "closed" else "open"
                }),
                sumTypes = tyVarStrings
            }]
                
        -- Type classes - format to match parser
        _ | isClassTyCon tc -> do
            -- liftIO $ putStrLn $ "  Processing as type class"
            let cls = tyConClass_maybe tc
            methods <- case cls of
                Just cls' -> do
                    -- liftIO $ putStrLn $ "  Class has " ++ show (length (classMethods cls')) ++ " methods"
                    return $ classMethods cls'
                Nothing -> do
                    -- liftIO $ putStrLn $ "  Could not get class methods"
                    return []
                
            -- Format methods similar to fields in the parser
            methodInfos <- forM methods $ \method -> do
                let methodName = showSDocUnsafe (ppr (getName method))
                let methodType = idType method
                structType <- typeToStructuredTypeRep dflags methodType
                return DataConInfo {
                    dataConNames = methodName,  -- Use method name as constructor name
                    fields = Map.singleton "method_type" structType,
                    sumTypes = []
                }
            
            -- If no methods, return a single representation of the class
            if null methodInfos then
                return [DataConInfo {
                    dataConNames = tcName,
                    fields = Map.empty,
                    sumTypes = []
                }]
            else
                return methodInfos
                
        -- Other special cases - match parser format
        _ | isPrimTyCon tc || isBuiltInSyntax (tyConName tc) -> do
            -- liftIO $ putStrLn $ "  Processing as primitive/built-in type"
            
            -- Create simple representation matching parser style
            return [DataConInfo {
                dataConNames = tcName,  -- Use type name
                fields = Map.empty,
                sumTypes = []
            }]
            
        -- Any other type constructor - match parser's empty list for unknown types
        _ -> do
            -- liftIO $ print $ ("  Unknown type constructor" , showSDocUnsafe $ ppr tc)
            return []  -- Return empty list as parser does for unknown types
    
    return $ (tcName,TypeInfo {
        name = tcName,          -- Type name
        typeKind = typeCategory, -- Use the category string determined above
        dataConstructors = dataConsInfo
    })

-- Helper to safely check if a TyCon is an algebraic type constructor
-- This replaces the unsafe direct pattern matching on algTyConRhs
tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe tc
    | isAlgTyCon tc = Just (tyConDataCons tc)  -- Safe because we've checked it's algebraic
    | otherwise = Nothing

-- Additional helper function to check for newtype specifically
isNewTyCon_maybe :: TyCon -> Maybe DataCon
isNewTyCon_maybe tc
    | isNewTyCon tc = Just (head (tyConDataCons tc))  -- Newtypes always have exactly one constructor
    | otherwise = Nothing

-- Safe function for extracting all TyCons
extractTyCons :: TcGblEnv -> [TyCon]
extractTyCons tcg = 
    -- Filter out TyCons that cause problems
    let tcs1 = filter isSafeTyCon (extractTyCons' (tcg_binds tcg))
        tcs2 = filter isSafeTyCon (extractTyCons' (tcg_tcs tcg))
    in nubBy (\tc1 tc2 -> tyConName tc1 == tyConName tc2) (tcs1 ++ tcs2)
  where
    extractTyCons' :: Data a => a -> [TyCon]
    extractTyCons' a = a ^? biplateRef
    
    -- Additional safety check for TyCons
    isSafeTyCon :: TyCon -> Bool
    isSafeTyCon tc = 
        -- Skip certain TyCons that cause problems
        not (isClassTyCon tc) &&         -- Skip type classes
        not (isPromotedDataCon tc) &&    -- Skip promoted data constructors
        not (isTcTyCon tc)            -- Skip type checker temporary TyCons

#if __GLASGOW_HASKELL__ >= 900
#else
scaledThing a = a
#endif

dataConToDataConInfo :: DynFlags -> DataCon -> TcM DataConInfo
dataConToDataConInfo dflags dc = do
    let dcName = showSDocUnsafe (ppr (dataConName dc))
    
    -- Get field information
    let fieldLabels = dataConFieldLabels dc
    let fieldTypes = dataConRepArgTys dc
    
    -- Debug information
    -- liftIO $ putStrLn $ "Processing DataCon: " ++ dcName
    -- liftIO $ putStrLn $ "  Field labels count: " ++ (showSDocUnsafe $ ppr (fieldLabels))
    -- liftIO $ putStrLn $ "  Field types count: " ++ (showSDocUnsafe $ ppr (fieldTypes))
    
    -- Handle cases where the number of field labels doesn't match the number of types
    fieldMap <- if not (null fieldLabels) && length fieldLabels == length (filter (\x -> not $ "~" `isInfixOf` (pack $ showSDocUnsafe $ ppr x)) fieldTypes)
        then do
            -- liftIO $ print $ showSDocUnsafe $ ppr (fieldLabels,fieldTypes)
            -- Normal case: build field map with real labels
            Map.fromList <$> zipWithM (\l t -> do
                    structType <- typeToStructuredTypeRep dflags (scaledThing t)
                    return (unpackFS (flLabel l), structType)
                ) fieldLabels fieldTypes
        else
            -- For non-record constructors, we still need to capture the arguments
            if not (null fieldTypes)
            then do
                -- liftIO $ putStrLn $ "  Non-record constructor with " ++ show (length fieldTypes) ++ " arguments"
                -- Use argument positions as field names
                Map.fromList <$> zipWithM (\i t -> do
                            structType <- typeToStructuredTypeRep dflags (scaledThing t)
                            -- liftIO $ putStrLn $ "    Arg " ++ show i ++ ": " ++ showSDocUnsafe (ppr (scaledThing t))
                            return (show i, structType)
                        ) [0..] fieldTypes
            else do
                -- liftIO $ putStrLn "  No arguments for this constructor"
                return Map.empty
    
    -- Safely get sum type information for GADTs 
    sumTypeNames <- mapM (\x -> pure $ showSDocUnsafe $ ppr $ binderVar x) (dataConUserTyVarBinders dc)
    
    return $ DataConInfo {
        dataConNames = dcName,
        fields = fieldMap,
        sumTypes = sumTypeNames
    }