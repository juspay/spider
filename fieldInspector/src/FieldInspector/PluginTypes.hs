
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FieldInspector.PluginTypes (plugin) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
    mkStringLit
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
    tyConName
 )
import Id (isExportedId,idType)
import Name (getSrcSpan)
import Control.Monad (forM)
import SrcLoc
import Streamly (parallely, serially)
import Streamly.Prelude hiding (concatMap, init, length, map, splitOn,foldl')
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import Unique (mkUnique)
import Var (isLocalId,varType)
import Prelude hiding (id, mapM, mapM_)
import FieldInspector.Types
import TcRnTypes
import TcRnMonad
import DataCon
import qualified Data.Record.Plugin as DRP 
import qualified Data.Record.Anon.Plugin as DRAP
import qualified Data.Record.Plugin.HasFieldPattern as DRPH
import qualified RecordDotPreprocessor as RDP

plugin :: Plugin
plugin = (defaultPlugin{ 
            -- installCoreToDos = install
        pluginRecompile = GhcPlugins.purePlugin
        -- , typeCheckResultAction = collectTypesTC
        , parsedResultAction = collectTypeInfoParser
        }) <> DRP.plugin <> DRAP.plugin <> DRPH.plugin <> RDP.plugin

instance Semigroup Plugin where
  p <> q = defaultPlugin {
      parsedResultAction = \args summary ->
            parsedResultAction p args summary
        >=> parsedResultAction q args summary
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

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcg = do
    dflags <- getDynFlags
    _ <- liftIO $
            do
                let prefixPath = case opts of
                        [] -> "/tmp/fieldInspector/"
                        local : _ -> local
                    moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> ms_hspp_file modSummary
                    typeEnv = tcg_type_env tcg
                    path = (intercalate "/" . init . splitOn "/") modulePath
                -- print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
                types <- toList $ parallely $ mapM (\tyThing ->
                            case tyThing of
                                ATyCon tyCon -> collectTyCon dflags tyCon
                                _            -> return []) (fromList $ typeEnvElts typeEnv)
                createDirectoryIfMissing True path
                DBS.writeFile (modulePath <> ".types.json") (toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
                -- print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
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
    pure hpm

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
