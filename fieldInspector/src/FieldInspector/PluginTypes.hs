
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
    CommandLineOption,Arg (..),
    HsParsedModule(..),
    Hsc,
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
import Name (getSrcSpan)
import SrcLoc
import Unique (mkUnique)
import Var (isLocalId,varType)
import FieldInspector.Types
import TcRnTypes
import TcRnMonad
import DataCon
#endif
import Debug.Trace

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
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import Prelude hiding (id, mapM, mapM_)
import Control.Exception (evaluate)
import qualified Data.Record.Plugin as DRP 
import qualified Data.Record.Anon.Plugin as DRAP
import qualified Data.Record.Plugin.HasFieldPattern as DRPH
import qualified RecordDotPreprocessor as RDP

plugin :: Plugin
plugin = (defaultPlugin{
            -- installCoreToDos = install
        pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectTypeInfoParser
        })
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

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    _ <- liftIO $ forkIO $
            do
                let prefixPath = case opts of
                        [] -> "/tmp/fieldInspector/"
                        local : _ -> local
                    moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> msHsFilePath modSummary
                    hm_module = unLoc $ hpm_module hpm
                    path = (intercalate "/" . init . splitOn "/") modulePath
                -- print ("generating types data for module: " <> moduleName' <> " at path: " <> path)
                types <- toList $ mapM (pure . getTypeInfo) (fromList $ hsmodDecls hm_module)
                createDirectoryIfMissing True path
                DBS.writeFile (modulePath <> ".types.parser.json") =<< (evaluate $ toStrict $ encodePretty $ Map.fromList $ Prelude.concat types)
                -- print ("generated types data for module: " <> moduleName' <> " at path: " <> path)
    pure hpm

getTypeInfo :: LHsDecl GhcPs -> [(String,TypeInfo)]
getTypeInfo (L _ (TyClD _ (DataDecl _ lname _ _ defn))) =
  [((showSDocUnsafe' lname) ,TypeInfo
    { name = showSDocUnsafe' lname
    , typeKind = "data"
    , dataConstructors = map getDataConInfo (dd_cons defn)
    })]
getTypeInfo (L _ (TyClD _ (SynDecl _ lname _ _ rhs))) =
    [((showSDocUnsafe' lname),TypeInfo
    { name = showSDocUnsafe' lname
    , typeKind = "type"
#if __GLASGOW_HASKELL__ >= 900
    , dataConstructors = [DataConInfo (showSDocUnsafe' lname) (maybe mempty (Map.singleton "synonym" . unpackHDS) (hsTypeToString $ unLoc rhs)) []]
#else
    , dataConstructors = [DataConInfo (showSDocUnsafe' lname) (Map.singleton "synonym" ((showSDocUnsafe . ppr . unLoc) rhs)) []]
#endif
    })]
getTypeInfo _ = []

instance Outputable Void where

getDataConInfo :: LConDecl GhcPs -> DataConInfo
getDataConInfo (L _ x@ConDeclH98{ con_name = lname, con_args = args }) =
  Debug.Trace.trace (showSDocUnsafe $ ppr args) $
    DataConInfo
      { dataConNames = showSDocUnsafe' lname
      , fields = getFieldMap args
      , sumTypes = [] -- For H98-style data constructors, sum types are not applicable
      }
getDataConInfo (L _ ConDeclGADT{ con_names = lnames, con_res_ty = ty }) =
  DataConInfo
    { dataConNames = intercalate ", " (map (showSDocUnsafe') lnames)
#if __GLASGOW_HASKELL__ >= 900
    , fields = maybe (mempty) (\x -> Map.singleton "gadt" $ unpackHDS x) (hsTypeToString $ unLoc ty)
#else
    , fields = Map.singleton "gadt" (showSDocUnsafe $ ppr ty)
#endif
    , sumTypes = [] -- For GADT-style data constructors, sum types can be represented by the type itself
    }

#if __GLASGOW_HASKELL__ >= 900
hsTypeToString :: HsType GhcPs -> Maybe HsDocString
hsTypeToString = f
  where
    f :: HsType GhcPs -> Maybe HsDocString
    f (HsDocTy _ _ lds) = Just (unLoc lds)
    f (HsBangTy _ _ (L _ (HsDocTy _ _ lds))) = Just (unLoc lds)
    f x = Just (mkHsDocString $ showSDocUnsafe $ ppr x)

extractInfixCon :: [HsType GhcPs] -> Map.Map String String
extractInfixCon x =
  let l = length x
  in Map.fromList $ map (\(a,b) -> (show a , b)) $ Prelude.zip [0..l] (map f x)
  where
    f :: HsType GhcPs -> (String)
    f (HsDocTy _ _ lds) = showSDocUnsafe $ ppr $ (unLoc lds)
    f (HsBangTy _ _ (L _ (HsDocTy _ _ lds))) = showSDocUnsafe $ ppr $ (unLoc lds)
    f x = (showSDocUnsafe $ ppr x)

extractConDeclField :: [ConDeclField GhcPs] -> Map.Map String String
extractConDeclField x = Map.fromList (go x)
  where
    go :: [ConDeclField GhcPs] -> [(String,String)]
    go [] = []
    go ((ConDeclField _ cd_fld_names cd_fld_type _):xs) =
        [((intercalate "," $ convertRdrNameToString cd_fld_names),(showSDocUnsafe $ ppr cd_fld_type))] <> (go xs)

    convertRdrNameToString x = map (showSDocUnsafe . ppr . rdrNameOcc . unLoc . reLocN . rdrNameFieldOcc . unXRec @(GhcPs)) x

getFieldMap :: HsConDeclH98Details GhcPs -> Map.Map String String
getFieldMap con_args =
  case con_args of
    PrefixCon _ args         -> extractInfixCon $ map (unLoc . hsScaledThing) args
    InfixCon arg1 arg2       -> extractInfixCon $ map (unLoc . hsScaledThing) [arg1,arg2]
    RecCon (fields)          -> extractConDeclField $ map unLoc $ (unXRec @(GhcPs)) fields

#else
getFieldMap :: HsConDeclDetails GhcPs -> Map String String
getFieldMap (PrefixCon args) = Map.fromList $ Prelude.zipWith (\i t -> (show i, showSDocUnsafe (ppr t))) [1..] args
getFieldMap (RecCon (L _ fields)) = Map.fromList $ concatMap getRecField fields
  where
    getRecField (L _ (ConDeclField _ fnames t _)) = [(showSDocUnsafe (ppr fname), showSDocUnsafe (ppr t)) | L _ fname <- fnames]
getFieldMap (InfixCon t1 t2) = Map.fromList [("field1", showSDocUnsafe (ppr t1)), ("field2", showSDocUnsafe (ppr t2))]
#endif

#if __GLASGOW_HASKELL__ >= 900
showSDocUnsafe' = showSDocUnsafe . ppr . GHC.unXRec @(GhcPs)
#else
showSDocUnsafe' = showSDocUnsafe . ppr
#endif