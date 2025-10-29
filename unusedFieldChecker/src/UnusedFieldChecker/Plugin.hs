{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module UnusedFieldChecker.Plugin (plugin) where

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Hs
import GHC.Tc.Types
import GHC.Types.Error
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error
import GHC.Utils.Outputable
#else
import Bag
import DataCon
import DynFlags
import GHC
import GhcPlugins hiding ((<>))
import HsSyn
import Name
import Outputable
import Plugins
import SrcLoc
import TcRnMonad
import TcRnTypes
import TyCon
import TyCoRep
import Type
import Var
#endif

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import UnusedFieldChecker.Types
import Socket (sendViaUnixSocket)

plugin :: Plugin
plugin = defaultPlugin
    { typeCheckResultAction = collectFieldInfo
    , pluginRecompile = \_ -> return NoForceRecompile
    }

collectFieldInfo :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectFieldInfo opts modSummary tcEnv = do
    let cliOptions = parseCliOptions opts
        modulePath = path cliOptions </> msHsFilePath modSummary
        modName = pack $ moduleNameString $ moduleName $ ms_mod modSummary
    
    fieldDefs <- extractFieldDefinitions modName tcEnv
    
    fieldUsages <- extractFieldUsages modName tcEnv
    
    let moduleInfo = ModuleFieldInfo
            { moduleFieldDefs = fieldDefs
            , moduleFieldUsages = fieldUsages
            , moduleName = modName
            }
    
    liftIO $ do
        let outputPath = path cliOptions
        createDirectoryIfMissing True outputPath
        sendViaUnixSocket outputPath 
                         (pack $ "/" <> modulePath <> ".fieldInfo.json")
                         (decodeUtf8 $ BL.toStrict $ encodePretty moduleInfo)
    
    return tcEnv

parseCliOptions :: [CommandLineOption] -> CliOptions
parseCliOptions [] = defaultCliOptions
parseCliOptions (opt:_) = 
    case decode (BL.fromStrict $ encodeUtf8 $ pack opt) of
        Just opts -> opts
        Nothing -> defaultCliOptions

extractFieldDefinitions :: Text -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions modName tcEnv = do
    let tyCons = extractTyCons tcEnv
    concat <$> mapM (extractFieldsFromTyCon modName) tyCons

extractFieldsFromTyCon :: Text -> TyCon -> TcM [FieldDefinition]
extractFieldsFromTyCon modName tc
    | isAlgTyCon tc && not (isClassTyCon tc) = do
        let dataCons = tyConDataCons tc
            typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
        concat <$> mapM (extractFieldsFromDataCon modName typeName) dataCons
    | otherwise = return []

extractFieldsFromDataCon :: Text -> Text -> DataCon -> TcM [FieldDefinition]
extractFieldsFromDataCon modName typeName dc = do
    let fieldLabels = dataConFieldLabels dc
        fieldTypes = dataConRepArgTys dc
    
    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
                fieldTypeStr = pack $ showSDocUnsafe $ ppr $ scaledThing fieldType
                isMaybe = isMaybeType (scaledThing fieldType)
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan $ getName dc
            
            return FieldDefinition
                { fieldDefName = fieldName
                , fieldDefType = fieldTypeStr
                , fieldDefTypeName = typeName
                , fieldDefIsMaybe = isMaybe
                , fieldDefModule = modName
                , fieldDefLocation = location
                }
        else return []

isMaybeType :: Type -> Bool
isMaybeType ty = case ty of
    TyConApp tc _ -> 
        let tcName = getOccString (getName tc)
        in tcName == "Maybe"
    _ -> False

extractFieldUsages :: Text -> TcGblEnv -> TcM [FieldUsage]
extractFieldUsages modName tcEnv = do
    let binds = bagToList $ tcg_binds tcEnv
    concat <$> mapM (extractUsagesFromBind modName) binds

extractUsagesFromBind :: Text -> LHsBindLR GhcTc GhcTc -> TcM [FieldUsage]
extractUsagesFromBind modName (L _ bind) = case bind of
    FunBind{fun_matches = matches} -> 
        extractUsagesFromMatchGroup modName matches
    AbsBinds{abs_binds = binds} ->
        concat <$> mapM (extractUsagesFromBind modName) (bagToList binds)
    _ -> return []

extractUsagesFromMatchGroup :: Text -> MatchGroup GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromMatchGroup modName (MG _ (L _ matches) _) =
#else
extractUsagesFromMatchGroup modName (MG _ (L _ matches) _ _) =
#endif
    concat <$> mapM (extractUsagesFromMatch modName) matches

extractUsagesFromMatch :: Text -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromMatch modName (L _ match) = do
    patUsages <- concat <$> mapM (extractUsagesFromPat modName) (m_pats match)
    exprUsages <- extractUsagesFromGRHSs modName (m_grhss match)
    return $ patUsages ++ exprUsages

extractUsagesFromGRHSs :: Text -> GRHSs GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromGRHSs modName (GRHSs _ grhss _) =
    concat <$> mapM (extractUsagesFromGRHS modName) grhss

extractUsagesFromGRHS :: Text -> LGRHS GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromGRHS modName (L _ (GRHS _ _ body)) =
    extractUsagesFromExpr modName body

extractUsagesFromPat :: Text -> LPat GhcTc -> TcM [FieldUsage]
extractUsagesFromPat modName (L loc pat) = case pat of
#if __GLASGOW_HASKELL__ >= 900
    ConPat _ _ details -> extractUsagesFromConPatDetails modName loc details
#else
    ConPatOut{pat_args = details} -> extractUsagesFromConPatDetails modName loc details
#endif
    _ -> return []

extractUsagesFromConPatDetails :: Text -> SrcSpan -> HsConPatDetails GhcTc -> TcM [FieldUsage]
extractUsagesFromConPatDetails modName loc details = case details of
    RecCon (HsRecFields fields _) -> 
        concat <$> mapM (extractUsageFromRecField modName loc PatternMatch) fields
    _ -> return []

extractUsageFromRecField :: Text -> SrcSpan -> UsageType -> LHsRecField GhcTc (LPat GhcTc) -> TcM [FieldUsage]
extractUsageFromRecField modName loc usageType (L _ HsRecField{hsRecFieldLbl = lbl}) = do
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
    return [FieldUsage
        { fieldUsageName = fieldName
        , fieldUsageType = usageType
        , fieldUsageTypeName = ""
        , fieldUsageModule = modName
        , fieldUsageLocation = location
        }]

extractUsagesFromExpr :: Text -> LHsExpr GhcTc -> TcM [FieldUsage]
extractUsagesFromExpr modName (L loc expr) = case expr of
    RecordCon{rcon_flds = HsRecFields fields _} ->
        concat <$> mapM (extractUsageFromRecFieldExpr modName loc RecordConstruct) fields
    
    RecordUpd{rupd_flds = fields} ->
        extractUsagesFromRecordUpdate modName loc fields
    
#if __GLASGOW_HASKELL__ >= 900
    HsGetField{} -> do
        let fieldName = pack $ showSDocUnsafe $ ppr expr
        return [FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = FieldAccess
            , fieldUsageTypeName = ""
            , fieldUsageModule = modName
            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
            }]
#endif
    
    HsApp _ e1 e2 -> do
        u1 <- extractUsagesFromExpr modName e1
        u2 <- extractUsagesFromExpr modName e2
        return $ u1 ++ u2
    
    HsLet _ binds body -> do
        bindUsages <- extractUsagesFromLocalBinds modName binds
        bodyUsages <- extractUsagesFromExpr modName body
        return $ bindUsages ++ bodyUsages
    
    HsCase _ scrut matches -> do
        scrutUsages <- extractUsagesFromExpr modName scrut
        matchUsages <- extractUsagesFromMatchGroup modName matches
        return $ scrutUsages ++ matchUsages
    
    HsIf _ cond then_ else_ -> do
        condUsages <- extractUsagesFromExpr modName cond
        thenUsages <- extractUsagesFromExpr modName then_
        elseUsages <- extractUsagesFromExpr modName else_
        return $ condUsages ++ thenUsages ++ elseUsages
    
    HsDo _ _ (L _ stmts) ->
        concat <$> mapM (extractUsagesFromStmt modName) stmts
    
    ExplicitList _ elems ->
        concat <$> mapM (extractUsagesFromExpr modName) elems
    
    HsPar _ e ->
        extractUsagesFromExpr modName e
    
    _ -> return []

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromRecordUpdate :: Text -> SrcSpan -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc (Left fields) =
    concat <$> mapM (extractUsageFromRecFieldExpr modName loc RecordUpdate) fields
extractUsagesFromRecordUpdate modName loc (Right _) = return []
#else
extractUsagesFromRecordUpdate :: Text -> SrcSpan -> [LHsRecUpdField GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc fields =
    concat <$> mapM (extractUsageFromRecFieldExpr modName loc RecordUpdate) fields
#endif

extractUsageFromRecFieldExpr :: Text -> SrcSpan -> UsageType -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsageFromRecFieldExpr modName loc usageType (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg}) = do
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
        usage = FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = usageType
            , fieldUsageTypeName = ""
            , fieldUsageModule = modName
            , fieldUsageLocation = location
            }
    argUsages <- extractUsagesFromExpr modName arg
    return $ usage : argUsages

extractUsagesFromLocalBinds :: Text -> HsLocalBinds GhcTc -> TcM [FieldUsage]
extractUsagesFromLocalBinds modName binds = case binds of
    HsValBinds _ valBinds -> extractUsagesFromValBinds modName valBinds
    _ -> return []

extractUsagesFromValBinds :: Text -> HsValBindsLR GhcTc GhcTc -> TcM [FieldUsage]
extractUsagesFromValBinds modName valBinds = case valBinds of
#if __GLASGOW_HASKELL__ >= 900
    XValBindsLR (NValBinds binds _) ->
        concat <$> mapM (\(_, bagBinds) -> 
            concat <$> mapM (extractUsagesFromBind modName) (bagToList bagBinds)) binds
#else
    ValBindsOut binds _ ->
        concat <$> mapM (\(_, bagBinds) -> 
            concat <$> mapM (extractUsagesFromBind modName) (bagToList bagBinds)) binds
#endif
    _ -> return []

extractUsagesFromStmt :: Text -> LStmt GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromStmt modName (L _ stmt) = case stmt of
    BindStmt _ pat expr -> do
        patUsages <- extractUsagesFromPat modName pat
        exprUsages <- extractUsagesFromExpr modName expr
        return $ patUsages ++ exprUsages
    BodyStmt _ expr _ _ ->
        extractUsagesFromExpr modName expr
    LetStmt _ binds ->
        extractUsagesFromLocalBinds modName binds
    LastStmt _ expr _ _ ->
        extractUsagesFromExpr modName expr
    _ -> return []

extractTyCons :: TcGblEnv -> [TyCon]
extractTyCons tcEnv = 
    let tcs = tcg_tcs tcEnv
    in filter isSafeTyCon tcs
  where
    isSafeTyCon tc = 
        not (isClassTyCon tc) &&
        not (isPromotedDataCon tc) &&
        not (isTcTyCon tc)

#if __GLASGOW_HASKELL__ >= 900
scaledThing :: Scaled a -> a
scaledThing (Scaled _ t) = t
#else
scaledThing :: a -> a
scaledThing = id
#endif
