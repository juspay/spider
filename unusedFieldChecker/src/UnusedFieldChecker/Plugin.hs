{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module UnusedFieldChecker.Plugin (plugin) where

import Prelude hiding (log)

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core.DataCon
import GHC.Core.TyCon
import qualified GHC.Core.TyCo.Rep as TyCo
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Hs
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (addMessages)
import GHC.Types.Error
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error
import GHC.Utils.Outputable hiding ((<>))
import qualified GHC.Utils.Error as Err
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
import Data.Char (isLower)
import Data.List (foldl', nub, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import UnusedFieldChecker.Types
import UnusedFieldChecker.Validator
import UnusedFieldChecker.Config
import Socket (sendViaUnixSocket)

#if __GLASGOW_HASKELL__ < 900
import ErrUtils (mkErrMsg)
import TcRnMonad (addErrs)
#endif

plugin :: Plugin
plugin = defaultPlugin
    { typeCheckResultAction = collectAndValidateFieldInfo
    , interfaceLoadAction = performCrossModuleValidation
    , pluginRecompile = \_ -> return NoForceRecompile
    }

collectAndValidateFieldInfo :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectAndValidateFieldInfo opts modSummary tcEnv = do
    let cliOptions = parseCliOptions opts
        modulePath = path cliOptions </> msHsFilePath modSummary
        modName = pack $ moduleNameString $ GHC.moduleName $ ms_mod modSummary
    
    -- Load exclusion config and check if this module should be excluded
    exclusionConfig <- liftIO $ loadExclusionConfig (exclusionConfigFile cliOptions)
    
    if isModuleExcluded exclusionConfig modName
        then do
            -- Skip processing this module entirely
            liftIO $ when (log cliOptions) $
                putStrLn $ "[UnusedFieldChecker] Skipping excluded module: " ++ T.unpack modName
            return tcEnv
        else do
            -- Extract field definitions first
            fieldDefs <- extractFieldDefinitions modName tcEnv
            
            -- Build field registry from current module and load cross-module fields
            allModuleInfos <- liftIO $ loadAllFieldInfo (path cliOptions)
            let fieldRegistry = buildFieldNameRegistry (fieldDefs : map moduleFieldDefs allModuleInfos)
            
            -- Extract field usages with the registry to filter out non-fields
            fieldUsages <- extractFieldUsagesWithRegistry modName tcEnv fieldRegistry
            
            -- Simple output: just field names and usage types
            liftIO $ when (log cliOptions && not (null fieldUsages)) $ do
                putStrLn $ "\n[" ++ T.unpack modName ++ "] Field Usages:"
                forM_ fieldUsages $ \usage ->
                    putStrLn $ "  " ++ T.unpack (fieldUsageName usage) ++ " -> " ++ show (fieldUsageType usage)
            
            let moduleInfo = ModuleFieldInfo
                    { moduleFieldDefs = fieldDefs
                    , moduleFieldUsages = fieldUsages
                    , moduleName = modName
                    }
            
            -- Save field info for cross-module analysis
            liftIO $ do
                let outputPath = path cliOptions
                createDirectoryIfMissing True outputPath
                sendViaUnixSocket outputPath 
                                 (pack $ "/" <> modulePath <> ".fieldInfo.json")
                                 (decodeUtf8 $ BL.toStrict $ encodePretty moduleInfo)
            
            -- Perform immediate validation for fields defined in this module
            let aggregated = aggregateFieldInfo [moduleInfo]
                validationResult = validateFieldsWithExclusions exclusionConfig aggregated
                errors = reportUnusedFields (unusedNonMaybeFields validationResult)
            
            -- Report errors
            when (not $ null errors) $ do
#if __GLASGOW_HASKELL__ >= 900
                let errMsgs = map makeError errors
                    msgEnvelopes = map mkErrorMsg errMsgs
                    msgs = mkMessages $ listToBag msgEnvelopes
                addMessages msgs
#else
                let errMsgs = map makeError errors
                addErrs errMsgs
#endif
            
            return tcEnv
  where
#if __GLASGOW_HASKELL__ >= 900
    makeError :: (Text, Text, Text) -> (SrcSpan, SDoc)
    makeError (locStr, msg, _) = 
        let loc = parseLocation locStr
        in (loc, text $ unpack msg)
    
    mkErrorMsg :: (SrcSpan, SDoc) -> MsgEnvelope DecoratedSDoc
    mkErrorMsg (loc, msg) = 
        Err.mkMsgEnvelope loc neverQualify msg
    
    parseLocation :: Text -> SrcSpan
    parseLocation locStr = 
        case T.splitOn ":" locStr of
            [file, line, col] -> 
                case (readMaybe (T.unpack line), readMaybe (T.unpack col)) of
                    (Just l, Just c) -> 
                        let srcLoc = mkSrcLoc (mkFastString $ T.unpack file) l c
                        in mkSrcSpan srcLoc srcLoc
                    _ -> noSrcSpan
            _ -> noSrcSpan
      where
        readMaybe :: Read a => String -> Maybe a
        readMaybe s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing
#else
    makeError :: (Text, Text, Text) -> (SrcSpan, SDoc)
    makeError (locStr, msg, _) = 
        let loc = parseLocation locStr
        in (loc, text $ unpack msg)
    
    parseLocation :: Text -> SrcSpan
    parseLocation locStr = 
        case T.splitOn ":" locStr of
            [file, line, col] -> 
                case (readMaybe (T.unpack line), readMaybe (T.unpack col)) of
                    (Just l, Just c) -> 
                        let srcLoc = mkSrcLoc (mkFastString $ T.unpack file) l c
                        in mkSrcSpan srcLoc srcLoc
                    _ -> noSrcSpan
            _ -> noSrcSpan
      where
        readMaybe :: Read a => String -> Maybe a
        readMaybe s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing
#endif

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
#if __GLASGOW_HASKELL__ >= 900
                fieldTypeStr = pack $ showSDocUnsafe $ ppr $ TyCo.scaledThing fieldType
                isMaybe = isMaybeType (TyCo.scaledThing fieldType)
#else
                fieldTypeStr = pack $ showSDocUnsafe $ ppr fieldType
                isMaybe = isMaybeType fieldType
#endif
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
#if __GLASGOW_HASKELL__ >= 900
    TyCo.TyConApp tc _ -> 
        let tcName = getOccString (getName tc)
        in tcName == "Maybe"
#else
    TyConApp tc _ -> 
        let tcName = getOccString (getName tc)
        in tcName == "Maybe"
#endif
    _ -> False

-- Build a registry of all known field names from field definitions
buildFieldNameRegistry :: [[FieldDefinition]] -> Set.Set Text
buildFieldNameRegistry fieldDefLists =
    Set.fromList $ concatMap (map fieldDefName) fieldDefLists

-- Extract field usages with filtering based on field registry
extractFieldUsagesWithRegistry :: Text -> TcGblEnv -> Set.Set Text -> TcM [FieldUsage]
extractFieldUsagesWithRegistry modName tcEnv fieldRegistry = do
    let binds = bagToList $ tcg_binds tcEnv
    allUsages <- concat <$> mapM (extractUsagesFromBindWithRegistry modName fieldRegistry) binds
    -- Filter to only include usages that are in the field registry
    return $ filter (isFieldUsage fieldRegistry) allUsages
  where
    isFieldUsage :: Set.Set Text -> FieldUsage -> Bool
    isFieldUsage registry usage =
        case fieldUsageType usage of
            -- Always include these usage types as they are explicitly field-related
            RecordConstruct -> True
            RecordUpdate -> True
            PatternMatch -> True
            NamedFieldPuns -> True
            RecordWildCards -> True
            RecordDotSyntax -> True
            -- For accessor functions and other types, check if the name is in the registry
            AccessorFunction -> fieldUsageName usage `Set.member` registry
            FunctionComposition -> fieldUsageName usage `Set.member` registry
            LensesOptics -> fieldUsageName usage `Set.member` registry
            HasFieldOverloaded -> True  -- These are explicitly field accesses
            GenericReflection -> True   -- These are explicitly field accesses
            TemplateHaskell -> False    -- TH is not field-specific
            DerivedInstances -> False   -- Derived instances are not field-specific
            DataSYB -> False            -- SYB is not field-specific

extractFieldUsages :: Text -> TcGblEnv -> TcM [FieldUsage]
extractFieldUsages modName tcEnv = do
    let binds = bagToList $ tcg_binds tcEnv
    concat <$> mapM (extractUsagesFromBind modName) binds

extractUsagesFromBindWithRegistry :: Text -> Set.Set Text -> LHsBindLR GhcTc GhcTc -> TcM [FieldUsage]
extractUsagesFromBindWithRegistry modName fieldRegistry lbind@(L loc bind) = do
    case bind of
        FunBind{fun_matches = matches} -> 
            extractUsagesFromMatchGroupWithRegistry modName fieldRegistry matches
        AbsBinds{abs_binds = binds} ->
            concat <$> mapM (extractUsagesFromBindWithRegistry modName fieldRegistry) (bagToList binds)
        _ -> return []

extractUsagesFromMatchGroupWithRegistry :: Text -> Set.Set Text -> MatchGroup GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromMatchGroupWithRegistry modName fieldRegistry (MG _ (L _ matches) _) =
#else
extractUsagesFromMatchGroupWithRegistry modName fieldRegistry (MG _ (L _ matches) _ _) =
#endif
    concat <$> mapM (extractUsagesFromMatchWithRegistry modName fieldRegistry) matches

extractUsagesFromMatchWithRegistry :: Text -> Set.Set Text -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromMatchWithRegistry modName fieldRegistry (L _ match) = do
    patUsages <- concat <$> mapM (extractUsagesFromPat modName) (m_pats match)
    exprUsages <- extractUsagesFromGRHSsWithRegistry modName fieldRegistry (m_grhss match)
    return $ patUsages ++ exprUsages

extractUsagesFromGRHSsWithRegistry :: Text -> Set.Set Text -> GRHSs GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromGRHSsWithRegistry modName fieldRegistry (GRHSs _ grhss _) =
    concat <$> mapM (extractUsagesFromGRHSWithRegistry modName fieldRegistry) grhss

extractUsagesFromGRHSWithRegistry :: Text -> Set.Set Text -> LGRHS GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
extractUsagesFromGRHSWithRegistry modName fieldRegistry (L _ (GRHS _ _ body)) =
    extractUsagesFromExpr modName body

extractUsagesFromBind :: Text -> LHsBindLR GhcTc GhcTc -> TcM [FieldUsage]
extractUsagesFromBind modName lbind@(L loc bind) = do
    case bind of
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
extractUsagesFromPat modName lpat = case unLoc lpat of
#if __GLASGOW_HASKELL__ >= 900
    ConPat _ _ details -> extractUsagesFromConPatDetails modName (getLoc lpat) details
#else
    ConPatOut{pat_args = details} -> extractUsagesFromConPatDetails modName (getLoc lpat) details
#endif
    _ -> return []

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromConPatDetails :: Text -> SrcSpanAnnA -> HsConPatDetails GhcTc -> TcM [FieldUsage]
extractUsagesFromConPatDetails modName loc details = case details of
    RecCon (HsRecFields fields dotdot) -> do
        -- Check for RecordWildCards (..)
        let wildcardUsages = case dotdot of
                Just _ -> [FieldUsage
                    { fieldUsageName = ".."
                    , fieldUsageType = RecordWildCards
                    , fieldUsageTypeName = ""
                    , fieldUsageModule = modName
                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                    }]
                Nothing -> []
        fieldUsages <- concat <$> mapM (extractUsageFromRecField modName loc) fields
        return $ wildcardUsages ++ fieldUsages
    _ -> return []
#else
extractUsagesFromConPatDetails :: Text -> SrcSpan -> HsConPatDetails GhcTc -> TcM [FieldUsage]
extractUsagesFromConPatDetails modName loc details = case details of
    RecCon (HsRecFields fields dotdot) -> do
        -- Check for RecordWildCards (..)
        let wildcardUsages = case dotdot of
                Just _ -> [FieldUsage
                    { fieldUsageName = ".."
                    , fieldUsageType = RecordWildCards
                    , fieldUsageTypeName = ""
                    , fieldUsageModule = modName
                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                    }]
                Nothing -> []
        fieldUsages <- concat <$> mapM (extractUsageFromRecField modName loc) fields
        return $ wildcardUsages ++ fieldUsages
    _ -> return []
#endif

#if __GLASGOW_HASKELL__ >= 900
extractUsageFromRecField :: Text -> SrcSpanAnnA -> LHsRecField GhcTc (LPat GhcTc) -> TcM [FieldUsage]
extractUsageFromRecField modName loc (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg, hsRecPun = pun}) = do
#else
extractUsageFromRecField :: Text -> SrcSpan -> LHsRecField GhcTc (LPat GhcTc) -> TcM [FieldUsage]
extractUsageFromRecField modName loc (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg, hsRecPun = pun}) = do
#endif
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
        -- Determine usage type: NamedFieldPuns if punned, otherwise PatternMatch
        usageType = if pun then NamedFieldPuns else PatternMatch
    return [FieldUsage
        { fieldUsageName = fieldName
        , fieldUsageType = usageType
        , fieldUsageTypeName = ""
        , fieldUsageModule = modName
        , fieldUsageLocation = location
        }]

extractUsagesFromExpr :: Text -> LHsExpr GhcTc -> TcM [FieldUsage]
extractUsagesFromExpr modName lexpr = 
    let loc = getLoc lexpr
        expr = unLoc lexpr
    in case expr of
    RecordCon{rcon_flds = HsRecFields fields _} ->
        concat <$> mapM (extractUsageFromRecFieldExpr modName loc RecordConstruct) fields
    
    RecordUpd{rupd_flds = fields} ->
        extractUsagesFromRecordUpdate modName loc fields
    
#if __GLASGOW_HASKELL__ >= 900
    HsGetField _ _ (L _ (HsFieldLabel _ (L _ field))) -> do
        let fieldName = pack $ unpackFS field
        return [FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = RecordDotSyntax
            , fieldUsageTypeName = ""
            , fieldUsageModule = modName
            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
            }]
#endif
    
    HsApp _ e1 e2 -> do
        -- Check for various field access patterns
        let (fieldAccessUsage, usageType) = case unLoc e1 of
                -- Regular accessor function: fieldName record
                HsVar _ (L _ varId) -> 
                    let varName = pack $ getOccString varId
                    in if not (T.null varName) && isLower (T.head varName)
                        then ([FieldUsage
                            { fieldUsageName = varName
                            , fieldUsageType = AccessorFunction
                            , fieldUsageTypeName = ""
                            , fieldUsageModule = modName
                            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                            }], AccessorFunction)
                        else ([], AccessorFunction)
                
                -- HasFieldOverloaded: getField @"fieldName" record
                HsAppType _ (L _ (HsVar _ (L _ varId))) _ ->
                    let varName = pack $ getOccString varId
                    in if varName == "getField"
                        then case unLoc e2 of
                            HsVar _ (L _ _) -> 
                                -- Extract field name from type application
                                let fieldName = extractFieldNameFromTypeApp e1
                                in if not (T.null fieldName)
                                    then ([FieldUsage
                                        { fieldUsageName = fieldName
                                        , fieldUsageType = HasFieldOverloaded
                                        , fieldUsageTypeName = ""
                                        , fieldUsageModule = modName
                                        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                        }], HasFieldOverloaded)
                                    else ([], AccessorFunction)
                            _ -> ([], AccessorFunction)
                        else ([], AccessorFunction)
                
                -- Lens operators: record ^. lens or record ^. field @"name"
                OpApp _ _ (L _ (HsVar _ (L _ opId))) _ ->
                    let opName = pack $ getOccString opId
                    in if opName == "^."
                        then case unLoc e2 of
                            -- Simple lens: record ^. fieldLens
                            HsVar _ (L _ lensId) ->
                                let lensName = pack $ getOccString lensId
                                in ([FieldUsage
                                    { fieldUsageName = lensName
                                    , fieldUsageType = LensesOptics
                                    , fieldUsageTypeName = ""
                                    , fieldUsageModule = modName
                                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                    }], LensesOptics)
                            
                            -- Generic lens: record ^. field @"name"
                            HsAppType _ (L _ (HsVar _ (L _ fieldId))) _ ->
                                let fieldFuncName = pack $ getOccString fieldId
                                in if fieldFuncName == "field"
                                    then let fieldName = extractFieldNameFromTypeApp e2
                                        in if not (T.null fieldName)
                                            then ([FieldUsage
                                                { fieldUsageName = fieldName
                                                , fieldUsageType = GenericReflection
                                                , fieldUsageTypeName = ""
                                                , fieldUsageModule = modName
                                                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                                }], GenericReflection)
                                            else ([], LensesOptics)
                                    else ([], LensesOptics)
                            _ -> ([], LensesOptics)
                        else ([], AccessorFunction)
                
                _ -> ([], AccessorFunction)
        
        -- Check for function composition: (field1 . field2 . field3)
        compositionUsages <- extractFunctionComposition modName loc e1
        
        -- Check for SYB operations: gmapQ, gmapT, etc.
        sybUsages <- extractSYBUsage modName loc e1 e2
        
        u1 <- extractUsagesFromExpr modName e1
        u2 <- extractUsagesFromExpr modName e2
        return $ fieldAccessUsage ++ compositionUsages ++ sybUsages ++ u1 ++ u2
    
    -- OpApp for infix operators (lens, composition, etc.)
    OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 -> do
        let opName = pack $ getOccString opId
        opUsages <- case opName of
            -- Lens view: record ^. lens
            "^." -> do
                lensUsages <- extractLensUsage modName loc e2
                return lensUsages
            
            -- Function composition: field1 . field2
            "." -> do
                compUsages <- extractCompositionFields modName loc e1 e2
                return compUsages
            
            -- Lens set: record & lens .~ value
            ".~" -> do
                lensUsages <- extractLensUsage modName loc e1
                return lensUsages
            
            "&" -> do
                -- record & lens .~ value pattern
                return []
            
            _ -> return []
        
        u1 <- extractUsagesFromExpr modName e1
        u2 <- extractUsagesFromExpr modName e2
        return $ opUsages ++ u1 ++ u2
    
    -- Type application for HasField and generic-lens
    HsAppType _ e1 _ -> do
        typeAppUsages <- extractTypeApplicationUsage modName loc expr
        u1 <- extractUsagesFromExpr modName e1
        return $ typeAppUsages ++ u1
    
    -- Template Haskell splices
    HsSpliceE _ splice -> do
        thUsages <- extractTemplateHaskellUsage modName loc splice
        return thUsages
    
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
extractUsagesFromRecordUpdate :: Text -> SrcSpanAnnA -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc (Left fields) =
    concat <$> mapM (extractUsageFromRecUpdField modName loc RecordUpdate) fields
extractUsagesFromRecordUpdate modName loc (Right _) = return []

extractUsageFromRecUpdField :: Text -> SrcSpanAnnA -> UsageType -> LHsRecUpdField GhcTc -> TcM [FieldUsage]
extractUsageFromRecUpdField modName loc usageType (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg}) = do
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
#else
extractUsagesFromRecordUpdate :: Text -> SrcSpan -> [LHsRecUpdField GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc fields =
    concat <$> mapM (extractUsageFromRecFieldExpr modName loc RecordUpdate) fields
#endif

#if __GLASGOW_HASKELL__ >= 900
extractUsageFromRecFieldExpr :: Text -> SrcSpanAnnA -> UsageType -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#else
extractUsageFromRecFieldExpr :: Text -> SrcSpan -> UsageType -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#endif
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

performCrossModuleValidation :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
performCrossModuleValidation opts modIface = do
    let cliOptions = parseCliOptions opts
    allModuleInfos <- liftIO $ loadAllFieldInfo (path cliOptions)
    let aggregated = aggregateFieldInfo allModuleInfos
    exclusionConfig <- liftIO $ loadExclusionConfig (exclusionConfigFile cliOptions)
    let validationResult = validateFieldsWithExclusions exclusionConfig aggregated
    return modIface

loadAllFieldInfo :: FilePath -> IO [ModuleFieldInfo]
loadAllFieldInfo outputPath = do
    exists <- doesFileExist outputPath
    if not exists
        then return []
        else do
            files <- listDirectory outputPath
            let jsonFiles = filter (\f -> takeExtension f == ".json") files
            catMaybes <$> mapM (loadFieldInfoFile outputPath) jsonFiles

-- Load a single field info JSON file
loadFieldInfoFile :: FilePath -> FilePath -> IO (Maybe ModuleFieldInfo)
loadFieldInfoFile outputPath filename = do
    let fullPath = outputPath </> filename
    exists <- doesFileExist fullPath
    if not exists
        then return Nothing
        else do
            content <- BS.readFile fullPath
            case decode (BL.fromStrict content) of
                Just info -> return (Just info)
                Nothing -> do
                    putStrLn $ "Warning: Failed to parse " ++ fullPath
                    return Nothing

#if __GLASGOW_HASKELL__ >= 900
extractFieldNameFromTypeApp :: LHsExpr GhcTc -> Text
extractFieldNameFromTypeApp lexpr = 
    -- Simplified implementation: extract from the pretty-printed representation
    let exprStr = pack $ showSDocUnsafe $ ppr lexpr
    in if "\"" `T.isInfixOf` exprStr
        then case T.splitOn "\"" exprStr of
            (_:fieldName:_) -> fieldName
            _ -> ""
        else ""
#else
extractFieldNameFromTypeApp :: LHsExpr GhcTc -> Text
extractFieldNameFromTypeApp _ = ""
#endif

-- Extract function composition patterns: (field1 . field2 . field3)
#if __GLASGOW_HASKELL__ >= 900
extractFunctionComposition :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> TcM [FieldUsage]
extractFunctionComposition modName loc lexpr = case unLoc lexpr of
    OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 -> do
        let opName = pack $ getOccString opId
        if opName == "."
            then do
                -- Check if both sides are field accessors
                let fields = extractComposedFields e1 ++ extractComposedFields e2
                return $ map (\fieldName -> FieldUsage
                    { fieldUsageName = fieldName
                    , fieldUsageType = FunctionComposition
                    , fieldUsageTypeName = ""
                    , fieldUsageModule = modName
                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                    }) fields
            else return []
    _ -> return []
  where
    extractComposedFields :: LHsExpr GhcTc -> [Text]
    extractComposedFields (L _ expr) = case expr of
        HsVar _ (L _ varId) ->
            let varName = pack $ getOccString varId
            in if not (T.null varName) && isLower (T.head varName)
                then [varName]
                else []
        OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 ->
            let opName = pack $ getOccString opId
            in if opName == "."
                then extractComposedFields e1 ++ extractComposedFields e2
                else []
        HsPar _ e -> extractComposedFields e
        _ -> []
#else
extractFunctionComposition :: Text -> SrcSpan -> LHsExpr GhcTc -> TcM [FieldUsage]
extractFunctionComposition modName loc lexpr = return []
#endif

-- Extract lens usage from expressions
#if __GLASGOW_HASKELL__ >= 900
extractLensUsage :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsage modName loc lexpr = case unLoc lexpr of
    HsVar _ (L _ varId) ->
        let varName = pack $ getOccString varId
        in if not (T.null varName) && isLower (T.head varName)
            then return [FieldUsage
                { fieldUsageName = varName
                , fieldUsageType = LensesOptics
                , fieldUsageTypeName = ""
                , fieldUsageModule = modName
                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                }]
            else return []
    
    -- Composed lenses: lens1 . lens2 . lens3
    OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 -> do
        let opName = pack $ getOccString opId
        if opName == "."
            then do
                u1 <- extractLensUsage modName loc e1
                u2 <- extractLensUsage modName loc e2
                return $ u1 ++ u2
            else return []
    
    -- Generic lens with type application: field @"name"
    HsAppType _ (L _ (HsVar _ (L _ varId))) _ ->
        let varName = pack $ getOccString varId
        in if varName == "field"
            then let fieldName = extractFieldNameFromTypeApp lexpr
                in if not (T.null fieldName)
                    then return [FieldUsage
                        { fieldUsageName = fieldName
                        , fieldUsageType = GenericReflection
                        , fieldUsageTypeName = ""
                        , fieldUsageModule = modName
                        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                        }]
                    else return []
            else return []
    
    _ -> return []
#else
extractLensUsage :: Text -> SrcSpan -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsage modName loc lexpr = return []
#endif

-- Extract composition fields for function composition operator
#if __GLASGOW_HASKELL__ >= 900
extractCompositionFields :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractCompositionFields modName loc e1 e2 = do
    let fields1 = getFieldAccessors e1
        fields2 = getFieldAccessors e2
        allFields = fields1 ++ fields2
    return $ map (\fieldName -> FieldUsage
        { fieldUsageName = fieldName
        , fieldUsageType = FunctionComposition
        , fieldUsageTypeName = ""
        , fieldUsageModule = modName
        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
        }) allFields
  where
    getFieldAccessors :: LHsExpr GhcTc -> [Text]
    getFieldAccessors (L _ expr) = case expr of
        HsVar _ (L _ varId) ->
            let varName = pack $ getOccString varId
            in if not (T.null varName) && isLower (T.head varName)
                then [varName]
                else []
        OpApp _ e1' (L _ (HsVar _ (L _ opId))) e2' ->
            let opName = pack $ getOccString opId
            in if opName == "."
                then getFieldAccessors e1' ++ getFieldAccessors e2'
                else []
        HsPar _ e -> getFieldAccessors e
        _ -> []
#else
extractCompositionFields :: Text -> SrcSpan -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractCompositionFields modName loc e1 e2 = return []
#endif

-- Extract SYB (Scrap Your Boilerplate) usage patterns
#if __GLASGOW_HASKELL__ >= 900
extractSYBUsage :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractSYBUsage modName loc e1 e2 = case unLoc e1 of
    HsVar _ (L _ varId) ->
        let varName = pack $ getOccString varId
        in if varName `elem` ["gmapQ", "gmapT", "gmapM", "gmapQl", "gmapQr", "gmapQi"]
            then return [FieldUsage
                { fieldUsageName = varName
                , fieldUsageType = DataSYB
                , fieldUsageTypeName = ""
                , fieldUsageModule = modName
                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                }]
            else return []
    _ -> return []
#else
extractSYBUsage :: Text -> SrcSpan -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractSYBUsage modName loc e1 e2 = return []
#endif

-- Extract type application usage (HasField, generic-lens)
#if __GLASGOW_HASKELL__ >= 900
extractTypeApplicationUsage :: Text -> SrcSpanAnnA -> HsExpr GhcTc -> TcM [FieldUsage]
extractTypeApplicationUsage modName loc expr = case expr of
    HsAppType _ (L _ (HsVar _ (L _ varId))) _ ->
        let varName = pack $ getOccString varId
        in if varName == "getField"
            then let fieldName = extractFieldNameFromTypeApp (L loc expr)
                in if not (T.null fieldName)
                    then return [FieldUsage
                        { fieldUsageName = fieldName
                        , fieldUsageType = HasFieldOverloaded
                        , fieldUsageTypeName = ""
                        , fieldUsageModule = modName
                        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                        }]
                    else return []
            else if varName == "field"
                then let fieldName = extractFieldNameFromTypeApp (L loc expr)
                    in if not (T.null fieldName)
                        then return [FieldUsage
                            { fieldUsageName = fieldName
                            , fieldUsageType = GenericReflection
                            , fieldUsageTypeName = ""
                            , fieldUsageModule = modName
                            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                            }]
                        else return []
                else return []
    _ -> return []
#else
extractTypeApplicationUsage :: Text -> SrcSpan -> HsExpr GhcTc -> TcM [FieldUsage]
extractTypeApplicationUsage modName loc expr = return []
#endif

-- Extract Template Haskell usage patterns
#if __GLASGOW_HASKELL__ >= 900
extractTemplateHaskellUsage :: Text -> SrcSpanAnnA -> HsSplice GhcTc -> TcM [FieldUsage]
extractTemplateHaskellUsage modName loc splice = do
    -- Check if the splice contains reify or other TH operations on types
    let spliceStr = pack $ showSDocUnsafe $ ppr splice
    if "reify" `T.isInfixOf` spliceStr
        then return [FieldUsage
            { fieldUsageName = "reify"
            , fieldUsageType = TemplateHaskell
            , fieldUsageTypeName = ""
            , fieldUsageModule = modName
            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
            }]
        else return []
#else
extractTemplateHaskellUsage :: Text -> SrcSpan -> HsSplice GhcTc -> TcM [FieldUsage]
extractTemplateHaskellUsage modName loc splice = return []
#endif
