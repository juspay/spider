{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module UnusedFieldChecker.Plugin 
    ( plugin
    , loadAllUnusedFields  -- Exported for external validation scripts
    ) where

import Prelude hiding (log)

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core.DataCon
import GHC.Core.TyCon
import qualified GHC.Core.TyCo.Rep as TyCo
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Driver.Env (hsc_mod_graph)
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Hs
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (getTopEnv, addErr, setSrcSpan)
import GHC.Tc.Utils.TcType (tcSplitTyConApp_maybe)
import GHC.Types.FieldLabel
import GHC.Types.Id (idType)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Unit.Module.Graph (mgModSummaries)
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types (moduleName, moduleUnit)
import GHC.Utils.Outputable (showSDocUnsafe, ppr, text)
#else
import Bag
import CoreMonad
import DataCon
import DynFlags
import FastString
import FieldLabel
import GHC
import GhcPlugins hiding ((<>))
import HsSyn
import Module (moduleName, moduleUnitId)
import Name
import Outputable
import Plugins
import SrcLoc
import TcRnMonad
import TcRnTypes
import TyCon
import TyCoRep
import Type
#endif

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isLower)
import Data.List (foldl', isSuffixOf)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.IO (Handle, hClose, openFile, IOMode(..))
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import Control.Exception (SomeException, try)
import UnusedFieldChecker.Types
import UnusedFieldChecker.Validator
import UnusedFieldChecker.Config
import UnusedFieldChecker.DefinitionExtractor
import UnusedFieldChecker.UsageExtractor

plugin :: Plugin
plugin = defaultPlugin
    { typeCheckResultAction = processModuleFields
    , pluginRecompile = \_ -> return NoForceRecompile
    }

-- | Main plugin action that runs for each module during type checking.
-- Implements log-and-remove pattern:
-- 1. Extract field definitions from types with FieldChecker instances
-- 2. Add non-Maybe fields to the unused field log
-- 3. Extract field usages from the current module
-- 4. Remove used fields from the log
-- 5. Write updated log to JSON file
-- 6. On last module: emit compilation errors for unused fields
processModuleFields :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
processModuleFields opts modSummary tcEnv = do
    let cliOptions = parseCliOptions opts
        modName = pack $ moduleNameString $ GHC.moduleName $ ms_mod modSummary
        currentModuleName = moduleNameString $ GHC.moduleName $ ms_mod modSummary
#if __GLASGOW_HASKELL__ >= 900
        currentPackage = GHC.Unit.Types.moduleUnit $ ms_mod modSummary
#else
        currentPackage = moduleUnitId $ ms_mod modSummary
#endif

    -- Detect if this is the last module in compilation order
#if __GLASGOW_HASKELL__ >= 900
    isLastModule <- detectLastModule modSummary
#else
    let isLastModule = False
#endif

    liftIO $ cleanupOldBuildIfNeeded (path cliOptions)
    exclusionConfig <- liftIO $ loadExclusionConfig (exclusionConfigFile cliOptions)

    -- Check if module should be processed based on exclusion config
    if isModuleExcluded exclusionConfig modName
        then do
            liftIO $ putStrLn $ "[Plugin] Module excluded: " ++ T.unpack modName
            -- Even if excluded, check for last module validation
            when isLastModule $ do
                liftIO $ putStrLn $ "[Plugin] Last module (excluded) - running validation"
                runFinalValidation cliOptions
            return tcEnv
        else do
            -- Step 1: Extract field definitions from types with FieldChecker
            fieldDefs <- extractFieldDefinitions modName currentPackage tcEnv
            
            -- Step 2: Extract field usages from this module
            fieldUsages <- extractFieldUsages modName tcEnv

            liftIO $ putStrLn $ "[Plugin] Module: " ++ T.unpack modName
            liftIO $ putStrLn $ "[Plugin]   - Extracted " ++ show (length fieldDefs) ++ " field definitions"
            liftIO $ putStrLn $ "[Plugin]   - Extracted " ++ show (length fieldUsages) ++ " field usages"

            liftIO $ do
                let outputPath = path cliOptions
                    gatewayName = extractGatewayName modName
                    gatewayFile = T.unpack gatewayName <> ".unusedFields.json"
                    fullPath = outputPath </> gatewayFile

                createDirectoryIfMissing True outputPath
                
                -- Step 3: Load existing unused field log
                existingLog <- loadUnusedFieldLog fullPath
                
                -- Step 4: Add new non-Maybe fields to log (replaces existing entries for same type/field)
                let logWithNewFields = addFieldsToLog fieldDefs existingLog
                
                -- Step 5: Remove fields that are used in this module
                let updatedLog = removeUsedFieldsFromLog logWithNewFields fieldUsages
                
                putStrLn $ "[Plugin]   - Log size: " ++ show (length existingLog) ++ 
                           " -> " ++ show (length logWithNewFields) ++
                           " -> " ++ show (length updatedLog)
                
                -- Step 6: Write updated log
                atomicWriteFile fullPath (encodePretty updatedLog)

            -- Step 7: If this is the last module, run final validation
            when isLastModule $ do
                liftIO $ putStrLn $ "[Plugin] Last module - running final validation"
                runFinalValidation cliOptions

            return tcEnv

#if __GLASGOW_HASKELL__ >= 900
-- | Detect if the current module is the last in the topologically sorted compilation order
detectLastModule :: ModSummary -> TcM Bool
detectLastModule modSummary = do
    hscEnv <- getTopEnv
    let moduleGraph = hsc_mod_graph hscEnv
        -- Get all ModSummary from the module graph
        allModSummaries = mgModSummaries moduleGraph
        sortedModules = map (moduleNameString . ms_mod_name) allModSummaries
        currentModuleName = moduleNameString $ ms_mod_name modSummary
    return $ lastMaybe sortedModules == Just currentModuleName
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)

-- | Run final validation after all modules are compiled
-- Loads all unused field logs and emits compilation errors if any remain
runFinalValidation :: CliOptions -> TcM ()
runFinalValidation cliOptions = do
    let outputPath = path cliOptions
        buildMarker = outputPath </> ".current-build"
    
    unusedFields <- liftIO $ loadAllUnusedFields outputPath
    
    liftIO $ putStrLn $ "[Plugin] Final validation: " ++ show (length unusedFields) ++ " unused fields remaining"
    
    -- Emit errors for each unused field
    when (failOnUnused cliOptions && not (null unusedFields)) $ do
        mapM_ emitUnusedFieldError unusedFields
    
    -- Cleanup: remove build marker so next build starts fresh
    liftIO $ safeRemoveFile buildMarker

-- | Emit a GHC compilation error for an unused field
-- Uses noSrcSpan to avoid showing misleading source locations
-- (the field's actual location is in the error message text)
emitUnusedFieldError :: FieldDefinition -> TcM ()
emitUnusedFieldError fieldDef = do
    let errorMsg = formatUnusedFieldError fieldDef
    -- Use setSrcSpan noSrcSpan to avoid showing the current module's location
    -- The actual field location is included in the error message itself
    setSrcSpan noSrcSpan $ addErr (text (T.unpack errorMsg))
#else
-- | For GHC < 9.0, skip last module detection
runFinalValidation :: CliOptions -> TcM ()
runFinalValidation _ = return ()
#endif

-- | Load the unused field log from a JSON file
loadUnusedFieldLog :: FilePath -> IO UnusedFieldLog
loadUnusedFieldLog filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return []
        else do
            content <- BS.readFile filePath
            case decode (BL.fromStrict content) of
                Just entries -> return entries
                Nothing -> do
                    putStrLn $ "[Plugin] Warning: Failed to parse " ++ filePath ++ ", starting fresh"
                    return []

-- | Load all unused fields from all gateway JSON files.
-- This is exported for use by external validation scripts that run after compilation.
loadAllUnusedFields :: FilePath -> IO [FieldDefinition]
loadAllUnusedFields outputPath = do
    exists <- doesDirectoryExist outputPath
    if not exists
        then return []
        else do
            allJsonFiles <- findAllUnusedFieldJsonFiles outputPath
            allLogs <- mapM loadUnusedFieldLog allJsonFiles
            return $ concat allLogs

-- | Find all .unusedFields.json files in the output directory
findAllUnusedFieldJsonFiles :: FilePath -> IO [FilePath]
findAllUnusedFieldJsonFiles dir = do
    dirExists <- doesDirectoryExist dir
    if not dirExists
        then return []
        else do
            contents <- listDirectory dir
            let jsonFiles = filter (isSuffixOf ".unusedFields.json") contents
                fullPaths = map (dir </>) jsonFiles
            return fullPaths

safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path = catchIOError (removeFile path) $ \e ->
    if isDoesNotExistError e
        then return ()
        else ioError e

cleanupOldBuildIfNeeded :: FilePath -> IO ()
cleanupOldBuildIfNeeded outputPath = do
    let buildMarker = outputPath </> ".current-build"
        cleanupLock = outputPath </> ".cleanup.lock"

    markerExists <- doesFileExist buildMarker
    if markerExists
        then return ()
        else do
            lockResult <- try (openFile cleanupLock WriteMode) :: IO (Either SomeException Handle)
            case lockResult of
                Left _ -> return ()
                Right lockHandle -> do
                    markerExistsNow <- doesFileExist buildMarker
                    if markerExistsNow
                        then do
                            hClose lockHandle
                            safeRemoveFile cleanupLock
                        else do
                            exists <- doesDirectoryExist outputPath
                            when exists $ do
                                contents <- listDirectory outputPath
                                let jsonFiles = filter (\f -> takeExtension f == ".json") contents
                                    fullPaths = map (outputPath </>) jsonFiles
                                    filesToRemove = filter (/= cleanupLock) fullPaths
                                mapM_ safeRemoveFile filesToRemove

                            writeFile buildMarker "cleanup-complete"
                            hClose lockHandle
                            safeRemoveFile cleanupLock

atomicWriteFile :: FilePath -> BL.ByteString -> IO ()
atomicWriteFile filePath content = do
    let tempPath = filePath <> ".tmp." <> show (hash filePath) <> "." <> show (BL.length content)
    createDirectoryIfMissing True (takeDirectory filePath)

    result <- try $ BL.writeFile tempPath content :: IO (Either SomeException ())
    case result of
        Left err -> do
            tempExists <- doesFileExist tempPath
            when tempExists $ safeRemoveFile tempPath
            error $ "Failed to write file atomically: " ++ show err
        Right _ -> do
            tempExists <- doesFileExist tempPath
            if tempExists
                then do
                    renameResult <- try $ renameFile tempPath filePath :: IO (Either SomeException ())
                    case renameResult of
                        Left renameErr -> do
                            safeRemoveFile tempPath
                            error $ "Failed to rename file atomically: " ++ show renameErr
                        Right _ -> return ()
                else error $ "Temp file disappeared: " ++ tempPath
  where
    hash :: String -> Int
    hash str = foldl' (\h c -> 31 * h + fromEnum c) (length str * 1000) str


-- Note: shouldRunValidation and markValidationComplete have been removed
-- Validation now runs in the driverPlugin hook after all modules are compiled

parseCliOptions :: [CommandLineOption] -> CliOptions
parseCliOptions [] = defaultCliOptions
parseCliOptions (opt:_) =
    case decode (BL.fromStrict $ encodeUtf8 $ pack opt) of
        Just opts -> opts
        Nothing -> defaultCliOptions

extractGatewayName :: Text -> Text
extractGatewayName modName =
    let parts = T.splitOn "." modName
        gatewayIndex = findGatewayIndex parts 0
    in case gatewayIndex of
        Just idx | idx < length parts -> parts !! idx
        _ -> modName
  where
    findGatewayIndex :: [Text] -> Int -> Maybe Int
    findGatewayIndex [] _ = Nothing
    findGatewayIndex (p1:p2:rest) idx
        | p1 == "Gateway" && p2 == "Gateway" && not (null rest) = Just (idx + 2)
        | otherwise = findGatewayIndex (p2:rest) (idx + 1)
    findGatewayIndex _ _ = Nothing


extractFieldUsages :: Text -> TcGblEnv -> TcM [FieldUsage]
extractFieldUsages modName tcEnv = do
    let binds = bagToList $ tcg_binds tcEnv
    concat <$> mapM (extractUsagesFromBind modName) binds

extractUsagesFromBind :: Text -> LHsBindLR GhcTc GhcTc -> TcM [FieldUsage]
extractUsagesFromBind modName lbind@(L _ bind) = do
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
    ConPat _ con details -> do
        let typeConStr = pack $ showSDocUnsafe $ ppr con
        extractUsagesFromConPatDetails modName (getLoc lpat) typeConStr details
#else
    ConPatOut{pat_con = con, pat_args = details} -> do
        let typeConStr = pack $ showSDocUnsafe $ ppr con
        extractUsagesFromConPatDetails modName (getLoc lpat) typeConStr details
#endif
    _ -> return []

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromConPatDetails :: Text -> SrcSpanAnnA -> Text -> HsConPatDetails GhcTc -> TcM [FieldUsage]
extractUsagesFromConPatDetails modName loc typeCon details = case details of
    RecCon (HsRecFields fields dotdot) -> do
        let wildcardUsages = case dotdot of
                Just _ -> [FieldUsage
                    { fieldUsageName = ".."
                    , fieldUsageType = RecordWildCards
                    , fieldUsageTypeName = typeCon
                    , fieldUsageModule = modName
                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                    , fieldUsageTypeConstructor = typeCon
                    }]
                Nothing -> []
        fieldUsages <- concat <$> mapM (extractUsageFromRecField modName loc typeCon) fields
        return $ wildcardUsages ++ fieldUsages
    _ -> return []
#else
extractUsagesFromConPatDetails :: Text -> SrcSpan -> Text -> HsConPatDetails GhcTc -> TcM [FieldUsage]
extractUsagesFromConPatDetails modName loc typeCon details = case details of
    RecCon (HsRecFields fields dotdot) -> do
        let wildcardUsages = case dotdot of
                Just _ -> [FieldUsage
                    { fieldUsageName = ".."
                    , fieldUsageType = RecordWildCards
                    , fieldUsageTypeName = typeCon
                    , fieldUsageModule = modName
                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                    , fieldUsageTypeConstructor = typeCon
                    }]
                Nothing -> []
        fieldUsages <- concat <$> mapM (extractUsageFromRecField modName loc typeCon) fields
        return $ wildcardUsages ++ fieldUsages
    _ -> return []
#endif

#if __GLASGOW_HASKELL__ >= 900
extractUsageFromRecField :: Text -> SrcSpanAnnA -> Text -> LHsRecField GhcTc (LPat GhcTc) -> TcM [FieldUsage]
extractUsageFromRecField modName loc typeCon (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = _, hsRecPun = pun}) = do
#else
extractUsageFromRecField :: Text -> SrcSpan -> Text -> LHsRecField GhcTc (LPat GhcTc) -> TcM [FieldUsage]
extractUsageFromRecField modName loc typeCon (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg, hsRecPun = pun}) = do
#endif
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
        usageType = if pun then NamedFieldPuns else PatternMatch
    return [FieldUsage
        { fieldUsageName = fieldName
        , fieldUsageType = usageType
        , fieldUsageTypeName = typeCon
        , fieldUsageModule = modName
        , fieldUsageLocation = location
        , fieldUsageTypeConstructor = typeCon
        }]

-- Helper to extract type constructor name from an expression (best effort)
extractTypeFromExpr :: LHsExpr GhcTc -> Text
extractTypeFromExpr lexpr = case unLoc lexpr of
    HsVar _ (L _ varId) -> pack $ getOccString varId
    _ -> ""

#if __GLASGOW_HASKELL__ >= 900
-- | Extract the type constructor from the type of an expression.
-- This is used to get the type constructor for accessor function usages.
-- For example, if we have `fieldName record`, we want to extract the type of `record`.
extractTypeConstructorFromExpr :: LHsExpr GhcTc -> Text
extractTypeConstructorFromExpr lexpr = case unLoc lexpr of
    HsVar _ (L _ varId) ->
        let varType = idType varId
        in extractTypeConstructorFromType varType
    HsApp _ _ arg -> extractTypeConstructorFromExpr arg
    HsPar _ inner -> extractTypeConstructorFromExpr inner
    _ -> ""

-- | Extract type constructor name from a Type
extractTypeConstructorFromType :: Type -> Text
extractTypeConstructorFromType ty =
    case tcSplitTyConApp_maybe ty of
        Just (tyCon, _) -> pack $ nameStableString $ tyConName tyCon
        Nothing -> ""
#else
extractTypeConstructorFromExpr :: LHsExpr GhcTc -> Text
extractTypeConstructorFromExpr _ = ""

extractTypeConstructorFromType :: Type -> Text
extractTypeConstructorFromType _ = ""
#endif

extractUsagesFromExpr :: Text -> LHsExpr GhcTc -> TcM [FieldUsage]
extractUsagesFromExpr modName lexpr =
    let loc = getLoc lexpr
        expr = unLoc lexpr
    in case expr of
    RecordCon{rcon_con = con, rcon_flds = HsRecFields fields _} -> do
        let typeCon = pack $ showSDocUnsafe $ ppr con
        concat <$> mapM (extractUsageFromRecFieldExpr modName loc typeCon RecordConstruct) fields

    RecordUpd{rupd_expr = updExpr, rupd_flds = fields} -> do
        let typeCon = extractTypeFromExpr updExpr
        extractUsagesFromRecordUpdate modName loc typeCon fields
    
#if __GLASGOW_HASKELL__ >= 900
    HsGetField _ baseExpr (L _ (HsFieldLabel _ (L _ field))) -> do
        let fieldName = pack $ unpackFS field
            typeCon = extractTypeConstructorFromExpr baseExpr
        return [FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = RecordDotSyntax
            , fieldUsageTypeName = typeCon
            , fieldUsageModule = modName
            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
            , fieldUsageTypeConstructor = typeCon
            }]
#endif
    
    HsApp _ e1 e2 -> do
        let typeCon = extractTypeConstructorFromExpr e2
            (fieldAccessUsage, _) = case unLoc e1 of
                HsVar _ (L _ varId) -> 
                    let varName' = pack $ getOccString varId
                    in if not (T.null varName') && isLower (T.head varName')
                        then ([FieldUsage
                            { fieldUsageName = varName'
                            , fieldUsageType = AccessorFunction
                            , fieldUsageTypeName = typeCon
                            , fieldUsageModule = modName
                            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                            , fieldUsageTypeConstructor = typeCon
                            }], AccessorFunction)
                        else ([], AccessorFunction)
                
                HsAppType _ (L _ (HsVar _ (L _ varId))) _ ->
                    let varName' = pack $ getOccString varId
                    in if varName' == "getField"
                        then case unLoc e2 of
                            HsVar _ (L _ _) -> 
                                let fieldName = extractFieldNameFromTypeApp e1
                                in if not (T.null fieldName)
                                    then ([FieldUsage
                                        { fieldUsageName = fieldName
                                        , fieldUsageType = HasFieldOverloaded
                                        , fieldUsageTypeName = typeCon
                                        , fieldUsageModule = modName
                                        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                        , fieldUsageTypeConstructor = typeCon
                                        }], HasFieldOverloaded)
                                    else ([], AccessorFunction)
                            _ -> ([], AccessorFunction)
                        else ([], AccessorFunction)
                
                OpApp _ lensTarget (L _ (HsVar _ (L _ opId))) _ ->
                    let opName = pack $ getOccString opId
                        lensTargetTypeCon = extractTypeConstructorFromExpr lensTarget
                    in if opName == "^."
                        then case unLoc e2 of
                            HsVar _ (L _ lensId) ->
                                let lensName = pack $ getOccString lensId
                                in ([FieldUsage
                                    { fieldUsageName = lensName
                                    , fieldUsageType = LensesOptics
                                    , fieldUsageTypeName = lensTargetTypeCon
                                    , fieldUsageModule = modName
                                    , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                    , fieldUsageTypeConstructor = lensTargetTypeCon
                                    }], LensesOptics)
                            
                            HsAppType _ (L _ (HsVar _ (L _ fieldId))) _ ->
                                let fieldFuncName = pack $ getOccString fieldId
                                in if fieldFuncName == "field"
                                    then let fieldName = extractFieldNameFromTypeApp e2
                                        in if not (T.null fieldName)
                                            then ([FieldUsage
                                                { fieldUsageName = fieldName
                                                , fieldUsageType = GenericReflection
                                                , fieldUsageTypeName = lensTargetTypeCon
                                                , fieldUsageModule = modName
                                                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                                                , fieldUsageTypeConstructor = lensTargetTypeCon
                                                }], GenericReflection)
                                            else ([], LensesOptics)
                                    else ([], LensesOptics)
                            _ -> ([], LensesOptics)
                        else ([], AccessorFunction)
                
                _ -> ([], AccessorFunction)
        
        compositionUsages <- extractFunctionComposition modName loc e1
        
        sybUsages <- extractSYBUsage modName loc e1 e2
        
        u1 <- extractUsagesFromExpr modName e1
        u2 <- extractUsagesFromExpr modName e2
        return $ fieldAccessUsage ++ compositionUsages ++ sybUsages ++ u1 ++ u2
    
    OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 -> do
        let opName = pack $ getOccString opId
        opUsages <- case opName of
            "^." -> do
                -- e1 ^. e2  => e1 is the record, e2 is the lens
                let targetTypeCon = extractTypeConstructorFromExpr e1
                lensUsages <- extractLensUsageWithType modName loc targetTypeCon e2
                return lensUsages
            
            "." -> do
                compUsages <- extractCompositionFields modName loc e1 e2
                return compUsages
            
            ".~" -> do
                -- lens .~ value => we need the record type from context (harder to get)
                lensUsages <- extractLensUsageWithType modName loc "" e1
                return lensUsages
            
            "&" -> do
                return []
            
            _ -> return []
        
        u1 <- extractUsagesFromExpr modName e1
        u2 <- extractUsagesFromExpr modName e2
        return $ opUsages ++ u1 ++ u2
    
    HsAppType _ e1 _ -> do
        typeAppUsages <- extractTypeApplicationUsage modName loc expr
        u1 <- extractUsagesFromExpr modName e1
        return $ typeAppUsages ++ u1
    
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

    HsLam _ matches ->
        extractUsagesFromMatchGroup modName matches

    HsVar _ _ -> return []  -- Variable references handled elsewhere
    HsLit _ _ -> return []  -- Literals don't contain field accesses
    HsOverLit _ _ -> return []  -- Overloaded literals don't contain field accesses

    _ -> return []

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromRecordUpdate :: Text -> SrcSpanAnnA -> Text -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc typeCon (Left fields) =
    concat <$> mapM (extractUsageFromRecUpdField modName loc typeCon RecordUpdate) fields
extractUsagesFromRecordUpdate _ _ _ (Right _) = return []

extractUsageFromRecUpdField :: Text -> SrcSpanAnnA -> Text -> UsageType -> LHsRecUpdField GhcTc -> TcM [FieldUsage]
extractUsageFromRecUpdField modName loc typeCon usageType (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg}) = do
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
        usage = FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = usageType
            , fieldUsageTypeName = typeCon
            , fieldUsageModule = modName
            , fieldUsageLocation = location
            , fieldUsageTypeConstructor = typeCon
            }
    argUsages <- extractUsagesFromExpr modName arg
    return $ usage : argUsages
#else
extractUsagesFromRecordUpdate :: Text -> SrcSpan -> Text -> [LHsRecUpdField GhcTc] -> TcM [FieldUsage]
extractUsagesFromRecordUpdate modName loc typeCon fields =
    concat <$> mapM (extractUsageFromRecFieldExpr modName loc typeCon RecordUpdate) fields
#endif

#if __GLASGOW_HASKELL__ >= 900
extractUsageFromRecFieldExpr :: Text -> SrcSpanAnnA -> Text -> UsageType -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#else
extractUsageFromRecFieldExpr :: Text -> SrcSpan -> Text -> UsageType -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [FieldUsage]
#endif
extractUsageFromRecFieldExpr modName loc typeCon usageType (L _ HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = arg}) = do
    let fieldName = pack $ showSDocUnsafe $ ppr lbl
        location = pack $ showSDocUnsafe $ ppr loc
        usage = FieldUsage
            { fieldUsageName = fieldName
            , fieldUsageType = usageType
            , fieldUsageTypeName = typeCon
            , fieldUsageModule = modName
            , fieldUsageLocation = location
            , fieldUsageTypeConstructor = typeCon
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
extractFieldNameFromTypeApp :: LHsExpr GhcTc -> Text
extractFieldNameFromTypeApp lexpr = 
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

#if __GLASGOW_HASKELL__ >= 900
-- | FunctionComposition usages are disabled because we cannot reliably
-- determine the type constructor for composed functions like (foo . bar . baz).
-- Fields used via composition are typically also used elsewhere.
extractFunctionComposition :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> TcM [FieldUsage]
extractFunctionComposition _ _ _ = return []
#else
extractFunctionComposition :: Text -> SrcSpan -> LHsExpr GhcTc -> TcM [FieldUsage]
extractFunctionComposition modName loc lexpr = return []
#endif

#if __GLASGOW_HASKELL__ >= 900
-- | Extract lens usage with a known type constructor
extractLensUsageWithType :: Text -> SrcSpanAnnA -> Text -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsageWithType modName loc typeCon lexpr = case unLoc lexpr of
    HsVar _ (L _ varId) ->
        let varName = pack $ getOccString varId
        in if not (T.null varName) && isLower (T.head varName)
            then return [FieldUsage
                { fieldUsageName = varName
                , fieldUsageType = LensesOptics
                , fieldUsageTypeName = typeCon
                , fieldUsageModule = modName
                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                , fieldUsageTypeConstructor = typeCon
                }]
            else return []
    
    OpApp _ e1 (L _ (HsVar _ (L _ opId))) e2 -> do
        let opName = pack $ getOccString opId
        if opName == "."
            then do
                -- For composed lenses like `foo . bar`, we can't easily determine
                -- intermediate types, so we use the provided typeCon for the first lens
                u1 <- extractLensUsageWithType modName loc typeCon e1
                u2 <- extractLensUsageWithType modName loc "" e2
                return $ u1 ++ u2
            else return []
    
    HsAppType _ (L _ (HsVar _ (L _ varId))) _ ->
        let varName = pack $ getOccString varId
        in if varName == "field"
            then let fieldName = extractFieldNameFromTypeApp lexpr
                in if not (T.null fieldName)
                    then return [FieldUsage
                        { fieldUsageName = fieldName
                        , fieldUsageType = GenericReflection
                        , fieldUsageTypeName = typeCon
                        , fieldUsageModule = modName
                        , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
                        , fieldUsageTypeConstructor = typeCon
                        }]
                    else return []
            else return []
    
    _ -> return []

-- | Legacy function for backward compatibility (used internally)
extractLensUsage :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsage modName loc lexpr = extractLensUsageWithType modName loc "" lexpr
#else
extractLensUsageWithType :: Text -> SrcSpan -> Text -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsageWithType modName loc typeCon lexpr = return []

extractLensUsage :: Text -> SrcSpan -> LHsExpr GhcTc -> TcM [FieldUsage]
extractLensUsage modName loc lexpr = return []
#endif

#if __GLASGOW_HASKELL__ >= 900
-- | Composition fields extraction is disabled because we cannot reliably
-- determine the type constructor for composed functions.
extractCompositionFields :: Text -> SrcSpanAnnA -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractCompositionFields _ _ _ _ = return []
#else
extractCompositionFields :: Text -> SrcSpan -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractCompositionFields modName loc e1 e2 = return []
#endif

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
                , fieldUsageTypeConstructor = ""
                }]
            else return []
    _ -> return []
#else
extractSYBUsage :: Text -> SrcSpan -> LHsExpr GhcTc -> LHsExpr GhcTc -> TcM [FieldUsage]
extractSYBUsage modName loc e1 e2 = return []
#endif

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
                        , fieldUsageTypeConstructor = ""
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
                            , fieldUsageTypeConstructor = ""
                        }]
                        else return []
                else return []
    _ -> return []
#else
extractTypeApplicationUsage :: Text -> SrcSpan -> HsExpr GhcTc -> TcM [FieldUsage]
extractTypeApplicationUsage modName loc expr = return []
#endif

#if __GLASGOW_HASKELL__ >= 900
extractTemplateHaskellUsage :: Text -> SrcSpanAnnA -> HsSplice GhcTc -> TcM [FieldUsage]
extractTemplateHaskellUsage modName loc splice = do
    let spliceStr = pack $ showSDocUnsafe $ ppr splice
    if "reify" `T.isInfixOf` spliceStr
        then return [FieldUsage
            { fieldUsageName = "reify"
            , fieldUsageType = TemplateHaskell
            , fieldUsageTypeName = ""
            , fieldUsageModule = modName
            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr loc
            , fieldUsageTypeConstructor = ""
            }]
        else return []
#else
extractTemplateHaskellUsage :: Text -> SrcSpan -> HsSplice GhcTc -> TcM [FieldUsage]
extractTemplateHaskellUsage modName loc splice = return []
#endif
