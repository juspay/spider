{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module UnusedFieldChecker.UsageExtractor
    ( extractFieldUsagesFromCore
    ) where

import Prelude hiding (log)
import UnusedFieldChecker.LibraryFilter (isLibraryTypeConstructor, isDerivedBinding)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Type
import qualified GHC.Core.TyCo.Rep as TyCo
import GHC.Data.FastString
import GHC.Types.FieldLabel
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var
import GHC.Unit.Types (Unit, moduleUnit, unitString)
import GHC.Utils.Outputable hiding ((<>))
#else
import CoreSyn
import DataCon
import FastString
import FieldLabel
import Id
import Module (moduleUnitId, unitIdString)
import Name
import Outputable
import TyCon
import TyCoRep
import Type
import Var
#endif

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import Data.Text (Text, pack)
import qualified Data.Text as T
import UnusedFieldChecker.Types

-- | Extract field usages from Core bindings
-- Only extracts usages for types from the current package
#if __GLASGOW_HASKELL__ >= 900
extractFieldUsagesFromCore :: Text -> Unit -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName currentPkg binds = do
    let currentPkgName = extractPackageName $ pack $ unitString currentPkg
    putStrLn $ "[DEBUG PKG] Module: " ++ T.unpack modName ++ " Package: " ++ T.unpack currentPkgName
    allUsages <- mapM (extractUsagesFromBind modName currentPkgName) binds
    return $ concat allUsages

-- | Extract usages from a single Core binding
extractUsagesFromBind :: Text -> Text -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkgName (NonRec binder expr) = do
#else
extractFieldUsagesFromCore :: Text -> UnitId -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName currentPkg binds = do
    let currentPkgName = extractPackageName $ pack $ unitIdString currentPkg
    putStrLn $ "[DEBUG PKG] Module: " ++ T.unpack modName ++ " Package: " ++ T.unpack currentPkgName
    allUsages <- mapM (extractUsagesFromBind modName currentPkgName) binds
    return $ concat allUsages

-- | Extract usages from a single Core binding
extractUsagesFromBind :: Text -> Text -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkgName (NonRec binder expr) = do
#endif
    let binderName = pack $ getOccString $ idName binder
    
    -- Skip derived/compiler-generated bindings entirely
    if isDerivedBinding binderName
        then return []
        else extractUsagesFromExpr modName currentPkgName expr

extractUsagesFromBind modName currentPkgName (Rec binds) = do
    usages <- mapM (\(binder, expr) -> 
        let binderName = pack $ getOccString $ idName binder
        in if isDerivedBinding binderName
            then return []
            else extractUsagesFromExpr modName currentPkgName expr
        ) binds
    return $ concat usages

-- | Extract usages from a Core expression
extractUsagesFromExpr :: Text -> Text -> CoreExpr -> IO [FieldUsage]
extractUsagesFromExpr modName currentPkgName expr = case expr of
    Var _ -> return []
    Lit _ -> return []
#if __GLASGOW_HASKELL__ >= 900
    Type _ -> return []
    Coercion _ -> return []
#else
    Type _ -> return []
    Coercion _ -> return []
#endif
    
    -- Application: check for HasField and recurse
    App func args -> do
        -- Debug logging for function applications
        case func of
            Var fv -> do
                let funcName = pack (nameStableString $ idName fv)
                when ("getField" `T.isInfixOf` funcName || "$sel:" `T.isInfixOf` funcName) $
                    liftIO $ putStrLn $ "[DEBUG APP] Function app: " ++ T.unpack funcName
            _ -> return ()

        funcUsages <- extractUsagesFromExpr modName currentPkgName func
        argUsages <- extractUsagesFromExpr modName currentPkgName args
        hasFieldUsages <- detectHasField modName currentPkgName func args
        return $ funcUsages ++ argUsages ++ hasFieldUsages
    
    -- Lambda: recurse into body and check for record patterns in binder
    Lam binder body -> do
        bodyUsages <- extractUsagesFromExpr modName currentPkgName body
        -- Check if lambda binds a record type and extracts fields
        binderUsages <- extractUsagesFromBinder modName currentPkgName binder body
        return $ binderUsages ++ bodyUsages
    
    -- Let: extract from both binding and body, including record destructuring
    Let bind body -> do
        liftIO $ putStrLn $ "[DEBUG LET] Processing let binding in module: " ++ T.unpack modName
        bindUsages <- extractUsagesFromBind modName currentPkgName bind
        liftIO $ putStrLn $ "[DEBUG LET] Bind usages: " ++ show (length bindUsages)
        bodyUsages <- extractUsagesFromExpr modName currentPkgName body
        liftIO $ putStrLn $ "[DEBUG LET] Body usages: " ++ show (length bodyUsages)
        -- Extract field usages from let-bound record patterns
        letPatternUsages <- extractLetPatternUsages modName currentPkgName bind
        liftIO $ putStrLn $ "[DEBUG LET] Pattern usages: " ++ show (length letPatternUsages)
        return $ bindUsages ++ bodyUsages ++ letPatternUsages
    
    -- Case: extract from scrutinee and alternatives
    Case scrut _ _ alts -> do
        scrutUsages <- extractUsagesFromExpr modName currentPkgName scrut
        altUsages <- mapM (extractUsagesFromAlt modName currentPkgName) alts
        return $ scrutUsages ++ concat altUsages
    
    -- Cast and Tick: recurse through
    Cast expr' _ -> extractUsagesFromExpr modName currentPkgName expr'
    Tick _ expr' -> extractUsagesFromExpr modName currentPkgName expr'

-- | Detect HasField constraints (the key to nested field access!)
-- Now accepts currentPkgName for filtering
detectHasField :: Text -> Text -> CoreExpr -> CoreExpr -> IO [FieldUsage]
detectHasField modName currentPkgName func args = do
    -- Debug: Log when we detect potential HasField usage
    case args of
        Var hasFieldVar -> do
            let varName = pack (nameStableString $ idName hasFieldVar)
            when ("$dHasField" `T.isInfixOf` varName || "getField" `T.isInfixOf` varName) $
                liftIO $ putStrLn $ "[DEBUG HasField] Found potential HasField: " ++ T.unpack varName
            detectHasFieldFromVar modName currentPkgName func hasFieldVar
        _ -> return []

detectHasFieldFromVar :: Text -> Text -> CoreExpr -> Id -> IO [FieldUsage]
detectHasFieldFromVar modName currentPkgName func hasFieldVar
    | "$_sys$$dHasField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) ||
      "$dHasField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) ||
      "getField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) = do
        -- Extract field info from HasField constraint
        case func of
            -- Pattern: App (App (App _ (Type fieldName)) (Type recordType)) (Type fieldType)
            App (App (App _ (Type fieldNameType)) (Type recordType)) (Type fieldType) -> do
                let fieldName = extractFieldNameFromType fieldNameType
                    typeName = extractTypeNameFromType recordType
                    typeConstructor = extractTypeConstructor recordType
                    -- Use a simple string representation for location
                    location = "HasField:" <> fieldName <> ":" <> typeName

                    -- Filter by package
                    packagePattern = "$" <> currentPkgName <> "-"
                    shouldInclude = packagePattern `T.isPrefixOf` typeConstructor

                liftIO $ putStrLn $ "[DEBUG HasField MATCH] Field: " ++ T.unpack fieldName ++
                                   " Type: " ++ T.unpack typeName ++
                                   " TypeConstructor: " ++ T.unpack typeConstructor
                liftIO $ putStrLn $ "[DEBUG HasField FILTER] PackagePattern: " ++ T.unpack packagePattern ++
                                   " ShouldInclude: " ++ show shouldInclude

                if shouldInclude
                    then do
                        liftIO $ putStrLn $ "[DEBUG HasField INCLUDED] Adding usage for: " ++ T.unpack fieldName
                        return [FieldUsage
                            { fieldUsageName = fieldName
                            , fieldUsageType = HasFieldOverloaded
                            , fieldUsageTypeName = typeName
                            , fieldUsageModule = modName
                            , fieldUsageLocation = location
                            , fieldUsageTypeConstructor = typeConstructor
                            }]
                    else do
                        liftIO $ putStrLn $ "[DEBUG HasField FILTERED OUT] Skipping cross-package usage: " ++ T.unpack fieldName
                        return []
            _ -> return []
    | otherwise = return []

-- | Extract field name from type-level string
extractFieldNameFromType :: Type -> Text
extractFieldNameFromType ty = 
    let tyStr = pack $ showSDocUnsafe $ ppr ty
    in case T.splitOn "\"" tyStr of
        (_:fieldName:_) -> fieldName
        _ -> tyStr

-- | Extract type name from type
extractTypeNameFromType :: Type -> Text
#if __GLASGOW_HASKELL__ >= 900
extractTypeNameFromType ty = case ty of
    TyCo.TyConApp tc _ -> pack $ nameStableString $ tyConName tc
    _ -> pack $ showSDocUnsafe $ ppr ty
#else
extractTypeNameFromType ty = case ty of
    TyConApp tc _ -> pack $ nameStableString $ tyConName tc
    _ -> pack $ showSDocUnsafe $ ppr ty
#endif

-- | Extract type constructor for matching
extractTypeConstructor :: Type -> Text
#if __GLASGOW_HASKELL__ >= 900
extractTypeConstructor ty = case ty of
    TyCo.TyConApp tc _ -> pack $ nameStableString $ tyConName tc
    _ -> ""
#else
extractTypeConstructor ty = case ty of
    TyConApp tc _ -> pack $ nameStableString $ tyConName tc
    _ -> ""
#endif

-- | Extract usages from case alternatives (pattern matching)
#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromAlt :: Text -> Text -> CoreAlt -> IO [FieldUsage]
extractUsagesFromAlt modName currentPkgName (Alt altCon boundVars expr) = 
    extractUsagesFromAlt' modName currentPkgName (altCon, boundVars, expr)

extractUsagesFromAlt' :: Text -> Text -> (AltCon, [Var], CoreExpr) -> IO [FieldUsage]
extractUsagesFromAlt' modName currentPkgName (DataAlt dataCon, boundVars, expr) = do
#else
extractUsagesFromAlt :: Text -> Text -> CoreAlt -> IO [FieldUsage]
extractUsagesFromAlt modName currentPkgName (altCon, boundVars, expr) = 
    extractUsagesFromAlt' modName currentPkgName (altCon, boundVars, expr)

extractUsagesFromAlt' :: Text -> Text -> (AltCon, [Var], CoreExpr) -> IO [FieldUsage]
extractUsagesFromAlt' modName currentPkgName (DataAlt dataCon, boundVars, expr) = do
#endif
    let tc = dataConTyCon dataCon
        tyConName = getName tc
        typeName = pack $ nameStableString tyConName
        typeConstructor = typeName
        
        -- Check if this is a library type being pattern matched
        isLibraryType = isLibraryTypeConstructor typeConstructor
        fieldLabels = dataConFieldLabels dataCon
    
    let packagePattern = "$" <> currentPkgName <> "-"
        shouldInclude = packagePattern `T.isPrefixOf` typeConstructor
    
    -- Only create pattern match usages for current package types
    -- Skip library types, external packages, and constructors without fields
    let patternUsages = if not shouldInclude || isLibraryType || null fieldLabels
            then []
            else zipWith (\var label -> FieldUsage
                { fieldUsageName = pack $ unpackFS $ flLabel label
                , fieldUsageType = PatternMatch
                , fieldUsageTypeName = typeName
                , fieldUsageModule = modName
                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr var
                , fieldUsageTypeConstructor = typeConstructor
                }) boundVars fieldLabels
    
    exprUsages <- extractUsagesFromExpr modName currentPkgName expr
    return $ patternUsages ++ exprUsages

extractUsagesFromAlt' modName currentPkgName (_, _, expr) = 
    extractUsagesFromExpr modName currentPkgName expr

extractLetPatternUsages :: Text -> Text -> CoreBind -> IO [FieldUsage]
extractLetPatternUsages modName currentPkgName (NonRec binder expr) = do
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            let fieldLabels = dataConFieldLabels dataCon
                typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
                typeConstructor = typeName
            
            fieldUsages <- extractFieldSelectorsFromExpr modName currentPkgName binder dataCon expr
            return fieldUsages
        Nothing -> return []

extractLetPatternUsages modName currentPkgName (Rec binds) = do
    usages <- mapM (\(binder, expr) -> extractLetPatternUsages modName currentPkgName (NonRec binder expr)) binds
    return $ concat usages

extractUsagesFromBinder :: Text -> Text -> Var -> CoreExpr -> IO [FieldUsage]
extractUsagesFromBinder modName currentPkgName binder body = do
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            extractFieldSelectorsFromExpr modName currentPkgName binder dataCon body
        Nothing -> return []

getRecordDataCon :: Type -> Maybe DataCon
getRecordDataCon ty = case splitTyConApp_maybe ty of
    Just (tyCon, _) -> 
        case tyConDataCons tyCon of
            [dataCon] | not (null $ dataConFieldLabels dataCon) -> Just dataCon
            _ -> Nothing
    Nothing -> Nothing

extractFieldSelectorsFromExpr :: Text -> Text -> Var -> DataCon -> CoreExpr -> IO [FieldUsage]
extractFieldSelectorsFromExpr modName currentPkgName targetVar dataCon expr = do
    let fieldLabels = dataConFieldLabels dataCon
        typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
        typeConstructor = typeName
    
    fieldUsages <- findFieldUsagesInExpr currentPkgName targetVar fieldLabels typeName typeConstructor expr
    return fieldUsages
  where
    findFieldUsagesInExpr :: Text -> Var -> [FieldLabel] -> Text -> Text -> CoreExpr -> IO [FieldUsage]
    findFieldUsagesInExpr currentPkgName var labels tName tCon e = case e of
        Case (Var scrutVar) _ _ alts | scrutVar == var -> do
            altUsages <- mapM (extractFieldsFromAlt currentPkgName labels tName tCon) alts
            return $ concat altUsages

        App e1 e2 -> do
            u1 <- findFieldUsagesInExpr currentPkgName var labels tName tCon e1
            u2 <- findFieldUsagesInExpr currentPkgName var labels tName tCon e2
            return $ u1 ++ u2
        
        Lam _ body -> findFieldUsagesInExpr currentPkgName var labels tName tCon body
        
        Let bind body -> do
            bindUsages <- case bind of
                NonRec _ bindExpr -> findFieldUsagesInExpr currentPkgName var labels tName tCon bindExpr
                Rec binds' -> do
                    usages <- mapM (\(_, bindExpr) -> findFieldUsagesInExpr currentPkgName var labels tName tCon bindExpr) binds'
                    return $ concat usages
            bodyUsages <- findFieldUsagesInExpr currentPkgName var labels tName tCon body
            return $ bindUsages ++ bodyUsages
        
        Case scrut _ _ alts -> do
            scrutUsages <- findFieldUsagesInExpr currentPkgName var labels tName tCon scrut
            altUsages <- mapM (findFieldUsagesInAlt currentPkgName var labels tName tCon) alts
            return $ scrutUsages ++ concat altUsages
        
        Cast e' _ -> findFieldUsagesInExpr currentPkgName var labels tName tCon e'
        Tick _ e' -> findFieldUsagesInExpr currentPkgName var labels tName tCon e'
        
        _ -> return []
    
#if __GLASGOW_HASKELL__ >= 900
    findFieldUsagesInAlt :: Text -> Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldUsagesInAlt currentPkgName var labels tName tCon (Alt _ _ expr') = 
        findFieldUsagesInExpr currentPkgName var labels tName tCon expr'
    
    extractFieldsFromAlt :: Text -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt currentPkgName labels tName tCon (Alt (DataAlt altCon) boundVars _) 
        | altCon == dataCon = do
            -- Filter by package - only create usages for current package types
            let packagePattern = "$" <> currentPkgName <> "-"
                shouldInclude = packagePattern `T.isPrefixOf` tCon
            
            if shouldInclude
                then do
                    -- This alternative matches our data constructor - bound vars are fields
                    let usages = zipWith (\var label -> FieldUsage
                            { fieldUsageName = pack $ unpackFS $ flLabel label
                            , fieldUsageType = PatternMatch
                            , fieldUsageTypeName = tName
                            , fieldUsageModule = modName
                            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr var
                            , fieldUsageTypeConstructor = tCon
                            }) boundVars labels
                    return usages
                else return []
    extractFieldsFromAlt _ _ _ _ _ = return []
#else
    findFieldUsagesInAlt :: Text -> Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldsInAlt currentPkgName var labels tName tCon (_, _, expr') = 
        findFieldUsagesInExpr currentPkgName var labels tName tCon expr'
    
    extractFieldsFromAlt :: Text -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt currentPkgName labels tName tCon (DataAlt altCon, boundVars, _) 
        | altCon == dataCon = do
            -- Filter by package - only create usages for current package types
            let packagePattern = "$" <> currentPkgName <> "-"
                shouldInclude = packagePattern `T.isPrefixOf` tCon
            
            if shouldInclude
                then do
                    -- This alternative matches our data constructor - bound vars are fields
                    let usages = zipWith (\var label -> FieldUsage
                            { fieldUsageName = pack $ unpackFS $ flLabel label
                            , fieldUsageType = PatternMatch
                            , fieldUsageTypeName = tName
                            , fieldUsageModule = modName
                            , fieldUsageLocation = pack $ showSDocUnsafe $ ppr var
                            , fieldUsageTypeConstructor = tCon
                            }) boundVars labels
                    return usages
                else return []
    extractFieldsFromAlt _ _ _ _ _ = return []
#endif

-- | Extract package name from full unit string
-- Examples:
--   "euler-api-gateway-0.1.0.1-inplace" -> "euler-api-gateway"
--   "euler-db-22.12.0-3et0lfACrmh4W6cv3STvRQ" -> "euler-db"
--   "common-0.1.0.1-inplace" -> "common"
extractPackageName :: Text -> Text
extractPackageName unitStr =
    -- Package format: name-version-hash or name-version-inplace
    -- We want just the "name" part before the first version number
    let parts = T.splitOn "-" unitStr
        -- Take parts until we hit a version number (starts with digit)
        nameParts = takeWhile (not . startsWithDigit) parts
    in T.intercalate "-" nameParts
  where
    startsWithDigit :: Text -> Bool
    startsWithDigit t = case T.uncons t of
        Just (c, _) -> c >= '0' && c <= '9'
        Nothing -> False
