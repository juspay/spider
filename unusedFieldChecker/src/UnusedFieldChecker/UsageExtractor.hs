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
import GHC.Unit.Types (Unit, moduleUnit)
import GHC.Utils.Outputable hiding ((<>))
#else
import CoreSyn
import DataCon
import FastString
import FieldLabel
import Id
import Module (moduleUnitId)
import Name
import Outputable
import TyCon
import TyCoRep
import Type
import Var
#endif

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
    allUsages <- mapM (extractUsagesFromBind modName currentPkg) binds
    return $ concat allUsages

-- | Extract usages from a single Core binding
extractUsagesFromBind :: Text -> Unit -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkg (NonRec binder expr) = do
#else
extractFieldUsagesFromCore :: Text -> UnitId -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName currentPkg binds = do
    allUsages <- mapM (extractUsagesFromBind modName currentPkg) binds
    return $ concat allUsages

-- | Extract usages from a single Core binding
extractUsagesFromBind :: Text -> UnitId -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkg (NonRec binder expr) = do
#endif
    let binderName = pack $ getOccString $ idName binder
    
    -- Skip derived/compiler-generated bindings entirely
    if isDerivedBinding binderName
        then return []
        else extractUsagesFromExpr modName currentPkg expr

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromBind modName currentPkg (Rec binds) = do
    usages <- mapM (\(binder, expr) -> 
        let binderName = pack $ getOccString $ idName binder
        in if isDerivedBinding binderName
            then return []
            else extractUsagesFromExpr modName currentPkg expr
        ) binds
    return $ concat usages

-- | Extract usages from a Core expression
extractUsagesFromExpr :: Text -> Unit -> CoreExpr -> IO [FieldUsage]
extractUsagesFromExpr modName currentPkg expr = case expr of
#else
extractUsagesFromBind modName currentPkg (Rec binds) = do
    usages <- mapM (\(binder, expr) -> 
        let binderName = pack $ getOccString $ idName binder
        in if isDerivedBinding binderName
            then return []
            else extractUsagesFromExpr modName currentPkg expr
        ) binds
    return $ concat usages

-- | Extract usages from a Core expression
extractUsagesFromExpr :: Text -> UnitId -> CoreExpr -> IO [FieldUsage]
extractUsagesFromExpr modName currentPkg expr = case expr of
#endif
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
        funcUsages <- extractUsagesFromExpr modName currentPkg func
        argUsages <- extractUsagesFromExpr modName currentPkg args
        hasFieldUsages <- detectHasField modName func args
        return $ funcUsages ++ argUsages ++ hasFieldUsages
    
    -- Lambda: recurse into body and check for record patterns in binder
    Lam binder body -> do
        bodyUsages <- extractUsagesFromExpr modName currentPkg body
        -- Check if lambda binds a record type and extracts fields
        binderUsages <- extractUsagesFromBinder modName currentPkg binder body
        return $ binderUsages ++ bodyUsages
    
    -- Let: extract from both binding and body, including record destructuring
    Let bind body -> do
        bindUsages <- extractUsagesFromBind modName currentPkg bind
        bodyUsages <- extractUsagesFromExpr modName currentPkg body
        -- Extract field usages from let-bound record patterns
        letPatternUsages <- extractLetPatternUsages modName currentPkg bind
        return $ bindUsages ++ bodyUsages ++ letPatternUsages
    
    -- Case: extract from scrutinee and alternatives
    Case scrut _ _ alts -> do
        scrutUsages <- extractUsagesFromExpr modName currentPkg scrut
        altUsages <- mapM (extractUsagesFromAlt modName currentPkg) alts
        return $ scrutUsages ++ concat altUsages
    
    -- Cast and Tick: recurse through
    Cast expr' _ -> extractUsagesFromExpr modName currentPkg expr'
    Tick _ expr' -> extractUsagesFromExpr modName currentPkg expr'

-- | Detect HasField constraints (the key to nested field access!)
detectHasField :: Text -> CoreExpr -> CoreExpr -> IO [FieldUsage]
detectHasField modName func (Var hasFieldVar)
    | "$_sys$$dHasField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) = do
        -- Extract field info from HasField constraint
        case func of
            -- Pattern: App (App (App _ (Type fieldName)) (Type recordType)) (Type fieldType)
            App (App (App _ (Type fieldNameType)) (Type recordType)) (Type fieldType) -> do
                let fieldName = extractFieldNameFromType fieldNameType
                    typeName = extractTypeNameFromType recordType
                    typeConstructor = extractTypeConstructor recordType
                    -- Use a simple string representation for location
                    location = "HasField:" <> fieldName <> ":" <> typeName
                
                return [FieldUsage
                    { fieldUsageName = fieldName
                    , fieldUsageType = HasFieldOverloaded
                    , fieldUsageTypeName = typeName
                    , fieldUsageModule = modName
                    , fieldUsageLocation = location
                    , fieldUsageTypeConstructor = typeConstructor
                    }]
            _ -> return []
    | otherwise = return []
detectHasField _ _ _ = return []

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
extractUsagesFromAlt :: Text -> Unit -> CoreAlt -> IO [FieldUsage]
extractUsagesFromAlt modName currentPkg (Alt altCon boundVars expr) = 
    extractUsagesFromAlt' modName currentPkg (altCon, boundVars, expr)

extractUsagesFromAlt' :: Text -> Unit -> (AltCon, [Var], CoreExpr) -> IO [FieldUsage]
extractUsagesFromAlt' modName currentPkg (DataAlt dataCon, boundVars, expr) = do
#else
extractUsagesFromAlt :: Text -> UnitId -> CoreAlt -> IO [FieldUsage]
extractUsagesFromAlt modName currentPkg (altCon, boundVars, expr) = 
    extractUsagesFromAlt' modName currentPkg (altCon, boundVars, expr)

extractUsagesFromAlt' :: Text -> UnitId -> (AltCon, [Var], CoreExpr) -> IO [FieldUsage]
extractUsagesFromAlt' modName currentPkg (DataAlt dataCon, boundVars, expr) = do
#endif
    let tc = dataConTyCon dataCon
        tyConName = getName tc
        typeName = pack $ nameStableString tyConName
        typeConstructor = typeName
        
        -- Check if this is a library type being pattern matched
        isLibraryType = isLibraryTypeConstructor typeConstructor
        
        -- Each bound variable represents a field being pattern matched
        -- We need to match these to actual field names from the data constructor
        fieldLabels = dataConFieldLabels dataCon
    
    -- OPTIMIZATION: Check package FIRST, before creating usages
#if __GLASGOW_HASKELL__ >= 900
    let shouldInclude = case nameModule_maybe tyConName of
            Just mod -> moduleUnit mod == currentPkg
            Nothing -> True
#else
    let shouldInclude = case nameModule_maybe tyConName of
            Just mod -> moduleUnitId mod == currentPkg
            Nothing -> True
#endif
    
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
    
    exprUsages <- extractUsagesFromExpr modName currentPkg expr
    return $ patternUsages ++ exprUsages

#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromAlt' modName currentPkg (_, _, expr) = 
    extractUsagesFromExpr modName currentPkg expr

extractLetPatternUsages :: Text -> Unit -> CoreBind -> IO [FieldUsage]
extractLetPatternUsages modName currentPkg (NonRec binder expr) = do
#else
extractUsagesFromAlt' modName currentPkg (_, _, expr) = 
    extractUsagesFromExpr modName currentPkg expr

extractLetPatternUsages :: Text -> UnitId -> CoreBind -> IO [FieldUsage]
extractLetPatternUsages modName currentPkg (NonRec binder expr) = do
#endif
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            let fieldLabels = dataConFieldLabels dataCon
                typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
                typeConstructor = typeName
            
            fieldUsages <- extractFieldSelectorsFromExpr modName currentPkg binder dataCon expr
            return fieldUsages
        Nothing -> return []

#if __GLASGOW_HASKELL__ >= 900
extractLetPatternUsages modName currentPkg (Rec binds) = do
    usages <- mapM (\(binder, expr) -> extractLetPatternUsages modName currentPkg (NonRec binder expr)) binds
    return $ concat usages

extractUsagesFromBinder :: Text -> Unit -> Var -> CoreExpr -> IO [FieldUsage]
extractUsagesFromBinder modName currentPkg binder body = do
#else
extractLetPatternUsages modName currentPkg (Rec binds) = do
    usages <- mapM (\(binder, expr) -> extractLetPatternUsages modName currentPkg (NonRec binder expr)) binds
    return $ concat usages

extractUsagesFromBinder :: Text -> UnitId -> Var -> CoreExpr -> IO [FieldUsage]
extractUsagesFromBinder modName currentPkg binder body = do
#endif
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            extractFieldSelectorsFromExpr modName currentPkg binder dataCon body
        Nothing -> return []

getRecordDataCon :: Type -> Maybe DataCon
getRecordDataCon ty = case splitTyConApp_maybe ty of
    Just (tyCon, _) -> 
        case tyConDataCons tyCon of
            [dataCon] | not (null $ dataConFieldLabels dataCon) -> Just dataCon
            _ -> Nothing
    Nothing -> Nothing

#if __GLASGOW_HASKELL__ >= 900
extractFieldSelectorsFromExpr :: Text -> Unit -> Var -> DataCon -> CoreExpr -> IO [FieldUsage]
extractFieldSelectorsFromExpr modName currentPkg targetVar dataCon expr = do
#else
extractFieldSelectorsFromExpr :: Text -> UnitId -> Var -> DataCon -> CoreExpr -> IO [FieldUsage]
extractFieldSelectorsFromExpr modName currentPkg targetVar dataCon expr = do
#endif
    let fieldLabels = dataConFieldLabels dataCon
        typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
        typeConstructor = typeName
    
    fieldUsages <- findFieldUsagesInExpr currentPkg targetVar fieldLabels typeName typeConstructor expr
    return fieldUsages
  where
#if __GLASGOW_HASKELL__ >= 900
    findFieldUsagesInExpr :: Unit -> Var -> [FieldLabel] -> Text -> Text -> CoreExpr -> IO [FieldUsage]
    findFieldUsagesInExpr currentPkg var labels tName tCon e = case e of
        Case (Var scrutVar) _ _ alts | scrutVar == var -> do
            altUsages <- mapM (extractFieldsFromAlt currentPkg labels tName tCon) alts
            return $ concat altUsages
        
        -- Recurse into sub-expressions
        App e1 e2 -> do
            u1 <- findFieldUsagesInExpr currentPkg var labels tName tCon e1
            u2 <- findFieldUsagesInExpr currentPkg var labels tName tCon e2
            return $ u1 ++ u2
        
        Lam _ body -> findFieldUsagesInExpr currentPkg var labels tName tCon body
        
        Let bind body -> do
            bindUsages <- case bind of
                NonRec _ bindExpr -> findFieldUsagesInExpr currentPkg var labels tName tCon bindExpr
                Rec binds' -> do
                    usages <- mapM (\(_, bindExpr) -> findFieldUsagesInExpr currentPkg var labels tName tCon bindExpr) binds'
                    return $ concat usages
            bodyUsages <- findFieldUsagesInExpr currentPkg var labels tName tCon body
            return $ bindUsages ++ bodyUsages
        
        Case scrut _ _ alts -> do
            scrutUsages <- findFieldUsagesInExpr currentPkg var labels tName tCon scrut
            altUsages <- mapM (findFieldUsagesInAlt currentPkg var labels tName tCon) alts
            return $ scrutUsages ++ concat altUsages
        
        Cast e' _ -> findFieldUsagesInExpr currentPkg var labels tName tCon e'
        Tick _ e' -> findFieldUsagesInExpr currentPkg var labels tName tCon e'
        
        _ -> return []
#else
    findFieldUsagesInExpr :: UnitId -> Var -> [FieldLabel] -> Text -> Text -> CoreExpr -> IO [FieldUsage]
    findFieldUsagesInExpr currentPkg var labels tName tCon e = case e of
        Case (Var scrutVar) _ _ alts | scrutVar == var -> do
            altUsages <- mapM (extractFieldsFromAlt currentPkg labels tName tCon) alts
            return $ concat altUsages
        
        -- Recurse into sub-expressions
        App e1 e2 -> do
            u1 <- findFieldUsagesInExpr currentPkg var labels tName tCon e1
            u2 <- findFieldUsagesInExpr currentPkg var labels tName tCon e2
            return $ u1 ++ u2
        
        Lam _ body -> findFieldUsagesInExpr currentPkg var labels tName tCon body
        
        Let bind body -> do
            bindUsages <- case bind of
                NonRec _ bindExpr -> findFieldUsagesInExpr currentPkg var labels tName tCon bindExpr
                Rec binds' -> do
                    usages <- mapM (\(_, bindExpr) -> findFieldUsagesInExpr currentPkg var labels tName tCon bindExpr) binds'
                    return $ concat usages
            bodyUsages <- findFieldUsagesInExpr currentPkg var labels tName tCon body
            return $ bindUsages ++ bodyUsages
        
        Case scrut _ _ alts -> do
            scrutUsages <- findFieldUsagesInExpr currentPkg var labels tName tCon scrut
            altUsages <- mapM (findFieldUsagesInAlt currentPkg var labels tName tCon) alts
            return $ scrutUsages ++ concat altUsages
        
        Cast e' _ -> findFieldUsagesInExpr currentPkg var labels tName tCon e'
        Tick _ e' -> findFieldUsagesInExpr currentPkg var labels tName tCon e'
        
        _ -> return []
#endif
    
#if __GLASGOW_HASKELL__ >= 900
    findFieldUsagesInAlt :: Unit -> Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldUsagesInAlt currentPkg var labels tName tCon (Alt _ _ expr') = 
        findFieldUsagesInExpr currentPkg var labels tName tCon expr'
    
    extractFieldsFromAlt :: Unit -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt currentPkg labels tName tCon (Alt (DataAlt altCon) boundVars _) 
        | altCon == dataCon = do
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
    extractFieldsFromAlt _ _ _ _ _ = return []
#else
    findFieldUsagesInAlt :: UnitId -> Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldUsagesInAlt currentPkg var labels tName tCon (_, _, expr') = 
        findFieldUsagesInExpr currentPkg var labels tName tCon expr'
    
    extractFieldsFromAlt :: UnitId -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt currentPkg labels tName tCon (DataAlt altCon, boundVars, _) 
        | altCon == dataCon = do
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
    extractFieldsFromAlt _ _ _ _ _ = return []
#endif
