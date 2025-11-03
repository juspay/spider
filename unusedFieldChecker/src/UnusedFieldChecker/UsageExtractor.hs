{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module UnusedFieldChecker.UsageExtractor
    ( extractFieldUsagesFromCore
    ) where

import Prelude hiding (log)

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
import GHC.Utils.Outputable hiding ((<>))
#else
import CoreSyn
import DataCon
import FastString
import FieldLabel
import Id
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
extractFieldUsagesFromCore :: Text -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName binds = do
    allUsages <- mapM (extractUsagesFromBind modName) binds
    return $ concat allUsages

-- | Extract usages from a single Core binding
extractUsagesFromBind :: Text -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName (NonRec _ expr) = 
    extractUsagesFromExpr modName expr
extractUsagesFromBind modName (Rec binds) = do
    usages <- mapM (\(_, expr) -> extractUsagesFromExpr modName expr) binds
    return $ concat usages

-- | Extract usages from a Core expression
extractUsagesFromExpr :: Text -> CoreExpr -> IO [FieldUsage]
extractUsagesFromExpr modName expr = case expr of
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
        funcUsages <- extractUsagesFromExpr modName func
        argUsages <- extractUsagesFromExpr modName args
        hasFieldUsages <- detectHasField modName func args
        return $ funcUsages ++ argUsages ++ hasFieldUsages
    
    -- Lambda: recurse into body and check for record patterns in binder
    Lam binder body -> do
        bodyUsages <- extractUsagesFromExpr modName body
        -- Check if lambda binds a record type and extracts fields
        binderUsages <- extractUsagesFromBinder modName binder body
        return $ binderUsages ++ bodyUsages
    
    -- Let: extract from both binding and body, including record destructuring
    Let bind body -> do
        bindUsages <- extractUsagesFromBind modName bind
        bodyUsages <- extractUsagesFromExpr modName body
        -- Extract field usages from let-bound record patterns
        letPatternUsages <- extractLetPatternUsages modName bind
        return $ bindUsages ++ bodyUsages ++ letPatternUsages
    
    -- Case: extract from scrutinee and alternatives
    Case scrut _ _ alts -> do
        scrutUsages <- extractUsagesFromExpr modName scrut
        altUsages <- mapM (extractUsagesFromAlt modName) alts
        return $ scrutUsages ++ concat altUsages
    
    -- Cast and Tick: recurse through
    Cast expr' _ -> extractUsagesFromExpr modName expr'
    Tick _ expr' -> extractUsagesFromExpr modName expr'

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
extractUsagesFromAlt :: Text -> CoreAlt -> IO [FieldUsage]
#if __GLASGOW_HASKELL__ >= 900
extractUsagesFromAlt modName (Alt altCon boundVars expr) = 
    extractUsagesFromAlt' modName (altCon, boundVars, expr)
#else
extractUsagesFromAlt modName (altCon, boundVars, expr) = 
    extractUsagesFromAlt' modName (altCon, boundVars, expr)
#endif

extractUsagesFromAlt' :: Text -> (AltCon, [Var], CoreExpr) -> IO [FieldUsage]
extractUsagesFromAlt' modName (DataAlt dataCon, boundVars, expr) = do
    let typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
        typeConstructor = typeName
        
        -- Each bound variable represents a field being pattern matched
        -- We need to match these to actual field names from the data constructor
        fieldLabels = dataConFieldLabels dataCon
        
        -- Create usages for fields that are actually bound in the pattern
        patternUsages = if null fieldLabels
            then []  -- Constructor has no fields
            else zipWith (\var label -> FieldUsage
                { fieldUsageName = pack $ unpackFS $ flLabel label
                , fieldUsageType = PatternMatch
                , fieldUsageTypeName = typeName
                , fieldUsageModule = modName
                , fieldUsageLocation = pack $ showSDocUnsafe $ ppr var
                , fieldUsageTypeConstructor = typeConstructor
                }) boundVars fieldLabels
    
    exprUsages <- extractUsagesFromExpr modName expr
    return $ patternUsages ++ exprUsages

extractUsagesFromAlt' modName (_, _, expr) = 
    extractUsagesFromExpr modName expr

extractLetPatternUsages :: Text -> CoreBind -> IO [FieldUsage]
extractLetPatternUsages modName (NonRec binder expr) = do
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            let fieldLabels = dataConFieldLabels dataCon
                typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
                typeConstructor = typeName
            
            fieldUsages <- extractFieldSelectorsFromExpr modName binder dataCon expr
            return fieldUsages
        Nothing -> return []

extractLetPatternUsages modName (Rec binds) = do
    usages <- mapM (\(binder, expr) -> extractLetPatternUsages modName (NonRec binder expr)) binds
    return $ concat usages

extractUsagesFromBinder :: Text -> Var -> CoreExpr -> IO [FieldUsage]
extractUsagesFromBinder modName binder body = do
    let binderType = idType binder
    case getRecordDataCon binderType of
        Just dataCon -> do
            extractFieldSelectorsFromExpr modName binder dataCon body
        Nothing -> return []

getRecordDataCon :: Type -> Maybe DataCon
getRecordDataCon ty = case splitTyConApp_maybe ty of
    Just (tyCon, _) -> 
        case tyConDataCons tyCon of
            [dataCon] | not (null $ dataConFieldLabels dataCon) -> Just dataCon
            _ -> Nothing
    Nothing -> Nothing

extractFieldSelectorsFromExpr :: Text -> Var -> DataCon -> CoreExpr -> IO [FieldUsage]
extractFieldSelectorsFromExpr modName targetVar dataCon expr = do
    let fieldLabels = dataConFieldLabels dataCon
        typeName = pack $ nameStableString $ tyConName $ dataConTyCon dataCon
        typeConstructor = typeName
    
    fieldUsages <- findFieldUsagesInExpr targetVar fieldLabels typeName typeConstructor expr
    return fieldUsages
  where
    findFieldUsagesInExpr :: Var -> [FieldLabel] -> Text -> Text -> CoreExpr -> IO [FieldUsage]
    findFieldUsagesInExpr var labels tName tCon e = case e of
        Case (Var scrutVar) _ _ alts | scrutVar == var -> do
            altUsages <- mapM (extractFieldsFromAlt labels tName tCon) alts
            return $ concat altUsages
        
        -- Recurse into sub-expressions
        App e1 e2 -> do
            u1 <- findFieldUsagesInExpr var labels tName tCon e1
            u2 <- findFieldUsagesInExpr var labels tName tCon e2
            return $ u1 ++ u2
        
        Lam _ body -> findFieldUsagesInExpr var labels tName tCon body
        
        Let bind body -> do
            bindUsages <- case bind of
                NonRec _ bindExpr -> findFieldUsagesInExpr var labels tName tCon bindExpr
                Rec binds' -> do
                    usages <- mapM (\(_, bindExpr) -> findFieldUsagesInExpr var labels tName tCon bindExpr) binds'
                    return $ concat usages
            bodyUsages <- findFieldUsagesInExpr var labels tName tCon body
            return $ bindUsages ++ bodyUsages
        
        Case scrut _ _ alts -> do
            scrutUsages <- findFieldUsagesInExpr var labels tName tCon scrut
            altUsages <- mapM (findFieldUsagesInAlt var labels tName tCon) alts
            return $ scrutUsages ++ concat altUsages
        
        Cast e' _ -> findFieldUsagesInExpr var labels tName tCon e'
        Tick _ e' -> findFieldUsagesInExpr var labels tName tCon e'
        
        _ -> return []
    
#if __GLASGOW_HASKELL__ >= 900
    findFieldUsagesInAlt :: Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldUsagesInAlt var labels tName tCon (Alt _ _ expr') = 
        findFieldUsagesInExpr var labels tName tCon expr'
#else
    findFieldUsagesInAlt :: Var -> [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    findFieldUsagesInAlt var labels tName tCon (_, _, expr') = 
        findFieldUsagesInExpr var labels tName tCon expr'
#endif
    
#if __GLASGOW_HASKELL__ >= 900
    extractFieldsFromAlt :: [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt labels tName tCon (Alt (DataAlt altCon) boundVars _) 
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
    extractFieldsFromAlt _ _ _ _ = return []
#else
    extractFieldsFromAlt :: [FieldLabel] -> Text -> Text -> CoreAlt -> IO [FieldUsage]
    extractFieldsFromAlt labels tName tCon (DataAlt altCon, boundVars, _) 
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
    extractFieldsFromAlt _ _ _ _ = return []
#endif
