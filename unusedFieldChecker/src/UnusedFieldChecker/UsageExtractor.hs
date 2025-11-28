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
import Data.Char (isUpper, isLower)
import Data.List (foldl')
import Data.Text (Text, pack)
import qualified Data.Text as T
import UnusedFieldChecker.Types

#if __GLASGOW_HASKELL__ >= 900
extractFieldUsagesFromCore :: Text -> Unit -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName currentPkg binds = do
    let currentPkgName = extractPackageName $ pack $ unitString currentPkg
    putStrLn $ "[DEBUG PKG] Module: " ++ T.unpack modName ++ " Package: " ++ T.unpack currentPkgName
    allUsages <- mapM (extractUsagesFromBind modName currentPkgName) binds
    return $ concat allUsages

extractUsagesFromBind :: Text -> Text -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkgName (NonRec binder expr) = do
#else
extractFieldUsagesFromCore :: Text -> UnitId -> [CoreBind] -> IO [FieldUsage]
extractFieldUsagesFromCore modName currentPkg binds = do
    let currentPkgName = extractPackageName $ pack $ unitIdString currentPkg
    putStrLn $ "[DEBUG PKG] Module: " ++ T.unpack modName ++ " Package: " ++ T.unpack currentPkgName
    allUsages <- mapM (extractUsagesFromBind modName currentPkgName) binds
    return $ concat allUsages

extractUsagesFromBind :: Text -> Text -> CoreBind -> IO [FieldUsage]
extractUsagesFromBind modName currentPkgName (NonRec binder expr) = do
#endif
    let binderName = pack $ getOccString $ idName binder
    
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
    
    App func args -> do
        funcUsages <- extractUsagesFromExpr modName currentPkgName func
        argUsages <- extractUsagesFromExpr modName currentPkgName args
        hasFieldUsages <- detectHasField modName currentPkgName func args
        return $ funcUsages ++ argUsages ++ hasFieldUsages
    
    Lam binder body -> do
        bodyUsages <- extractUsagesFromExpr modName currentPkgName body
        binderUsages <- extractUsagesFromBinder modName currentPkgName binder body
        return $ binderUsages ++ bodyUsages
    
    Let bind body -> do
        liftIO $ putStrLn $ "[DEBUG LET] Processing let binding in module: " ++ T.unpack modName
        bindUsages <- extractUsagesFromBind modName currentPkgName bind
        liftIO $ putStrLn $ "[DEBUG LET] Bind usages: " ++ show (length bindUsages)
        bodyUsages <- extractUsagesFromExpr modName currentPkgName body
        liftIO $ putStrLn $ "[DEBUG LET] Body usages: " ++ show (length bodyUsages)
        letPatternUsages <- extractLetPatternUsages modName currentPkgName bind
        liftIO $ putStrLn $ "[DEBUG LET] Pattern usages: " ++ show (length letPatternUsages)
        return $ bindUsages ++ bodyUsages ++ letPatternUsages
    Case scrut _ _ alts -> do
        scrutUsages <- extractUsagesFromExpr modName currentPkgName scrut
        altUsages <- mapM (extractUsagesFromAlt modName currentPkgName) alts
        return $ scrutUsages ++ concat altUsages
    Cast expr' _ -> extractUsagesFromExpr modName currentPkgName expr'
    Tick _ expr' -> extractUsagesFromExpr modName currentPkgName expr'

detectHasField :: Text -> Text -> CoreExpr -> CoreExpr -> IO [FieldUsage]
detectHasField modName currentPkgName func args = do
    case args of
        Var hasFieldVar -> do
            let varName = pack (nameStableString $ idName hasFieldVar)
            when ("$dHasField" `T.isInfixOf` varName || "getField" `T.isInfixOf` varName) $
                liftIO $ putStrLn $ "[DEBUG HasField] Found potential HasField: " ++ T.unpack varName
            detectHasFieldFromVar modName currentPkgName func hasFieldVar
        _ -> do
            lensUsages <- detectLensOperations modName currentPkgName func args
            recordUsages <- detectRecordOperations modName currentPkgName func args
            return $ lensUsages ++ recordUsages

detectHasFieldFromVar :: Text -> Text -> CoreExpr -> Id -> IO [FieldUsage]
detectHasFieldFromVar modName currentPkgName func hasFieldVar
    | "$_sys$$dHasField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) ||
      "$dHasField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) ||
      "getField" `T.isInfixOf` pack (nameStableString $ idName hasFieldVar) = do
        liftIO $ putStrLn $ "[DEBUG HasField VAR MATCH] Checking func pattern in module: " ++ T.unpack modName
        case func of
            App (App (App _ (Type fieldNameType)) (Type recordType)) (Type fieldType) -> do
                let fieldName = extractFieldNameFromType fieldNameType
                    typeName = extractTypeNameFromType recordType
                    typeConstructor = extractTypeConstructor recordType
                    location = "HasField:" <> fieldName <> ":" <> typeName

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
            _ -> do
                liftIO $ putStrLn $ "[DEBUG HasField PATTERN MISS] Func pattern doesn't match in module: " ++ T.unpack modName
                return []
    | otherwise = return []

extractFieldNameFromType :: Type -> Text
extractFieldNameFromType ty = 
    let tyStr = pack $ showSDocUnsafe $ ppr ty
    in case T.splitOn "\"" tyStr of
        (_:fieldName:_) -> fieldName
        _ -> tyStr

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

        isLibraryType = isLibraryTypeConstructor typeConstructor
        fieldLabels = dataConFieldLabels dataCon

    when ("NotificationRequestItem" `T.isInfixOf` typeName || "NotificationItem" `T.isInfixOf` typeName) $
        liftIO $ putStrLn $ "[DEBUG CASE] Pattern match on: " ++ T.unpack typeName ++
                           " BoundVars: " ++ show (length boundVars) ++
                           " FieldLabels: " ++ show (length fieldLabels)

    let packagePattern = "$" <> currentPkgName <> "-"
        shouldInclude = packagePattern `T.isPrefixOf` typeConstructor
    
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
            let packagePattern = "$" <> currentPkgName <> "-"
                shouldInclude = packagePattern `T.isPrefixOf` tCon
            
            if shouldInclude
                then do
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
            let packagePattern = "$" <> currentPkgName <> "-"
                shouldInclude = packagePattern `T.isPrefixOf` tCon
            
            if shouldInclude
                then do
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

extractPackageName :: Text -> Text
extractPackageName unitStr =
    let parts = T.splitOn "-" unitStr
        nameParts = takeWhile (not . startsWithDigit) parts
    in T.intercalate "-" nameParts
  where
    startsWithDigit :: Text -> Bool
    startsWithDigit t = case T.uncons t of
        Just (c, _) -> c >= '0' && c <= '9'
        Nothing -> False

detectLensOperations :: Text -> Text -> CoreExpr -> CoreExpr -> IO [FieldUsage]
detectLensOperations modName currentPkgName func args = do
    let funcStr = extractExprString func
        argsStr = extractExprString args

    if any (`T.isInfixOf` funcStr) ["^.", ".~", "%~", "&", "view", "set", "over"]
        then do
            let fieldName = extractPotentialFieldName argsStr
            if not (T.null fieldName)
                then return [FieldUsage
                    { fieldUsageName = fieldName
                    , fieldUsageType = LensesOptics
                    , fieldUsageTypeName = ""
                    , fieldUsageModule = modName
                    , fieldUsageLocation = "Lens:" <> fieldName
                    , fieldUsageTypeConstructor = ""
                    }]
                else return []
        else return []

detectRecordOperations :: Text -> Text -> CoreExpr -> CoreExpr -> IO [FieldUsage]
detectRecordOperations modName currentPkgName func args = do
    case func of
        Var funcVar -> do
            let funcName = pack $ getOccString $ idName funcVar
            if isRecordConstructor funcName || isFieldAccessor funcName
                then return [FieldUsage
                    { fieldUsageName = extractFieldFromName funcName
                    , fieldUsageType = if isRecordConstructor funcName then RecordConstruct else AccessorFunction
                    , fieldUsageTypeName = ""
                    , fieldUsageModule = modName
                    , fieldUsageLocation = "Record:" <> funcName
                    , fieldUsageTypeConstructor = ""
                    }]
                else return []
        _ -> return []
  where
    isRecordConstructor name = not (T.null name) && isUpper (T.head name)
    isFieldAccessor name = not (T.null name) && isLower (T.head name) && not (name `elem` commonFunctions)
    extractFieldFromName = id
    commonFunctions = ["map", "filter", "foldl", "foldr", "return", "pure", ">>=", ">>", "show", "read"]

filterSerializationUsage :: [FieldUsage] -> IO [FieldUsage]
filterSerializationUsage usages = return $ filter (not . isSerializationContext) usages
  where
    isSerializationContext :: FieldUsage -> Bool
    isSerializationContext usage =
        let modName = fieldUsageModule usage
            location = fieldUsageLocation usage
            usageName = fieldUsageName usage
        in any (`T.isInfixOf` location)
            [ "parseJSON", "toJSON", "toEncoding", ".:?", ".:!", ".:", ".="
            , "$fFromJSON", "$fToJSON", "$fGeneric", "$dm"
            ] ||
           any (`T.isSuffixOf` modName) [".FromJSON", ".ToJSON", ".Generic"] ||
           fieldUsageType usage `elem` [DerivedInstances, TemplateHaskell] ||
           "$" `T.isPrefixOf` usageName

extractPotentialFieldName :: Text -> Text
extractPotentialFieldName exprStr
    | T.isInfixOf "\"" exprStr =
        case T.splitOn "\"" exprStr of
            (_:fieldName:_) -> fieldName
            _ -> ""
    | otherwise = ""

extractExprString :: CoreExpr -> Text
extractExprString expr = case expr of
    Var v -> pack $ getOccString $ idName v
    Lit _ -> "literal"
    App e1 e2 -> extractExprString e1 <> " " <> extractExprString e2
    Lam _ body -> "lambda " <> extractExprString body
    Let _ body -> "let " <> extractExprString body
    Case _ _ _ _ -> "case"
    Cast e _ -> extractExprString e
    Tick _ e -> extractExprString e
#if __GLASGOW_HASKELL__ >= 900
    Type _ -> "type"
    Coercion _ -> "coercion"
#else
    Type _ -> "type"
    Coercion _ -> "coercion"
#endif
