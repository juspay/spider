{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Validator
    ( aggregateFieldInfo
    , validateFields
    , validateFieldsWithExclusions
    , validateFieldsForTypesUsedInConfiguredModules
    , reportUnusedFields
    , categorizeFieldByType
    , isFieldMaybeType
    , filterSerializationUsages
    , validateServantAPITypes
    , formatMissingFieldCheckerError
    , formatRecursiveMissingError
    ) where

import Control.Monad (when)
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import UnusedFieldChecker.Types
import UnusedFieldChecker.Config


aggregateFieldInfo :: [ModuleFieldInfo] -> AggregatedFieldInfo
aggregateFieldInfo modules =
    let allDefs = concatMap moduleFieldDefs modules
        allUsages = concatMap moduleFieldUsages modules
        
        defMap = foldl' (\acc def -> 
            Map.insertWith (++) (fieldDefName def) [def] acc
            ) Map.empty allDefs
        
        usageMap = foldl' (\acc usage ->
            Map.insertWith (++) (fieldUsageName usage) [usage] acc
            ) Map.empty allUsages
    
    in AggregatedFieldInfo
        { allFieldDefs = defMap
        , allFieldUsages = usageMap
        }

validateFields :: AggregatedFieldInfo -> ValidationResult
validateFields AggregatedFieldInfo{..} =
    let allDefs = concat $ Map.elems allFieldDefs
        
        (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) allDefs
    
    in ValidationResult
        { unusedNonMaybeFields = nub unusedNonMaybe
        , unusedMaybeFields = nub unusedMaybe
        , usedFields = nub used
        }
  where
    categorizeField :: ([FieldDefinition], [FieldDefinition], [FieldDefinition]) 
                    -> FieldDefinition 
                    -> ([FieldDefinition], [FieldDefinition], [FieldDefinition])
    categorizeField (unusedMaybe, unusedNonMaybe, used) fieldDef =
        let fieldName = fieldDefName fieldDef
            isUsed = case Map.lookup fieldName allFieldUsages of
                Nothing -> False
                Just usages -> any isRealUsage usages
            isSingleFieldRecord = fieldDefIsSingleField fieldDef
            _ = unsafePerformIO $ when (fieldName == "notificationRequestItem") $ do
                    putStrLn $ "[DEBUG SINGLE] Field: " ++ T.unpack fieldName ++
                              " isSingleField: " ++ show isSingleFieldRecord ++
                              " isUsed: " ++ show isUsed
        in if isUsed || isSingleFieldRecord
            then (unusedMaybe, unusedNonMaybe, fieldDef : used)
            else if fieldDefIsMaybe fieldDef
                then (fieldDef : unusedMaybe, unusedNonMaybe, used)
                else (unusedMaybe, fieldDef : unusedNonMaybe, used)
      where
        isRealUsage usage = case fieldUsageType usage of
            AccessorFunction -> True
            PatternMatch -> True
            NamedFieldPuns -> True
            RecordWildCards -> True
            RecordDotSyntax -> True
            RecordConstruct -> True
            RecordUpdate -> True
            HasFieldOverloaded -> True
            FunctionComposition -> True
            LensesOptics -> True
            GenericReflection -> True
            TemplateHaskell -> False
            DerivedInstances -> False
            DataSYB -> False

validateFieldsWithExclusions :: ExclusionConfig -> AggregatedFieldInfo -> ValidationResult
validateFieldsWithExclusions exclusionConfig AggregatedFieldInfo{..} =
    let allDefs = concat $ Map.elems allFieldDefs
        allowedModuleDefs = filter (isFromAllowedModule exclusionConfig) allDefs

        (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) allowedModuleDefs

    in ValidationResult
        { unusedNonMaybeFields = nub unusedNonMaybe
        , unusedMaybeFields = nub unusedMaybe
        , usedFields = nub used
        }
  where
    isFromAllowedModule :: ExclusionConfig -> FieldDefinition -> Bool
    isFromAllowedModule ExclusionConfig{..} FieldDefinition{..} =
        case includeFiles of
            Just includes -> any (`matchesPattern` fieldDefModule) includes
            Nothing -> True
      where
        matchesPattern :: Text -> Text -> Bool
        matchesPattern pattern modName
            | pattern == "*" = True
            | T.isSuffixOf ".*" pattern =
                let prefix = T.dropEnd 2 pattern
                in prefix `T.isPrefixOf` modName
            | T.isPrefixOf "*." pattern =
                let suffix = T.drop 1 pattern
                in suffix `T.isSuffixOf` modName
            | otherwise = pattern == modName

    categorizeField :: ([FieldDefinition], [FieldDefinition], [FieldDefinition])
                    -> FieldDefinition
                    -> ([FieldDefinition], [FieldDefinition], [FieldDefinition])
    categorizeField (unusedMaybe, unusedNonMaybe, used) fieldDef =
        let fieldName = fieldDefName fieldDef
            typeName = fieldDefTypeName fieldDef
            defModule = fieldDefModule fieldDef
            fullyQualifiedType = fieldDefFullyQualifiedType fieldDef
            isUsed = case Map.lookup fieldName allFieldUsages of
                Nothing -> False
                Just usages -> any (isRealUsageOfThisField fieldDef) usages
        in if isUsed
            then (unusedMaybe, unusedNonMaybe, fieldDef : used)
            else if fieldDefIsMaybe fieldDef
                then (fieldDef : unusedMaybe, unusedNonMaybe, used)
                else (unusedMaybe, fieldDef : unusedNonMaybe, used)
      where
        isRealUsageOfThisField :: FieldDefinition -> FieldUsage -> Bool
        isRealUsageOfThisField fieldDef usage =
            let fieldName = fieldDefName fieldDef
                defTypeConstructor = fieldDefTypeConstructor fieldDef
                usageName = fieldUsageName usage
                usageTypeConstructor = fieldUsageTypeConstructor usage
                nameMatches = usageName == fieldName
                typeMatches = case (usageTypeConstructor, defTypeConstructor) of
                    ("", _) -> True  -- Unknown type in usage, conservatively match
                    (_, "") -> True  -- Unknown type in definition, conservatively match
                    (usageType, defType) -> usageType == defType
                isExplicitRecordOp = case fieldUsageType usage of
                    RecordConstruct -> True
                    RecordUpdate -> True
                    PatternMatch -> True
                    NamedFieldPuns -> True
                    RecordWildCards -> True
                    RecordDotSyntax -> True
                    HasFieldOverloaded -> True  -- From Core-level HasField detection
                    GenericReflection -> True

                    AccessorFunction -> typeMatches
                    FunctionComposition -> typeMatches
                    LensesOptics -> typeMatches

                    TemplateHaskell -> False
                    DerivedInstances -> False
                    DataSYB -> False

            in nameMatches && (isExplicitRecordOp || typeMatches)

categorizeFieldByType :: FieldDefinition -> [FieldUsage] -> FieldCategory
categorizeFieldByType fieldDef usages =
    let hasRealUsage = any isRealFieldUsage usages
        isMaybeField = isFieldMaybeType (fieldDefType fieldDef)
    in if hasRealUsage
        then UsedField
        else if isMaybeField
            then UnusedMaybeField
            else UnusedNonMaybeField
  where
    isRealFieldUsage usage = case fieldUsageType usage of
        AccessorFunction -> True
        PatternMatch -> True
        RecordConstruct -> True
        RecordUpdate -> True
        RecordDotSyntax -> True
        HasFieldOverloaded -> True
        LensesOptics -> True
        _ -> False

isFieldMaybeType :: Text -> Bool
isFieldMaybeType fieldType =
    let normalized = T.strip fieldType
    in "Maybe" `T.isPrefixOf` normalized ||
       "Maybe (" `T.isPrefixOf` normalized ||
       "Maybe(" `T.isPrefixOf` normalized ||
       " -> Maybe" `T.isInfixOf` normalized ||
       "m (Maybe" `T.isInfixOf` normalized

data FieldCategory
    = UsedField
    | UnusedMaybeField
    | UnusedNonMaybeField
    deriving (Show, Eq)

filterSerializationUsages :: [FieldUsage] -> [FieldUsage]
filterSerializationUsages = filter (not . isSerializationUsage)
  where
    isSerializationUsage usage =
        let location = fieldUsageLocation usage
            moduleName = fieldUsageModule usage
            usageType = fieldUsageType usage
        in any (`T.isInfixOf` location) serializationKeywords ||
           any (`T.isSuffixOf` moduleName) serializationModules ||
           usageType `elem` [DerivedInstances, TemplateHaskell]

    serializationKeywords =
        [ "parseJSON", "toJSON", "toEncoding"
        , ".:?", ".:!", ".:", ".="
        , "$fFromJSON", "$fToJSON", "$fGeneric"
        , "FromJSON", "ToJSON", "Generic"
        ]

    serializationModules =
        [ ".FromJSON", ".ToJSON", ".Generic"
        , ".Aeson", ".Data.Aeson"
        ]

validateFieldsForTypesUsedInConfiguredModules :: ExclusionConfig -> AggregatedFieldInfo -> IO ValidationResult
validateFieldsForTypesUsedInConfiguredModules exclusionConfig AggregatedFieldInfo{..} = do
    let allDefs = concat $ Map.elems allFieldDefs

        allowedModuleDefs = filter (isFromAllowedModule exclusionConfig) allDefs

    putStrLn $ "\n[DEBUG Phase 2] Total field definitions: " ++ show (length allDefs)
    putStrLn $ "[DEBUG Phase 2] Allowed module definitions: " ++ show (length allowedModuleDefs)
    case includeFiles exclusionConfig of
        Just includes -> putStrLn $ "[DEBUG Phase 2] Configured modules: " ++ show includes
        Nothing -> putStrLn $ "[DEBUG Phase 2] No configured modules (includeFiles is Nothing)"

    let (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) allowedModuleDefs

    putStrLn $ "[DEBUG Phase 2] Results:"
    putStrLn $ "  - Used fields: " ++ show (length used)
    putStrLn $ "  - Unused Maybe fields: " ++ show (length unusedMaybe)
    putStrLn $ "  - Unused non-Maybe fields: " ++ show (length unusedNonMaybe)
    putStrLn $ "[DEBUG Phase 2] Unused non-Maybe fields:"
    mapM_ (\field -> putStrLn $ "    " ++ T.unpack (fieldDefName field) ++ " :: " ++ T.unpack (fieldDefType field) ++ " (in " ++ T.unpack (fieldDefTypeName field) ++ ")") unusedNonMaybe

    return ValidationResult
        { unusedNonMaybeFields = nub unusedNonMaybe
        , unusedMaybeFields = nub unusedMaybe
        , usedFields = nub used
        }
  where
    isFromAllowedModule :: ExclusionConfig -> FieldDefinition -> Bool
    isFromAllowedModule ExclusionConfig{..} FieldDefinition{..} =
        case includeFiles of
            Just includes -> any (`matchesPattern` fieldDefModule) includes
            Nothing -> True
      where
        matchesPattern :: Text -> Text -> Bool
        matchesPattern pattern modName
            | pattern == "*" = True
            | T.isSuffixOf ".*" pattern =
                let prefix = T.dropEnd 2 pattern
                in prefix `T.isPrefixOf` modName
            | T.isPrefixOf "*." pattern =
                let suffix = T.drop 1 pattern
                in suffix `T.isSuffixOf` modName
            | otherwise = pattern == modName

    isUsageInConfiguredModule :: ExclusionConfig -> FieldUsage -> Bool
    isUsageInConfiguredModule ExclusionConfig{..} FieldUsage{..} =
        case includeFiles of
            Just includes -> any (`matchesPattern` fieldUsageModule) includes
            Nothing -> False  -- If no includes specified, don't enforce Phase 2 validation
      where
        matchesPattern :: Text -> Text -> Bool
        matchesPattern pattern modName
            | pattern == "*" = True
            | T.isSuffixOf ".*" pattern =
                let prefix = T.dropEnd 2 pattern
                in prefix `T.isPrefixOf` modName
            | T.isPrefixOf "*." pattern =
                let suffix = T.drop 1 pattern
                in suffix `T.isSuffixOf` modName
            | otherwise = pattern == modName

    categorizeField :: ([FieldDefinition], [FieldDefinition], [FieldDefinition])
                    -> FieldDefinition
                    -> ([FieldDefinition], [FieldDefinition], [FieldDefinition])
    categorizeField (unusedMaybe, unusedNonMaybe, used) fieldDef =
        let fieldName = fieldDefName fieldDef

            isSingleFieldRecord = fieldDefIsSingleField fieldDef

            (hasUsageInConfiguredModules, usageDetails) = case Map.lookup fieldName allFieldUsages of
                Nothing -> (False, "no usages found")
                Just usages ->
                    let configuredUsages = filter (isUsageInConfiguredModule exclusionConfig) usages
                        allUsageModules = map fieldUsageModule usages
                        configuredUsageModules = map fieldUsageModule configuredUsages
                    in (not (null configuredUsages),
                        "total usages: " ++ show (length usages) ++
                        ", in modules: " ++ show allUsageModules ++
                        ", configured usages: " ++ show (length configuredUsages) ++
                        ", configured modules: " ++ show configuredUsageModules)

            _ = unsafePerformIO $
                if length (unusedMaybe ++ unusedNonMaybe ++ used) < 10  -- Only log first 10 fields
                then putStrLn $ "    [FIELD] " ++ T.unpack fieldName ++ " :: " ++ T.unpack (fieldDefType fieldDef) ++
                               " (isMaybe: " ++ show (fieldDefIsMaybe fieldDef) ++
                               ", isSingleField: " ++ show isSingleFieldRecord ++
                               ", hasConfiguredUsage: " ++ show hasUsageInConfiguredModules ++
                               ", " ++ usageDetails ++ ")"
                else return ()

        in if hasUsageInConfiguredModules || isSingleFieldRecord
            then (unusedMaybe, unusedNonMaybe, fieldDef : used)
            else if fieldDefIsMaybe fieldDef
                then (fieldDef : unusedMaybe, unusedNonMaybe, used)
                else (unusedMaybe, fieldDef : unusedNonMaybe, used)

reportUnusedFields :: [FieldDefinition] -> [(Text, Text, Text)]
reportUnusedFields fields = map generateSimpleError fields
  where
    generateSimpleError :: FieldDefinition -> (Text, Text, Text)
    generateSimpleError FieldDefinition{..} =
        let errorMsg = formatUnusedFieldError FieldDefinition{..}
        in (fieldDefLocation, errorMsg, fieldDefModule)

formatUnusedFieldError :: FieldDefinition -> Text
formatUnusedFieldError FieldDefinition{..} = T.unlines
    [ "[FieldChecker] Unused Maybe field: " <> fieldDefName <> " :: " <> fieldDefType
    , "    In type: " <> fieldDefTypeName
    , "    Location: " <> fieldDefLocation
    , ""
    , "    To fix, either use the field or exclude it:"
    , ""
    , "    instance FieldChecker " <> fieldDefTypeName <> " where"
    , "        excludedFields _ = [\"" <> fieldDefName <> "\"]"
    ]

formatMissingFieldCheckerError :: Text -> Text -> Text -> Text
formatMissingFieldCheckerError typeName endpoint moduleInfo = T.unlines
    [ "[FieldChecker] Missing required instance: FieldChecker " <> typeName
    , "    Used in API endpoint: " <> endpoint
    , "    Defined in module: " <> moduleInfo
    , ""
    , "    All types used in Servant APIs must have a FieldChecker instance."
    , "    Add the following instance to fix this error:"
    , ""
    , "    instance FieldChecker " <> typeName <> " where"
    , "        excludedFields _ = []"
    ]

validateServantAPITypes :: [ServantAPIType] -> [(Text, Text)]
validateServantAPITypes apiTypes =
    let missingInstances = filter (not . apiHasFieldChecker) apiTypes
    in map formatError missingInstances
  where
    formatError :: ServantAPIType -> (Text, Text)
    formatError api =
        let missingTypes = apiMissingInstances api
            errorMsg = if null missingTypes
                then formatMissingFieldCheckerError (apiTypeName api) (apiEndpoint api) (apiTypeModule api)
                else formatRecursiveMissingError (apiTypeName api) (apiEndpoint api) (apiTypeModule api) missingTypes
        in (errorMsg, apiLocation api)

formatRecursiveMissingError :: Text -> Text -> Text -> [Text] -> Text
formatRecursiveMissingError rootType endpoint moduleInfo missingTypes = T.unlines $
    [ "[FieldChecker] Parent type '" <> rootType <> "' has FieldChecker instance, but child types are missing instances"
    , "    Defined in module: " <> moduleInfo
    , ""
    , "    The following child/nested types must also have FieldChecker instances:"
    , ""
    ] ++ concatMap (\t -> T.lines $ formatMissingFieldCheckerError t endpoint moduleInfo) missingTypes ++
    [ ""
    , "    Since the parent type is used in a Servant API, all its child types must also have FieldChecker instances."
    ]

generateSummaryReport :: [FieldDefinition] -> Text
generateSummaryReport [] = "✅ No unused fields detected!"
generateSummaryReport unusedFields =
    let count = length unusedFields
        fieldsByModule = groupByModule unusedFields
        moduleCount = length fieldsByModule
    in T.unlines $
        [ "📊 UNUSED FIELDS SUMMARY"
        , "══════════════════════════="
        , "Found " <> T.pack (show count) <> " unused field(s) in " <> T.pack (show moduleCount) <> " module(s)"
        , ""
        ] ++
        concatMap formatModuleGroup fieldsByModule ++
        [ ""
        , "🚀 QUICK FIX: To make all fields optional:"
        , "   Find/Replace: ':: SomeType' → ':: Maybe SomeType'"
        , ""
        , "📖 More help: https://docs.yourproject.com/unused-field-checker"
        ]
  where
    groupByModule :: [FieldDefinition] -> [(Text, [FieldDefinition])]
    groupByModule fields =
        let grouped = foldr addToGroup [] fields
        in grouped

    addToGroup :: FieldDefinition -> [(Text, [FieldDefinition])] -> [(Text, [FieldDefinition])]
    addToGroup field [] = [(fieldDefModule field, [field])]
    addToGroup field ((modName, fields):rest)
        | fieldDefModule field == modName = (modName, field:fields) : rest
        | otherwise = (modName, fields) : addToGroup field rest

    formatModuleGroup :: (Text, [FieldDefinition]) -> [Text]
    formatModuleGroup (moduleName, fields) =
        ("📁 " <> moduleName <> " (" <> T.pack (show (length fields)) <> " fields):") :
        map (\f -> "   ▸ " <> fieldDefName f <> " :: " <> fieldDefType f <> " (in " <> fieldDefTypeName f <> ")") fields
