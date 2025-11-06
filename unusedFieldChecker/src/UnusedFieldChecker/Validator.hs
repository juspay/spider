{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Validator
    ( aggregateFieldInfo
    , validateFields
    , validateFieldsWithExclusions
    , validateFieldsForTypesUsedInConfiguredModules
    , reportUnusedFields
    ) where

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
            -- Single-field records are automatically considered "used" since GHC optimizes away the accessor
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
        -- All usage types now count as real usage
        isRealUsage _ = True

validateFieldsWithExclusions :: ExclusionConfig -> AggregatedFieldInfo -> ValidationResult
validateFieldsWithExclusions exclusionConfig AggregatedFieldInfo{..} =
    let allDefs = concat $ Map.elems allFieldDefs

        -- First filter by include/exclude rules to get only fields from allowed modules
        allowedModuleDefs = filter (isFromAllowedModule exclusionConfig) allDefs

        -- Then filter by field-specific exclusions
        nonExcludedDefs = filter (not . isFieldExcluded exclusionConfig) allowedModuleDefs

        (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) nonExcludedDefs

    in ValidationResult
        { unusedNonMaybeFields = nub unusedNonMaybe
        , unusedMaybeFields = nub unusedMaybe
        , usedFields = nub used
        }
  where
    -- Check if a field definition is from an allowed module
    isFromAllowedModule :: ExclusionConfig -> FieldDefinition -> Bool
    isFromAllowedModule ExclusionConfig{..} FieldDefinition{..} =
        case includeFiles of
            Just includes -> any (`matchesPattern` fieldDefModule) includes
            Nothing -> not (any (`matchesPattern` fieldDefModule) excludeFiles)
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

            -- Check if this specific field (by name, type, and module) is used
            isUsed = case Map.lookup fieldName allFieldUsages of
                Nothing -> False
                Just usages -> any (isRealUsageOfThisField fieldDef) usages
        in if isUsed
            then (unusedMaybe, unusedNonMaybe, fieldDef : used)
            else if fieldDefIsMaybe fieldDef
                then (fieldDef : unusedMaybe, unusedNonMaybe, used)
                else (unusedMaybe, fieldDef : unusedNonMaybe, used)
      where
        -- Check if usage is actually of this specific field definition
        isRealUsageOfThisField :: FieldDefinition -> FieldUsage -> Bool
        isRealUsageOfThisField fieldDef usage =
            let fieldName = fieldDefName fieldDef
                defTypeConstructor = fieldDefTypeConstructor fieldDef
                usageName = fieldUsageName usage
                usageTypeConstructor = fieldUsageTypeConstructor usage

                -- Names must match
                nameMatches = usageName == fieldName

                -- Type constructors should match (when available)
                typeMatches = case (usageTypeConstructor, defTypeConstructor) of
                    ("", _) -> True  -- Unknown type in usage, conservatively match
                    (_, "") -> True  -- Unknown type in definition, conservatively match
                    (usageType, defType) -> usageType == defType

                -- For explicit record operations, we can be confident it's the right field
                isExplicitRecordOp = case fieldUsageType usage of
                    -- These usage types are explicit about which record they're accessing
                    RecordConstruct -> True
                    RecordUpdate -> True
                    PatternMatch -> True
                    NamedFieldPuns -> True
                    RecordWildCards -> True
                    RecordDotSyntax -> True
                    HasFieldOverloaded -> True  -- From Core-level HasField detection
                    GenericReflection -> True

                    -- For these, we need type matching to be sure
                    AccessorFunction -> typeMatches
                    FunctionComposition -> typeMatches
                    LensesOptics -> typeMatches

                    -- These are not field-specific
                    TemplateHaskell -> False
                    DerivedInstances -> False
                    DataSYB -> False

            in nameMatches && (isExplicitRecordOp || typeMatches)

-- Phase 2: Only check fields that have no usage within configured modules
validateFieldsForTypesUsedInConfiguredModules :: ExclusionConfig -> AggregatedFieldInfo -> IO ValidationResult
validateFieldsForTypesUsedInConfiguredModules exclusionConfig AggregatedFieldInfo{..} = do
    let allDefs = concat $ Map.elems allFieldDefs
        -- Apply exclusions to field definitions
        nonExcludedDefs = filter (not . isFieldExcluded exclusionConfig) allDefs

    -- Debug logging
    putStrLn $ "\n[DEBUG Phase 2] Total field definitions: " ++ show (length allDefs)
    putStrLn $ "[DEBUG Phase 2] Non-excluded definitions: " ++ show (length nonExcludedDefs)
    case includeFiles exclusionConfig of
        Just includes -> putStrLn $ "[DEBUG Phase 2] Configured modules: " ++ show includes
        Nothing -> putStrLn $ "[DEBUG Phase 2] No configured modules (includeFiles is Nothing)"

    let (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) nonExcludedDefs

    -- More debug logging
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
    -- Check if a usage occurs within a configured module
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

            -- Single-field records are automatically considered "used" since GHC optimizes away the accessor
            isSingleFieldRecord = fieldDefIsSingleField fieldDef

            -- Check if this field has any usage within the configured modules
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

            -- Debug logging for first few fields
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
reportUnusedFields fields = map generateError fields
  where
    generateError :: FieldDefinition -> (Text, Text, Text)
    generateError FieldDefinition{..} =
        let errorMsg = T.concat
                [ "\n"
                , "Unused field detected:\n"
                , "  Field: ", fieldDefName, "\n"
                , "  Type: ", fieldDefTypeName, "\n"
                , "  Field Type: ", fieldDefType, "\n"
                , "  Module: ", fieldDefModule, "\n"
                , "\n"
                , "This non-Maybe field is defined but never used in the codebase.\n"
                , "\n"
                , "Suggested fixes:\n"
                , "  1. Use this field somewhere in your code\n"
                , "  2. Change the field type to 'Maybe ", fieldDefType, "' if it's optional\n"
                , "  3. Add it to the exclusion config (UnusedFieldChecker.yaml) if intentionally unused\n"
                ]
        in (fieldDefLocation, errorMsg, fieldDefModule)
