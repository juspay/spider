{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Validator
    ( aggregateFieldInfo
    , validateFields
    , validateFieldsWithExclusions
    , reportUnusedFields
    ) where

import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
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
        in if isUsed
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
                typeName = fieldDefTypeName fieldDef
                defModule = fieldDefModule fieldDef
                usageName = fieldUsageName usage

                -- For explicit record operations, we can be confident it's the right field
                isDefinitelyThisField = case fieldUsageType usage of
                    -- These usage types are explicit about which record they're accessing
                    RecordConstruct -> True
                    RecordUpdate -> True
                    PatternMatch -> True
                    NamedFieldPuns -> True
                    RecordWildCards -> True
                    RecordDotSyntax -> True
                    HasFieldOverloaded -> True
                    GenericReflection -> True

                    -- For these, we need to check if the usage could reasonably be this field
                    AccessorFunction ->
                        -- This is tricky - we can't easily determine which type's field is being accessed
                        -- For now, conservatively assume it could be this field if names match
                        -- and the usage is from the same module or imported module
                        usageName == fieldName

                    FunctionComposition ->
                        usageName == fieldName

                    LensesOptics ->
                        usageName == fieldName

                    -- These are not field-specific
                    TemplateHaskell -> False
                    DerivedInstances -> False
                    DataSYB -> False

            in isDefinitelyThisField

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
