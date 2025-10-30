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
        
        nonExcludedDefs = filter (not . isFieldExcluded exclusionConfig) allDefs
        
        (unusedMaybe, unusedNonMaybe, used) = foldl' categorizeField ([], [], []) nonExcludedDefs
    
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
