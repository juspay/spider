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
            isUsed = Map.member fieldName allFieldUsages
        in if isUsed
            then (unusedMaybe, unusedNonMaybe, fieldDef : used)
            else if fieldDefIsMaybe fieldDef
                then (fieldDef : unusedMaybe, unusedNonMaybe, used)
                else (unusedMaybe, fieldDef : unusedNonMaybe, used)

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
            isUsed = Map.member fieldName allFieldUsages
        in if isUsed
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
                [ "Field '"
                , fieldDefName
                , "' in type '"
                , fieldDefTypeName
                , "' is never used and is not a Maybe type. "
                , "Either use this field or make it a Maybe type."
                ]
        in (fieldDefLocation, errorMsg, fieldDefModule)
