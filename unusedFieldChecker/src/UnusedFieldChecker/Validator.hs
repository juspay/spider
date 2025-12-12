{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Validator
    ( -- * Log management
      addFieldsToLog
    , removeUsedFieldsFromLog
      -- * Error reporting
    , reportUnusedFields
    , formatUnusedFieldError
    ) where

import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import UnusedFieldChecker.Types

-- | Add new field definitions to the unused field log.
-- If a field with the same (typeName, fieldName) already exists, it is replaced.
-- Only non-Maybe fields are added to the log.
addFieldsToLog :: [FieldDefinition] -> UnusedFieldLog -> UnusedFieldLog
addFieldsToLog newFields existingLog =
    let -- Filter to only non-Maybe fields from new definitions
        nonMaybeNewFields = filter (not . fieldDefIsMaybe) newFields
        
        -- Create a combined list with new fields first (so nubBy keeps them)
        combined = nonMaybeNewFields ++ existingLog
        
        -- Remove duplicates, keeping the first occurrence (new fields take precedence)
    in nubBy sameField combined
  where
    sameField :: FieldDefinition -> FieldDefinition -> Bool
    sameField f1 f2 = 
        fieldDefTypeName f1 == fieldDefTypeName f2 &&
        fieldDefName f1 == fieldDefName f2

-- | Remove used fields from the log based on detected usages.
-- Uses strict matching: (fieldDefTypeConstructor, fieldDefName) == (fieldUsageTypeConstructor, fieldUsageName)
-- Filters out RecordWildCards, TemplateHaskell, DerivedInstances, and DataSYB usages.
removeUsedFieldsFromLog :: UnusedFieldLog -> [FieldUsage] -> UnusedFieldLog
removeUsedFieldsFromLog logEntries usages =
    let -- Filter out non-real usages
        realUsages = filter isRealUsage usages
    in filter (not . isFieldUsed realUsages) logEntries
  where
    -- | Check if a usage type counts as real field usage
    isRealUsage :: FieldUsage -> Bool
    isRealUsage usage = case fieldUsageType usage of
        RecordWildCards -> False   -- ".." doesn't count as explicit field usage
        TemplateHaskell -> False   -- TH is not field-specific
        DerivedInstances -> False  -- Derived instances are not field-specific
        DataSYB -> False           -- SYB is not field-specific
        FunctionComposition -> False  -- Cannot determine type constructor reliably
        -- All other usage types count as real usage
        AccessorFunction -> True
        PatternMatch -> True
        NamedFieldPuns -> True
        LensesOptics -> True
        HasFieldOverloaded -> True
        GenericReflection -> True
        RecordDotSyntax -> True
        RecordConstruct -> True
        RecordUpdate -> True

    -- | Check if a field definition is used by any of the usages
    -- Uses strict matching on (typeConstructor, fieldName)
    isFieldUsed :: [FieldUsage] -> FieldDefinition -> Bool
    isFieldUsed usageList fieldDef = any (matchesField fieldDef) usageList

    -- | Field matching with fallback strategy:
    -- - If both type constructors are available: strict match (type + name)
    -- - If type constructor unavailable: fallback to name-only match
    -- This prevents false positives when type extraction fails.
    matchesField :: FieldDefinition -> FieldUsage -> Bool
    matchesField fieldDef usage =
        let defTypeConstructor = fieldDefTypeConstructor fieldDef
            defFieldName = fieldDefName fieldDef
            usageTypeConstructor = fieldUsageTypeConstructor usage
            usageFieldName = fieldUsageName usage
            nameMatches = defFieldName == usageFieldName
            typeMatches = defTypeConstructor == usageTypeConstructor
            hasTypes = not (T.null defTypeConstructor) && not (T.null usageTypeConstructor)

        in if hasTypes
            then nameMatches && typeMatches  -- Strict match when types available
            else nameMatches  -- Fallback: name-only match when type extraction failed

-- | Generate error reports for unused fields
reportUnusedFields :: [FieldDefinition] -> [(Text, Text, Text)]
reportUnusedFields fields = map generateSimpleError fields
  where
    generateSimpleError :: FieldDefinition -> (Text, Text, Text)
    generateSimpleError fieldDef@FieldDefinition{..} =
        let errorMsg = formatUnusedFieldError fieldDef
        in (fieldDefLocation, errorMsg, fieldDefModule)

-- | Format a single unused field error message
formatUnusedFieldError :: FieldDefinition -> Text
formatUnusedFieldError FieldDefinition{..} = T.unlines
    [ "[FieldChecker] Unused non-Maybe field: " <> fieldDefName <> " :: " <> fieldDefType
    , "    In type: " <> fieldDefTypeName
    , "    Location: " <> fieldDefLocation
    , ""
    , "    To fix, either:"
    , "    1. Make the field optional: " <> fieldDefName <> " :: Maybe " <> fieldDefType
    , "    2. Exclude it in the FieldChecker instance:"
    , ""
    , "       instance FieldChecker " <> fieldDefTypeName <> " where"
    , "           excludedFields _ = [\"" <> fieldDefName <> "\"]"
    ]
