{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Config
    ( loadExclusionConfig
    , isFieldExcluded
    , isModuleExcluded
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, ParseException)
import System.Directory (doesFileExist)
import UnusedFieldChecker.Types

loadExclusionConfig :: FilePath -> IO ExclusionConfig
loadExclusionConfig configPath = do
    exists <- doesFileExist configPath
    if not exists
        then return emptyExclusionConfig
        else do
            result <- decodeFileEither configPath
            case result of
                Left err -> do
                    putStrLn $ "Warning: Failed to parse " ++ configPath ++ ": " ++ show err
                    return emptyExclusionConfig
                Right config -> do
                    putStrLn $ "Loaded exclusion config from: " ++ configPath
                    return config


isFieldExcluded :: ExclusionConfig -> FieldDefinition -> Bool
isFieldExcluded ExclusionConfig{..} FieldDefinition{..} =
    any matchesModuleExclusion exclusions
  where
    matchesModuleExclusion :: ModuleExclusion -> Bool
    matchesModuleExclusion ModuleExclusion{..} =
        moduleMatches exclModule fieldDefModule &&
        any matchesTypeExclusion exclTypes

    matchesTypeExclusion :: TypeExclusion -> Bool
    matchesTypeExclusion TypeExclusion{..} =
        typeMatches exclDataType fieldDefTypeName &&
        fieldMatches exclFields fieldDefName

    moduleMatches :: Text -> Text -> Bool
    moduleMatches pattern modName
        | pattern == "*" = True
        | T.isSuffixOf ".*" pattern =
            let prefix = T.dropEnd 2 pattern
            in prefix `T.isPrefixOf` modName
        | otherwise = pattern == modName

    typeMatches :: Text -> Text -> Bool
    typeMatches pattern typeName
        | pattern == "*" = True
        | otherwise = pattern == typeName

    fieldMatches :: [Text] -> Text -> Bool
    fieldMatches [] _ = True
    fieldMatches fields fieldName = fieldName `elem` fields

isModuleExcluded :: ExclusionConfig -> Text -> Bool
isModuleExcluded ExclusionConfig{..} modName =
    case includeFiles of
        Just includes -> not (any (`matchesPattern` modName) includes)
        Nothing -> any (`matchesPattern` modName) excludeFiles
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
