{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Config
    ( loadExclusionConfig
    , isFieldExcluded
    , isModuleExcluded
    , validateConfigurationStrictly
    , enforceFieldExclusions
    , applyConfigurationRules
    ) where

import Control.Monad (when)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, ParseException)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import UnusedFieldChecker.Types

-- | Load and validate exclusion configuration with strict error checking
loadExclusionConfig :: FilePath -> IO ExclusionConfig
loadExclusionConfig configPath = do
    exists <- doesFileExist configPath
    if not exists
        then do
            putStrLn $ "[CONFIG] No config file found at: " ++ configPath ++ " - using defaults"
            return emptyExclusionConfig
        else do
            result <- decodeFileEither configPath
            case result of
                Left err -> do
                    hPutStrLn stderr $ "ERROR: Failed to parse " ++ configPath ++ ": " ++ show err
                    hPutStrLn stderr "Using empty configuration due to parse error"
                    return emptyExclusionConfig
                Right config -> do
                    -- Strict validation of the loaded configuration
                    validated <- validateConfigurationStrictly config
                    case validated of
                        Left configError -> do
                            hPutStrLn stderr $ "ERROR: Invalid configuration: " ++ T.unpack configError
                            hPutStrLn stderr "Using empty configuration due to validation error"
                            return emptyExclusionConfig
                        Right validConfig -> do
                            putStrLn $ "[CONFIG] Successfully loaded and validated: " ++ configPath
                            logConfigSummary validConfig
                            return validConfig
  where
    logConfigSummary ExclusionConfig{..} = do
        case includeFiles of
            Just includes -> putStrLn $ "[CONFIG] Include patterns: " ++ show includes
            Nothing -> putStrLn $ "[CONFIG] Exclude patterns: " ++ show excludeFiles
        when (not $ null exclusions) $
            putStrLn $ "[CONFIG] Field exclusions: " ++ show (length exclusions) ++ " module(s)"


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

-- | Strict configuration validation - ensures all patterns and references are valid
validateConfigurationStrictly :: ExclusionConfig -> IO (Either Text ExclusionConfig)
validateConfigurationStrictly config@ExclusionConfig{..} = do
    let errors = concat
            [ validateModulePatterns includeFiles "includeFiles"
            , validateModulePatterns (Just excludeFiles) "excludeFiles"
            , validateExclusions exclusions
            , validatePatternConflicts config
            ]

    if null errors
        then return $ Right config
        else return $ Left $ T.intercalate "; " errors
  where
    validateModulePatterns :: Maybe [Text] -> Text -> [Text]
    validateModulePatterns Nothing _ = []
    validateModulePatterns (Just patterns) fieldName =
        [ fieldName <> ": Invalid regex pattern '" <> pattern <> "'"
        | pattern <- patterns
        , not (isValidPattern pattern)
        ]

    validateExclusions :: [ModuleExclusion] -> [Text]
    validateExclusions = concatMap validateModuleExclusion

    validateModuleExclusion :: ModuleExclusion -> [Text]
    validateModuleExclusion ModuleExclusion{..} =
        let moduleErrors = if isValidPattern exclModule
                then []
                else ["Invalid module pattern: '" <> exclModule <> "'"]
            typeErrors = concatMap validateTypeExclusion exclTypes
        in moduleErrors ++ typeErrors

    validateTypeExclusion :: TypeExclusion -> [Text]
    validateTypeExclusion TypeExclusion{..} =
        if T.null exclDataType || null exclFields
            then ["Empty dataType or fields in exclusion: " <> exclDataType]
            else []

    validatePatternConflicts :: ExclusionConfig -> [Text]
    validatePatternConflicts ExclusionConfig{..} =
        case includeFiles of
            Just includes | not (null excludeFiles) ->
                ["Cannot specify both includeFiles and excludeFiles - includeFiles takes precedence"]
            _ -> []

-- | Check if a pattern is valid (basic character validation)
isValidPattern :: Text -> Bool
isValidPattern pattern =
    not (T.null pattern) && T.all isValidPatternChar pattern
  where
    isValidPatternChar c =
        c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._*-")

-- | Apply configuration rules with strict filtering
applyConfigurationRules :: ExclusionConfig -> [FieldDefinition] -> IO [FieldDefinition]
applyConfigurationRules config fieldDefs = do
    let moduleFiltered = filter (isFromAllowedModule config) fieldDefs
        fieldFiltered = filter (not . isFieldExcluded config) moduleFiltered

    putStrLn $ "[CONFIG FILTER] Total fields: " ++ show (length fieldDefs)
    putStrLn $ "[CONFIG FILTER] After module filter: " ++ show (length moduleFiltered)
    putStrLn $ "[CONFIG FILTER] After field exclusions: " ++ show (length fieldFiltered)

    return fieldFiltered
  where
    isFromAllowedModule :: ExclusionConfig -> FieldDefinition -> Bool
    isFromAllowedModule ExclusionConfig{..} FieldDefinition{..} =
        case includeFiles of
            Just includes -> any (`strictMatchesPattern` fieldDefModule) includes
            Nothing -> not (any (`strictMatchesPattern` fieldDefModule) excludeFiles)

-- | Enforce field exclusions - completely remove excluded fields from analysis
enforceFieldExclusions :: [FieldDefinition] -> ExclusionConfig -> IO [FieldDefinition]
enforceFieldExclusions fieldDefs config = do
    let nonExcluded = filter (not . isFieldExcluded config) fieldDefs
        excludedCount = length fieldDefs - length nonExcluded

    when (excludedCount > 0) $
        putStrLn $ "[CONFIG] Excluded " ++ show excludedCount ++ " field(s) from analysis"

    return nonExcluded

-- | Strict pattern matching with exact semantics
strictMatchesPattern :: Text -> Text -> Bool
strictMatchesPattern pattern modName
    | pattern == "*" = True
    | ".*" `T.isSuffixOf` pattern =
        let prefix = T.dropEnd 2 pattern
        in not (T.null prefix) && prefix `T.isPrefixOf` modName
    | "*." `T.isPrefixOf` pattern =
        let suffix = T.drop 2 pattern
        in not (T.null suffix) && suffix `T.isSuffixOf` modName
    | "*" `T.isInfixOf` pattern =
        -- Complex wildcard patterns - simple matching
        matchesWildcard pattern modName
    | otherwise = pattern == modName

-- | Simple wildcard matching without regex dependency
matchesWildcard :: Text -> Text -> Bool
matchesWildcard pattern text
    | T.null pattern = T.null text
    | T.null text = pattern == "*" || T.all (== '*') pattern
    | T.head pattern == '*' =
        let rest = T.tail pattern
        in if T.null rest
            then True  -- Pattern is just "*"
            else any (matchesWildcard rest) (T.tails text)
    | T.head pattern == T.head text =
        matchesWildcard (T.tail pattern) (T.tail text)
    | otherwise = False
