{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.Config
    ( loadExclusionConfig
    , isModuleExcluded
    , validateConfigurationStrictly
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
            Nothing -> putStrLn $ "[CONFIG] No module filters - checking all modules"

isModuleExcluded :: ExclusionConfig -> Text -> Bool
isModuleExcluded ExclusionConfig{..} modName =
    case includeFiles of
        Just includes -> not (any (`matchesPattern` modName) includes)
        Nothing -> False
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

validateConfigurationStrictly :: ExclusionConfig -> IO (Either Text ExclusionConfig)
validateConfigurationStrictly config@ExclusionConfig{..} = do
    let errors = validateModulePatterns includeFiles "includeFiles"

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

isValidPattern :: Text -> Bool
isValidPattern pattern =
    not (T.null pattern) && T.all isValidPatternChar pattern
  where
    isValidPatternChar c =
        c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._*-")

applyConfigurationRules :: ExclusionConfig -> [FieldDefinition] -> IO [FieldDefinition]
applyConfigurationRules config fieldDefs = do
    let moduleFiltered = filter (isFromAllowedModule config) fieldDefs

    putStrLn $ "[CONFIG FILTER] Total fields: " ++ show (length fieldDefs)
    putStrLn $ "[CONFIG FILTER] After module filter: " ++ show (length moduleFiltered)

    return moduleFiltered
  where
    isFromAllowedModule :: ExclusionConfig -> FieldDefinition -> Bool
    isFromAllowedModule ExclusionConfig{..} FieldDefinition{..} =
        case includeFiles of
            Just includes -> any (`strictMatchesPattern` fieldDefModule) includes
            Nothing -> True

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
        matchesWildcard pattern modName
    | otherwise = pattern == modName

matchesWildcard :: Text -> Text -> Bool
matchesWildcard pattern text
    | T.null pattern = T.null text
    | T.null text = pattern == "*" || T.all (== '*') pattern
    | T.head pattern == '*' =
        let rest = T.tail pattern
        in if T.null rest
            then True
            else any (matchesWildcard rest) (T.tails text)
    | T.head pattern == T.head text =
        matchesWildcard (T.tail pattern) (T.tail text)
    | otherwise = False
