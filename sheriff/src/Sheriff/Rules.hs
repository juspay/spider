module Sheriff.Rules where

import Sheriff.Types

-- TODO: Take these from the configuration file
badPracticeRules :: Rules
badPracticeRules = [
    defaultRule
  , logRule1 
  , logRule2 
  , logRule3 
  , logRule4 
  , logRule5 
  , logRule6
  , logRule7
  , logRule8
  , logRule9
  , showRule
  ]

logArgNo :: ArgNo
logArgNo = 2

logRule1 :: Rule
logRule1 = Rule "LogRule" "logErrorT" logArgNo stringifierFns [] textTypesToCheck

logRule2 :: Rule
logRule2 = Rule "LogRule" "logErrorV" logArgNo stringifierFns [] textTypesToCheck

logRule3 :: Rule
logRule3 = Rule "LogRule" "logError" logArgNo stringifierFns [] textTypesToCheck

logRule4 :: Rule
logRule4 = Rule "LogRule" "logInfoT" logArgNo stringifierFns [] textTypesToCheck

logRule5 :: Rule
logRule5 = Rule "LogRule" "logInfoV" logArgNo stringifierFns [] textTypesToCheck

logRule6 :: Rule
logRule6 = Rule "LogRule" "logInfo" logArgNo stringifierFns [] textTypesToCheck

logRule7 :: Rule
logRule7 = Rule "LogRule" "logDebugT" logArgNo stringifierFns [] textTypesToCheck

logRule8 :: Rule
logRule8 = Rule "LogRule" "logDebugV" logArgNo stringifierFns [] textTypesToCheck

logRule9 :: Rule
logRule9 = Rule "LogRule" "logDebug" logArgNo stringifierFns [] textTypesToCheck

showRule :: Rule
showRule = Rule "ShowRule" "show" 1 stringifierFns textTypesBlocked textTypesToCheck

noUseRule :: Rule
noUseRule = Rule "NoDecodeUtf8Rule" "$text-1.2.4.1$Data.Text.Encoding$decodeUtf8" 0 [] [] []

stringifierFns :: FnsBlockedInArg
stringifierFns = ["show", "encode", "encodeJSON"]

textTypesBlocked :: TypesBlockedInArg
textTypesBlocked = ["Text", "String", "Char", "[Char]"]

textTypesToCheck :: TypesToCheckInArg
textTypesToCheck = ["Text", "String", "Char", "[Char]"]
