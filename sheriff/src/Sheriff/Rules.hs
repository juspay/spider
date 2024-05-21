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
  , logRule10
  , logRule11
  , logRule12
  , logRule13
  , logRule14
  , logRule15
  , showRule
  ]

logArgNo :: ArgNo
logArgNo = 2

logRule1 :: Rule
logRule1 = FunctionRuleT $ FunctionRule "LogRule" "logErrorT" logArgNo stringifierFns [] textTypesToCheck

logRule2 :: Rule
logRule2 = FunctionRuleT $ FunctionRule "LogRule" "logErrorV" logArgNo stringifierFns [] textTypesToCheck

logRule3 :: Rule
logRule3 = FunctionRuleT $ FunctionRule "LogRule" "logError" logArgNo stringifierFns [] textTypesToCheck

logRule4 :: Rule
logRule4 = FunctionRuleT $ FunctionRule "LogRule" "logInfoT" logArgNo stringifierFns [] textTypesToCheck

logRule5 :: Rule
logRule5 = FunctionRuleT $ FunctionRule "LogRule" "logInfoV" logArgNo stringifierFns [] textTypesToCheck

logRule6 :: Rule
logRule6 = FunctionRuleT $ FunctionRule "LogRule" "logInfo" logArgNo stringifierFns [] textTypesToCheck

logRule7 :: Rule
logRule7 = FunctionRuleT $ FunctionRule "LogRule" "logDebugT" logArgNo stringifierFns [] textTypesToCheck

logRule8 :: Rule
logRule8 = FunctionRuleT $ FunctionRule "LogRule" "logDebugV" logArgNo stringifierFns [] textTypesToCheck

logRule9 :: Rule
logRule9 = FunctionRuleT $ FunctionRule "LogRule" "logDebug" logArgNo stringifierFns [] textTypesToCheck

logRule10 :: Rule
logRule10 = FunctionRuleT $ FunctionRule "LogRule" "logErrorWithCategory" logArgNo stringifierFns [] textTypesToCheck

logRule11 :: Rule
logRule11 = FunctionRuleT $ FunctionRule "LogRule" "logErrorWithCategoryV" logArgNo stringifierFns [] textTypesToCheck

logRule12 :: Rule
logRule12 = FunctionRuleT $ FunctionRule "LogRule" "forkErrorLog" logArgNo stringifierFns [] textTypesToCheck

logRule13 :: Rule
logRule13 = FunctionRuleT $ FunctionRule "LogRule" "forkInfoLog" logArgNo stringifierFns [] textTypesToCheck

logRule14 :: Rule
logRule14 = FunctionRuleT $ FunctionRule "LogRule" "debugLog" logArgNo stringifierFns [] textTypesToCheck

logRule15 :: Rule
logRule15 = FunctionRuleT $ FunctionRule "LogRule" "warnLog" logArgNo stringifierFns [] textTypesToCheck

showRule :: Rule
showRule = FunctionRuleT $ FunctionRule "ShowRule" "show" 1 stringifierFns textTypesBlocked textTypesToCheck

noUseRule :: Rule
noUseRule = FunctionRuleT $ FunctionRule "NoDecodeUtf8Rule" "$text-1.2.4.1$Data.Text.Encoding$decodeUtf8" 0 [] [] []

dbRule :: Rule
dbRule = DBRuleT $ DBRule "NonIndexedDBRule" "TxnRiskCheck" [NonCompositeKey "partitionKey"]

dbRuleCustomer :: Rule
dbRuleCustomer = DBRuleT $ DBRule "NonIndexedDBRule" "MerchantKey" [NonCompositeKey "status"]

stringifierFns :: FnsBlockedInArg
stringifierFns = ["show", "encode", "encodeJSON"]

textTypesBlocked :: TypesBlockedInArg
textTypesBlocked = ["Text", "String", "Char", "[Char]"]

textTypesToCheck :: TypesToCheckInArg
textTypesToCheck = ["Text", "String", "Char", "[Char]"]
