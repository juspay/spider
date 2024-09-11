module Sheriff.Rules where

import Sheriff.Types
import Sheriff.Utils

defaultSheriffRules :: Rules
defaultSheriffRules = [
    defaultRule
  -- , logRule1 
  -- , logRule2 
  -- , logRule3 
  -- , logRule4 
  -- , logRule5 
  -- , logRule6
  -- , logRule7
  -- , logRule8
  -- , logRule9
  -- , logRule10
  -- , logRule11
  -- , logRule12
  -- , logRule13
  -- , logRule14
  -- , logRule15
  -- , noKVDBRule
  , showRule
  ]

-- Exceptions to rule out if these rules are also applied to same LHsExpr
defaultSheriffExceptionsRules :: Rules
defaultSheriffExceptionsRules = [
    defaultRule
  -- , updateFunctionRuleArgNo logRule1  1
  -- , updateFunctionRuleArgNo logRule2  1
  -- , updateFunctionRuleArgNo logRule3  1
  -- , updateFunctionRuleArgNo logRule4  1
  -- , updateFunctionRuleArgNo logRule5  1
  -- , updateFunctionRuleArgNo logRule6 1
  -- , updateFunctionRuleArgNo logRule7 1
  -- , updateFunctionRuleArgNo logRule8 1
  -- , updateFunctionRuleArgNo logRule9 1
  -- , updateFunctionRuleArgNo logRule10 1
  -- , updateFunctionRuleArgNo logRule11 1
  -- , updateFunctionRuleArgNo logRule12 1
  -- , updateFunctionRuleArgNo logRule13 1
  -- , updateFunctionRuleArgNo logRule14 1
  -- , updateFunctionRuleArgNo logRule15 1
  ]

logArgNo :: ArgNo
logArgNo = 2

logRule1 :: Rule
logRule1 = FunctionRuleT $ FunctionRule "LogRule" ["logErrorT"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule2 :: Rule
logRule2 = FunctionRuleT $ FunctionRule "LogRule" ["logErrorV"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule3 :: Rule
logRule3 = FunctionRuleT $ FunctionRule "LogRule" ["logError"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule4 :: Rule
logRule4 = FunctionRuleT $ FunctionRule "LogRule" ["logInfoT"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule5 :: Rule
logRule5 = FunctionRuleT $ FunctionRule "LogRule" ["logInfoV"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule6 :: Rule
logRule6 = FunctionRuleT $ FunctionRule "LogRule" ["logInfo"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule7 :: Rule
logRule7 = FunctionRuleT $ FunctionRule "LogRule" ["logDebugT"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule8 :: Rule
logRule8 = FunctionRuleT $ FunctionRule "LogRule" ["logDebugV"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule9 :: Rule
logRule9 = FunctionRuleT $ FunctionRule "LogRule" ["logDebug"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule10 :: Rule
logRule10 = FunctionRuleT $ FunctionRule "LogRule" ["logErrorWithCategory"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule11 :: Rule
logRule11 = FunctionRuleT $ FunctionRule "LogRule" ["logErrorWithCategoryV"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule12 :: Rule
logRule12 = FunctionRuleT $ FunctionRule "LogRule" ["forkErrorLog"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule13 :: Rule
logRule13 = FunctionRuleT $ FunctionRule "LogRule" ["forkInfoLog"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule14 :: Rule
logRule14 = FunctionRuleT $ FunctionRule "LogRule" ["debugLog"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule15 :: Rule
logRule15 = FunctionRuleT $ FunctionRule "LogRule" ["warnLog"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule16 :: Rule
logRule16 = FunctionRuleT $ FunctionRule "LogRule" ["logWarningT"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule17 :: Rule
logRule17 = FunctionRuleT $ FunctionRule "LogRule" ["logWarningV"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

logRule18 :: Rule
logRule18 = FunctionRuleT $ FunctionRule "LogRule" ["logWarning"] logArgNo stringifierFns [] textTypesToCheck logRuleSuggestions [] []

showRuleExceptions :: Rules
showRuleExceptions = [
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
  , logRule16
  , logRule17
  , logRule18
  , updateFunctionRuleArgNo logRule1  1
  , updateFunctionRuleArgNo logRule2  1
  , updateFunctionRuleArgNo logRule3  1
  , updateFunctionRuleArgNo logRule4  1
  , updateFunctionRuleArgNo logRule5  1
  , updateFunctionRuleArgNo logRule6 1
  , updateFunctionRuleArgNo logRule7 1
  , updateFunctionRuleArgNo logRule8 1
  , updateFunctionRuleArgNo logRule9 1
  , updateFunctionRuleArgNo logRule10 1
  , updateFunctionRuleArgNo logRule11 1
  , updateFunctionRuleArgNo logRule12 1
  , updateFunctionRuleArgNo logRule13 1
  , updateFunctionRuleArgNo logRule14 1
  , updateFunctionRuleArgNo logRule15 1
  , updateFunctionRuleArgNo logRule16 1
  , updateFunctionRuleArgNo logRule17 1
  , updateFunctionRuleArgNo logRule18 1
  ]

showRule :: Rule
showRule = FunctionRuleT $ FunctionRule "ShowRule" ["show"] 1 stringifierFns textTypesBlocked textTypesToCheck showRuleSuggestions showRuleExceptions []

noUseRule :: Rule
noUseRule = FunctionRuleT $ FunctionRule "NoDecodeUtf8Rule" ["$text-1.2.4.1$Data.Text.Encoding$decodeUtf8"] 0 [] [] [] ["You might want to use some other wrapper function."] [] []

noKVDBRule :: Rule
noKVDBRule = FunctionRuleT $ FunctionRule "ART KVDB Rule" ["runKVDB"] 0 [] [] [] ["You might want to use some other wrapper function from `EulerHS.Extra.Redis` module.", "For e.g. - rExists, rDel, rGet, rExpire, etc."] [] []

dbRule :: Rule
dbRule = DBRuleT $ DBRule "NonIndexedDBRule" "TxnRiskCheck" [NonCompositeKey "partitionKey"] dbRuleSuggestions []

dbRuleCustomer :: Rule
dbRuleCustomer = DBRuleT $ DBRule "NonIndexedDBRule" "MerchantKey" [NonCompositeKey "status"] dbRuleSuggestions []

updateFunctionRuleArgNo :: Rule -> ArgNo -> Rule
updateFunctionRuleArgNo (FunctionRuleT fnRule) newArgNo = FunctionRuleT $ fnRule{arg_no = newArgNo}
updateFunctionRuleArgNo rule _ = rule

typesAllowedInStringifierFns :: TypesAllowedInArg
typesAllowedInStringifierFns = ["EnumTypes", "Integer", "Double", "Float", "Int64", "Int", "Bool", "Number", "(,)", "[]", "Maybe"]

stringifierFns :: FnsBlockedInArg
stringifierFns = [("show", 1, typesAllowedInStringifierFns), ("encode", 1, []), ("encodeJSON", 1, [])]

textTypesBlocked :: TypesBlockedInArg
textTypesBlocked = ["Text", "String", "Char", "[Char]", "Maybe", "(,)", "[]"]

textTypesToCheck :: TypesToCheckInArg
textTypesToCheck = ["Text", "String", "Char", "[Char]", "Maybe", "(,)", "[]"]

-- Suggestions

dbRuleSuggestions :: Suggestions
dbRuleSuggestions = ["You might want to include an indexed column in the `where` clause of the query."]

logRuleSuggestions :: Suggestions
logRuleSuggestions = 
  [
    "Remove `show` function call from the error location and use `L.logErrorV @Text` or `L.logDebugV @Text` or `L.logInfoV @Text` function(s) imported from `EulerHS.Language` module.",
    "Make sure that there is `ToJSON` instance on the value we are logging.",
    "You may use tuples for combining string and objects. For e.g., (\"Failed to fetch object: \" :: Text, obj)"
  ]

showRuleSuggestions :: Suggestions
showRuleSuggestions = 
  [
    "Remove `show` function call from the error location. If quotes are required, manually add them to the text.",
    "You might want to use a convertor function like `Data.Text.pack`, `Data.Text.unpack`, `decodeUtf8`, `encodeUtf8`, etc."
  ]