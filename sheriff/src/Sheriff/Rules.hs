module Sheriff.Rules where

import Sheriff.Types
import Sheriff.TypesUtils
import Sheriff.Utils

defaultSheriffRules :: Rules
defaultSheriffRules = [
    defaultRule
  -- , noKVDBRule
  , showRule
  ]

-- Exceptions to rule out if these rules are also applied to same LHsExpr
defaultSheriffExceptionsRules :: Rules
defaultSheriffExceptionsRules = [
    defaultRule
  ]

showRuleExceptions :: Rules
showRuleExceptions = [
    defaultRule
  ]

showRule :: Rule
showRule = FunctionRuleT $ FunctionRule "ShowRule" ["show"] 1 [] stringifierFns textTypesBlocked textTypesToCheck showRuleSuggestions showRuleExceptions [] ["*"] []

noKVDBRule :: Rule
noKVDBRule = FunctionRuleT $ FunctionRule "ART KVDB Rule" ["runKVDB"] 0 [] [] [] [] ["You might want to use some other wrapper function from `EulerHS.Extra.Redis` module.", "For e.g. - rExists, rDel, rGet, rExpire, etc."] [] [] ["*"] []

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