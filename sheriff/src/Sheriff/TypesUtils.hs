module Sheriff.TypesUtils where

import           Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.List.Extra (splitOn)
import           Sheriff.CommonTypes
import           Sheriff.Types
import           Sheriff.Utils

#if __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Utils.Outputable as OP hiding ((<>))
#else
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))
#endif

instance ToJSON CompileError where
  toJSON (CompileError pkg modName errMsg srcLoc vlt suggestions errorInfo) =
    object [ 
             "error_info"      .= errorInfo
           , "error_message"   .= errMsg
           , "module_name"     .= modName
           , "package_name"    .= pkg
           , "src_span"        .= show srcLoc
           , "suggested_fixes" .= suggestions
           , "violated_rule"   .= getViolationRuleName vlt
           , "violation_type"  .= getViolationType vlt
           ]

getViolationSuggestions :: Violation -> Suggestions
getViolationSuggestions v = case v of
  ArgTypeBlocked _ _ _ r -> fn_rule_fixes r
  FnBlockedInArg _ _ _ r -> fn_rule_fixes r
  FnUseBlocked _ r -> fn_rule_fixes r
  FnSigBlocked _ _ r -> fn_rule_fixes r
  NonIndexedDBColumn _ _ r -> db_rule_fixes r
  InfiniteRecursionDetected r -> infinite_recursion_rule_fixes r
  NoViolation -> []

getViolationType :: Violation -> String
getViolationType v = case v of
  ArgTypeBlocked _ _ _ _ -> "ArgTypeBlocked"
  FnBlockedInArg _ _ _ _ -> "FnBlockedInArg"
  FnUseBlocked _ _ -> "FnUseBlocked"
  FnSigBlocked _ _ _ -> "FnSigBlocked"
  NonIndexedDBColumn _ _ _ -> "NonIndexedDBColumn"
  InfiniteRecursionDetected _ -> "InfiniteRecursionDetected"
  NoViolation -> "NoViolation"

getViolationRule :: Violation -> Rule
getViolationRule v = case v of
  ArgTypeBlocked _ _ _ r -> FunctionRuleT r
  FnBlockedInArg _ _ _ r -> FunctionRuleT r
  FnUseBlocked _ r -> FunctionRuleT r
  FnSigBlocked _ _ r -> FunctionRuleT r
  NonIndexedDBColumn _ _ r -> DBRuleT r
  InfiniteRecursionDetected r -> InfiniteRecursionRuleT r
  NoViolation -> defaultRule

getViolationRuleName :: Violation -> String
getViolationRuleName v = case v of
  ArgTypeBlocked _ _ _ r -> fn_rule_name r
  FnBlockedInArg _ _ _ r -> fn_rule_name r
  FnUseBlocked _ r -> fn_rule_name r
  FnSigBlocked _ _ r -> fn_rule_name r
  NonIndexedDBColumn _ _ r -> db_rule_name r
  InfiniteRecursionDetected r -> infinite_recursion_rule_name r
  NoViolation -> "NA"

getViolationRuleExceptions :: Violation -> Rules
getViolationRuleExceptions = getRuleExceptions . getViolationRule

getErrorInfoFromViolation :: Violation -> Value
getErrorInfoFromViolation violation = case violation of
  FnBlockedInArg _ _ errInfo _ -> errInfo
  _ -> A.Null

getRuleFromCompileError :: CompileError -> Rule
getRuleFromCompileError = getViolationRule . violation

getRuleExceptionsFromCompileError :: CompileError -> Rules
getRuleExceptionsFromCompileError = getRuleExceptions . getRuleFromCompileError

getRuleExceptions :: Rule -> Rules
getRuleExceptions rule = case rule of 
  DBRuleT dbRule -> db_rule_exceptions dbRule
  FunctionRuleT fnRule -> fn_rule_exceptions fnRule
  InfiniteRecursionRuleT infiniteRecursionRule -> infinite_recursion_rule_exceptions infiniteRecursionRule
  _ -> []

getRuleIgnoreModules :: Rule -> Modules
getRuleIgnoreModules rule = case rule of 
  FunctionRuleT fnRule -> fn_rule_ignore_modules fnRule
  InfiniteRecursionRuleT infiniteRecursionRule -> infinite_recursion_rule_ignore_modules infiniteRecursionRule
  _ -> []

getRuleIgnoreFunctions :: Rule -> Modules
getRuleIgnoreFunctions rule = case rule of 
  FunctionRuleT fnRule -> fn_rule_ignore_functions fnRule
  InfiniteRecursionRuleT infiniteRecursionRule -> infinite_recursion_rule_ignore_functions infiniteRecursionRule
  _ -> []

getRuleCheckModules :: Rule -> Modules
getRuleCheckModules rule = case rule of 
  FunctionRuleT fnRule -> fn_rule_check_modules fnRule
  InfiniteRecursionRuleT infiniteRecursionRule -> infinite_recursion_rule_check_modules infiniteRecursionRule
  _ -> []

getRuleName :: Rule -> String
getRuleName rule = case rule of
  FunctionRuleT fnRule -> fn_rule_name fnRule
  DBRuleT dbRule -> db_rule_name dbRule
  InfiniteRecursionRuleT infiniteRecursionRule -> infinite_recursion_rule_name infiniteRecursionRule
  _ -> "Rule not handled"

noSuggestion :: Suggestions
noSuggestion = []

defaultRule :: Rule
defaultRule = FunctionRuleT defaultFunctionRule

emptyLoggingError :: CompileError
emptyLoggingError = CompileError "" "" "$NA$" noSrcSpan NoViolation noSuggestion A.Null

yamlToDbRule :: YamlTable -> Rule
yamlToDbRule table = DBRuleT $ DBRule "NonIndexedDBRule" (tableName table) (indexedKeys table) ["You might want to include an indexed column in the `where` clause of the query."] []

updateValInOpts :: String -> String -> PluginOpts -> PluginOpts
updateValInOpts key val currentOpts = case key of
  "saveToFile" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {saveToFile = v}
      Nothing -> currentOpts
  "throwCompilationError" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {throwCompilationError = v}
      Nothing -> currentOpts
  "failOnFileNotFound" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {failOnFileNotFound = v}
      Nothing -> currentOpts
  "savePath" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {savePath = v}
      Nothing -> currentOpts
  "indexedKeysPath" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {indexedKeysPath = v}
      Nothing -> currentOpts
  "rulesConfigPath" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {rulesConfigPath = v}
      Nothing -> currentOpts
  "exceptionsConfigPath" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {exceptionsConfigPath = v}
      Nothing -> currentOpts
  "matchAllInsideAnd" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {matchAllInsideAnd = v}
      Nothing -> currentOpts
  "shouldCheckExceptions" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {shouldCheckExceptions = v}
      Nothing -> currentOpts
  "logDebugInfo" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {logDebugInfo = v}
      Nothing -> currentOpts
  "logWarnInfo" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {logWarnInfo = v}
      Nothing -> currentOpts
  "logTypeDebugging" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {logTypeDebugging = v}
      Nothing -> currentOpts
  "useIOForSourceCode" -> 
    case decode (Char8.pack val) of
      Just v -> currentOpts {useIOForSourceCode = v}
      Nothing -> currentOpts
  _ -> currentOpts

{- Note: We do not allow sheriff plugin opts in individual module as of now
decodeAndUpdateOpts :: [String] -> PluginOpts -> PluginOpts
decodeAndUpdateOpts [] currentOpts       = currentOpts
decodeAndUpdateOpts (x : xs) currentOpts = case A.decode (Char8.pack x) of
                                            Just decodedOpts -> decodeAndUpdateOpts xs decodedOpts
                                            Nothing -> case (splitOn "=" x) of
                                                        (key:val:[]) -> decodeAndUpdateOpts xs (updateValInOpts key val currentOpts)
                                                        _ -> decodeAndUpdateOpts xs currentOpts
-}

decodeAndUpdateOpts :: [String] -> PluginOpts -> PluginOpts
decodeAndUpdateOpts [] currentOpts       = currentOpts
decodeAndUpdateOpts (x : xs) currentOpts = case A.decode (Char8.pack x) of
                                            Just decodedOpts -> decodedOpts
                                            Nothing -> currentOpts