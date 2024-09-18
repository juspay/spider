module Sheriff.Types where
  
import Sheriff.Utils
import Data.Aeson as A
import Control.Applicative ((<|>))
import Data.Text (unpack)
import Data.Data (Data)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Utils.Outputable as OP hiding ((<>))
#else
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))
#endif

data PluginOpts = PluginOpts {
    saveToFile :: Bool,
    throwCompilationError :: Bool,
    failOnFileNotFound :: Bool,
    savePath :: String,
    indexedKeysPath :: String,
    rulesConfigPath :: String,
    exceptionsConfigPath :: String,
    matchAllInsideAnd :: Bool,
    shouldCheckExceptions :: Bool,
    logDebugInfo :: Bool,
    logWarnInfo :: Bool,
    logTypeDebugging :: Bool,
    useIOForSourceCode :: Bool
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = 
  PluginOpts { 
    saveToFile = False, 
    throwCompilationError = True, 
    failOnFileNotFound = True, 
    matchAllInsideAnd = False,
    savePath = ".juspay/tmp/sheriff/", 
    indexedKeysPath = ".juspay/indexedKeys.yaml" ,
    rulesConfigPath = ".juspay/sheriffRules.yaml",
    exceptionsConfigPath = ".juspay/sheriffExceptionRules.yaml",
    logDebugInfo = False,
    logWarnInfo = True,
    logTypeDebugging = False,
    shouldCheckExceptions = True,
    useIOForSourceCode = False
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    saveToFile <- o .:? "saveToFile" .!= (saveToFile defaultPluginOpts)
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    throwCompilationError <- o .:? "throwCompilationError" .!= (throwCompilationError defaultPluginOpts)
    savePath <- o .:? "savePath" .!= (savePath defaultPluginOpts)
    indexedKeysPath <- o .:? "indexedKeysPath" .!= (indexedKeysPath defaultPluginOpts)
    rulesConfigPath <- o .:? "rulesConfigPath" .!= (rulesConfigPath defaultPluginOpts)
    exceptionsConfigPath <- o .:? "exceptionsConfigPath" .!= (exceptionsConfigPath defaultPluginOpts)
    matchAllInsideAnd <- o .:? "matchAllInsideAnd" .!= (matchAllInsideAnd defaultPluginOpts)
    shouldCheckExceptions <- o .:? "matchAllInsideAnd" .!= (shouldCheckExceptions defaultPluginOpts)
    logDebugInfo <- o .:? "logDebugInfo" .!= (logDebugInfo defaultPluginOpts)
    logWarnInfo <- o .:? "logWarnInfo" .!= (logWarnInfo defaultPluginOpts)
    logTypeDebugging <- o .:? "logTypeDebugging" .!= (logTypeDebugging defaultPluginOpts)
    useIOForSourceCode <- o .:? "useIOForSourceCode" .!= (useIOForSourceCode defaultPluginOpts)
    return PluginOpts { 
      saveToFile = saveToFile, 
      throwCompilationError = throwCompilationError, 
      matchAllInsideAnd = matchAllInsideAnd, 
      savePath = savePath, 
      indexedKeysPath = indexedKeysPath, 
      rulesConfigPath = rulesConfigPath, 
      exceptionsConfigPath = exceptionsConfigPath, 
      failOnFileNotFound = failOnFileNotFound, 
      shouldCheckExceptions = shouldCheckExceptions, 
      logWarnInfo = logWarnInfo, 
      logDebugInfo = logDebugInfo, 
      logTypeDebugging = logTypeDebugging ,
      useIOForSourceCode = useIOForSourceCode
      }

data SheriffRules = SheriffRules
  { rules :: Rules
  } deriving (Show, Eq)

instance FromJSON SheriffRules where
  parseJSON = withObject "SheriffRules" $ \o -> do
    rules <- o .: "rules"
    return SheriffRules { rules = rules }

data YamlTables = YamlTables
  { tables :: [YamlTable]
  } deriving (Show, Eq)

instance FromJSON YamlTables where
  parseJSON = withObject "YamlTables" $ \o -> do
    tableList <- o .: "tables"
    return YamlTables { tables = tableList }

data YamlTable = YamlTable
  { tableName :: String
  , indexedKeys :: [YamlTableKeys]
  } deriving (Show, Eq)

instance FromJSON YamlTable where
  parseJSON = withObject "YamlTable" $ \o -> do
    name <- o .: "name"
    keys <- o .: "indexedKeys"
    return YamlTable { tableName = name, indexedKeys = keys }

data YamlTableKeys = NonCompositeKey String | CompositeKey { cols :: [String] }
  deriving (Show, Eq)

instance FromJSON YamlTableKeys where
  parseJSON str = composite str <|> nonComposite str
    where 
      composite = withObject "YamlTableKeys" $ \o -> do
                    keys <- o .: "and"
                    return (CompositeKey { cols = keys })
      
      nonComposite = withText "YamlTableKeys" $ \s -> return (NonCompositeKey (unpack s))

data CompileError = CompileError
  {
    pkg_name :: String,
    mod_name :: String,
    err_msg :: String,
    src_span :: SrcSpan,
    violation :: Violation,
    suggested_fixes  :: Suggestions,
    error_info       :: Value
  } 
  deriving (Eq, Show)

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

type Rules = [Rule]
type ArgNo = Int
type ArgTypes = [String]
type SignaturesBlockedInFn = [String]
type FnsBlockedInArg = [(String, ArgNo, TypesAllowedInArg)]
type TypesAllowedInArg = [String]
type TypesBlockedInArg = [String]
type TypesToCheckInArg = [String]
type Suggestions = [String]
type Modules = [String]
type FunctionNames = [String]

data FunctionRule = 
  FunctionRule
    {
      fn_rule_name           :: String,
      fn_name                :: FunctionNames,
      arg_no                 :: ArgNo,
      fn_sigs_blocked       :: SignaturesBlockedInFn,
      fns_blocked_in_arg     :: FnsBlockedInArg,
      types_blocked_in_arg   :: TypesBlockedInArg,
      types_to_check_in_arg  :: TypesToCheckInArg,
      fn_rule_fixes          :: Suggestions,
      fn_rule_exceptions     :: Rules,
      fn_rule_ignore_modules :: Modules,
      fn_rule_check_modules  :: Modules
    }
  deriving (Show, Eq)  

instance FromJSON FunctionRule where
  parseJSON = withObject "FunctionRule" $ \o -> do
                fn_rule_name <- o .: "fn_rule_name"
                fn_name <- (o .: "fn_name" >>= parseAsListOrString)
                arg_no <- o .: "arg_no"
                fn_sigs_blocked <- o .:? "fn_sigs_blocked" .!= ([] :: [String])
                fns_blocked_in_arg <- o .: "fns_blocked_in_arg"
                types_blocked_in_arg <- o .: "types_blocked_in_arg"
                types_to_check_in_arg <- o .: "types_to_check_in_arg"
                fn_rule_fixes <- o .: "fn_rule_fixes"
                fn_rule_exceptions <- o .: "fn_rule_exceptions"
                fn_rule_ignore_modules <- o .: "fn_rule_ignore_modules"
                fn_rule_check_modules <- o .:? "fn_rule_check_modules" .!= ["*"]
                return (FunctionRule {fn_rule_name = fn_rule_name, fn_name = fn_name, arg_no = arg_no, fn_sigs_blocked = fn_sigs_blocked, fns_blocked_in_arg = fns_blocked_in_arg, types_blocked_in_arg = types_blocked_in_arg, types_to_check_in_arg = types_to_check_in_arg, fn_rule_fixes = fn_rule_fixes, fn_rule_exceptions = fn_rule_exceptions, fn_rule_ignore_modules = fn_rule_ignore_modules, fn_rule_check_modules = fn_rule_check_modules })

data DBRule =
  DBRule 
    {
      db_rule_name       :: String,
      table_name         :: String,
      indexed_cols_names :: [YamlTableKeys],
      db_rule_fixes      :: Suggestions,
      db_rule_exceptions :: Rules
    }
  deriving (Show, Eq)  

instance FromJSON DBRule where
  parseJSON = withObject "DBRule" $ \o -> do
                db_rule_name       <- o .: "db_rule_name"
                table_name         <- o .: "table_name"
                indexed_cols_names <- o .: "indexed_cols_names"
                db_rule_fixes      <- o .: "db_rule_fixes"
                db_rule_exceptions <- o .: "db_rule_exceptions"
                return (DBRule {db_rule_name = db_rule_name, table_name = table_name, indexed_cols_names = indexed_cols_names, db_rule_fixes = db_rule_fixes, db_rule_exceptions = db_rule_exceptions})

data Action = Allowed | Blocked
  deriving (Show, Eq)

instance FromJSON Action where
  parseJSON = withText "Action" $ \s -> case s of
                "Allowed" -> return Allowed
                "Blocked" -> return Blocked
                _         -> fail "Invalid Action in rule"

-- First check for arg_types if it is allowed or not. If allowed, then further check for arg_fns
data FunctionInfo = 
  FunctionInfo 
    {
      fnName         :: String,
      isQualified    :: Bool,
      argNo          :: ArgNo,
      fnAction       :: Action,
      argTypes       :: ArgTypes,
      argFns         :: [FunctionInfo],
      suggestedFixes :: Suggestions
    }
  deriving (Show, Eq)  

instance FromJSON FunctionInfo where
  parseJSON = withObject "FunctionInfo" $ \o -> do
                fnName         <- o .: "fnName"
                isQualified    <- o .:? "isQualified" .!= False
                argNo          <- o .: "argNo"
                fnAction       <- o .: "action"
                argTypes       <- o .: "argTypes"
                argFns         <- o .: "argFns"
                suggestedFixes <- o .: "suggestedFixes"
                return (FunctionInfo { fnName = fnName, isQualified = isQualified, argNo = argNo, fnAction = fnAction, argTypes = argTypes, argFns = argFns, suggestedFixes = suggestedFixes })

-- First check for all conditions to be true, and if satisfied, then
data GeneralRule = 
  GeneralRule 
    {
      ruleName   :: String,
      conditions :: [FunctionInfo],
      ruleInfo   :: FunctionInfo
    }
  deriving (Show, Eq)

instance FromJSON GeneralRule where
  parseJSON = withObject "GeneralRule" $ \o -> do
                ruleName   <- o .: "ruleName"
                conditions <- o .: "conditions"
                ruleInfo   <- o .: "ruleInfo"
                return (GeneralRule {ruleName = ruleName, conditions = conditions, ruleInfo = ruleInfo})

data Rule = 
    DBRuleT DBRule
  | FunctionRuleT FunctionRule
  | GeneralRuleT GeneralRule
  deriving (Show, Eq)  

instance FromJSON Rule where
  parseJSON str = (DBRuleT <$> parseJSON str) <|> (FunctionRuleT <$> parseJSON str) <|> (GeneralRuleT <$> parseJSON str) <|> (fail $ "Invalid Rule: " <> show str)

data LocalVar = FnArg Var | FnWhere Var | FnLocal Var
  deriving (Eq)

instance Show LocalVar where
  show (FnArg x)   = "FnArg " Prelude.<> showS x
  show (FnWhere x) = "FnWhere " Prelude.<> showS x
  show (FnLocal x) = "FnLocal " Prelude.<> showS x

data DBFieldSpecType = 
    Selector  -- DB.fieldName ==> $sel:fieldName:TableName
  | Lens      -- (\x -> x ^. _fieldNameLens), (^. _fieldNameLens)
  | RecordDot -- (\x -> x.fieldName), (.fieldName) ==> (\\ x -> (getField @\"fieldName\" x)) 
  | None
  deriving (Show, Eq)

data Violation = 
    ArgTypeBlocked String String String FunctionRule
  | FnBlockedInArg (String, String) String Value FunctionRule
  | NonIndexedDBColumn String String DBRule
  | FnUseBlocked String FunctionRule
  | FnSigBlocked String String FunctionRule
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show (ArgTypeBlocked typ exprTy ruleFnName rule) = "Use of '" <> ruleFnName <> "' on '" <> typ <> "' is not allowed in the overall expression type '" <> exprTy <> "'."
  show (FnBlockedInArg (fnName, typ) ruleFnName _ rule) = "Use of '" <> fnName <> "' on type '" <> typ <> "' inside argument of '" <> ruleFnName <> "' is not allowed."
  show (FnUseBlocked ruleFnName rule) = "Use of '" <> ruleFnName <> "' in the code is not allowed."
  show (FnSigBlocked ruleFnName ruleFnSig rule) = "Use of '" <> ruleFnName <> "' with signature '" <> ruleFnSig <> "' is not allowed in the code."
  show (NonIndexedDBColumn colName tableName _) = "Querying on non-indexed column '" <> colName <> "' of table '" <> (tableName) <> "' is not allowed."
  show NoViolation = "NoViolation"

-- Types Utils

getViolationSuggestions :: Violation -> Suggestions
getViolationSuggestions v = case v of
  ArgTypeBlocked _ _ _ r -> fn_rule_fixes r
  FnBlockedInArg _ _ _ r -> fn_rule_fixes r
  FnUseBlocked _ r -> fn_rule_fixes r
  FnSigBlocked _ _ r -> fn_rule_fixes r
  NonIndexedDBColumn _ _ r -> db_rule_fixes r
  NoViolation -> []

getViolationType :: Violation -> String
getViolationType v = case v of
  ArgTypeBlocked _ _ _ _ -> "ArgTypeBlocked"
  FnBlockedInArg _ _ _ _ -> "FnBlockedInArg"
  FnUseBlocked _ _ -> "FnUseBlocked"
  FnSigBlocked _ _ _ -> "FnSigBlocked"
  NonIndexedDBColumn _ _ _ -> "NonIndexedDBColumn"
  NoViolation -> "NoViolation"

getViolationRule :: Violation -> Rule
getViolationRule v = case v of
  ArgTypeBlocked _ _ _ r -> FunctionRuleT r
  FnBlockedInArg _ _ _ r -> FunctionRuleT r
  FnUseBlocked _ r -> FunctionRuleT r
  FnSigBlocked _ _ r -> FunctionRuleT r
  NonIndexedDBColumn _ _ r -> DBRuleT r
  NoViolation -> defaultRule

getViolationRuleName :: Violation -> String
getViolationRuleName v = case v of
  ArgTypeBlocked _ _ _ r -> fn_rule_name r
  FnBlockedInArg _ _ _ r -> fn_rule_name r
  FnUseBlocked _ r -> fn_rule_name r
  FnSigBlocked _ _ r -> fn_rule_name r
  NonIndexedDBColumn _ _ r -> db_rule_name r
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
  _ -> []

getRuleIgnoreModules :: Rule -> Modules
getRuleIgnoreModules rule = case rule of 
  FunctionRuleT fnRule -> fn_rule_ignore_modules fnRule
  _ -> []

getRuleCheckModules :: Rule -> Modules
getRuleCheckModules rule = case rule of 
  FunctionRuleT fnRule -> fn_rule_check_modules fnRule
  _ -> []

getRuleName :: Rule -> String
getRuleName rule = case rule of
  FunctionRuleT fnRule -> fn_rule_name fnRule
  DBRuleT dbRule -> db_rule_name dbRule
  _ -> "Rule not handled"

noSuggestion :: Suggestions
noSuggestion = []

defaultRule :: Rule
defaultRule = FunctionRuleT $ FunctionRule "NA" ["NA"] (-1) [] [] [] [] noSuggestion [] [] []

emptyLoggingError :: CompileError
emptyLoggingError = CompileError "" "" "$NA$" noSrcSpan NoViolation noSuggestion A.Null

yamlToDbRule :: YamlTable -> Rule
yamlToDbRule table = DBRuleT $ DBRule "NonIndexedDBRule" (tableName table) (indexedKeys table) ["You might want to include an indexed column in the `where` clause of the query."] []