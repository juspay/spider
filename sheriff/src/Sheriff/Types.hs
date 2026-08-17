{-# LANGUAGE RecordWildCards #-}

module Sheriff.Types where
  
import Sheriff.Utils
import Data.Aeson as A
import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Data (Data)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc
import GHC.Types.Var
#else
import SrcLoc 
import Var
#endif

data PluginOpts = PluginOpts {
    saveToFile            :: Bool,
    throwCompilationError :: Bool,
    failOnFileNotFound    :: Bool,
    savePath              :: String,
    indexedKeysPath       :: String,
    rulesConfigPath       :: String,
    exceptionsConfigPath  :: String,
    matchAllInsideAnd     :: Bool,
    checkEmptyWhereClause :: Bool,
    shouldCheckExceptions :: Bool,
    logDebugInfo          :: Bool,
    logWarnInfo           :: Bool,
    logTypeDebugging      :: Bool,
    useIOForSourceCode    :: Bool,
    skipIndexedKeysCheck  :: Bool
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts 
defaultPluginOpts = 
  PluginOpts { 
    saveToFile            = False, 
    throwCompilationError = True, 
    failOnFileNotFound    = True, 
    matchAllInsideAnd     = False,
    checkEmptyWhereClause = False,
    savePath              = ".juspay/tmp/sheriff/", 
    indexedKeysPath       = ".juspay/indexedKeys.yaml" ,
    rulesConfigPath       = ".juspay/sheriffRules.yaml",
    exceptionsConfigPath  = ".juspay/sheriffExceptionRules.yaml",
    logDebugInfo          = False,
    logWarnInfo           = True,
    logTypeDebugging      = False,
    shouldCheckExceptions = True,
    useIOForSourceCode    = False,
    skipIndexedKeysCheck  = False
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    saveToFile            <- o .:? "saveToFile"            .!= (saveToFile defaultPluginOpts)
    failOnFileNotFound    <- o .:? "failOnFileNotFound"    .!= (failOnFileNotFound defaultPluginOpts)
    throwCompilationError <- o .:? "throwCompilationError" .!= (throwCompilationError defaultPluginOpts)
    savePath              <- o .:? "savePath"              .!= (savePath defaultPluginOpts)
    indexedKeysPath       <- o .:? "indexedKeysPath"       .!= (indexedKeysPath defaultPluginOpts)
    rulesConfigPath       <- o .:? "rulesConfigPath"       .!= (rulesConfigPath defaultPluginOpts)
    exceptionsConfigPath  <- o .:? "exceptionsConfigPath"  .!= (exceptionsConfigPath defaultPluginOpts)
    matchAllInsideAnd     <- o .:? "matchAllInsideAnd"     .!= (matchAllInsideAnd defaultPluginOpts)
    checkEmptyWhereClause <- o .:? "checkEmptyWhereClause" .!= (checkEmptyWhereClause defaultPluginOpts)
    shouldCheckExceptions <- o .:? "matchAllInsideAnd"     .!= (shouldCheckExceptions defaultPluginOpts)
    logDebugInfo          <- o .:? "logDebugInfo"          .!= (logDebugInfo defaultPluginOpts)
    logWarnInfo           <- o .:? "logWarnInfo"           .!= (logWarnInfo defaultPluginOpts)
    logTypeDebugging      <- o .:? "logTypeDebugging"      .!= (logTypeDebugging defaultPluginOpts)
    useIOForSourceCode    <- o .:? "useIOForSourceCode"    .!= (useIOForSourceCode defaultPluginOpts)
    skipIndexedKeysCheck  <- o .:? "skipIndexedKeysCheck"  .!= (skipIndexedKeysCheck defaultPluginOpts)
    return PluginOpts {..}

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
type ModulesWithFunctions = [String]

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
  { tableName        :: String
  , indexedKeys      :: [YamlTableKeys]
  , ignoredFunctions :: ModulesWithFunctions
  , ignoredModules   :: Modules
  , checkModules     :: Modules
  } deriving (Show, Eq)

instance FromJSON YamlTable where
  parseJSON = withObject "YamlTable" $ \o -> do
    tableName        <- o .:  "name"
    indexedKeys      <- o .:  "indexedKeys"
    ignoredFunctions <- o .:? "ignoredFunctions" .!= []
    ignoredModules   <- o .:? "ignoredModules"   .!= []
    checkModules     <- o .:? "checkModules"     .!= ["*"]
    return YamlTable {..}

data YamlTableKeys = 
    NonCompositeKey String 
  | CompositeKey { cols :: [String] }
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
    pkg_name        :: String,
    mod_name        :: String,
    err_msg         :: String,
    src_span        :: SrcSpan,
    violation       :: Violation,
    suggested_fixes :: Suggestions,
    error_info      :: Value
  } 
  deriving (Eq, Show)

data FunctionRule = 
  FunctionRule
    {
      fn_rule_name             :: String,
      fn_name                  :: FunctionNames,
      arg_no                   :: ArgNo,
      fn_sigs_blocked          :: SignaturesBlockedInFn,
      fns_blocked_in_arg       :: FnsBlockedInArg,
      types_blocked_in_arg     :: TypesBlockedInArg,
      types_to_check_in_arg    :: TypesToCheckInArg,
      fn_rule_fixes            :: Suggestions,
      fn_rule_exceptions       :: Rules,
      fn_rule_ignore_modules   :: Modules,
      fn_rule_check_modules    :: Modules,
      fn_rule_ignore_functions :: ModulesWithFunctions
    }
  deriving (Show, Eq)  

defaultFunctionRule :: FunctionRule
defaultFunctionRule = FunctionRule {
    fn_rule_name             = "NA",
    fn_name                  = [],
    arg_no                   = -1,
    fn_sigs_blocked          = [],
    fns_blocked_in_arg       = [],
    types_blocked_in_arg     = [],
    types_to_check_in_arg    = [],
    fn_rule_fixes            = [],
    fn_rule_exceptions       = [],
    fn_rule_ignore_modules   = [],
    fn_rule_check_modules    = ["*"],
    fn_rule_ignore_functions = []
  }

instance FromJSON FunctionRule where
  parseJSON = withObject "FunctionRule" $ \o -> do
                fn_rule_name             <- o .:  "fn_rule_name"
                fn_name                  <- o .:  "fn_name"                  >>= parseAsListOrString
                arg_no                   <- o .:  "arg_no"
                fn_sigs_blocked          <- o .:? "fn_sigs_blocked"          .!= (fn_sigs_blocked defaultFunctionRule)
                fns_blocked_in_arg       <- o .:  "fns_blocked_in_arg"
                types_blocked_in_arg     <- o .:  "types_blocked_in_arg"
                types_to_check_in_arg    <- o .:  "types_to_check_in_arg"
                fn_rule_fixes            <- o .:  "fn_rule_fixes"
                fn_rule_exceptions       <- o .:  "fn_rule_exceptions"
                fn_rule_ignore_modules   <- o .:  "fn_rule_ignore_modules"
                fn_rule_check_modules    <- o .:? "fn_rule_check_modules"    .!= (fn_rule_check_modules defaultFunctionRule)
                fn_rule_ignore_functions <- o .:? "fn_rule_ignore_functions" .!= (fn_rule_ignore_functions defaultFunctionRule)
                return FunctionRule {..}

data InfiniteRecursionRule = 
  InfiniteRecursionRule
    {
      infinite_recursion_rule_name             :: String,
      infinite_recursion_rule_fixes            :: Suggestions,
      infinite_recursion_rule_exceptions       :: Rules,
      infinite_recursion_rule_ignore_modules   :: Modules,
      infinite_recursion_rule_check_modules    :: Modules,
      infinite_recursion_rule_ignore_functions :: ModulesWithFunctions
    }
  deriving (Show, Eq)  

defaultInfiniteRecursionRule :: InfiniteRecursionRule
defaultInfiniteRecursionRule = InfiniteRecursionRule {
    infinite_recursion_rule_name             = "NA",
    infinite_recursion_rule_fixes            = [],
    infinite_recursion_rule_exceptions       = [],
    infinite_recursion_rule_ignore_modules   = [],
    infinite_recursion_rule_check_modules    = ["*"],
    infinite_recursion_rule_ignore_functions = []
  }

instance FromJSON InfiniteRecursionRule where
  parseJSON = withObject "InfiniteRecursionRule" $ \o -> do
                infinite_recursion_rule_name              <- o .:  "infinite_recursion_rule_name"
                infinite_recursion_rule_fixes             <- o .:  "infinite_recursion_rule_fixes"
                infinite_recursion_rule_exceptions        <- o .:? "infinite_recursion_rule_exceptions"       .!= (infinite_recursion_rule_exceptions defaultInfiniteRecursionRule)
                infinite_recursion_rule_ignore_modules    <- o .:? "infinite_recursion_rule_ignore_modules"   .!= (infinite_recursion_rule_ignore_modules defaultInfiniteRecursionRule)
                infinite_recursion_rule_check_modules     <- o .:? "infinite_recursion_rule_check_modules"    .!= (infinite_recursion_rule_check_modules defaultInfiniteRecursionRule)
                infinite_recursion_rule_ignore_functions  <- o .:? "infinite_recursion_rule_ignore_functions" .!= (infinite_recursion_rule_ignore_functions defaultInfiniteRecursionRule)
                return InfiniteRecursionRule {..}

data DBRule =
  DBRule 
    {
      db_rule_name             :: String,
      table_name               :: String,
      indexed_cols_names       :: [YamlTableKeys],
      db_rule_fixes            :: Suggestions,
      db_rule_exceptions       :: Rules,
      db_rule_check_modules    :: Modules,
      db_rule_ignore_modules   :: Modules,
      db_rule_ignore_functions :: ModulesWithFunctions
    }
  deriving (Show, Eq)  

defaultDBRule :: DBRule
defaultDBRule = DBRule {
    db_rule_name             = "NA",
    table_name               = "NA",
    indexed_cols_names       = [],
    db_rule_fixes            = [],
    db_rule_exceptions       = [],
    db_rule_check_modules    = ["*"],
    db_rule_ignore_modules   = [],
    db_rule_ignore_functions = []
  }

instance FromJSON DBRule where
  parseJSON = withObject "DBRule" $ \o -> do
                db_rule_name             <- o .:  "db_rule_name"
                table_name               <- o .:  "table_name"
                indexed_cols_names       <- o .:  "indexed_cols_names"
                db_rule_fixes            <- o .:  "db_rule_fixes"
                db_rule_exceptions       <- o .:  "db_rule_exceptions"
                db_rule_check_modules    <- o .:? "db_rule_check_modules"    .!= (db_rule_check_modules defaultDBRule)
                db_rule_ignore_modules   <- o .:? "db_rule_ignore_modules"   .!= (db_rule_ignore_modules defaultDBRule)
                db_rule_ignore_functions <- o .:? "db_rule_ignore_functions" .!= (db_rule_ignore_functions defaultDBRule)
                return DBRule {..}

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
                fnName         <- o .:  "fnName"
                isQualified    <- o .:? "isQualified" .!= False
                argNo          <- o .:  "argNo"
                fnAction       <- o .:  "action"
                argTypes       <- o .:  "argTypes"
                argFns         <- o .:  "argFns"
                suggestedFixes <- o .:  "suggestedFixes"
                return FunctionInfo {..}

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
                return GeneralRule {..}

data ColumnAccessRule =
  ColumnAccessRule
    { column_access_rule_name             :: String
    , column_name                         :: String
    , allowed_tables                      :: [String]
    , column_access_rule_fixes            :: Suggestions
    , column_access_rule_ignore_modules   :: Modules
    , column_access_rule_ignore_functions :: ModulesWithFunctions
    , skip_known_tables_check             :: Bool
    }
  deriving (Show, Eq)

defaultColumnAccessRule :: ColumnAccessRule
defaultColumnAccessRule = ColumnAccessRule {
    column_access_rule_name             = "NA",
    column_name                         = "NA",
    allowed_tables                      = [],
    column_access_rule_fixes            = [],
    column_access_rule_ignore_modules   = [],
    column_access_rule_ignore_functions = [],
    skip_known_tables_check             = False
  }

instance FromJSON ColumnAccessRule where
  parseJSON = withObject "ColumnAccessRule" $ \o -> do
                column_access_rule_name             <- o .: "column_access_rule_name"
                column_name                         <- o .: "column_name"
                allowed_tables                      <- o .: "allowed_tables"
                column_access_rule_fixes            <- o .: "column_access_rule_fixes"
                column_access_rule_ignore_modules   <- o .:? "column_access_rule_ignore_modules"   .!= (column_access_rule_ignore_modules defaultColumnAccessRule)
                column_access_rule_ignore_functions <- o .:? "column_access_rule_ignore_functions" .!= (column_access_rule_ignore_functions defaultColumnAccessRule)
                skip_known_tables_check             <- o .:? "skip_known_tables_check"             .!= (skip_known_tables_check defaultColumnAccessRule)
                return ColumnAccessRule {..}

{-
  Blocks a set of functions from being applied to a value whose type *is*, or
  *transitively contains*, one of `blocked_types`.

  This differs from `FunctionRule`'s `types_blocked_in_arg`, which only compares
  the argument's own type name. Use this rule for narrow marker types such as an
  encrypted `PII` newtype, where a match anywhere inside the value is a problem:

    newtype PII = PII Text
    data A = A { var1 :: PII }   -- `encode (a :: A)` is blocked
    data B = B { var0 :: A }     -- `encode (b :: B)` is blocked too

  Do not point it at common types like `Text`; every record contains one
  somewhere, so it would flag nearly the whole codebase. Use `FunctionRule` for
  that shallow case instead.
-}
data TypeBlockedRule =
  TypeBlockedRule
    {
      type_blocked_rule_name             :: String,
      type_blocked_fn_name               :: FunctionNames,        -- functions to check, e.g. show/encode/toJSON
      type_blocked_arg_no                :: [ArgNo],              -- 1-based args to check; [] means every arg
      blocked_types                      :: TypesBlockedInArg,    -- e.g. ["PII"], ["Types.PII"], ["Euler.*.PII"]
      type_blocked_rule_ignore_types     :: [String],             -- types to neither match nor look inside
      type_blocked_rule_fixes            :: Suggestions,
      type_blocked_rule_exceptions       :: Rules,
      type_blocked_rule_ignore_modules   :: Modules,
      type_blocked_rule_check_modules    :: Modules,
      type_blocked_rule_ignore_functions :: ModulesWithFunctions
    }
  deriving (Show, Eq)

defaultTypeBlockedRule :: TypeBlockedRule
defaultTypeBlockedRule = TypeBlockedRule {
    type_blocked_rule_name             = "NA",
    type_blocked_fn_name               = [],
    type_blocked_arg_no                = [],
    blocked_types                      = [],
    type_blocked_rule_ignore_types     = [],
    type_blocked_rule_fixes            = [],
    type_blocked_rule_exceptions       = [],
    type_blocked_rule_ignore_modules   = [],
    type_blocked_rule_check_modules    = ["*"],
    type_blocked_rule_ignore_functions = []
  }

instance FromJSON TypeBlockedRule where
  parseJSON = withObject "TypeBlockedRule" $ \o -> do
                type_blocked_rule_name             <- o .:  "type_blocked_rule_name"
                type_blocked_fn_name               <- o .:  "type_blocked_fn_name"               >>= parseAsListOrSingle
                type_blocked_arg_no                <- o .:? "type_blocked_arg_no"                .!= A.Null >>= parseArgNos
                blocked_types                      <- o .:  "blocked_types"                      >>= parseAsListOrSingle
                type_blocked_rule_ignore_types     <- o .:? "type_blocked_rule_ignore_types"     .!= (type_blocked_rule_ignore_types defaultTypeBlockedRule)
                type_blocked_rule_fixes            <- o .:? "type_blocked_rule_fixes"            .!= (type_blocked_rule_fixes defaultTypeBlockedRule)
                type_blocked_rule_exceptions       <- o .:? "type_blocked_rule_exceptions"       .!= (type_blocked_rule_exceptions defaultTypeBlockedRule)
                type_blocked_rule_ignore_modules   <- o .:? "type_blocked_rule_ignore_modules"   .!= (type_blocked_rule_ignore_modules defaultTypeBlockedRule)
                type_blocked_rule_check_modules    <- o .:? "type_blocked_rule_check_modules"    .!= (type_blocked_rule_check_modules defaultTypeBlockedRule)
                type_blocked_rule_ignore_functions <- o .:? "type_blocked_rule_ignore_functions" .!= (type_blocked_rule_ignore_functions defaultTypeBlockedRule)
                return TypeBlockedRule {..}
    where
      -- Accepts `type_blocked_arg_no: 1`, `type_blocked_arg_no: [1, 2]`, or omitted (= all args)
      parseArgNos A.Null = pure (type_blocked_arg_no defaultTypeBlockedRule)
      parseArgNos v      = parseAsListOrSingle v

data Rule =
    DBRuleT DBRule
  | FunctionRuleT FunctionRule
  | InfiniteRecursionRuleT InfiniteRecursionRule
  | GeneralRuleT GeneralRule
  | ColumnAccessRuleT ColumnAccessRule
  | TypeBlockedRuleT TypeBlockedRule
  deriving (Show, Eq)

-- Rules are distinguished by which mandatory keys they carry, so each new arm must
-- require a key no other rule has. `TypeBlockedRule` is matched last and requires
-- `type_blocked_rule_name`, keeping the existing rules' parse order untouched.
instance FromJSON Rule where
  parseJSON str = (DBRuleT <$> parseJSON str) <|> (FunctionRuleT <$> parseJSON str) <|> (InfiniteRecursionRuleT <$> parseJSON str) <|> (GeneralRuleT <$> parseJSON str) <|> (ColumnAccessRuleT <$> parseJSON str) <|> (TypeBlockedRuleT <$> parseJSON str) <|> (fail $ "Invalid Rule: " <> show str)

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
  | EmptyWhereClause String DBRule
  | FnUseBlocked String FunctionRule
  | FnSigBlocked String String FunctionRule
  | InfiniteRecursionDetected InfiniteRecursionRule
  | ColumnAccessViolation String String ColumnAccessRule
  | BlockedTypeInArg String String String [String] TypeBlockedRule -- fnName, argType, blockedType, containment path
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show violation = case violation of
    (ArgTypeBlocked typ exprTy ruleFnName rule)      -> "Use of '" <> ruleFnName <> "' on '" <> typ <> "' is not allowed in the overall expression type '" <> exprTy <> "'."
    (FnBlockedInArg (fnName, typ) ruleFnName _ rule) -> "Use of '" <> fnName <> "' on type '" <> typ <> "' inside argument of '" <> ruleFnName <> "' is not allowed."
    (FnUseBlocked ruleFnName rule)                   -> "Use of '" <> ruleFnName <> "' in the code is not allowed."
    (FnSigBlocked ruleFnName ruleFnSig rule)         -> "Use of '" <> ruleFnName <> "' with signature '" <> ruleFnSig <> "' is not allowed in the code."
    (NonIndexedDBColumn colName tableName _)         -> "Querying on non-indexed column '" <> colName <> "' of table '" <> (tableName) <> "' is not allowed."
    (EmptyWhereClause tableName _)                   -> "Query with empty where clause on table '" <> (tableName) <> "' is not allowed."
    (InfiniteRecursionDetected _)                    -> "Infinite recursion detected in expression"
    (ColumnAccessViolation colName tableName _)      -> "Accessing column '" <> colName <> "' on table '" <> tableName <> "' is not allowed."
    -- The path holds one entry per step from the argument's own type down to the
    -- blocked type, so a single entry means the argument type matched directly.
    (BlockedTypeInArg fnName typ blockedTyp path _)
      | length path <= 1 -> "Use of '" <> fnName <> "' on '" <> typ <> "' is not allowed."
      | otherwise        -> "Use of '" <> fnName <> "' on '" <> typ <> "' is not allowed as it contains '" <> blockedTyp <> "'."
                              <> "\n      Containment path: " <> intercalate " -> " path
    NoViolation                                      -> "NoViolation"
