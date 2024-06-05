module Sheriff.Types where
  
import Data.Aeson
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))
import Control.Applicative ((<|>))
import Data.Text (unpack)

data PluginOpts = PluginOpts {
    saveToFile :: Bool,
    throwCompilationError :: Bool,
    failOnFileNotFound :: Bool,
    savePath :: String,
    indexedKeysPath :: String,
    matchAllInsideAnd :: Bool
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = 
  PluginOpts { 
    saveToFile = False, 
    throwCompilationError = True, 
    failOnFileNotFound = True, 
    matchAllInsideAnd = False,
    savePath = ".juspay/tmp/sheriff/", 
    indexedKeysPath = ".juspay/indexedKeys.yaml" 
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    saveToFile <- o .:? "saveToFile" .!= (saveToFile defaultPluginOpts)
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    throwCompilationError <- o .:? "throwCompilationError" .!= (throwCompilationError defaultPluginOpts)
    savePath <- o .:? "savePath" .!= (savePath defaultPluginOpts)
    indexedKeysPath <- o .:? "indexedKeysPath" .!= (indexedKeysPath defaultPluginOpts)
    matchAllInsideAnd <- o .:? "matchAllInsideAnd" .!= (matchAllInsideAnd defaultPluginOpts)
    return PluginOpts { saveToFile = saveToFile, throwCompilationError = throwCompilationError, matchAllInsideAnd = matchAllInsideAnd, savePath = savePath, indexedKeysPath = indexedKeysPath, failOnFileNotFound = failOnFileNotFound }


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
    suggested_fixes  :: Suggestions
  } deriving (Eq, Show)

instance ToJSON CompileError where
  toJSON (CompileError pkg modName errMsg srcLoc _vlt suggestions) =
    object [ "package_name"    .= pkg
           , "module_name"     .= modName
           , "error_message"   .= errMsg
           , "src_span"        .= show srcLoc
           , "violation_type"  .= getViolationType _vlt
           , "violated_rule"   .= getRuleName _vlt
           , "suggested_fixes" .= suggestions
           ]

type Rules = [Rule]
type ArgNo = Int
type FnsBlockedInArg = [(String, ArgNo, TypesAllowedInArg)]
type TypesAllowedInArg = [String]
type TypesBlockedInArg = [String]
type TypesToCheckInArg = [String]
type Suggestions = [String]

data FunctionRule = 
  FunctionRule
    {
      fn_rule_name          :: String,
      fn_name               :: String,
      arg_no                :: ArgNo,
      fns_blocked_in_arg    :: FnsBlockedInArg,
      types_blocked_in_arg  :: TypesBlockedInArg,
      types_to_check_in_arg :: TypesToCheckInArg,
      fnRuleFixes           :: Suggestions
    }
  deriving (Show, Eq)  

data DBRule =
  DBRule 
    {
      db_rule_name       :: String,
      table_name         :: String,
      indexed_cols_names :: [YamlTableKeys],
      dbRuleFixes        :: Suggestions
    }
  deriving (Show, Eq)  

data Rule = 
    DBRuleT DBRule
  | FunctionRuleT FunctionRule
  deriving (Show, Eq)  

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
    ArgTypeBlocked String FunctionRule
  | FnBlockedInArg (String, String) FunctionRule
  | NonIndexedDBColumn String String DBRule
  | FnUseBlocked FunctionRule
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show (ArgTypeBlocked typ rule) = "Use of '" <> (fn_name rule) <> "' on '" <> typ <> "' is not allowed."
  show (FnBlockedInArg (fnName, typ) rule) = "Use of '" <> fnName <> "' on type '" <> typ <> "' inside argument of '" <> (fn_name rule) <> "' is not allowed."
  show (FnUseBlocked rule) = "Use of '" <> (fn_name rule) <> "' in the code is not allowed."
  show (NonIndexedDBColumn colName tableName _) = "Querying on non-indexed column '" <> colName <> "' of table '" <> (tableName) <> "' is not allowed."
  show NoViolation = "NoViolation"

getViolationSuggestions :: Violation -> Suggestions
getViolationSuggestions v = case v of
  ArgTypeBlocked _ r -> fnRuleFixes r
  FnBlockedInArg _ r -> fnRuleFixes r
  FnUseBlocked r -> fnRuleFixes r
  NonIndexedDBColumn _ _ r -> dbRuleFixes r
  NoViolation -> []

getViolationType :: Violation -> String
getViolationType v = case v of
  ArgTypeBlocked _ _ -> "ArgTypeBlocked"
  FnBlockedInArg _ _ -> "FnBlockedInArg"
  FnUseBlocked _ -> "FnUseBlocked"
  NonIndexedDBColumn _ _ _ -> "NonIndexedDBColumn"
  NoViolation -> "NoViolation"

getViolationRule :: Violation -> Rule
getViolationRule v = case v of
  ArgTypeBlocked _ r -> FunctionRuleT r
  FnBlockedInArg _ r -> FunctionRuleT r
  FnUseBlocked r -> FunctionRuleT r
  NonIndexedDBColumn _ _ r -> DBRuleT r
  NoViolation -> defaultRule

getRuleName :: Violation -> String
getRuleName v = case v of
  ArgTypeBlocked _ r -> fn_rule_name r
  FnBlockedInArg _ r -> fn_rule_name r
  FnUseBlocked r -> fn_rule_name r
  NonIndexedDBColumn _ _ r -> db_rule_name r
  NoViolation -> "NA"

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

noSuggestion :: Suggestions
noSuggestion = []

defaultRule :: Rule
defaultRule = FunctionRuleT $ FunctionRule "NA" "NA" (-1) [] [] [] noSuggestion

emptyLoggingError :: CompileError
emptyLoggingError = CompileError "" "" "$NA$" noSrcSpan NoViolation noSuggestion

yamlToDbRule :: YamlTable -> Rule
yamlToDbRule table = DBRuleT $ DBRule "NonIndexedDBRule" (tableName table) (indexedKeys table) ["You might want to include an indexed column in the `where` clause of the query."]