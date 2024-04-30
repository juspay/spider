module Sheriff.Types where
  
import Data.Aeson
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))

data PluginOpts = PluginOpts {
    saveToFile :: Bool,
    throwCompilationError :: Bool,
    failOnFileNotFound :: Bool,
    savePath :: String,
    indexedKeysPath :: String
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = PluginOpts { saveToFile = False, throwCompilationError = True, failOnFileNotFound = True, savePath = ".juspay/tmp/sheriff/", indexedKeysPath = ".juspay/indexedKeys.yaml" }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    saveToFile <- o .:? "saveToFile" .!= (saveToFile defaultPluginOpts)
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    throwCompilationError <- o .:? "throwCompilationError" .!= (throwCompilationError defaultPluginOpts)
    savePath <- o .:? "savePath" .!= (savePath defaultPluginOpts)
    indexedKeysPath <- o .:? "indexedKeysPath" .!= (indexedKeysPath defaultPluginOpts)
    return PluginOpts { saveToFile = saveToFile, throwCompilationError = throwCompilationError, savePath = savePath, indexedKeysPath = indexedKeysPath, failOnFileNotFound = failOnFileNotFound }


data YamlTables = YamlTables
  { tables :: [YamlTable]
  } deriving (Show, Eq)

instance FromJSON YamlTables where
  parseJSON = withObject "YamlTables" $ \o -> do
    tableList <- o .: "tables"
    return YamlTables { tables = tableList }

data YamlTable = YamlTable
  { tableName :: String
  , indexedKeys :: [String]
  } deriving (Show, Eq)

instance FromJSON YamlTable where
  parseJSON = withObject "YamlTable" $ \o -> do
    name <- o .: "name"
    keys <- o .: "indexedKeys"
    return YamlTable { tableName = name, indexedKeys = keys }

data CompileError = CompileError
  {
    pkg_name :: String,
    mod_name :: String,
    err_msg :: String,
    src_span :: SrcSpan,
    violation :: Violation
  } deriving (Eq, Show)

instance ToJSON CompileError where
  toJSON (CompileError pkg modName errMsg srcLoc _vlt) =
    object [ "package_name"   .= pkg
           , "module_name"    .= modName
           , "error_message"  .= errMsg
           , "src_span"       .= show srcLoc
           , "violation_type" .= getViolationType _vlt
           , "violated_rule"  .= rule_name (getRule _vlt)
           ]

type Rules = [Rule]
type ArgNo = Int
type FnsBlockedInArg = [String]
type TypesBlockedInArg = [String]
type TypesToCheckInArg = [String]

data Rule = 
    FunctionRule
    {
      rule_name             :: String,
      fn_name               :: String,
      arg_no                :: ArgNo,
      fns_blocked_in_arg    :: FnsBlockedInArg,
      types_blocked_in_arg  :: TypesBlockedInArg,
      types_to_check_in_arg :: TypesToCheckInArg
    }
  | DBRule 
    {
      rule_name          :: String,
      table_name         :: String,
      indexed_cols_names :: [String]
    } 
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
    ArgTypeBlocked String Rule
  | FnBlockedInArg String Rule
  | NonIndexedDBColumn String String Rule
  | FnUseBlocked Rule
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show (ArgTypeBlocked typ rule) = "Use of '" <> (fn_name rule) <> "' on '" <> typ <> "' is not allowed."
  show (FnBlockedInArg fnName rule) = "Use of '" <> fnName <> "' inside argument of '" <> (fn_name rule) <> "' is not allowed."
  show (FnUseBlocked rule) = "Use of '" <> (fn_name rule) <> "' in the code is not allowed."
  show (NonIndexedDBColumn colName tableName rule) = "Querying on non-indexed column '" <> colName <> "' of table '" <> (tableName) <> "' is not allowed."
  show NoViolation = "NoViolation"

getViolationType :: Violation -> String
getViolationType v = case v of
  ArgTypeBlocked _ _ -> "ArgTypeBlocked"
  FnBlockedInArg _ _ -> "FnBlockedInArg"
  FnUseBlocked _ -> "FnUseBlocked"
  NonIndexedDBColumn _ _ _ -> "NonIndexedDBColumn"
  NoViolation -> "NoViolation"

getRule :: Violation -> Rule
getRule v = case v of
  ArgTypeBlocked _ r -> r
  FnBlockedInArg _ r -> r
  FnUseBlocked r -> r
  NonIndexedDBColumn _ _ r -> r
  NoViolation -> defaultRule

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

defaultRule :: Rule
defaultRule = FunctionRule "NA" "NA" (-1) [] [] []

emptyLoggingError :: CompileError
emptyLoggingError = CompileError "" "" "$NA$" noSrcSpan NoViolation

yamlToDbRule :: YamlTable -> Rule
yamlToDbRule table = DBRule "NonIndexedDBRule" (tableName table) (indexedKeys table)