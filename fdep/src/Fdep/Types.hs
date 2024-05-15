module Fdep.Types where
import Data.Aeson

data DataTypeUC = DataTypeUC {
    function_name_ :: [String]
    , typeVsFields :: [TypeVsFields]
    } deriving (Show, Eq, Ord)

data TypeVsFields = TypeVsFields {
    type_name :: String
    , fieldsVsExprs :: [(FieldRep)]
} deriving (Show, Eq, Ord)

data FieldRep = FieldRep {
  field_name :: String
  , expression :: String
  , field_type :: String
} deriving (Show, Eq, Ord)

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  , _type :: String
  , src_Loc    :: String
  , arguments :: [String]
  } deriving (Show, Eq, Ord)

data Function = Function
  {  function_name    :: String
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: String
  } deriving (Show, Eq, Ord)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: String
  , typeSignature :: String
} deriving (Show, Eq, Ord)

instance ToJSON MissingTopLevelBindsSignature where
  toJSON (MissingTopLevelBindsSignature srcSpan typeSignature) =
    object [ "srcSpan" .= srcSpan
           , "typeSignature"  .= typeSignature
           ]

instance ToJSON FunctionInfo where
  toJSON (FunctionInfo pkg modName funcName _type srcLoc arguments) =
    object [ "package_name" .= pkg
           , "module_name"  .= modName
           , "name"         .= funcName
           , "_type"         .= _type
           , "src_loc"         .= srcLoc
           , "arguments"         .= arguments
           ]

instance ToJSON Function where
  toJSON (Function funcName funcsCalled whereFuncs srcLoc) =
    object [ "function_name"    .= funcName
           , "functions_called" .= funcsCalled
           , "where_functions"  .= whereFuncs
           , "src_loc"         .= srcLoc
           ]

instance FromJSON FunctionInfo where
  parseJSON = withObject "FunctionInfo" $ \v ->
    FunctionInfo <$> v .: "package_name"
                 <*> v .: "module_name"
                 <*> v .: "name"
                 <*> v .: "_type"
                 <*> v .: "src_loc"
                 <*> v .: "arguments"

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v ->
    Function <$> v .: "function_name"
             <*> v .: "functions_called"
             <*> v .: "where_functions"
             <*> v .: "src_loc"

instance ToJSON DataTypeUC where
    toJSON (DataTypeUC fn fields) =
        object ["function_name" .= fn, "typeVsFields" .= fields]

instance FromJSON DataTypeUC where
    parseJSON (Object v) =
        DataTypeUC <$> v .: "function_name" <*> v .: "typeVsFields"
    parseJSON _ = fail "Invalid DataTypeUC JSON"

instance ToJSON FieldRep where
    toJSON (FieldRep field_name expression field_type) =
        object ["field_name" .= field_name, "expression" .= expression , "field_type" .= field_type]

instance FromJSON FieldRep where
    parseJSON (Object v) =
        FieldRep <$> v .: "field_name" <*> v .: "expression" <*> v .: "field_type"
    parseJSON _ = fail "Invalid FieldRep JSON"

instance ToJSON TypeVsFields where
    toJSON (TypeVsFields tn fes) =
        object ["type_name" .= tn, "fieldsVsExprs" .= fes]

instance FromJSON TypeVsFields where
    parseJSON (Object v) =
        TypeVsFields <$> v .: "type_name" <*> v .: "fieldsVsExprs"
    parseJSON _ = fail "Invalid TypeVsFields JSON"