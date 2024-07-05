{-# LANGUAGE DeriveAnyClass #-}
module Fdep.Types where
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text

data DataTypeUC = DataTypeUC {
    function_name_ :: [Text]
    , typeVsFields :: [TypeVsFields]
    } deriving (Show, Eq, Ord)

data TypeVsFields = TypeVsFields {
    type_name :: Text
    , fieldsVsExprs :: [(FieldRep)]
} deriving (Show, Eq, Ord)

data FieldRep = FieldRep {
  field_name :: Text
  , expression :: Text
  , field_type :: Text
} deriving (Show, Eq, Ord)

data FunctionInfo = FunctionInfo
  { package_name :: Text
  , module_name :: Text
  , name    :: Text
  , _type :: Text
  , src_Loc    :: Text
  , arguments :: [Text]
  } deriving (Show, Eq, Ord)

data Function = Function
  {  function_name    :: Text
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: Text
  , stringified_code :: Text
  , function_signature :: Text
  } deriving (Show, Eq, Ord)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: Text
  , typeSignature :: Text
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
  toJSON (Function funcName funcsCalled whereFuncs srcLoc codeStringified function_signature) =
    object [ "function_name"    .= funcName
           , "functions_called" .= funcsCalled
           , "where_functions"  .= whereFuncs
           , "src_loc"         .= srcLoc
           , "code_string"         .= codeStringified
           , "function_signature" .= function_signature
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
             <*> v .: "code_string"
             <*> v .: "function_signature"

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


data PFunction = PFunction {
    parser_name :: Text
    , parser_stringified_code :: Text
    , parser_src_loc :: Text
}
    deriving (Generic,Show, Eq, Ord,ToJSON ,FromJSON)