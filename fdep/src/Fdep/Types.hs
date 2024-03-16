module Fdep.Types where
import Data.Aeson

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  , src_Loc    :: String
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
  toJSON (FunctionInfo pkg modName funcName srcLoc) =
    object [ "package_name" .= pkg
           , "module_name"  .= modName
           , "name"         .= funcName
           , "src_loc"         .= srcLoc
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
                 <*> v .: "src_loc"

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v ->
    Function <$> v .: "function_name"
             <*> v .: "functions_called"
             <*> v .: "where_functions"
             <*> v .: "src_loc"