{-# LANGUAGE DeriveAnyClass #-}
module Fdep.Types where
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text
import Data.Binary
import Control.DeepSeq

data FunctionInfo = FunctionInfo
  { package_name :: Text
  , module_name :: Text
  , name    :: Text
  , _type :: Text
  , src_Loc    :: Text
  , arguments :: [Text]
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data Function = Function
  {  function_name    :: Text
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: Text
  , stringified_code :: Text
  , function_signature :: Text
  } deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: Text
  , typeSignature :: Text
} deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)

data PFunction = PFunction {
    parser_name :: Text
    , parser_stringified_code :: Text
    , parser_src_loc :: Text
}
    deriving (Show, Eq, Ord,Binary,Generic,NFData,ToJSON,FromJSON)