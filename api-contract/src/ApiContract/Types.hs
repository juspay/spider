{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE StandaloneDeriving #-}

module ApiContract.Types where

import Data.Aeson (ToJSON,FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map as Map
-- import GHC.Hs (SrcSpanAnnA)
import GHC (SrcSpan(..), RealSrcSpan(..))

data CliOptions = CliOptions {
    path :: FilePath,
    port :: Int,
    host :: String,
    log :: Bool,
    tc_funcs :: Maybe Bool,
    api_contract :: Maybe Bool
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data TypeOfInstance =
      Derived
      | Custom
   deriving (Eq,Show, Ord,Generic,FromJSON,ToJSON)

data CaseType =
       SnakeCase
       | CamelCase
       | PascalCase
       | KebabCase
   deriving (Eq,Show,Ord,Generic,FromJSON,ToJSON)

data InstancePresence =
       ToJSON
       | ParseJSON
       | ToEncoding
   deriving (Eq,Show, Ord,Generic,FromJSON,ToJSON)

data TypeRule = TypeRule
   { caseType  :: Maybe CaseType
   , dataConstructors :: Map.Map String DataConInfo
   , instances :: Map.Map String InstanceFromTC
   , typeKind :: String
   } deriving (Show,Eq,Ord ,Generic,FromJSON,ToJSON)

data Types = Types
   { types :: Map.Map String TypeRule
   } deriving (Show, Generic,FromJSON,ToJSON)

data DataConInfo = DataConInfo
  { fields'      :: Map.Map String String
  , sumTypes    :: [String]
  } deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data InstanceFromTC = InstanceFromTC
   {
      fieldsList :: [String]
      , typeOfInstance :: TypeOfInstance
   }
   deriving (Show, Eq, Ord,Generic,FromJSON,ToJSON)


data ApiContractError =
    -- fieldName typeName
    MISSING_FIELD_IN_RULES String String
    -- fieldName typeName
    | MISSING_FIELD_IN_CODE String String
    -- fieldName expectedType typeFromCode typeName
    | TYPE_MISMATCH String String String String
    -- dataConName typeName
    | MISSING_DATACON String String
    -- typeName fieldName caseType
    | FIELD_CASE_MISMATCH String String CaseType
    -- typeName
    | MISSING_TYPE_CODE String
    -- typeName ruleYAML
    | MISSING_TYPE_IN_RULE String String
    -- typeName instanceName
    | MISSING_INSTANCE_IN_CODE String String
    -- typeName instanceName ruleYAML
    | MISSING_INSTANCE_IN_RULES String String String
    -- typeName instanceName typeOfInsance
    | TYPE_OF_INSTANCE_CHANGED String String TypeOfInstance
    -- typeName instanceName fieldName
    | MISSING_FIELD_IN_INSTANCE_CODE String String String
    -- typeName instanceName fieldName
    | MISSING_FIELD_IN_INSTANCE_RULES String String String
    deriving (Eq,Show, Generic,FromJSON,ToJSON)

generateErrorMessage :: FilePath -> ApiContractError -> String
generateErrorMessage yamlFilePath (MISSING_FIELD_IN_RULES fieldName typeName) =
    "Error: The field '" ++ fieldName ++ "' is missing in the rules for type '" ++ typeName ++ "'.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd the field under the appropriate type's fields section."
generateErrorMessage yamlFilePath (MISSING_FIELD_IN_CODE fieldName typeName) =
    "Error: The field '" ++ fieldName ++ "' is missing in the code for type '" ++ typeName ++ "'.\n\n" ++
    "\tPlease add the field '" ++ fieldName ++ "' to the type '" ++ typeName ++ "' in your code."
generateErrorMessage yamlFilePath (TYPE_MISMATCH fieldName expectedType typeFromCode typeName) =
    "Error: Type mismatch for field '" ++ fieldName ++ "' in type '" ++ typeName ++ "'. Expected type: '" ++ expectedType ++ "', but found: '" ++ typeFromCode ++ "'.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tChange the type of the field '" ++ fieldName ++ "' to '" ++ expectedType ++ "' under the appropriate type's fields section."
generateErrorMessage yamlFilePath (MISSING_DATACON dataConName typeName) =
    "Error: The data constructor '" ++ dataConName ++ "' is missing in type '" ++ typeName ++ "'.\n\n" ++
    "Please add the data constructor '" ++ dataConName ++ "' to the type '" ++ typeName ++ "' in your code."
generateErrorMessage yamlFilePath (FIELD_CASE_MISMATCH typeName fieldName caseType) =
    "Error: Field name case mismatch for field '" ++ fieldName ++ "' in type '" ++ typeName ++ "'. Expected case: " ++ show caseType ++ ".\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tChange the field name to follow the " ++ show caseType ++ " convention under the appropriate type's fields section."
generateErrorMessage yamlFilePath (MISSING_TYPE_CODE typeName) =
    "Error: The type '" ++ typeName ++ "' is missing.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd the type '" ++ typeName ++ "' to the types section."
generateErrorMessage yamlFilePath (MISSING_TYPE_IN_RULE typeName yamlRule) =
    "Error: The type '" ++ typeName ++ "' is missing in the rules.\n\n" ++
    "\tYou should add the rule in the file: " ++ yamlFilePath ++
    "\n\tAdd the type '" ++ typeName ++ "' to the types section." ++ "\n\n" ++ yamlRule
generateErrorMessage yamlFilePath (MISSING_INSTANCE_IN_CODE typeName instanceName) =
    "Error: The instance '" ++ instanceName ++ "' is missing for type '" ++ typeName ++ "'in the code.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd the instance '" ++ instanceName ++ "' under the appropriate type's instances section."
generateErrorMessage yamlFilePath (MISSING_INSTANCE_IN_RULES typeName instanceName yamlRule) =
    "Error: The instance '" ++ instanceName ++ "' is missing for type '" ++ typeName ++ "' in the rules.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd the instance '" ++ instanceName ++ "' under the appropriate type's instances section." ++ "\n\n" ++ yamlRule
generateErrorMessage yamlFilePath (TYPE_OF_INSTANCE_CHANGED typeName instanceName typeOfInstance) =
    "Error: The type of instance '" ++ instanceName ++ "' for type '" ++ typeName ++ "' has changed to " ++ show typeOfInstance ++ ".\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tChange the type of the instance '" ++ instanceName ++ "' under the appropriate type's instances section."
generateErrorMessage yamlFilePath (MISSING_FIELD_IN_INSTANCE_CODE typeName instanceName fieldName) =
    "Error: The field '" ++ fieldName ++ "' is missing in the instance '" ++ instanceName ++ "' for type '" ++ typeName ++ "'.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd/remove the field '" ++ fieldName ++ "' under the appropriate instance's fieldsList section."
generateErrorMessage yamlFilePath (MISSING_FIELD_IN_INSTANCE_RULES typeName instanceName fieldName) =
    "Error: The field '" ++ fieldName ++ "' is missing in rules in instance '" ++ instanceName ++ "' for type '" ++ typeName ++ "'.\n\n" ++
    "\tYou can update the change in the file: " ++ yamlFilePath ++
    "\n\tAdd/remove the field '" ++ fieldName ++ "' under the appropriate instance's fieldsList section."
