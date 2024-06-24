{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass,RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syn2Chart.Types where

import GHC.Generics (Generic)
import GhcPlugins
    ( AltCon(..),
      Expr(Lam, Lit, Var, Type, App, Let, Case),
      Var,
      CoreBind,
      Bind(Rec, NonRec),
      idName,
      nameStableString,
      showSDocUnsafe,
      Outputable(ppr) )
import qualified Data.Map as Map
import Data.Aeson
import Data.Data ( Data(toConstr) )
import Data.Text (pack, Text,isInfixOf)
import Prelude hiding (id)
import Crypto.Hash
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM

data Function =
      Function Text Text Bool [Function] (Maybe Text) Bool (Maybe Text) (Maybe Text)
      | CaseFunction Text (Maybe CaseExtract) Text Text Bool [Function] (Maybe Text)
      | CaseRelation Text Text Bool [Function] (Maybe Text)
    deriving (Generic,Eq,Show,ToJSON,FromJSON)

instance ToJSON Var where
  toJSON var = String $ pack $ nameStableString (idName var)

instance ToJSON (Expr Var) where
  toJSON (Var id) = toJSON id
  toJSON (Type id) = String $ pack $ showSDocUnsafe $ ppr id
  toJSON x@(Lit _) =
    object
      [ "type" .= String "Lit",
        "literal" .= pack (showSDocUnsafe (ppr x))
      ]
  toJSON (App (Var id) (Type t)) =
    object
      [ "type" .= String "App",
        "function" .= toJSON id,
        "argument" .= String (pack $ showSDocUnsafe $ ppr t)
      ]
  toJSON (App fun (Type _)) = toJSON fun
  toJSON (App fun arg) =
    object
      [ "type" .= String "App",
        "function" .= toJSON fun,
        "argument" .= toJSON arg
      ]
  toJSON (Lam bind expr) =
    object
      [ "type" .= String "Lam",
        "function" .= toJSON bind,
        "argument" .= toJSON expr
      ]
  toJSON (Let bind expr) =
    object
      [ "type" .= String "Let",
        "function_list" .=
            let bindsList = bindToJSON bind
            in Map.fromList bindsList,
        "argument" .= toJSON expr
      ]
  toJSON (Case condition bind _type alts) =
    object
      [ "type" .= String "Case",
        "condition_type" .= String (pack $ showSDocUnsafe $ ppr _type),
        "condition" .= toJSON condition,
        "function" .= toJSON bind,
        "matches" .= toJSON alts
      ]
  toJSON v = object ["unhandled" .= String (pack $ show $ toConstr v),"value" .= String (pack $ showSDocUnsafe $ ppr v)]

instance ToJSON AltCon where
  toJSON (DataAlt dataCon) = String (pack $ showSDocUnsafe $ ppr dataCon)
  toJSON (LitAlt lit) = String (pack $ showSDocUnsafe $ ppr lit)
  toJSON DEFAULT = String "DEFAULT"

data LBind = LNonRec Text Text LExpr
            | LRec [(Text, Text , LExpr)]
            | LNull
  deriving (Generic,Data,Show,ToJSON,FromJSON)

class (ToJSON t) => HashGenerator t where
    generateHash :: t -> Text

data CaseExtract
  = GetField Text Text Text Text
  | AppOnVar Text Text Text
  | OnField Text Text Text
  | AppOnField Text Text Text Text
  | AppAppOnField Text Text Text Text Text Text
  | AppOnGetField Text Text CaseExtract
  | MaybeOrEitherOnGetField Text Text Text CaseExtract
    deriving (Generic,Data,Show,ToJSON,FromJSON,Eq)

instance HashGenerator CaseExtract where
    generateHash (OnField fieldName fieldType renamerfieldName) =
      let k = case fieldType of
                "Text" -> fieldName
                "String" -> fieldName
                "Bool" -> fieldName
                "Int" -> fieldName
                "Maybe Text" -> fieldName
                "Maybe String" -> fieldName
                "Maybe Bool" -> fieldName
                "Maybe Int" -> fieldName
                "$_sys$wild" -> renamerfieldName
                _ -> fieldType
      in pack $ show (hash (toStrict $ encode (toJSON k)) :: Digest  SHA3_256)
    generateHash (AppOnField functionName outputType fieldName inputType) =
      let isInputGeneric = foldl' (\acc x -> acc || (x `isInfixOf` inputType && not ("Either" `isInfixOf` outputType))) False ["Text","String","Bool","Int"]
          isOutputGeneric = foldl' (\acc x -> acc || (x `isInfixOf` outputType && not ("Either" `isInfixOf` outputType))) False ["Text","String","Bool","Int"]
          k = if isInputGeneric && isOutputGeneric 
                then toJSON $ AppOnField functionName outputType fieldName inputType
                else toJSON $ AppOnField functionName outputType "fieldName" inputType
      in pack $ show (hash (toStrict $ encode k) :: Digest  SHA3_256)
    generateHash strs = pack $ show (hash (toStrict $ encode (toJSON strs)) :: Digest  SHA3_256)

data LExpr
  = LVar   Text Text Text Bool Bool
  | LLit   Text Text Bool
  | LType  Text
  | LApp   LExpr LExpr Text Text
  | LLam   Text LExpr
  | LLet   LBind LExpr
  | LCase  Text (Maybe CaseExtract) LExpr Text Text Text [LAlt]
  | LUnhandled Text Text
  deriving (Generic,Data,Show,ToJSON,FromJSON)

type LAlt = (LAltCon, [LExpr], LExpr)

data LAltCon
  = LDataAlt Text
  | LLitAlt  Text
  | LDEFAULT
  deriving (Generic, Eq, Data,Show,ToJSON,FromJSON)

extractNameFromLAltCon :: LAltCon -> Text
extractNameFromLAltCon (LDataAlt name) = name
extractNameFromLAltCon (LLitAlt name) = name
extractNameFromLAltCon LDEFAULT = "DEFAULT"

bindToJSON :: CoreBind -> [(Text, Value)]
bindToJSON (NonRec binder ((Lam _ expr))) =
  [(pack $ nameStableString (idName binder), toJSON expr)]
bindToJSON (NonRec binder expr) =
  [(pack $ nameStableString (idName binder), toJSON expr)]
bindToJSON (Rec binds) =
  map (\(b, e) -> (pack $ nameStableString (idName b), toJSON e)) binds

data Config = Config {
  functions :: [String]
  , dumpDir :: String
  , endpoints :: [String]
  , cleanUp :: Bool
  , graphDBEnabled :: Bool
  , shouldLog :: Bool
  , diff :: Bool
}
  deriving (Generic,Show,ToJSON,FromJSON)

type Edge = (Text,Bool,Text)
data NodeTypes = NFunction | NStartFunction | NEnd | NError
  deriving (Eq)
type Relation = Text
instance Show NodeTypes where
  show NFunction = "Function"
  show NStartFunction = "StartFunction"
  show NEnd = "END"
  show NError = "NError"

type CaseIDName = Text
type CaseCache = HM.HashMap CaseIDName (HM.HashMap Relation [Function])


data FunctionModified =
  FunctionModified  {
    moduleName :: Text
    , functions' :: [Text]
    }
  deriving (Generic,Show,ToJSON)

data EndPointsModified =
  EndPointsModified {
    moduleName' :: Text
    , apiHandler :: Text
    , functionsModified :: [FunctionModified]
  }
  deriving (Generic,Show,ToJSON)

instance FromJSON EndPointsModified where
  parseJSON = withObject "EndPointsModified" $ \o -> do
    moduleName' <- (o .: "moduleName")
    apiHandler <- (o .: "apiHandler")
    functionsModified <- (o .: "functionsModified")
    pure EndPointsModified {..}

instance FromJSON FunctionModified where
  parseJSON = withObject "EndPointsModified" $ \o -> do
    moduleName <- (o .: "moduleName")
    functions' <- (o .: "functions")
    pure FunctionModified {..}