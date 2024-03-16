{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass #-}

module Syn2Chart.Types where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import GhcPlugins hiding (TB,(<>))
import qualified Data.Map as Map
import Data.Aeson
import Data.Data
import Data.Text (pack)

data Function = Function String String Bool [Function]
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name _type isCase f') =
        Object $
            HM.fromList [("name",toJSON name),("body",toJSON f'),("type",toJSON _type),("isCase",toJSON isCase)]

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

data LBind = LNonRec String String LExpr
            | LRec [(String, String , LExpr)]
            | LNull
  deriving (Generic,Data,Show,ToJSON,FromJSON)

data LExpr
  = LVar   String String
  | LLit   String
  | LType  String
  | LApp   LExpr LExpr
  | LLam   String LExpr
  | LLet   LBind LExpr
  | LCase  String String String [LAlt]
  | LUnhandled String String
  -- | LCast  (LExpr) Coercion
  -- | LTick  (Tickish Id) (LExpr)
  -- | LCoercion Coercion
  deriving (Generic,Data,Show,ToJSON,FromJSON)

type LAlt = (LAltCon, [LExpr], LExpr)

data LAltCon
  = LDataAlt String
  | LLitAlt  String
  | LDEFAULT
  deriving (Generic, Eq, Data,Show,ToJSON,FromJSON)

extractNameFromLAltCon :: LAltCon -> String
extractNameFromLAltCon (LDataAlt name) = name
extractNameFromLAltCon (LLitAlt name) = name
extractNameFromLAltCon LDEFAULT = "DEFAULT"

bindToJSON :: CoreBind -> [(String, Value)]
bindToJSON (NonRec binder ((Lam _ expr))) =
  [(nameStableString (idName binder), toJSON expr)]
bindToJSON (NonRec binder expr) =
  [(nameStableString (idName binder), toJSON expr)]
bindToJSON (Rec binds) =
  map (\(b, e) -> (nameStableString (idName b), toJSON e)) binds