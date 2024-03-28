module Sheriff.Types where
  
import Data.Aeson
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))

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

data Rule = Rule 
  {
    rule_name :: String,
    fn_name :: String,
    arg_no :: ArgNo,
    fns_blocked_in_arg :: FnsBlockedInArg,
    types_blocked_in_arg :: TypesBlockedInArg,
    types_to_check_in_arg :: TypesToCheckInArg
  } deriving (Show, Eq)  

data LocalVar = FnArg Var | FnWhere Var | FnLocal Var
  deriving (Eq)

instance Show LocalVar where
  show (FnArg x)   = "FnArg " Prelude.<> showS x
  show (FnWhere x) = "FnWhere " Prelude.<> showS x
  show (FnLocal x) = "FnLocal " Prelude.<> showS x

data Violation = 
    ArgTypeBlocked String Rule
  | FnBlockedInArg String Rule
  | FnUseBlocked Rule
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show (ArgTypeBlocked typ rule) = "Use of '" <> (fn_name rule) <> "' on '" <> typ <> "' is not allowed."
  show (FnBlockedInArg fnName rule) = "Use of '" <> fnName <> "' inside argument of '" <> (fn_name rule) <> "' is not allowed."
  show (FnUseBlocked rule) = "Use of '" <> (fn_name rule) <> "' in the code is not allowed."
  show NoViolation = "NoViolation"

getViolationType :: Violation -> String
getViolationType v = case v of
  ArgTypeBlocked _ _ -> "ArgTypeBlocked"
  FnBlockedInArg _ _ -> "FnBlockedInArg"
  FnUseBlocked _ -> "FnUseBlocked"
  NoViolation -> "NoViolation"

getRule :: Violation -> Rule
getRule v = case v of
  ArgTypeBlocked _ r -> r
  FnBlockedInArg _ r -> r
  FnUseBlocked r -> r
  NoViolation -> defaultRule

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

defaultRule :: Rule
defaultRule = Rule "NA" "NA" (-1) [] [] []

emptyLoggingError :: CompileError
emptyLoggingError = CompileError "" "" "$NA$" noSrcSpan NoViolation