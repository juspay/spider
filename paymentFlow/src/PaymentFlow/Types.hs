module PaymentFlow.Types where
  
import Data.Aeson

#if __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc
#else
import SrcLoc 
#endif

data PluginOpts = PluginOpts {
    failOnFileNotFound :: Bool,
    rulesConfigPath :: String
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = 
  PluginOpts { 
    failOnFileNotFound = True, 
    rulesConfigPath = ".juspay/paymentFlowRules.yaml"
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    rulesConfigPath <- o .:? "rulesConfigPath" .!= (rulesConfigPath defaultPluginOpts)
    return PluginOpts {rulesConfigPath = rulesConfigPath, failOnFileNotFound = failOnFileNotFound }

type Suggestion = String

data Rule = 
  Rule
    { type_name                    :: String
    , field_access_whitelisted_fns :: [String]
    , blocked_field                :: String
    , field_rule_fixes             :: Suggestion
    , whitelisted_line_nos         :: [Int]
    -- | Opt-in value-flow check: functions the /value/ of @blocked_field@ must
    -- never reach, however many hops away. Empty (the default) keeps the rule
    -- exactly as it was: a check on where the field is read.
    , blocked_sinks                :: [String]
    -- | Functions that make the value safe (masking, hashing). A flow that
    -- passes through one of these is not reported.
    , sink_sanitizers              :: [String]
    } deriving (Show, Eq)  

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \o -> do
    type_name <- o .: "type_name"
    field_access_whitelisted_fns <- o .: "field_access_whitelisted_fns"
    blocked_field <- o .: "blocked_field"
    field_rule_fixes <- o .: "field_rule_fixes"
    whitelisted_line_nos <- o .: "whitelisted_line_nos"
    blocked_sinks <- o .:? "blocked_sinks" .!= []
    sink_sanitizers <- o .:? "sink_sanitizers" .!= []
    return Rule
      { type_name = type_name
      , field_access_whitelisted_fns = field_access_whitelisted_fns
      , blocked_field = blocked_field
      , field_rule_fixes = field_rule_fixes
      , whitelisted_line_nos = whitelisted_line_nos
      , blocked_sinks = blocked_sinks
      , sink_sanitizers = sink_sanitizers
      }

data PFRules = PFRules
  { rules :: [Rule]
  } deriving (Show, Eq)

instance FromJSON PFRules where
  parseJSON = withObject "PFRules" $ \o -> do
    rules <- o .: "rules"
    return PFRules { rules = rules } 

data VoilationRuleResult = VoilationRuleResult
  { fnName  :: String
  , srcSpan :: SrcSpan
  , rule    :: Rule
  , coreFnName :: String
  } deriving (Show, Eq)