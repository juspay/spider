module KeyLookupTracker.Types where
  
import Data.Aeson

newtype PluginOpts = PluginOpts {
  rulesConfigPath :: String
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = 
  PluginOpts { 
    rulesConfigPath = ".juspay/keyLookupTrackerRules.yaml"
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    rulesConfigPath <- o .:? "rulesConfigPath" .!= rulesConfigPath defaultPluginOpts
    return PluginOpts {rulesConfigPath = rulesConfigPath}

newtype Rules = 
  Rules
    { additionalEligibleLookupFns :: [String]
    } deriving (Show, Eq)  

instance FromJSON Rules where
  parseJSON = withObject "Rule" $ \o -> do
    eligibleLookupFns <- o .: "eligible_lookup_fns"
    return Rules
      { additionalEligibleLookupFns = eligibleLookupFns
      }

newtype KeyLookupRules = PFRules
  { rules :: Rules
  } deriving (Show, Eq)

instance FromJSON KeyLookupRules where
  parseJSON = withObject "KeyLookupRules" $ \o -> do
    rules <- o .: "rules"
    return PFRules { rules = rules } 