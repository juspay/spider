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

data Rules =
  Rules
    { additionalEligibleLookupFns :: [String]
    -- | Position of the key argument for lookup functions whose key is not the
    -- first argument. Optional; anything unlisted uses 0, which is correct for
    -- @Data.Map.lookup@ and @Data.HashMap.Strict.lookup@.
    , lookupKeyArgIndexes :: [(String, Int)]
    } deriving (Show, Eq)

instance FromJSON Rules where
  parseJSON = withObject "Rule" $ \o -> do
    eligibleLookupFns <- o .: "eligible_lookup_fns"
    keyArgIndexes <- o .:? "lookup_key_arg_indexes" .!= []
    return Rules
      { additionalEligibleLookupFns = eligibleLookupFns
      , lookupKeyArgIndexes = map (\(KeyArgIndex fn ix) -> (fn, ix)) keyArgIndexes
      }

-- | @- fn: lookupWithDefault@ / @  index: 1@ in the rules file.
data KeyArgIndex = KeyArgIndex String Int
  deriving (Show, Eq)

instance FromJSON KeyArgIndex where
  parseJSON = withObject "KeyArgIndex" $ \o ->
    KeyArgIndex <$> o .: "fn" <*> o .: "index"

newtype KeyLookupRules = PFRules
  { rules :: Rules
  } deriving (Show, Eq)

instance FromJSON KeyLookupRules where
  parseJSON = withObject "KeyLookupRules" $ \o -> do
    rules <- o .: "rules"
    return PFRules { rules = rules } 