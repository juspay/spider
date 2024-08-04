module PaymentFlow.Types where
  
import Data.Aeson

#if __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc
#else
import SrcLoc 
#endif

data PluginOpts = PluginOpts {
    saveToFile :: Bool,
    throwCompilationError :: Bool,
    failOnFileNotFound :: Bool,
    savePath :: String,
    rulesConfigPath :: String
  } deriving (Show, Eq)

defaultPluginOpts :: PluginOpts
defaultPluginOpts = 
  PluginOpts { 
    saveToFile = False, 
    throwCompilationError = True, 
    failOnFileNotFound = True, 
    savePath = ".juspay/tmp/paymentFlows/", 
    rulesConfigPath = ".juspay/paymentFlowRules.yaml"
  }

instance FromJSON PluginOpts where
  parseJSON = withObject "PluginOpts" $ \o -> do
    saveToFile <- o .:? "saveToFile" .!= (saveToFile defaultPluginOpts)
    failOnFileNotFound <- o .:? "failOnFileNotFound" .!= (failOnFileNotFound defaultPluginOpts)
    throwCompilationError <- o .:? "throwCompilationError" .!= (throwCompilationError defaultPluginOpts)
    savePath <- o .:? "savePath" .!= (savePath defaultPluginOpts)
    rulesConfigPath <- o .:? "rulesConfigPath" .!= (rulesConfigPath defaultPluginOpts)
    return PluginOpts { saveToFile = saveToFile, throwCompilationError = throwCompilationError,savePath = savePath, rulesConfigPath = rulesConfigPath, failOnFileNotFound = failOnFileNotFound }

type Suggestion = String

data Rule = 
  Rule
    { type_name                    :: String
    , field_access_whitelisted_fns :: [String]
    , blocked_field                :: String
    , field_rule_fixes             :: Suggestion
    , whitelisted_line_nos         :: [Int]
    } deriving (Show, Eq)  

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \o -> do
    type_name <- o .: "type_name"
    field_access_whitelisted_fns <- o .: "field_access_whitelisted_fns"
    blocked_field <- o .: "blocked_field"
    field_rule_fixes <- o .: "field_rule_fixes"
    whitelisted_line_nos <- o .: "whitelisted_line_nos"
    return Rule
      { type_name = type_name
      , field_access_whitelisted_fns = field_access_whitelisted_fns
      , blocked_field = blocked_field
      , field_rule_fixes = field_rule_fixes
      , whitelisted_line_nos = whitelisted_line_nos
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

defaultRule :: [Rule]
defaultRule =
  [ Rule "MerchantAccount" ["isMerchantEnabledForPaymentFlow", "getStartPayPath", "transformECMerchantAccountToEulerMerchantAccount", "surchargeConfigStatusAndValue"] "shouldAddSurcharge" "Direct access of `shouldAddSurcharge` from `MerchantAccount` type is not allowed. Use the `isMerchantEnabledForPaymentFlow` function instead." []
  , Rule "MerchantAccount" ["getMerchantConfigStatusAndvalueForPaymentFlow", "getMerchantConfigStatusAndValueForMAPfs", "transformECMerchantAccountToEulerMerchantAccount", "surchargeConfigStatusAndValue"] "showSurchargeBreakupScreen" "Direct access of `showSurchargeBreakupScreen` from `MerchantAccount` type is not allowed." []
  , Rule "MerchantAccount" ["getMerchantConfigStatusAndValueForMAPfs", "transformECMerchantAccountToEulerMerchantAccount"] "includeSurchargeAmountForRefund" "Direct access of `includeSurchargeAmountForRefund` from `MerchantAccount` type is not allowed." []
  , Rule "MerchantAccount" ["isMerchantEnabledForPaymentFlow", "transformECMerchantAccountToEulerMerchantAccount"] "offerEnabled" "Direct access of `offerEnabled` from `MerchantAccount` type is not allowed. Use `isMerchantEnabledForPaymentFlow` function instead." []
  , Rule "MerchantAccountAuth" ["offerEnableCheck"] "offerEnabled" "Direct access of `offerEnabled` from `MerchantAccount` type is not allowed. Use `isMerchantEnabledForPaymentFlow` function instead." []
  , Rule "MerchantAccount" ["getMerchantConfigStatusAndValueForMAPfs", "transformECMerchantAccountToEulerMerchantAccount"] "autoRefundConflictTransactions" "Direct access of `autoRefundConflictTransactions` from `MerchantAccount` type is not allowed. Use `getMerchantConfigStatusAndValueForMAPfs` function instead." []
  , Rule "MerchantAccount" ["getMerchantConfigStatusAndValueForMAPfs", "transformECMerchantAccountToEulerMerchantAccount"] "autoRefundMultipleChargedTransactions" "Direct access of `autoRefundMultipleChargedTransactions` from `MerchantAccount` type is not allowed. Use `getMerchantConfigStatusAndValueForMAPfs` function instead." []
  , Rule "MerchantAccount" ["getMerchantConfigStatusAndValueForMAPfs", "transformECMerchantAccountToEulerMerchantAccount"] "autoRefundConflictThresholdInMins" "Direct access of `autoRefundConflictThresholdInMins` from `MerchantAccount` type is not allowed. Use `getMerchantConfigStatusAndValueForMAPfs` function instead." []
  , Rule "MerchantAccount" ["isMerchantEnabledForPaymentFlow", "transformECMerchantAccountToEulerMerchantAccount"] "enabledInstantRefund" "Direct access of `enabledInstantRefund` from `MerchantAccount` type is not allowed. Use `isMerchantEnabledForPaymentFlow` function instead." []
  , Rule "MerchantAccount" ["isMerchantEnabledForPaymentFlow", "transformECMerchantAccountToEulerMerchantAccount"] "enableExternalRiskCheck" "Direct access of `enableExternalRiskCheck` from `MerchantAccount` type is not allowed. Use `isMerchantEnabledForPaymentFlow` function instead." []
  ]
