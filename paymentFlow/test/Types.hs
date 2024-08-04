{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
  
import Data.Aeson
import Data.Text
import Control.Lens

data TxnDetail = TxnDetail

data MerchantAccount = MerchantAccount {
  merchantId :: Maybe Text,
  shouldAddSurcharge :: Bool,
  -- showSurchargeBreakupScreen :: Bool
  _showSurchargeBreakupScreen :: Bool
}

data AK = Skip Bool | Force

data MerchantConfigStatus = PaymentFlowNotEligible | Disabled | Enabled

makeLenses ''MerchantAccount