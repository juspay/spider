{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
  
import Data.Aeson
import Data.Text
import Control.Lens
-- The pinned record-dot-preprocessor rev no longer auto-injects these;
-- the generated HasField instances reference them qualified.
import qualified GHC.Records
import qualified GHC.Records.Extra

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