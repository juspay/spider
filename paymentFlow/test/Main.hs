{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-tc-ast #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Prelude
import Data.Aeson as A
import Types as PT
import Types1 as PT1
import Control.Lens

main :: IO ()
main = putStrLn "Test suite not yet implemented."

decidePayStartPathbySurchargeAmt :: PT.TxnDetail -> Text -> Text -> PT.MerchantAccount -> Text
decidePayStartPathbySurchargeAmt txn defaultStartPayPath payStartPath mAcc = do
  -- let surchargeConfigStatusAndValue = getMerchantConfigStatusAndvalueForPaymentFlow (mAcc ^. PT.showSurchargeBreakupScreen)
  let surchargeConfigStatusAndValue = getMerchantConfigStatus
    --  getMerchantConfigStatusAndvalueForPaymentFlow (getMerchantPIdFromMerchantAccount mAcc) (fromMaybe "" (merchantId mAcc))  (Skip mMCLookupConfig)
      shouldShowSurchargePage = case surchargeConfigStatusAndValue of
        (PT.PaymentFlowNotEligible, _) -> 
          -- (mAcc.shouldAddSurcharge ) && (mAcc.showSurchargeBreakupScreen)
          -- mAcc.shouldAddSurcharge && mAcc.showSurchargeBreakupScreen
          -- mAcc.shouldAddSurcharge && mAcc ^. PT.showSurchargeBreakupScreen
          mAcc ^. PT.showSurchargeBreakupScreen && mAcc.shouldAddSurcharge
          -- (PT.shouldAddSurcharge mAcc) && (PT.showSurchargeBreakupScreen mAcc)
        (PT.Disabled, _) -> False
        (PT.Enabled, surchargeConfigV) ->
          (fromMaybe False $ (surchargeConfigV >>= (\sc -> sc.showSurchargeBreakupScreen)) <|> (Just $ mAcc ^. PT.showSurchargeBreakupScreen))
          -- (fromMaybe False $ (surchargeConfigV >>= (\sc -> PT1.showSurchargeBreakupScreen sc)) <|> (Just (PT.showSurchargeBreakupScreen mAcc)))
  if shouldShowSurchargePage
    then payStartPath
    else defaultStartPayPath
  
  where

    getMerchantConfigStatus :: (PT.MerchantConfigStatus, Maybe PT1.SurchargeConfig)
    getMerchantConfigStatus = 
      -- getMerchantConfigStatusAndvalueForPaymentFlow $ PT.showSurchargeBreakupScreen mAcc
      -- getMerchantConfigStatusAndvalueForPaymentFlow (PT.showSurchargeBreakupScreen mAcc)
      getMerchantConfigStatusAndvalueForPaymentFlow (mAcc ^. PT.showSurchargeBreakupScreen)

getMerchantConfigStatusAndvalueForPaymentFlow ::Bool -> (PT.MerchantConfigStatus, Maybe PT1.SurchargeConfig)
getMerchantConfigStatusAndvalueForPaymentFlow _ = (PT.Enabled, Just $ PT1.SurchargeConfig {shouldAddSurchargeToRefund = False, showSurchargeBreakupScreen = Just True})