{-# LANGUAGE OverloadedStrings #-}

-- | Sample module for the tracer.
--
-- Build with @-fvariableTracer:Dev@ (see the cabal file) and the plugin dumps a
-- graph for this module plus traces for @finalAmount@ and @receipt@.
--
-- The interesting bits, in the order the tracer has to handle them:
--
--   * @finalAmount@ is a @let@ binder built from an arithmetic application
--     whose arguments come from a record field, a @case@ and a parameter;
--   * @netAmount@ is a function parameter, so it only resolves once the tracer
--     walks out to the call sites in 'settleOrder' and 'main';
--   * @receipt@ is built by record construction out of a monadic bind and a
--     pattern projection.
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T

data Order = Order
  { orderId :: Text
  , grossAmount :: Int
  , currency :: Text
  , coupon :: Maybe Coupon
  }

data Coupon = Coupon
  { couponCode :: Text
  , discountPct :: Int
  }

data Receipt = Receipt
  { receiptOrder :: Text
  , receiptAmount :: Int
  , receiptNote :: Text
  }
  deriving (Show)

-- | Two layers of arithmetic over a record field, a case and a parameter.
computeFinalAmount :: Order -> Int -> Int
computeFinalAmount order taxPaise =
  let gross = grossAmount order
      discount = case coupon order of
        Just c -> (gross * discountPct c) `div` 100
        Nothing -> 0
      finalAmount = gross - discount + taxPaise
   in finalAmount

-- | @netAmount@ is a parameter: tracing it has to leave this function.
formatAmount :: Text -> Int -> Text
formatAmount currencyCode netAmount =
  currencyCode <> " " <> T.pack (show (netAmount `div` 100))

settleOrder :: Order -> IO Receipt
settleOrder order = do
  taxPaise <- pure (grossAmount order `div` 10)
  let amount = computeFinalAmount order taxPaise
      (label, note) = describe order
      receipt =
        Receipt
          { receiptOrder = orderId order
          , receiptAmount = amount
          , receiptNote = label <> ": " <> note <> " " <> formatAmount (currency order) amount
          }
  pure receipt

-- | A pattern binding, so the tracer has a tuple projection to record.
describe :: Order -> (Text, Text)
describe order = case coupon order of
  Just c -> ("discounted", couponCode c)
  Nothing -> ("full price", "no coupon")

sampleOrder :: Order
sampleOrder =
  Order
    { orderId = "ord_1001"
    , grossAmount = 250000
    , currency = "INR"
    , coupon = Just (Coupon "NEW50" 10)
    }

main :: IO ()
main = do
  receipt <- settleOrder sampleOrder
  print receipt
