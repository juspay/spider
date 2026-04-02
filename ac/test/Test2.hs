module Test2 (TxnDetail(..), getHelperFunction) where

import Data.Maybe

data TxnDetail = TxnDetail
    { surchargeAmount :: Int
    , taxAmount :: Int
    }
    deriving (Show)

getHelperFunction :: TxnDetail -> Maybe Int
getHelperFunction txnDetail = Just (surchargeAmount txnDetail)