{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable #-}

module Main (main) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson.KeyMap
import Data.Data


main :: IO ()
main = putStrLn "Test suite not yet implemented."

data RefundAttempt' = RefundAttempt'
 { id''                :: Maybe Text
 , created''           :: Text
 , ref''               :: Maybe Text
 , error_message''     :: Maybe Text
 , error_code''        :: Maybe Value
 , last_modified''     :: Maybe Text
 }
 deriving (Show, Eq, Generic)

deriving instance Data RefundAttempt'
deriving instance ToJSON RefundAttempt'
deriving instance FromJSON RefundAttempt'

data RefundAttempt = RefundAttempt
 { id'                :: Maybe Text
 , created           :: Text
 , ref               :: Maybe Text
 , error_message     :: Maybe Text
 , error_code        :: Maybe Value
 , last_modified     :: Maybe Text
 }
 deriving (Show, Eq, Generic)

deriving instance Data RefundAttempt

instance ToJSON RefundAttempt where
--  toJSON RefundAttempt{..} = Data.Aeson.object $ mconcat [[
--      "id'"      .= id',
--      "created" .= created
--    ]
--    , toV "ref" ref
--    , toV "error_message" error_message
--    , toV "error_code" error_code
--    , toV "last_modified" last_modified
--    ]
--    where
--      toV _ Nothing  = []
--      toV t (Just v) = [t .= v]

 toEncoding RefundAttempt{..} = Data.Aeson.pairs $ mconcat [
     "id'"      .= id',
     "created" .= created
   , toE "ref" ref
   , toE "error_message" error_message
   , toE "error_code" error_code
   , toE "last_modified" last_modified
   ]
   where
     toE _ Nothing  = mempty
     toE t (Just v) = t .= v

instance FromJSON RefundAttempt where
 parseJSON = withObject "Refund'" $ \o -> do
   id'            <- o .:? "id'"
   created       <- o .: "created"
   ref           <- o .:? "ref"
   error_message <- o .:? "error_message"
   error_code    <- o .:? "error_code"
   last_modified <- o .:? "last_modified"
   pure $ RefundAttempt {..}

