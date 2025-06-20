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
{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable,DerivingVia #-}

module Main (main) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson.KeyMap
import Data.Data
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import  Control.Newtype ( Newtype(..), over, pack, unpack )
import qualified Data.Aeson as A

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

data B = B
 { id'''                :: Maybe Text}
  deriving (Show, Eq, Generic)

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


deriving instance Data RefundAttempt'
deriving instance ToJSON RefundAttempt'
deriving instance FromJSON RefundAttempt'

deriving instance ToJSON B
deriving instance FromJSON B

data A = A
 { id''''                :: Maybe Text}
  deriving (Show, Eq, Generic)

deriving instance ToJSON A
deriving instance FromJSON A

newtype IgnoredInJson a = IgnoredInJson { value :: a }

instance FromJSON (IgnoredInJson a) where
  parseJSON _ = mempty

instance ToJSON (IgnoredInJson a) where
  toJSON _ = Null

data NonEmpty a = NonEmpty a
  deriving stock (Show)
instance FromJSON (NonEmpty a) where
  parseJSON _ = mempty

instance ToJSON (NonEmpty a) where
  toJSON _ = Null

newtype Stringly a = Stringly
    { unStringly :: a }
    deriving ( ToJSON
             ) via a
instance FromJSON a => FromJSON (Stringly a) where
    parseJSON v = mempty
newtype OneOrMany a = OneOrMany
    { unOneOrMany :: NonEmpty a }
    deriving ( ToJSON
             ) via (NonEmpty a)
instance FromJSON a => FromJSON (OneOrMany a) where
    parseJSON _ = mempty

newtype NormalizeKeys a = NormalizeKeys
    { unwrapNormalizeKeys :: a }
    deriving ( ToJSON
             ) via a
instance FromJSON a => FromJSON (NormalizeKeys a) where
    parseJSON _ = mempty
  
data CacheModelsMap a = CacheModelsMap {keys :: KM.KeyMap Int, values :: [a]}
  deriving anyclass (Newtype (CacheModelsMap a))

instance FromJSON a => FromJSON (CacheModelsMap a) where
  parseJSON _ = mempty

instance ToJSON a => ToJSON (CacheModelsMap a) where
  toJSON _ = Null

data DCacheModelsMap a = DCacheModelsMap {keyss :: KM.KeyMap Int, valuess :: [a]}
  deriving anyclass (Newtype (DCacheModelsMap a))

instance A.FromJSON a => FromJSON (DCacheModelsMap a) where
  parseJSON _ = mempty

instance A.ToJSON a => ToJSON (DCacheModelsMap a) where
  toJSON _ = Null

newtype ViaGenericEncodeDecode a = ViaGenericEncodeDecode a

instance (A.FromJSON a) => A.FromJSON (ViaGenericEncodeDecode a) where
  parseJSON a = mempty

instance (A.ToJSON a) => A.ToJSON (ViaGenericEncodeDecode a) where
  toJSON (ViaGenericEncodeDecode a) = Null