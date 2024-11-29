{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Text
import Servant

main :: IO ()
main = putStrLn "Test suite not yet implemented."

-- Sample data types
data User = User
  { userId :: Int
  , userName :: Text
  }

data Product = Product
  { productId :: Int
  , productName :: Text
  , productPrice :: Double
  }

-- API definition
type SampleAPI = "api" :> (
       "users" :> UsersAPI
  :<|> "products" :> ProductsAPI
  :<|> "health" :> Get '[JSON] Text
  )

type UsersAPI =
       Get '[JSON] [User]
  :<|> Capture "userId" Int :> Get '[JSON] User
  :<|> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] User
  :<|> Capture "userId" Int :> Delete '[JSON] NoContent

type ProductsAPI =
       Get '[JSON] [Product]
  :<|> Capture "productId" Int :> Get '[JSON] Product
  :<|> ReqBody '[JSON] Product :> Post '[JSON] Product
  :<|> Capture "productId" Int :> ReqBody '[JSON] Product :> Put '[JSON] Product
  :<|> Capture "productId" Int :> Delete '[JSON] NoContent
  :<|> "search" :> QueryParam "query" Text :> Get '[JSON] [Product]

-- The main API type that your plugin will analyze
type EulerAPI = "v1" :> SampleAPI