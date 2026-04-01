{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module UnusedFieldChecker.FieldChecker
    ( FieldChecker(..)
    ) where

import Data.Proxy (Proxy(..))

class FieldChecker a where
  excludedFields :: Proxy a -> [String]
  excludedFields _ = []
