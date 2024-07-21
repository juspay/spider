-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Types1 where
  
import Data.Aeson
import Control.Lens

data SurchargeConfig = SurchargeConfig 
  {shouldAddSurchargeToRefund :: Bool,  showSurchargeBreakupScreen :: Maybe Bool}
  deriving (Show, Eq)  


