-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Types1 where
  
import Data.Aeson
import Control.Lens
-- The pinned record-dot-preprocessor rev no longer auto-injects these;
-- the generated HasField instances reference them qualified.
import qualified GHC.Records
import qualified GHC.Records.Extra

data SurchargeConfig = SurchargeConfig 
  {shouldAddSurchargeToRefund :: Bool,  showSurchargeBreakupScreen :: Maybe Bool}
  deriving (Show, Eq)  


