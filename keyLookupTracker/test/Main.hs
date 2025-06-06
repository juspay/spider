-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-tc-ast #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text as T
import Prelude
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = putStrLn "Test suite not yet implemented."

getLookupFlows :: Maybe Text
getLookupFlows = 
  let (hm :: HM.HashMap Text Text) = HM.insert "EF" "efg" $ HM.insert "DE" "def" $  HM.insert "CD" "cde" $ HM.insert "BC" "bcd" $ HM.insert "AB" "abc" HM.empty
      _ = HM.lookup "AB" hm
      nameInFix = "BC" `HM.lookup` hm
      _ = ak hm
      _ = Prelude.map ("CD" `HM.lookup` ) [hm]
      _ = getKeyFromHM "EF" hm
  in nameInFix

  where
    ak hm = (HM.lookup "DE") <$> [hm]

getKeyFromHM :: Text -> HM.HashMap Text Text -> Maybe Text
getKeyFromHM key hm = HM.lookup key hm
