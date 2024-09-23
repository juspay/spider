{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- {-# OPTIONS_GHC -fplugin-opt=Sheriff.Plugin:throwCompilationError=false #-} -- Uncomment to disable compilation error throw

module Test2 where

import           Control.Concurrent (threadDelay)
import qualified Sheriff.Plugin ()
import qualified TestUtils as TU

main :: IO ()
main = do
    print $ TU.throwExceptionV2 "Hello" -- Should not throw Error as per rules
    print $ TU.throwExceptionV4 "Hello" -- Should not throw Error as per rules

-- Infinite recursion, but genuine case; should be ignored to test 
pattern6 :: IO a -> Int -> IO a
pattern6 flow delay = do
  !_ <- flow
  !_ <- threadDelay delay
  pattern6 flow delay
