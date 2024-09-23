{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test2 where

import qualified Sheriff.Plugin ()
import qualified TestUtils as TU

main :: IO ()
main = do
    print $ TU.throwExceptionV2 "Hello" -- Should not throw Error as per rules
    print $ TU.throwExceptionV4 "Hello" -- Should not throw Error as per rules