{-# LANGUAGE NamedFieldPuns,DataKinds,FlexibleInstances,MultiParamTypeClasses,TypeFamilies,DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass,RecordWildCards #-}
module Main  where

import GHC.Generics (Generic)


main :: IO ()
main = do 
    print $ demo $ (A "Test suite not yet implemented." 0)
    pure ()


data A = A {name :: String,age :: Int} 
    deriving (Generic,Show)

demo :: A -> String
demo a = 
    case a of
        (A {name}) -> name