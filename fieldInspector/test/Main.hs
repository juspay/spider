{-# LANGUAGE DeriveAnyClass,UndecidableInstances,ConstraintKinds,MultiParamTypeClasses,FlexibleInstances,ExistentialQuantification,TypeFamilies,NamedFieldPuns,DataKinds,ViewPatterns,DerivingStrategies,FlexibleContexts, KindSignatures #-}
-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin -fplugin=Data.Record.Anon.Plugin -fplugin=Data.Record.Plugin.HasFieldPattern #-}


module Main  where
import Data.Record.Plugin.Options (LargeRecordOptions (LargeRecordOptions), largeRecord)
import GHC.Generics (Generic)
import  Control.Newtype ( Newtype(..), over, pack, unpack )

main :: IO ()
main = do 
    print $ demo $ (A "Test suite not yet implemented." 0)
    pure ()

-- {-# ANN type A largeRecord #-}
-- {-# ANN type B largeRecord #-}

data A = A {name :: String,age :: Int} 
    deriving anyclass (Newtype (A))

data B = AA A | BB String 
    deriving anyclass (Newtype (B))

newtype D a = C a

demo :: A -> String
demo a = 
    case a of
        (A {name}) -> name 