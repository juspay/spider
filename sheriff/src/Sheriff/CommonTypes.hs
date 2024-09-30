{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sheriff.CommonTypes where

import Data.Hashable
import GHC hiding (exprType)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins hiding ((<>), getHscEnv)
import GHC.Tc.Types
#else
import GhcPlugins hiding ((<>), getHscEnv)
import TcRnMonad
#endif

data PluginCommonOpts a = PluginCommonOpts {
    currentModule :: String,
    pluginOpts    :: a
  }
  deriving (Show, Eq)

type (HasPluginOpts a) = ?pluginOpts :: (PluginCommonOpts a)

-- Recursive data type for simpler type representation
data TypeData = TextTy String | NestedTy [TypeData]
  deriving (Show, Eq)

data SimpleTcExpr = 
    SimpleVar Var
  | SimpleFnNameVar Var -- Just for lenient checking
  | SimpleList [SimpleTcExpr]
  | SimpleAliasPat SimpleTcExpr SimpleTcExpr
  | SimpleTuple [SimpleTcExpr]
  | SimpleDataCon (Maybe Var) [SimpleTcExpr]
  | SimpleLit (HsLit GhcTc)
  | SimpleOverloadedLit OverLitVal
  | SimpleUnhandledTcExpr

instance Outputable SimpleTcExpr where
  ppr simpleTcExpr = case simpleTcExpr of
    SimpleVar v -> "SimpleVar " $$ ppr v
    SimpleFnNameVar v -> "SimpleFnNameVar " $$ ppr v
    SimpleList ls -> "SimpleList " $$ ppr ls
    SimpleAliasPat p1 p2 -> "SimpleAliasPat "
    SimpleTuple ls -> "SimpleTuple " $$ ppr ls
    SimpleDataCon mbCon ls -> "SimpleDataCon " $$ ppr mbCon $$ ppr ls
    SimpleLit lit -> "SimpleLit " $$ ppr lit
    SimpleOverloadedLit overloadedLit -> "SimpleOverloadedLit " $$ ppr overloadedLit
    SimpleUnhandledTcExpr -> "SimpleUnhandledTcExpr"

instance Eq SimpleTcExpr where
  (==) (SimpleAliasPat pat11 pat12) (SimpleAliasPat pat21 pat22) = pat11 == pat12 || pat12 == pat22 || pat11 == pat22 || pat12 == pat21
  (==) (SimpleAliasPat pat1 pat2)   pat                          = pat1 == pat || pat2 == pat
  (==) pat                          (SimpleAliasPat pat1 pat2)   = pat1 == pat || pat2 == pat
  (==) (SimpleVar var1)             (SimpleVar var2)             = var1 == var2
  (==) (SimpleFnNameVar var1)       (SimpleFnNameVar var2)       = nameOccName (varName var1) == nameOccName (varName var2)
  (==) (SimpleList pat1)            (SimpleList pat2)            = pat1 == pat2
  (==) (SimpleTuple pat1)           (SimpleTuple pat2)           = pat1 == pat2
  (==) (SimpleDataCon mbVar1 pat1)  (SimpleDataCon mbVar2 pat2)  = mbVar1 == mbVar2 && pat1 == pat2
  (==) (SimpleLit lit1)             (SimpleLit lit2)             = lit1 == lit2
  (==) (SimpleOverloadedLit lit1)   (SimpleOverloadedLit lit2)   = lit1 == lit2
  (==) (SimpleUnhandledTcExpr)      (SimpleUnhandledTcExpr)      = False
  (==) _                            _                            = False

-- Data type to represent asterisk matching
data AsteriskMatching = AsteriskInFirst | AsteriskInSecond | AsteriskInBoth
  deriving (Show, Eq)

-- Type family and GADT for generic phase related stuff
#if __GLASGOW_HASKELL__ >= 900
type family PassMonad (p :: Pass) a
type instance PassMonad 'Parsed a = Hsc a
type instance PassMonad 'Renamed a = TcRn a
type instance PassMonad 'Typechecked a = TcM a
#else
data MyGhcPass (p :: Pass) where
  GhcPs :: MyGhcPass 'Parsed 
  GhcRn :: MyGhcPass 'Renamed
  GhcTc :: MyGhcPass 'Typechecked

class IsPass (p :: Pass) where
  ghcPass :: MyGhcPass p

instance IsPass 'Parsed where
  ghcPass = GhcPs

instance IsPass 'Renamed where
  ghcPass = GhcRn

instance IsPass 'Typechecked where
  ghcPass = GhcTc

type family PassMonad (p :: Pass) a
type instance PassMonad 'Parsed a = Hsc a
type instance PassMonad 'Renamed a = TcRn a
type instance PassMonad 'Typechecked a = TcM a
#endif

instance Hashable (Located Var) where
  hashWithSalt salt (L srcSpan var) = hashWithSalt salt $ show srcSpan <> "::" <> (nameStableString . getName $ var) 