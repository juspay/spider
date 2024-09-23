{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Sheriff.CommonTypes where

import GHC hiding (exprType)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins hiding ((<>), getHscEnv)
import GHC.Tc.Types
#else
import GhcPlugins hiding ((<>), getHscEnv)
import TcRnMonad
#endif

-- Recursive data type for simpler type representation
data TypeData = TextTy String | NestedTy [TypeData]
  deriving (Show, Eq)

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