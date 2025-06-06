{-# LANGUAGE PatternSynonyms #-}

module PaymentFlow.Patterns where

import GHC hiding (exprType)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.TyCo.Rep
import GHC.Tc.Types.Evidence
import Language.Haskell.Syntax.Expr
#else
import GHC.Hs.Expr
import TcEvidence
import TyCoRep
#endif

#if __GLASGOW_HASKELL__ >= 900

pattern PatHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsWrap wrapper expr <- (XExpr (WrapExpr (HsWrap wrapper expr))) 

pattern PatHsExpansion :: HsExpr GhcRn -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsExpansion orig expanded <- (XExpr (ExpansionExpr (HsExpanded orig expanded)))

#else

pattern PatHsWrap :: HsWrapper -> HsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsWrap wrapper expr <- (HsWrap _ wrapper expr)

#endif