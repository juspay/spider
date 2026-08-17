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

#if __GLASGOW_HASKELL__ >= 904
-- GHC 9.4 removed the `HsConLikeOut` constructor; the typechecked conlike is now
-- carried by the `ConLikeTc` extension constructor.
import GHC.Hs.Expr (XXExprGhcTc(..))
import GHC.Core.ConLike (ConLike)
import GHC.Types.Var (Var)
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

-- `HsPar`/`HsAppType` gained token fields in GHC 9.4; expose version-stable
-- patterns that bind only the meaningful sub-terms so call sites stay identical.
#if __GLASGOW_HASKELL__ >= 904
pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ _ e _

pattern PatHsAppType :: LHsExpr (GhcPass p) -> LHsWcType (NoGhcTc (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsAppType e t <- HsAppType _ e _ t

-- Restores the `HsConLikeOut ext con` shape removed in GHC 9.4; the first field
-- binds the (unused) coercion type variables.
pattern HsConLikeOut :: [Var] -> ConLike -> HsExpr GhcTc
pattern HsConLikeOut tvs con <- XExpr (ConLikeTc con tvs _)
#else
pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ e

pattern PatHsAppType :: LHsExpr (GhcPass p) -> LHsWcType (NoGhcTc (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsAppType e t <- HsAppType _ e t
#endif
