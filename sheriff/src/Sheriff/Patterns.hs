{-# LANGUAGE PatternSynonyms #-}

module Sheriff.Patterns where

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
-- GHC 9.4 removed the `HsConLikeOut` constructor of `HsExpr GhcTc`; the
-- typechecked conlike is now carried by the `ConLikeTc` extension constructor.
import GHC.Hs.Expr (XXExprGhcTc(..))
import GHC.Core.ConLike (ConLike)
import GHC.Types.Var (Var)
#endif

#if __GLASGOW_HASKELL__ >= 900
#if __GLASGOW_HASKELL__ >= 906
pattern PatFunTy :: FunTyFlag -> Type -> Type -> Type
#else
pattern PatFunTy :: AnonArgFlag -> Type -> Type -> Type
#endif
pattern PatFunTy anonArgFlag ty1 ty2 <- (FunTy anonArgFlag _ ty1 ty2)

pattern PatHsIf :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsIf pred thenCl elseCl <- (HsIf _ pred thenCl elseCl)

pattern PatHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsWrap wrapper expr = (XExpr (WrapExpr (HsWrap wrapper expr)))

pattern PatHsExpansion :: HsExpr GhcRn -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsExpansion orig expanded <- (XExpr (ExpansionExpr (HsExpanded orig expanded)))

pattern PatExplicitList :: (XExplicitList (GhcPass p)) -> [LHsExpr (GhcPass p)] -> HsExpr (GhcPass p)
pattern PatExplicitList typ arg = (ExplicitList typ arg)

#else
pattern PatFunTy :: AnonArgFlag -> Type -> Type -> Type
pattern PatFunTy anonArgFlag ty1 ty2 <- (FunTy anonArgFlag ty1 ty2)

pattern PatHsIf :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsIf pred thenCl elseCl <- (HsIf _ _ pred thenCl elseCl)

pattern PatHsWrap :: HsWrapper -> HsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsWrap wrapper expr <- (HsWrap _ wrapper expr) where
        PatHsWrap wrapper expr = (HsWrap NoExtField wrapper expr)

pattern PatExplicitList :: XExplicitList (GhcPass p) -> [LHsExpr (GhcPass p)] -> HsExpr (GhcPass p)
pattern PatExplicitList typ arg <- (ExplicitList typ _ arg) where
        PatExplicitList typ arg = (ExplicitList typ Nothing arg)

#endif

-- `HsPar` gained surrounding token fields in GHC 9.4; expose a version-stable
-- pattern that binds only the inner expression so the call sites stay identical.
#if __GLASGOW_HASKELL__ >= 904
pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ _ e _

-- `HsAppType` gained an `@` token field in GHC 9.4; bind only the inner
-- expression and the type argument so call sites stay version-stable.
pattern PatHsAppType :: LHsExpr (GhcPass p) -> LHsWcType (NoGhcTc (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsAppType e t <- HsAppType _ e _ t

-- Restores the `HsConLikeOut ext con` shape removed in GHC 9.4. The first field
-- is bound to the (unused) coercion type variables so existing `HsConLikeOut _ cl`
-- matches continue to work unchanged. The empty list of scaled types is sufficient
-- for the constructed expression to round-trip through pattern matching; if the
-- caller needs the precise argument types, they were already discarded by the
-- non-bidirectional form as well.
pattern HsConLikeOut :: [Var] -> ConLike -> HsExpr GhcTc
pattern HsConLikeOut tvs con <- XExpr (ConLikeTc con tvs _)
  where HsConLikeOut tvs con = XExpr (ConLikeTc con tvs [])
#else
pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ e

pattern PatHsAppType :: LHsExpr (GhcPass p) -> LHsWcType (NoGhcTc (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsAppType e t <- HsAppType _ e t
#endif
