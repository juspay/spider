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

#if __GLASGOW_HASKELL__ >= 900
pattern PatFunTy :: AnonArgFlag -> Type -> Type -> Type
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