{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Sheriff.Utils where

import Sheriff.Patterns
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import Data.Maybe (maybe)
import qualified Data.Text as T
import Data.Yaml
import GHC hiding (exprType)
import GHC.Hs.Dump
import GHC.Hs.Extension
import Language.Haskell.GHC.ExactPrint (exactPrint)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.TyCo.Rep
import GHC.Data.IOEnv
import GHC.Driver.Main
import GHC.HsToCore.Expr
import GHC.HsToCore.Monad
import GHC.Plugins hiding ((<>), getHscEnv)
import GHC.Tc.Gen.Expr
import GHC.Tc.Module
import GHC.Tc.Types
import GHC.Tc.Utils.TcType
import Language.Haskell.GHC.ExactPrint (ExactPrint)
#else
import DsMonad
import DsExpr
import GhcPlugins hiding ((<>), getHscEnv)
import HscMain
import Language.Haskell.GHC.ExactPrint.Annotater (Annotate)
import TcExpr
import TcRnDriver
import TcRnMonad
import TcRnTypes
import TcType
import TyCoRep
#endif

{-
  These are the common utility functions which can be used for building any plugin of any sort
  Mainly it has generic functions for all - parse, rename and typecheck plugin.
-}

-- Debug Show any haskell internal representation type
showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

-- Pretty print haskell internal representation types using `exactprint`
#if __GLASGOW_HASKELL__ >= 900
showPrettyPrinted :: (ExactPrint a) => Located a -> String
showPrettyPrinted = exactPrint

showAst :: Data a => a -> String
showAst = showSDocUnsafe . showAstData BlankSrcSpan BlankEpAnnotations

noExtFieldOrAnn :: EpAnn a
noExtFieldOrAnn = noAnn

getLoc2 :: GenLocated (SrcSpanAnn' a) e -> SrcSpan
getLoc2 = getLocA

noExprLoc :: a -> Located a
noExprLoc = noLoc

getLocated :: GenLocated (SrcSpanAnn' a) e -> Located e
getLocated ap = L (getLocA ap) (unLoc ap)
#else 
showPrettyPrinted :: (Annotate a) => Located a -> String
showPrettyPrinted = flip exactPrint mempty

showAst :: Data a => a -> String
showAst = showSDocUnsafe . showAstData BlankSrcSpan

noExtFieldOrAnn :: NoExtField
noExtFieldOrAnn = noExtField

getLoc2 :: HasSrcSpan a => a -> SrcSpan
getLoc2 = getLoc

noExprLoc :: (HasSrcSpan a) => SrcSpanLess a -> a
noExprLoc = noLoc

getLocated :: (HasSrcSpan a) => a -> Located (SrcSpanLess a)
getLocated ap = L (getLoc ap) (unLoc ap)
#endif

-- Debug print the Type represented in Haskell
debugPrintType :: Type -> String
debugPrintType (TyVarTy v) = "(TyVar " <> showS v <> ")"
debugPrintType (AppTy ty1 ty2) = "(AppTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
debugPrintType (TyConApp tycon tys) = "(TyCon (" <> showS tycon <> ") [" <> foldr (\x r -> debugPrintType x <> ", " <> r) "" tys <> "]"
debugPrintType (ForAllTy _ ty) = "(ForAllTy " <> debugPrintType ty <> ")"
debugPrintType (PatFunTy ty1 ty2) = "(FunTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
debugPrintType (LitTy litTy) = "(LitTy " <> showS litTy <> ")"
debugPrintType _ = ""

-- Get final return type of any type/function signature
getReturnType :: Type -> [Type]
getReturnType typ 
  | isFunTy typ = getReturnType $ tcFunResultTy typ
  | otherwise = let (x, y) = tcSplitAppTys typ in x : y

-- Get HsLit literal type
-- Similar to GHC library's `hsLitType` function
getLitType :: HsLit (GhcPass p) -> [Type]
getLitType (HsChar _ _) = [charTy]
getLitType (HsCharPrim _ _) = [charTy]
getLitType (HsString _ _) = [stringTy]
getLitType (HsStringPrim _ _) = [stringTy]
getLitType (HsInt _ _) = [intTy]
getLitType (HsIntPrim _ _) = [intTy]
getLitType (HsWordPrim _ _) = [wordTy]
getLitType (HsInt64Prim _ _) = [intTy]
getLitType (HsWord64Prim _ _) = [wordTy]
getLitType (HsInteger _ _ _) = [intTy]
getLitType (HsRat _ _ _) = [doubleTy]
getLitType (HsFloatPrim _ _) = [floatTy]
getLitType (HsDoublePrim _ _) = [doubleTy]
#if __GLASGOW_HASKELL__ < 900
getLitType _ = []
#endif

-- Check if 1st array has any element in 2nd array
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _          = False             -- An empty search list: always false
hasAny _ []          = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

-- Check if a Var is fun type
isFunVar :: Var -> Bool
isFunVar = isFunTy . dropForAlls . idType 

-- Check if a Type is Enum type
isEnumType :: Type -> Bool
isEnumType (TyConApp tyCon _) = isEnumerationTyCon tyCon
isEnumType _ = False

-- Pretty print the Internal Representations
showOutputable :: (MonadIO m, Outputable a) => a -> m ()
showOutputable = liftIO . putStrLn . showS

-- Print the AST
printAst :: (MonadIO m, Data a) => a -> m ()
printAst = liftIO . putStrLn . showAst

-- Parse the YAML file
parseYAMLFile :: (FromJSON a) => FilePath -> IO (Either ParseException a)
parseYAMLFile file = decodeFileEither file

-- get RealSrcSpan from SrcSpanAnn
extractRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
extractRealSrcSpan srcSpan = case srcSpan of
#if __GLASGOW_HASKELL__ >= 900
  RealSrcSpan span _ -> Just span
  _ -> Nothing
#else
  RealSrcSpan span -> Just span
  _ -> Nothing
#endif

-- Function to extract the code segment based on SrcSpan
extractSrcSpanSegment :: SrcSpan -> FilePath -> String -> IO String
extractSrcSpanSegment srcSpan filePath oldCode = case extractRealSrcSpan srcSpan of
  Just span -> do
    content' <- try (readFile filePath) :: IO (Either SomeException String)
    case content' of 
      Left _ -> pure oldCode
      Right content -> do
        let fileLines = T.lines (T.pack content)
            startLine = srcSpanStartLine span
            endLine = srcSpanEndLine span
            startCol = srcSpanStartCol span
            endCol = srcSpanEndCol span

            -- Extract relevant lines
            relevantLines = take (endLine - startLine + 1) $ drop (startLine - 1) fileLines
            -- Handle single-line and multi-line spans
            result = case relevantLines of
                        [] -> ""
                        [singleLine] -> T.take (endCol - startCol) $ T.drop (startCol - 1) singleLine
                        _ -> T.unlines $ [T.drop (startCol - 1) (head relevantLines)] ++
                                        (init (tail relevantLines)) ++
                                        [T.take endCol (last relevantLines)]
        pure $ T.unpack result
  _ -> pure oldCode

-- Get all nodes with given type `b` starting from `a` (Alternative to `biplateRef`)
traverseAst :: (Data from, Data to) => from -> [to]
traverseAst node = traverseAstConditionally node (const False)

-- Get all nodes with given type `b` starting from `a` (Alternative to `biplateRef` but with more granular control using a predicate)
traverseAstConditionally :: (Data from, Data to) => from -> (to -> Bool) -> [to]
traverseAstConditionally node pred = 
  let firstLevel = childrenBi node
  in traverseConditionalUni pred firstLevel

-- Takes a predicate which return true if further expansion is not required while traversing AST, false otherwise
traverseConditionalUni :: (Data to) => (to -> Bool) -> [to] -> [to]
traverseConditionalUni _ [] = []
traverseConditionalUni p (x : xs) = 
  if p x 
    then x : traverseConditionalUni p xs
    else (x : traverseConditionalUni p (children x)) <> traverseConditionalUni p xs

-- Get type for a LHsExpr GhcTc
getHsExprType :: Bool -> LHsExpr GhcTc -> TcM Type
getHsExprType logTypeDebugging expr = do
  coreExpr <- initDsTc $ dsLExpr expr
  let typ = exprType coreExpr
  when logTypeDebugging $ liftIO . print $ "DebugType = " <> (debugPrintType typ)
  pure typ

-- Get type for a LHsExpr GhcTc with resolving type aliases to `data` or `newtype`
getHsExprTypeWithResolver :: Bool -> LHsExpr GhcTc -> TcM Type
getHsExprTypeWithResolver logTypeDebugging expr = deNoteType <$> getHsExprType logTypeDebugging expr

-- Get name of the variable
getVarName :: IdP GhcTc -> String
getVarName var = occNameString . occName $ var

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

-- Generic function to get type for a LHsExpr (GhcPass p) at any compilation phase p
getHsExprTypeGeneric :: forall p m. (IsPass p) => Bool -> LHsExpr (GhcPass p) -> PassMonad p (Maybe Type)
getHsExprTypeGeneric logTypeDebugging expr = case ghcPass @p of
    GhcPs -> do 
      e <- getHscEnv
      (_, mbType) <- liftIO $ tcRnExpr e TM_Inst expr
      when logTypeDebugging $ liftIO . print $ "DebugType = " <> (maybe "Type can not be decoded" debugPrintType mbType)
      pure mbType
    GhcRn -> do
      e <- getEnv
      (_, typ) <- liftIO $ runIOEnv e $ tcInferRho expr
      when logTypeDebugging $ liftIO . print $ "DebugType = " <> (debugPrintType typ)
      pure (Just typ)
    GhcTc -> do
      e <- getEnv
      typ <- liftIO $ runIOEnv e $ exprType <$> initDsTc (dsLExpr expr)
      when logTypeDebugging $ liftIO . print $ "DebugType = " <> (debugPrintType typ)
      pure (Just typ)