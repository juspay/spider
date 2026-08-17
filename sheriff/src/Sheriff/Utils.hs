{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Sheriff.Utils where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Bool
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import qualified Data.HashMap.Strict as HM
import Data.List.Extra (splitOn, trim, isInfixOf)
import Data.Maybe (maybe, catMaybes, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Yaml
import GHC hiding (exprType)
import GHC.Hs.Dump
import GHC.Hs.Extension
import Language.Haskell.GHC.ExactPrint (exactPrint)
import Sheriff.Patterns
import Sheriff.CommonTypes

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.ConLike
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
import GHC.Types.FieldLabel (flLabel)
import Language.Haskell.GHC.ExactPrint (ExactPrint)
#if __GLASGOW_HASKELL__ >= 906
import GHC.Types.Var (isInvisibleFunArg)
#endif
#else
import ConLike
import DsMonad
import DsExpr
import FieldLabel (flLabel)
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

#if __GLASGOW_HASKELL__ >= 904
-- GHC 9.4 changed `initDsTc :: DsM a -> TcM (Messages DsMessage, Maybe a)`
-- (previously it returned the desugared result directly). For a typechecked
-- expression desugaring always succeeds, so unwrap the `Maybe` to keep the
-- original behaviour of yielding the core expression.
unwrapDsResult :: (msgs, Maybe a) -> a
unwrapDsResult (_, Just x)  = x
unwrapDsResult (_, Nothing) = error "Sheriff: desugaring produced no core expression"
#endif

-- Debug Show any haskell internal representation type
showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

matchLocatedVarNamesWithModuleName :: (HasPluginOpts a) => Located Var -> Located Var -> AsteriskMatching -> Bool
matchLocatedVarNamesWithModuleName v1 v2 asteriskMatching = matchVarNamesWithModuleName (unLoc v1) (unLoc v2) asteriskMatching

matchVarNamesWithModuleName :: (HasPluginOpts a) => Var -> Var -> AsteriskMatching -> Bool
matchVarNamesWithModuleName v1 v2 asteriskMatching = 
  let var1nameWithModule = getVarNameWithModuleName v1
      var2nameWithModule = getVarNameWithModuleName v2
  in matchNamesWithModuleName var1nameWithModule var2nameWithModule asteriskMatching

getLocatedVarNameWithModuleName :: (HasPluginOpts a) => Located Var -> String
getLocatedVarNameWithModuleName lvar = getVarNameWithModuleName $ unLoc lvar

getVarNameWithModuleName :: (HasPluginOpts a) => Var -> String
getVarNameWithModuleName var = getNameWithModuleName $ varName var

getNameWithModuleName :: (HasPluginOpts a) => Name -> String
getNameWithModuleName name = 
  let occName = getOccString name
  in getModuleName name <> "." <> occName

getModuleName :: (HasPluginOpts a) => Name -> String
getModuleName name = 
  case nameModule_maybe name of
    Just modName -> (moduleNameString $ moduleName modName)
    Nothing -> (currentModule ?pluginOpts)

getNameAndModuleNameWithNMV :: (HasPluginOpts a) => Name -> (Name, String)
getNameAndModuleNameWithNMV name = 
  let modNameMap = nameModuleMap ?pluginOpts
  in case getNameAndModuleFromNMV modNameMap (NMV_Name name) of
    (nm, Just modName) -> (nm, modName)
    (nm, Nothing) -> (nm, getModuleName nm)

getNameAndModuleFromNMV :: HM.HashMap NameModuleValue NameModuleValue -> NameModuleValue -> (Name, Maybe String)
getNameAndModuleFromNMV mp nmv = case HM.lookup nmv mp of
  Just val -> getNameAndModuleFromNMV mp val
  Nothing -> case nmv of
    NMV_Name nm -> (nm, Nothing)
    NMV_ClassModule nm modName -> (nm, Just modName)

matchNamesWithModuleName :: String -> String -> AsteriskMatching -> Bool
matchNamesWithModuleName varNameWithModule fnToMatch asteriskMatching = 
  let (varModuleName, varName) = splitAtLastChar '.' varNameWithModule
  in case splitAtLastChar '.' fnToMatch of
      ("", fnName) -> matchNamesWithAsterisk asteriskMatching varName fnName
      (modName, fnName) -> matchNamesWithAsterisk AsteriskInBoth varModuleName modName && matchNamesWithAsterisk asteriskMatching varName fnName
  where
    splitAtLastChar :: Char -> String -> (String, String)
    splitAtLastChar ch str = 
      let (before, after) = break (== ch) (reverse str)
      in (reverse (drop 1 after), reverse before) 

matchNamesWithAsterisk :: AsteriskMatching -> String -> String -> Bool
matchNamesWithAsterisk asteriskMatching str1 str2 = 
  let splitList1 = splitOn "." str1
      splitList2 = splitOn "." str2
  in go "" "" splitList1 splitList2
  where
    checkAsteriskInFirst  = (asteriskMatching == AsteriskInFirst || asteriskMatching == AsteriskInBoth)
    checkAsteriskInSecond = (asteriskMatching == AsteriskInSecond || asteriskMatching == AsteriskInBoth)

    go :: String -> String -> [String] -> [String] -> Bool
    go lastX lastY [] []             = True
    go lastX lastY xs []             = lastY == "*" && checkAsteriskInSecond
    go lastX lastY [] ys             = lastX == "*" && checkAsteriskInFirst
    go lastX lastY (x : xs) (y : ys) = (x == y || checkAsteriskInFirst && x == "*" || y == "*" && checkAsteriskInSecond) && go x y xs ys
      
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

getLocated :: GenLocated (SrcSpanAnn' a) e -> (SrcSpanAnn' b) -> Located e
getLocated ap (SrcSpanAnn _ loc) = L loc (unLoc ap)

mkGenLocated :: a -> SrcSpan -> GenLocated (SrcAnn ann) a
mkGenLocated e srcSpan = L (noAnnSrcSpan srcSpan) e

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

getLocated :: (HasSrcSpan a) => a -> SrcSpan -> Located (SrcSpanLess a)
getLocated ap loc = L loc (unLoc ap)

mkGenLocated :: a -> SrcSpan -> GenLocated SrcSpan a
mkGenLocated e srcSpan = L srcSpan e
#endif

-- Create Located HSExpr for HsVar type
mkLHsVar :: Located Var -> LHsExpr GhcTc
mkLHsVar (L srcSpan e) = mkGenLocated (HsVar noExtField $ mkGenLocated e srcSpan) srcSpan

-- Debug print the Type represented in Haskell
debugPrintType :: Type -> String
debugPrintType (TyVarTy v) = "(TyVarTy " <> showS v <> ")"
debugPrintType (AppTy ty1 ty2) = "(AppTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
debugPrintType (TyConApp tycon tys) = "(TyConApp (" <> showS tycon <> ") [" <> foldr (\x r -> debugPrintType x <> ", " <> r) "" tys <> "]"
debugPrintType (ForAllTy _ ty) = "(ForAllTy " <> debugPrintType ty <> ")"
debugPrintType (PatFunTy _ ty1 ty2) = "(FunTy " <> debugPrintType ty1 <> " " <> debugPrintType ty2 <> ")"
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
#elif __GLASGOW_HASKELL__ >= 904
-- GHC 9.4+ added sized primitive literals (HsInt8Prim/HsWord8Prim/…); they are
-- not types we introspect, so fall through to the empty result as before.
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
#if __GLASGOW_HASKELL__ >= 904
  coreExpr <- unwrapDsResult <$> (initDsTc $ dsLExpr expr)
#else
  coreExpr <- initDsTc $ dsLExpr expr
#endif
  let typ = exprType coreExpr
  when logTypeDebugging $ liftIO . print $ "DebugType = " <> (debugPrintType typ)
  pure typ

-- Get type for a LHsExpr GhcTc with resolving type aliases to `data` or `newtype`
getHsExprTypeWithResolver :: Bool -> LHsExpr GhcTc -> TcM Type
getHsExprTypeWithResolver logTypeDebugging expr = deNoteType <$> getHsExprType logTypeDebugging expr

-- TODO: Add support for matching constraints
-- Get Qualified Types as List
getHsExprTypeAsTypeDataListWithConstraintCheck :: (HasPluginOpts a) => Bool -> Type -> [TypeData]
getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg typ = case typ of
  LitTy ty -> [TextTy $ showS ty]
  TyVarTy var -> [TextTy $ getVarNameWithModuleName var]
  TyConApp tycon tys -> [NestedTy $ [TextTy $ getNameWithModuleName (tyConName tycon)] <> (concat $ fmap (getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg) tys)]
  AppTy ty1 ty2 -> getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty1 <> getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty2
  ForAllTy _ ty -> getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty
  PatFunTy anonArgFlag ty1 ty2 ->
    let isInvis =
#if __GLASGOW_HASKELL__ >= 906
          isInvisibleFunArg anonArgFlag
#else
          anonArgFlag == InvisArg
#endif
    in bool (getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty1 <> getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty2) (getHsExprTypeAsTypeDataListWithConstraintCheck ignoreConstraintArg ty2) (ignoreConstraintArg && isInvis)
  _ -> []

-- Get Qualified Types as List Ignoring constraint checks
getHsExprTypeAsTypeDataList :: (HasPluginOpts a) => Type -> [TypeData]
getHsExprTypeAsTypeDataList = getHsExprTypeAsTypeDataListWithConstraintCheck True

-- Get Qualified Types as List
getHsExprTypeAsTypeDataListKeepConstraints :: (HasPluginOpts a) => Type -> [TypeData]
getHsExprTypeAsTypeDataListKeepConstraints = getHsExprTypeAsTypeDataListWithConstraintCheck False

parseParenData :: String -> ([TypeData], String)
parseParenData [] = ([], [])
parseParenData (x:xs)
    | x == '('  = let (nestedData, rest) = parseParenData xs
                      (remainingData, rest') = parseParenData rest
                  in (NestedTy nestedData : remainingData, rest')
    | x == ')'  = ([], xs)
    | otherwise = let (textData, rest) = parseParenData xs
                  in case textData of
                       (TextTy t : ts) -> if x == ' ' then (TextTy t : ts, rest) else (TextTy (x:t) : ts, rest) -- append char to current text if it is not empty space
                       _ -> if x == ' ' then (textData, rest) else (TextTy [x] : textData, rest)         -- start new text if it is not empty space

-- Top-level function to handle parsing from the root
extractParenData :: String -> [TypeData]
extractParenData str = fst (parseParenData str)

-- Match function signatures
matchFnSignatures :: [TypeData] -> String -> Bool
matchFnSignatures exprSig ruleSig = 
  let splitRuleSig = fmap (NestedTy . extractParenData . trim) $ splitOn "->" ruleSig
  in go exprSig splitRuleSig
  where
    go :: [TypeData] -> [TypeData] -> Bool
    go [] []             = True
    go (x : xs) []       = x == TextTy "*"
    go [] (y : ys)       = y == TextTy "*"
    go (x : xs) (y : ys)
      | x == TextTy "*" = go xs ys
      | y == TextTy "*" = go xs ys
      | otherwise = case (x, y) of
        (TextTy a, TextTy b)     -> matchNamesWithModuleName a b AsteriskInBoth && go xs ys
        (NestedTy a, NestedTy b) -> go a b && go xs ys
        _                        -> False

-- Get name of the variable
getVarName :: IdP GhcTc -> String
getVarName var = occNameString . occName $ var

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
#if __GLASGOW_HASKELL__ >= 904
      typ <- liftIO $ runIOEnv e $ (exprType . unwrapDsResult) <$> initDsTc (dsLExpr expr)
#else
      typ <- liftIO $ runIOEnv e $ exprType <$> initDsTc (dsLExpr expr)
#endif
      when logTypeDebugging $ liftIO . print $ "DebugType = " <> (debugPrintType typ)
      pure (Just typ)

-- Parse a YAML/JSON field that may be written either as a single value or as a list
parseAsListOrSingle :: (FromJSON a) => Value -> Parser [a]
parseAsListOrSingle v = parseJSON v <|> fmap (:[]) (parseJSON v)

parseAsListOrString :: Value -> Parser [String]
parseAsListOrString = parseAsListOrSingle

{-
  Type Containment Check
  ~~~~~~~~~~~~~~~~~~~~~~
  Answers "does this type contain one of the given types anywhere inside it?".

  Unlike `validateType` in the plugin (which compares the pretty printed type
  against a list of names, and only looks inside a hardcoded set of containers),
  this walks the type structurally in two ways:

    1. Type arguments   -- `Maybe PII`, `[PII]`, `(Text, PII)`, `Map Text PII`
    2. Constructor fields -- `data A = A { var1 :: PII }`, and transitively
                             `data B = B { var0 :: A }`

  Termination
  -----------
  Recursion over type arguments is structural (a `Type` is a finite term), so it
  always terminates on its own. Only field expansion can cycle, e.g.
  `data Rec = Rec { self :: Maybe Rec }`, so the visited set guards *field
  expansion only*. Guarding the whole node instead would be wrong: it would make
  `Map Text (Map Text PII)` stop at the inner `Map` and miss the `PII`.

  Unresolvable types fail open
  ----------------------------
  Abstract tycons (constructors not exported / hs-boot), type families, classes
  and primitives are not expanded. A missed detection is preferable to a false
  compile error the developer cannot act on.

  Cost
  ----
  Bounded by the size of the reachable type graph, and only paid at call sites
  whose function name already matched the rule. If this ever shows up in compile
  times, memoise `constructorFieldsOfTyCon` on the tycon's stable name.
-}

-- A readable breadcrumb of how a blocked type is reached from the root type,
-- e.g. ["B", "var0 :: A", "var1 :: PII"]
type TypeContainmentPath = [String]

-- Find a path from the given type down to any one of the blocked types.
-- Returns the blocked type that matched along with the containment path.
findBlockedTypeInType :: (HasPluginOpts a)
  => [String]                                 -- ^ blocked type names (`PII`, `Types.PII`, `Euler.*.PII`)
  -> [String]                                 -- ^ type names to neither match nor look inside
  -> Type                                     -- ^ type to inspect
  -> Maybe (String, TypeContainmentPath)
findBlockedTypeInType blockedTypes ignoredTypes rootTy
  | null blockedTypes = Nothing
  | otherwise         = go mempty [showS rootTy] rootTy
  where
    go :: Set.Set String -> TypeContainmentPath -> Type -> Maybe (String, TypeContainmentPath)
    go visited path ty = case expandTypeSynonyms ty of
      TyConApp tyCon args
        | matchesAnyOf ignoredTypes tyCon           -> Nothing
        | Just blocked <- matchedBlockedType tyCon  -> Just (blocked, path)
        | otherwise                                 -> firstMatch (argHits <> fieldHits)
        where
          argHits = fmap (\argTy -> go visited (path <> [showS argTy]) argTy) args

          -- Only expand a tycon's fields once along a path; see "Termination" above
          fieldHits
            | Set.member tyConKey visited = []
            | otherwise =
                fmap
                  (\(fieldName, fieldTy) -> go visited' (path <> [fieldName <> " :: " <> showS fieldTy]) fieldTy)
                  (constructorFieldsOfTyCon tyCon)

          tyConKey = nameStableString (tyConName tyCon)
          visited' = Set.insert tyConKey visited

      AppTy ty1 ty2    -> firstMatch [go visited (path <> [showS ty1]) ty1, go visited (path <> [showS ty2]) ty2]
      PatFunTy _ argTy resTy -> firstMatch [go visited (path <> [showS argTy]) argTy, go visited (path <> [showS resTy]) resTy]
      ForAllTy _ body  -> go visited path body
      _                -> Nothing -- TyVarTy, LitTy, CastTy, CoercionTy

    matchedBlockedType :: TyCon -> Maybe String
    matchedBlockedType tyCon = findMatchingName blockedTypes tyCon

    matchesAnyOf :: [String] -> TyCon -> Bool
    matchesAnyOf names tyCon = case findMatchingName names tyCon of
      Just _  -> True
      Nothing -> False

    -- Matches against both the plain (`PII`) and module qualified (`Types.PII`)
    -- name, reusing the same matcher the plugin uses for function names, so
    -- asterisk forms like `Euler.*.PII` work too.
    findMatchingName :: [String] -> TyCon -> Maybe String
    findMatchingName names tyCon =
      let tyConNameWithModule = getNameWithModuleName (tyConName tyCon)
      in listToMaybe $ filter (\n -> matchNamesWithModuleName tyConNameWithModule n AsteriskInSecond) names

-- Get the (field name, field type) pairs of every constructor of a tycon.
-- Yields [] for anything we cannot or should not look inside.
constructorFieldsOfTyCon :: TyCon -> [(String, Type)]
constructorFieldsOfTyCon tyCon
  | not (isExpandableTyCon tyCon) = []
  | otherwise = case tyConDataCons_maybe tyCon of
      Nothing       -> [] -- abstract tycon (constructors not exported / hs-boot)
      Just dataCons -> concatMap fieldsOfDataCon dataCons

-- Type families, classes, primitives and abstract tycons have no user fields we can inspect
isExpandableTyCon :: TyCon -> Bool
isExpandableTyCon tyCon =
  not (isFamilyTyCon tyCon || isClassTyCon tyCon || isPrimTyCon tyCon || isAbstractTyCon tyCon)

-- Field names come from the record labels; positional constructors fall back to `Con#1`
fieldsOfDataCon :: DataCon -> [(String, Type)]
fieldsOfDataCon dataCon =
  let argTys = dataConFieldTypes dataCon
      labels = fmap (showS . flLabel) (dataConFieldLabels dataCon)
      names  = if length labels == length argTys
                then labels
                else fmap (\idx -> showS (dataConName dataCon) <> "#" <> show idx) [1 .. length argTys]
  in zip names argTys

dataConFieldTypes :: DataCon -> [Type]
#if __GLASGOW_HASKELL__ >= 900
-- GHC 9.0 made constructor argument types linearity annotated (`Scaled Type`)
dataConFieldTypes = fmap scaledThing . dataConOrigArgTys
#else
dataConFieldTypes = dataConOrigArgTys
#endif

-- First match in a lazily consumed list of results
firstMatch :: [Maybe a] -> Maybe a
firstMatch = listToMaybe . catMaybes

-- Get Var for the data constructor
conLikeWrapId :: ConLike -> Maybe Var
conLikeWrapId (RealDataCon dc) = Just (dataConWrapId dc)
conLikeWrapId _ = Nothing

-- TODO: Verify the correctness of this function
-- Get Pattern Match as SimpleTcExpr 
trfPatToSimpleTcExpr :: Pat GhcTc -> SimpleTcExpr
trfPatToSimpleTcExpr pat = case pat of
  VarPat _ (L _ var)           -> SimpleVar var
  LazyPat _ (L _ lPat)         -> trfPatToSimpleTcExpr lPat
#if __GLASGOW_HASKELL__ >= 904
  AsPat _ (L _ var) _ (L _ sPat) -> SimpleAliasPat (SimpleVar var) (trfPatToSimpleTcExpr sPat)
  ParPat _ _ (L _ sPat) _      -> trfPatToSimpleTcExpr sPat
#else
  AsPat _ (L _ var) (L _ sPat) -> SimpleAliasPat (SimpleVar var) (trfPatToSimpleTcExpr sPat)
  ParPat _ (L _ sPat)          -> trfPatToSimpleTcExpr sPat
#endif
  BangPat _ (L _ sPat)         -> trfPatToSimpleTcExpr sPat
  SigPat _ (L _ sPat) _        -> trfPatToSimpleTcExpr sPat
  ListPat _ lPatList           -> SimpleList (fmap (trfPatToSimpleTcExpr . unLoc) lPatList)
  TuplePat _ lPatList _        -> SimpleTuple (fmap (trfPatToSimpleTcExpr . unLoc) lPatList)
  LitPat _ lit                 -> SimpleLit lit
  NPat _ (L _ (OverLit{ol_val = overloadedLit})) _ _ -> SimpleOverloadedLit overloadedLit
#if __GLASGOW_HASKELL__ >= 900
  ConPat _ (L _ con) (PrefixCon [] lPatList) -> SimpleDataCon (conLikeWrapId con) (fmap (trfPatToSimpleTcExpr . unLoc) lPatList)
#else
  ConPatIn (L _ con) (PrefixCon lPatList)            -> SimpleDataCon (Just con) (fmap (trfPatToSimpleTcExpr . unLoc) lPatList)
  ConPatOut (L _ con) _ _ _ _ (PrefixCon lPatList) _ -> SimpleDataCon (conLikeWrapId con) (fmap (trfPatToSimpleTcExpr . unLoc) lPatList)
#endif
  _                            -> SimpleUnhandledTcExpr

-- TODO: Verify the correctness of this function
-- Get LHsExpr as SimpleTcExpr
trfLHsExprToSimpleTcExpr :: LHsExpr GhcTc -> SimpleTcExpr
trfLHsExprToSimpleTcExpr (L loc hsExpr) = case hsExpr of
  HsVar _ (L _ var)            -> SimpleVar var
  HsConLikeOut _ cl            -> SimpleDataCon (conLikeWrapId cl) []
  HsLit _ lit                  -> SimpleLit lit
  PatHsPar expr                -> trfLHsExprToSimpleTcExpr expr
  PatHsAppType expr _          -> trfLHsExprToSimpleTcExpr expr
  PatHsWrap _ expr             -> trfLHsExprToSimpleTcExpr (L loc expr)
  ExplicitTuple _ ls _         -> SimpleTuple (fmap trfTupleArg ls)
  PatExplicitList _ ls         -> SimpleList (fmap trfLHsExprToSimpleTcExpr ls)
  ExprWithTySig _ expr _       -> trfLHsExprToSimpleTcExpr expr
#if __GLASGOW_HASKELL__ >= 900
  PatHsExpansion _ expanded    -> trfLHsExprToSimpleTcExpr (L loc expanded)
#endif
  HsOverLit _ (OverLit{ol_val = overloadedLit}) -> SimpleOverloadedLit overloadedLit
  HsApp _ (L _ (HsConLikeOut _ cl)) funr -> SimpleDataCon (conLikeWrapId cl) [trfLHsExprToSimpleTcExpr funr]
  HsApp _ funl funr -> 
    case trfLHsExprToSimpleTcExpr funl of
      SimpleDataCon mbVar ls -> SimpleDataCon mbVar (ls ++ [trfLHsExprToSimpleTcExpr funr])
      _ -> SimpleUnhandledTcExpr
  _                            -> SimpleUnhandledTcExpr
  where
#if __GLASGOW_HASKELL__ >= 900
    trfTupleArg :: HsTupArg GhcTc -> SimpleTcExpr
    trfTupleArg hsTupleArg = case hsTupleArg of
      Present _ lhsExpr -> trfLHsExprToSimpleTcExpr lhsExpr
      _                 -> SimpleUnhandledTcExpr
#else
    trfTupleArg :: LHsTupArg GhcTc -> SimpleTcExpr
    trfTupleArg (L _ hsTupleArg) = case hsTupleArg of
      Present _ lhsExpr -> trfLHsExprToSimpleTcExpr lhsExpr
      _                 -> SimpleUnhandledTcExpr
#endif

instance StrictEq SimpleTcExpr where
  (===) (SimpleFnNameVar var1 ty1) (SimpleFnNameVar var2 ty2) = 
    -- trace (if "sameName" `isInfixOf` getVarName var1; then show (getNameAndModuleNameWithNMV (varName var1)) <> " ::: " <> show (getNameAndModuleNameWithNMV (varName var2)); else "") $
    (getNameAndModuleNameWithNMV (varName var1) == getNameAndModuleNameWithNMV (varName var2)) && -- match name unique and module name
    (getVarName var1 == getVarName var2) &&  -- match function name (can be avoided)
    (getHsExprTypeAsTypeDataList ty1 == getHsExprTypeAsTypeDataList ty2) -- Match types for instances resolution
  (===) var1                       var2                       = (var1 == var2)