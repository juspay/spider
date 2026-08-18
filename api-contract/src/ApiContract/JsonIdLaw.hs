{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ApiContract.JsonIdLaw
  ( collectJsonIdLaw
  , checkJsonIdLaw
  ) where

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Data.Bag (bagToList)
import GHC.Core.Class (className, classMethods)
import GHC.Core.ConLike (ConLike, conLikeName)
import GHC.Core.DataCon (dataConFieldLabels, dataConName)
import GHC.Core.InstEnv (ClsInst(..))
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon (tyConDataCons, isAlgTyCon, isClassTyCon, isPromotedDataCon, isTcTyCon, tyConName, TyCon)
import GHC.Core.Type
import GHC.Driver.Env
import GHC.Driver.Plugins (CommandLineOption)
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (TcM)
import qualified GHC.Tc.Utils.Monad as TCError
import GHC.Hs.Expr (HsWrap(..), XXExprGhcTc(..), HsExpansion(..))
import GHC.Types.Name hiding (varName)
import GHC.Types.Var (Var)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.SrcLoc
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module (moduleNameString)
import GHC.Unit.Types
import GHC.Utils.Outputable (Outputable(..), showSDocUnsafe, ppr, docToSDoc)
import qualified GHC.Utils.Ppr as Pretty
import GHC.Types.FieldLabel (flLabel)
import GHC.Data.FastString (unpackFS, mkFastString)
#if __GLASGOW_HASKELL__ >= 904
-- GHC 9.4 replaced the raw @SDoc@ error API with structured @TcRnMessage@s.
import GHC.Tc.Errors.Types (mkTcRnUnknownMessage)
import qualified GHC.Types.Error as ParseError
#endif
#if __GLASGOW_HASKELL__ >= 908
-- GHC 9.8 made @FieldLabelString@ a newtype around @FastString@.
import Language.Haskell.Syntax.Basic (field_label)
#endif
#else
import GHC hiding (typeKind)
import Bag (bagToList)
import Class (className, classMethods)
import ConLike (conLikeName)
import DataCon (dataConFieldLabels, dataConName, tyConDataCons)
import InstEnv (ClsInst(..))
import TyCoRep
import TyCon
import Type
import GhcPlugins hiding ((<>), varName)
import TcRnTypes (TcGblEnv(..), TcM)
import qualified TcRnMonad as TCError
import Name (nameOccName, occNameString, nameModule, nameModule_maybe)
import RdrName (RdrName(..), rdrNameOcc)
import SrcLoc
import Outputable (Outputable(..), showSDocUnsafe, ppr)
import qualified Pretty
import FieldLabel (flLabel)
import FastString (unpackFS)
#endif

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (foldM, when, unless, guard)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Bool (bool)
import Data.Data (Data)
import Data.Char (toLower)
import Data.List (foldl', nub, sort, isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, fromJust, mapMaybe, catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import GHC.IO (unsafePerformIO)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import qualified Data.Yaml as YAML
import ApiContract.JsonIdLaw.Types
import ApiContract.Types (CliOptions(..))

#if __GLASGOW_HASKELL__ >= 900
unXRecTc :: XRec GhcTc a -> a
unXRecTc = GHC.unXRec @GhcTc
#else
unXRecTc :: LHsExpr GhcTc -> HsExpr GhcTc
unXRecTc (L _ e) = e
#endif

----------------------------------------------------------------------
-- Version-stable pattern synonyms bridging the post-9.2 AST changes.
-- GHC 9.4+ dropped the `HsConLikeOut` constructor (the typechecked conlike
-- now rides on the `ConLikeTc` extension), moved `AbsBinds` behind
-- `XHsBindsLR`, dropped `FunBind`'s tick field, and gave the paren / let /
-- lambda-case constructors extra token fields.  Matching through these
-- synonyms keeps every call site below identical across compilers.
----------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 904
pattern HsConLikeOut :: [Var] -> ConLike -> HsExpr GhcTc
pattern HsConLikeOut tvs con <- XExpr (ConLikeTc con tvs _)

pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ _ e _

pattern PatParPat :: LPat (GhcPass p) -> Pat (GhcPass p)
pattern PatParPat p <- ParPat _ _ p _

pattern PatHsLet :: HsLocalBinds (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsLet binds body <- HsLet _ _ binds _ body

pattern PatHsLamCase :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsLamCase mg <- HsLamCase _ _ mg

pattern PatFunBind :: LIdP GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> HsBindLR GhcTc GhcTc
pattern PatFunBind fid mg <- FunBind _ fid mg

pattern PatAbsBinds :: LHsBinds GhcTc -> HsBindLR GhcTc GhcTc
pattern PatAbsBinds binds <- XHsBindsLR (AbsBinds{abs_binds = binds})
#else
pattern PatHsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsPar e <- HsPar _ e

pattern PatParPat :: LPat (GhcPass p) -> Pat (GhcPass p)
pattern PatParPat p <- ParPat _ p

pattern PatHsLet :: LHsLocalBinds (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
pattern PatHsLet binds body <- HsLet _ binds body

pattern PatHsLamCase :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
pattern PatHsLamCase mg <- HsLamCase _ mg

#if __GLASGOW_HASKELL__ >= 900
-- 9.0/9.2: `fun_co_fn` is gone, `fun_tick` is not.
pattern PatFunBind :: LIdP GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> HsBindLR GhcTc GhcTc
pattern PatFunBind fid mg <- FunBind _ fid mg _
#else
pattern PatFunBind fid mg <- FunBind _ fid mg _ _
#endif

pattern PatAbsBinds :: LHsBinds GhcTc -> HsBindLR GhcTc GhcTc
pattern PatAbsBinds binds <- AbsBinds{abs_binds = binds}
#endif

----------------------------------------------------------------------
-- Shared state between the parsed and type-check phases.
----------------------------------------------------------------------

-- Per type (keyed by the head type-constructor's occNameString), the info we
-- can only recover from the parsed AST: the options/via used by TH-derived or
-- DerivingVia instances. Plain deriving and custom instances are detected in
-- the type-check phase.
data ParsedSideInfo = ParsedSideInfo
  { psiOptsEnc  :: Maybe Text
  , psiOptsDec  :: Maybe Text
  , psiViaEnc   :: Maybe Text
  , psiViaDec   :: Maybe Text
  , psiPlainEnc :: Bool
  , psiPlainDec :: Bool
  , psiSpan     :: SrcSpan
  } deriving (Show)

emptyParsedSideInfo :: ParsedSideInfo
emptyParsedSideInfo = ParsedSideInfo Nothing Nothing Nothing Nothing False False noSrcSpan

-- Keyed by module file path -> (type-name -> info).
idLawStateVar :: MVar (Map String (Map Text ParsedSideInfo))
{-# NOINLINE idLawStateVar #-}
idLawStateVar = unsafePerformIO (newMVar Map.empty)

----------------------------------------------------------------------
-- Phase 1 (parsed): collect TH-options / via for derived instances.
----------------------------------------------------------------------

collectJsonIdLaw :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectJsonIdLaw opts modSummary hpm = do
  let cliOptions = parseCli opts
  when (idLawEnabled cliOptions) $ liftIO $ do
    let moduleKey = msHsFilePath modSummary
        decls = hsmodDecls (unLoc (hpm_module hpm))
    perType <- foldM (\acc -> pure . foldParsedDecl acc) Map.empty decls
    modifyMVar_ idLawStateVar $ pure . Map.insert moduleKey perType
  pure hpm

foldParsedDecl :: Map Text ParsedSideInfo -> LHsDecl GhcPs -> Map Text ParsedSideInfo
#if __GLASGOW_HASKELL__ >= 900
foldParsedDecl acc (L l decl) = case decl of
  TyClD _ (DataDecl _ _ _ _ _) -> acc
  DerivD _ d -> handleDerivDecl acc (locA l) d
  SpliceD _ s -> handleSpliceDecl acc (locA l) s
  _ -> acc
#else
foldParsedDecl acc (L l decl) = case decl of
  TyClD _ (DataDecl _ _ _ _ _) -> acc
  DerivD _ d -> handleDerivDecl acc l d
  SpliceD _ s -> handleSpliceDecl acc l s
  _ -> acc
#endif

handleDerivDecl :: Map Text ParsedSideInfo -> SrcSpan -> DerivDecl GhcPs -> Map Text ParsedSideInfo
handleDerivDecl acc l (DerivDecl{deriv_type = sigTy, deriv_strategy = mStrat}) =
  case extractClassAndType sigTy of
    Nothing -> acc
    Just (cls, ty) -> case (cls, mStrat) of
      ("ToJSON", Just (L _ (ViaStrategy viaTy))) -> insertField ty (\psi -> psi{psiViaEnc = Just (pprText viaTy), psiSpan = l}) acc
      ("FromJSON", Just (L _ (ViaStrategy viaTy))) -> insertField ty (\psi -> psi{psiViaDec = Just (pprText viaTy), psiSpan = l}) acc
      ("ToJSON", _) -> insertField ty (\psi -> psi{psiPlainEnc = True, psiSpan = l}) acc
      ("FromJSON", _) -> insertField ty (\psi -> psi{psiPlainDec = True, psiSpan = l}) acc
      _ -> acc

handleSpliceDecl :: Map Text ParsedSideInfo -> SrcSpan -> SpliceDecl GhcPs -> Map Text ParsedSideInfo
handleSpliceDecl acc l (SpliceDecl _ (L _ splice) _) =
  case spliceSpine splice of
    Nothing -> acc
    Just (fnName, optsStr, tyName) ->
      let key = T.pack tyName in
      case fnName of
        "deriveJSON"        -> insertField key (\psi -> psi{psiOptsEnc = Just optsStr, psiOptsDec = Just optsStr, psiSpan = l}) acc
        "deriveToJSON"      -> insertField key (\psi -> psi{psiOptsEnc = Just optsStr, psiSpan = l}) acc
        "deriveFromJSON"    -> insertField key (\psi -> psi{psiOptsDec = Just optsStr, psiSpan = l}) acc
        "mkToJSON"          -> insertField key (\psi -> psi{psiOptsEnc = Just optsStr, psiSpan = l}) acc
        "mkParseJSON"       -> insertField key (\psi -> psi{psiOptsDec = Just optsStr, psiSpan = l}) acc
        _ -> acc

#if __GLASGOW_HASKELL__ >= 904
-- `HsSplice` was split up after 9.2: a declaration splice now carries an
-- `HsUntypedSplice` (typed splices became an `HsExpr` constructor and cannot
-- appear at declaration level, so nothing is lost here).
spliceSpine :: HsUntypedSplice GhcPs -> Maybe (String, Text, String)
spliceSpine (HsUntypedSpliceExpr _ expr) = spineOf (unLoc expr)
spliceSpine _ = Nothing
#elif __GLASGOW_HASKELL__ >= 900
spliceSpine :: HsSplice GhcPs -> Maybe (String, Text, String)
spliceSpine (HsUntypedSplice _ _ _ expr) = spineOf (unLoc expr)
spliceSpine (HsTypedSplice _ _ _ expr) = spineOf (unLoc expr)
spliceSpine _ = Nothing
#else
spliceSpine :: HsSplice GhcPs -> Maybe (String, Text, String)
spliceSpine (HsUntypedSplice _ _ expr) = spineOf (unLoc expr)
spliceSpine (HsTypedSplice _ _ expr) = spineOf (unLoc expr)
spliceSpine _ = Nothing
#endif

-- Flatten a left-nested @HsApp@ spine into [head, arg1, arg2, ...].
spineOf :: HsExpr GhcPs -> Maybe (String, Text, String)
spineOf e = case appSpine e of
  (HsVar _ (L _ rdr) : args) ->
    let fnName = occNameString (rdrNameOcc rdr) in
    if fnName `elem` deriveFns
      then case args of
        (optsExpr : tyExpr : _) ->
          Just (fnName, pprText optsExpr, tyNameOfExpr tyExpr)
        _ -> Nothing
      else Nothing
  _ -> Nothing
  where
    deriveFns = ["deriveJSON","deriveToJSON","deriveFromJSON","mkToJSON","mkParseJSON"]
    -- Extract the type name from a TH name expression, stripping module
    -- qualifiers so the key matches occNameString-based keys elsewhere.
    tyNameOfExpr :: HsExpr GhcPs -> String
    tyNameOfExpr (HsVar _ (L _ rdr')) = stripQuotes (occNameString (rdrNameOcc rdr'))
    tyNameOfExpr other = stripQuotes (T.unpack (pprText other))

appSpine :: HsExpr GhcPs -> [HsExpr GhcPs]
appSpine (PatHsPar e) = appSpine (GHC.unXRec @GhcPs e)
appSpine (HsApp _ f a) = appSpine (GHC.unXRec @GhcPs f) ++ [GHC.unXRec @GhcPs a]
appSpine e = [e]

-- @deriv_type@ is @HsSigWcType@; its body is @HsSigType@ whose @sig_body@ is
-- the @HsType@. For @ToJSON T@ this is @HsAppTy ToJSON T@.
extractClassAndType :: LHsSigWcType GhcPs -> Maybe (String, Text)
#if __GLASGOW_HASKELL__ >= 900
extractClassAndType sigWc =
  case unLoc (sig_body (unLoc (hswc_body sigWc))) of
    HsAppTy _ (L _ clsTy) (L _ argTy) -> Just (rdrTyName clsTy, pprText argTy)
    HsQualTy _ _ (L _ (HsAppTy _ (L _ clsTy) (L _ argTy))) -> Just (rdrTyName clsTy, pprText argTy)
    _ -> Nothing
#else
extractClassAndType sigWc =
  case unLoc (hsSigType (hswc_body sigWc)) of
    HsAppTy _ (L _ clsTy) (L _ argTy) -> Just (rdrTyName clsTy, pprText argTy)
    HsQualTy _ _ (L _ (HsAppTy _ (L _ clsTy) (L _ argTy))) -> Just (rdrTyName clsTy, pprText argTy)
    _ -> Nothing
#endif

rdrTyName :: HsType GhcPs -> String
rdrTyName (HsTyVar _ _ n) = occNameString . occName . unXPs $ n
rdrTyName t = stripQuotes (T.unpack (pprText t))

insertField :: Text -> (ParsedSideInfo -> ParsedSideInfo) -> Map Text ParsedSideInfo -> Map Text ParsedSideInfo
insertField key f m = Map.alter (Just . f . fromMaybe emptyParsedSideInfo) key m

----------------------------------------------------------------------
-- Phase 2 (type-check): collect keys, classify origins, run checks.
----------------------------------------------------------------------

checkJsonIdLaw :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
checkJsonIdLaw opts modSummary tcg = do
  let cliOptions = parseCli opts
  if not (idLawEnabled cliOptions)
    then pure tcg
    else do
      let moduleKey = msHsFilePath modSummary
          moduleSrcSpan = mkFileSrcSpan (ms_location modSummary)
      parsedMap <- liftIO $ fromMaybe Map.empty . Map.lookup moduleKey <$> readMVar idLawStateVar
      localKeys <- liftIO $ collectLocalKeys tcg
      tagValues <- liftIO $ collectTagValues tcg
      altGroups <- liftIO $ collectAltGroups tcg
      (excTypes, excModules) <- liftIO $ loadExceptions (id_law_exceptions_path cliOptions)
      let definedHere = definedHereTyCons tcg modSummary
          instPresence = jsonInstancePresence tcg
          currentMod = T.pack (moduleNameString (moduleName (tcg_mod tcg)))
          allTypes = allTypesOfInterest parsedMap localKeys definedHere instPresence
          typesToCheck = filter (\ty ->
            let qKey = qualifiedKey ty tcg
            in not (qKey `Set.member` excTypes || currentMod `Set.member` excModules))
            allTypes
          errs = concatMap (checkType moduleSrcSpan parsedMap localKeys tagValues altGroups definedHere instPresence) typesToCheck
          errsNub = nub errs
#if __GLASGOW_HASKELL__ >= 904
      -- `addErrs` takes structured `TcRnMessage`s from GHC 9.4 onwards.
      unless (null errsNub) $ TCError.addErrs (map (\(sp,e) -> (sp, mkTcRnUnknownMessage (ParseError.mkPlainError ParseError.noHints (docToSDoc (Pretty.text (generateJsonIdLawError e)))))) errsNub)
#else
      unless (null errsNub) $ TCError.addErrs (map (\(sp,e) -> (sp, docToSDoc (Pretty.text (generateJsonIdLawError e)))) errsNub)
#endif
      pure tcg

-- | Per type (keyed by head tycon occNameString): the type's source span and
-- record-field labels, so that a 'DerivedPlain' side's keys can be taken as
-- the field labels.
-- | What we know about a type declared in this module.
data TyConInfo = TyConInfo
  { tciSpan    :: SrcSpan
  , tciFields  :: [Text]
  -- ^ Record field labels, across all constructors.
  , tciNumCons :: Int
  -- ^ Number of data constructors.  aeson only emits a constructor tag for a
  -- single-constructor type when @tagSingleConstructors@ is set, so this decides
  -- whether the tag-related 'Options' fields can affect any JSON key.
  }

emptyTyConInfo :: SrcSpan -> TyConInfo
emptyTyConInfo sp = TyConInfo sp [] 0

definedHereTyCons :: TcGblEnv -> ModSummary -> Map Text TyConInfo
definedHereTyCons tcg modSummary =
  Map.fromListWith mergeInfo
    [ (keyOfTyCon tc, TyConInfo (nameSrcSpan (tyConName tc)) (fieldLabelsOf tc) (length (tyConDataCons tc)))
    | tc <- tcsOf tcg
    , isAlgTyCon tc
    , not (isClassTyCon tc)
    , tcDefinedHere modSummary tc
    ]
  where
    tcsOf = filter isSafeTyCon . maybe [] id . fmap (\e -> e ^? biplateRef) . Just . tcg_tcs
    mergeInfo a b = TyConInfo (combineSpan (tciSpan a) (tciSpan b))
                              (nub (tciFields a ++ tciFields b))
                              (max (tciNumCons a) (tciNumCons b))
    combineSpan s1 s2 | s1 /= noSrcSpan = s1
                      | otherwise       = s2

tcDefinedHere :: ModSummary -> TyCon -> Bool
tcDefinedHere modSummary tc = nameModule_maybe (tyConName tc) == Just (ms_mod modSummary)

fieldLabelsOf :: TyCon -> [Text]
fieldLabelsOf tc = nub
#if __GLASGOW_HASKELL__ >= 908
  -- `flLabel` yields a `FieldLabelString` newtype from GHC 9.8 onwards.
  [ T.pack (unpackFS (field_label (flLabel fl)))
#else
  [ T.pack (unpackFS (flLabel fl))
#endif
  | dc <- tyConDataCons tc
  , fl <- dataConFieldLabels dc
  ]

-- | Which of ToJSON / FromJSON are present for each type (local or imported),
-- from the instance environment.
jsonInstancePresence :: TcGblEnv -> Map Text (Bool, Bool)
jsonInstancePresence tcg =
  Map.fromListWith (\(a,b) (c,d) -> (a||c, b||d))
    [ (tyKey, (isTo, isFrom))
    | inst <- tcg_insts tcg
    , let clsName = getName (is_cls inst)
    , isAesonClassName clsName
    , Just tyKey <- [headTypeKeyOfInst inst]
    , let occ = occNameString (nameOccName clsName)
          isTo = occ == "ToJSON"
          isFrom = occ == "FromJSON"
    ]

isAesonClassName :: Name -> Bool
isAesonClassName n =
  occNameString (nameOccName n) `elem` ["ToJSON","FromJSON"]
  && maybe False (T.isInfixOf "Aeson" . T.pack . moduleNameString . moduleName) (nameModule_maybe n)

headTypeKeyOfInst :: ClsInst -> Maybe Text
headTypeKeyOfInst inst = case is_tys inst of
  [] -> Nothing
  ts -> Just (keyOfType (last ts))

----------------------------------------------------------------------
-- Local (custom) instance key extraction from @tcg_binds@.
----------------------------------------------------------------------

-- | Markers used in place of a pretty-printed @Options@ expression when a side
-- builds its JSON in a way we cannot read statically.  They are not valid
-- Haskell, so they can never collide with a real @Options@ text.
nonStandardEncoding, nonStandardDecoding :: Text
nonStandardEncoding = "<non-standard encoding>"
nonStandardDecoding = "<non-standard decoding>"

isUnknownOptsText :: Text -> Bool
isUnknownOptsText t = "<non-standard" `T.isPrefixOf` t

-- | Is this method body just the class default method (@$dmtoJSON@ and
-- friends)?  GHC emits a real @FunBind@ named after the method for
-- @deriving anyclass@ instances and for any method an instance omits, so
-- without this test such a binding looks like a hand-written method that
-- produces no JSON keys at all.  Returns the default method's @OccName@.
defaultMethodOf :: HsExpr GhcTc -> Maybe String
defaultMethodOf body = case appSpineTc body of
  (h : _) | Just (occ, _) <- opName h, "$dm" `isPrefixOf` occ -> Just occ
  _ -> Nothing

-- type occNameString key -> (encSpan, encode keys, decSpan, decode keys,
--                              encGenericOpts, decGenericOpts, delegated enc type key)
collectLocalKeys :: TcGblEnv -> IO (Map Text (SrcSpan, [KeyInfo], SrcSpan, [KeyInfo], Maybe Text, Maybe Text, Maybe Text))
collectLocalKeys tcg = do
  rows <- mapM processBind (bagToList (tcg_binds tcg))
  pure (Map.fromListWith mergeRows (concat rows))
  where
    currentModName = moduleNameString (moduleName (tcg_mod tcg))
    mergeRows (es1,ek1,ds1,dk1,go1,go2,del1) (es2,ek2,ds2,dk2,go3,go4,del2) =
      (combineSpan es1 es2, ek1++ek2, combineSpan ds1 ds2, dk1++dk2, go1 `plusOpt` go3, go2 `plusOpt` go4, del1 <|> del2)
    combineSpan s1 s2 | s1 /= noSrcSpan = s1
                      | otherwise       = s2
    -- A type can contribute several rows (e.g. @toJSON@ and @toEncoding@).  An
    -- "unknown keys" marker must win over a concrete @Options@ text regardless of
    -- the order the binds happen to come out of the bag, or the verdict would
    -- depend on @tcg_binds@ ordering.
    plusOpt Nothing x = x
    plusOpt x Nothing = x
    plusOpt x@(Just a) y@(Just b)
      | isUnknownOptsText a = x
      | isUnknownOptsText b = y
      | otherwise           = x

    processBind :: LHsBindLR GhcTc GhcTc -> IO [(Text, (SrcSpan, [KeyInfo], SrcSpan, [KeyInfo], Maybe Text, Maybe Text, Maybe Text))]
#if __GLASGOW_HASKELL__ >= 900
    processBind (L l (PatFunBind id' matches)) = methodKeys (locA l) id' matches
    processBind (L _ (PatAbsBinds binds)) = concat <$> mapM processBind (bagToList binds)
#else
    processBind (L l (PatFunBind id' matches)) = methodKeys l id' matches
    processBind (L _ (PatAbsBinds binds)) = concat <$> mapM processBind (bagToList binds)
#endif
    processBind _ = pure []

    -- Every top-level function in this module, so that a @toJSON@/@parseJSON@
    -- that delegates to one can be followed into it.  The JSON methods
    -- themselves are excluded: they are handled by 'methodKeys'.
    helperBinds :: Map String (MatchGroup GhcTc (LHsExpr GhcTc))
    helperBinds = Map.fromList (concatMap collectHelper (bagToList (tcg_binds tcg)))

    collectHelper :: LHsBindLR GhcTc GhcTc -> [(String, MatchGroup GhcTc (LHsExpr GhcTc))]
    collectHelper (L _ (PatFunBind fid mg))
      | let occ = occNameString (nameOccName (getName (unXRecTc fid)))
      , occ `notElem` jsonMethodNames
      = [(occ, mg)]
    collectHelper (L _ (PatAbsBinds binds)) = concatMap collectHelper (bagToList binds)
    collectHelper _ = []

    -- Names of same-module functions applied in these expressions -- the same
    -- notion of "local call" that 'hasLocalFuncCall' tests for.  Record field
    -- selectors land here too; they are harmless because they contribute no keys.
    localCalleeNames :: [LHsExpr GhcTc] -> [String]
    localCalleeNames es = nub
      [ occ
      | L _ e <- es
      , (h : _) <- [appSpineTc e]
      , Just (occ, Just m) <- [opName h]
      , m == currentModName
      , occ /= "parseJSON"
      ]

    -- The keys a same-module helper contributes, when its body can be read in
    -- full.  'Nothing' means "not statically readable", which leaves the calling
    -- side unknown exactly as it was before this tracing existed.  An empty key
    -- set is also reported as unreadable: a helper we can parse but which yields
    -- no keys (a value transformer, a dynamic @fromText k@ lookup) must not be
    -- mistaken for a decoder that reads nothing.
    tracedHelperKeys :: Side -> MatchGroup GhcTc (LHsExpr GhcTc) -> Maybe [KeyInfo]
    tracedHelperKeys side mg
      | any unreadable bodies = Nothing
      | delegatesFurther = Nothing
      | null keys = Nothing
      | otherwise = Just keys
      where
        alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
        subExprs = alts ^? biplateRef :: [LHsExpr GhcTc]
        bodies = [ b | L _ m <- alts, Just b <- [matchBody m] ]
        keys = helperOwnKeys side mg
        unreadable b = hasObjectConstruction b || hasCompositionGeneric b
                    || (case side of
                          DecodeSide -> hasDecDelegation b
                          EncodeSide -> hasDelegation b)
        -- Does this helper hand the JSON off to a *further* helper that carries
        -- keys of its own?  We only follow one level, so anything deeper would
        -- give us a partial key set, which is worse than no key set.  Callees
        -- that carry no keys -- record field selectors, formatting utilities --
        -- are not delegation and must not block the trace.
        delegatesFurther =
          any (\c -> maybe False (not . null . helperOwnKeys side) (Map.lookup c helperBinds))
              (localCalleeNames subExprs)

    -- The keys a function's own body mentions, ignoring anything it delegates to.
    helperOwnKeys :: Side -> MatchGroup GhcTc (LHsExpr GhcTc) -> [KeyInfo]
    helperOwnKeys side mg = nub $ case side of
        DecodeSide -> concatMap (extractKeysFromExpr (Just currentModName) DecodeSide . unLoc) subExprs
        EncodeSide -> concatMap (walkEncKeys (Just currentModName)) bodies
                        ++ concat [ whereClauseKeys (Just currentModName) m | L _ m <- alts ]
      where
        alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
        subExprs = alts ^? biplateRef :: [LHsExpr GhcTc]
        bodies = [ b | L _ m <- alts, Just b <- [matchBody m] ]

    -- Follow a delegating method into the one helper that actually yields keys.
    -- Ambiguity (two readable helpers) means we cannot tell which one produces
    -- the JSON, so the side stays unknown.
    tracedKeysFor :: Side -> [LHsExpr GhcTc] -> [KeyInfo]
    tracedKeysFor side es =
      case [ ks
           | c <- localCalleeNames es
           , Just mg <- [Map.lookup c helperBinds]
           , Just ks <- [tracedHelperKeys side mg]
           ] of
        [ks] -> ks
        _    -> []

    methodKeys :: SrcSpan -> LIdP GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> IO [(Text, (SrcSpan, [KeyInfo], SrcSpan, [KeyInfo], Maybe Text, Maybe Text, Maybe Text))]
    methodKeys l id' matches = do
      let methodName = occNameString (nameOccName (getName (unXRecTc id')))
          ty = idType (unXRecTc id')
          mTyKey = case methodAppliedType methodName ty of
                     Just t -> Just (keyOfType t)
                     Nothing -> Nothing
      case mTyKey of
        Nothing -> pure []
        Just tyKey -> do
          let matchAlts = unLoc (mg_alts matches :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
              exprs = matchAlts ^? biplateRef :: [LHsExpr GhcTc]
              side | methodName `elem` ["toJSON","toEncoding"] = EncodeSide
                   | methodName == "parseJSON" = DecodeSide
                   | otherwise = EncodeSide
              mGenOpts = detectGenericDeriving matchAlts
              allBodies = [ body | L _ m <- matchAlts, Just body <- [matchBody m] ]
              defaultMethods = mapMaybe defaultMethodOf allBodies
              -- Set only when *every* alternative is the class default method.
              mDefaultMethod
                | not (null allBodies)
                , length defaultMethods == length allBodies = listToMaybe defaultMethods
                | otherwise = Nothing
          case () of
            -- @deriving anyclass@, or a method the instance omitted: the keys are
            -- whatever aeson's generic default produces, i.e. the field labels.
            _ | Just "$dmtoJSON" <- mDefaultMethod ->
                  pure [(tyKey, (l, [], noSrcSpan, [], Just "defaultOptions", Nothing, Nothing))]
              | Just "$dmparseJSON" <- mDefaultMethod ->
                  pure [(tyKey, (noSrcSpan, [], l, [], Nothing, Just "defaultOptions", Nothing))]
              -- Any other defaulted method (@toEncoding@, @toJSONList@, ...) says
              -- nothing about this type's keys.  A row here would claim the side
              -- has a local binding producing zero keys -- which is how every
              -- hand-written 'ToJSON' picks up a spurious empty encode row from
              -- its defaulted @toEncoding@.
              | isJust mDefaultMethod -> pure []
              | methodName `elem` ["toJSON","toEncoding"] -> do
                  let ownEncKeys = nub (concat
                                    [ walkEncKeys (Just currentModName) body
                                    | L _ m <- matchAlts, Just body <- [matchBody m] ]
                                      ++ concat [ whereClauseKeys (Just currentModName) m
                                                | L _ m <- matchAlts ])
                      -- An encoder that builds part or all of its object in a
                      -- same-module helper (@toJSON x = mkPayload x@) has those
                      -- keys nowhere in its own body; read them out of the
                      -- helper when it is simple enough.
                      encKeys = nub (ownEncKeys ++ tracedKeysFor EncodeSide exprs)
                      hasDel = any hasDelegation allBodies
                      hasObj = any hasObjectConstruction allBodies
                      hasCompGen = any hasCompositionGeneric allBodies
                      hasLocalCall = any (hasLocalFuncCall currentModName) exprs
                      -- Without this, an encoder whose helper we could not read
                      -- looks like an encoder that writes no keys at all, and
                      -- every decoder key is reported as missing.
                      nonStd = hasDel || hasObj || hasCompGen || (hasLocalCall && null encKeys)
                      finalGenOpts = mGenOpts <|> (guard nonStd >> Just nonStandardEncoding)
                      mDelegatedKey = if hasDel && not hasObj && null encKeys
                                        then findDelegatedTypeKey matchAlts
                                        else Nothing
                  pure [(tyKey, (l, encKeys, noSrcSpan, [], finalGenOpts, Nothing, mDelegatedKey))]
              | methodName == "parseJSON" -> do
                  let decKeys' = concatMap (extractKeysFromExpr (Just currentModName) side . unLoc) exprs
                      badWhereKeys = concat
                        [ [ ki
                          | L _ sub <- lbs ^? biplateRef :: [LHsExpr GhcTc]
                          , ki <- extractBadWhereKeys (Just currentModName) sub
                          ]
                        | L _ m <- matchAlts
                        , let lbs = grhssLocalBinds (matchGRHSs m)
                        ]
                      ownDecKeys = nub (filter (\k -> kiKey k `notElem` map kiKey badWhereKeys) decKeys')
                      -- @parseJSON v = parseThing v@: some or all of the keys
                      -- live in a helper, so merge whatever it yields.
                      allDecKeys = nub (ownDecKeys ++ tracedKeysFor DecodeSide exprs)
                      -- Envelope keys whose alternation has a keyless branch.
                      altKeys = defaultedAltKeys (Just currentModName) exprs
                      decKeys = [ k | k <- allDecKeys, not (kiKey k `Set.member` altKeys) ]
                      hasDecDel = any hasDecDelegation allBodies
                      hasCompGen = any hasCompositionGeneric allBodies
                      hasLocalCall = any (hasLocalFuncCall currentModName) (matchAlts ^? biplateRef :: [LHsExpr GhcTc])
                      nonStdDec = hasDecDel || hasCompGen || hasLocalCall
                      finalDecGenOpts = mGenOpts <|> (guard (nonStdDec && null decKeys) >> Just nonStandardDecoding)
                  pure [(tyKey, (noSrcSpan, [], l, decKeys, Nothing, finalDecGenOpts, Nothing))]
              | otherwise -> pure []

-- | The aeson class methods this check reads.  Anything else bound at the top
-- level of a module is an ordinary function, and therefore a potential helper.
jsonMethodNames :: [String]
jsonMethodNames = ["toJSON", "toEncoding", "parseJSON", "toJSONList", "parseJSONList"]

methodAppliedType :: String -> Type -> Maybe Type
#if __GLASGOW_HASKELL__ >= 900
methodAppliedType "toJSON"      (FunTy _ _ arg _) = Just arg
methodAppliedType "toEncoding"  (FunTy _ _ arg _) = Just arg
methodAppliedType "parseJSON"   (FunTy _ _ _ res) = lastTypeArg res
#else
methodAppliedType "toJSON"      (FunTy _ arg _) = Just arg
methodAppliedType "toEncoding"  (FunTy _ arg _) = Just arg
methodAppliedType "parseJSON"   (FunTy _ _ res) = lastTypeArg res
#endif
methodAppliedType _ _ = Nothing

lastTypeArg :: Type -> Maybe Type
lastTypeArg (TyConApp _ ts) | not (null ts) = Just (last ts)
lastTypeArg (AppTy _ t) = Just t
lastTypeArg _ = Nothing

keyOfType :: Type -> Text
keyOfType (TyConApp tc _) = keyOfTyCon tc
keyOfType (AppTy t _) = keyOfType t
keyOfType _ = ""

keyOfTyCon :: TyCon -> Text
keyOfTyCon tc = T.pack (occNameString (nameOccName (tyConName tc)))

----------------------------------------------------------------------
-- Tag-value extraction for sum-type constructor tag checks.
----------------------------------------------------------------------

-- | Per type: (encoder tag values, decoder tag values,
--   encoder constructor→tag map, decoder catch-all constructor,
--   encoder constructors, decoder constructors).
type TagValueMap = Map Text (Set Text, Set Text, Map Text Text, Maybe Text, Set Text, Set Text)

-- | Per type: map from decode key to set of alternative decode keys
--   that appear together in a @<|>@ fallback expression.
type AltGroupMap = Map Text (Map Text (Set Text))

-- | Walk @tcg_binds@ to extract tag values from toJSON/parseJSON method bodies.
-- Encoder tags: the VALUE in @"tag" .= value"@ or @String "VALUE"@.
-- Decoder tags: string-literal patterns in @case tag of "X" -> ...; _ -> ...@.
-- Also tracks constructor correlation: for each encoder match alternative,
-- maps the constructor name (from the pattern) to its tag value. For the
-- decoder, records the constructor returned by the catch-all @_@ branch
-- (if any), so we can verify round-trip correctness.
collectTagValues :: TcGblEnv -> IO TagValueMap
collectTagValues tcg = do
  rows <- mapM processBind (bagToList (tcg_binds tcg))
  pure (Map.fromListWith mergeRows (concat rows))
  where
    currentModName = moduleNameString (moduleName (tcg_mod tcg))
    mergeRows (e1,d1,c1,ca1,ec1,dc1) (e2,d2,c2,ca2,ec2,dc2) =
      (Set.union e1 e2, Set.union d1 d2, Map.union c1 c2, ca1 <|> ca2, Set.union ec1 ec2, Set.union dc1 dc2)

    -- Same-module functions that write the @"tag"@ key of a value handed to
    -- them, keyed by name and carrying the parameter positions the tag arrives
    -- in.  An encoder delegating to one of these still has a statically known
    -- tag value even though its own body never mentions @"tag"@.
    helperTagPos :: Map String [Int]
    helperTagPos = Map.fromListWith (\a b -> nub (a ++ b))
                     (concatMap collectTagHelper (bagToList (tcg_binds tcg)))

    collectTagHelper :: LHsBindLR GhcTc GhcTc -> [(String, [Int])]
    collectTagHelper (L _ (PatFunBind fid mg))
      | let occ = occNameString (nameOccName (getName (unXRecTc fid)))
      , occ `notElem` jsonMethodNames
      , poss <- helperTagParamPositions mg
      , not (null poss)
      = [(occ, poss)]
    collectTagHelper (L _ (PatAbsBinds binds)) = concatMap collectTagHelper (bagToList binds)
    collectTagHelper _ = []

    processBind :: LHsBindLR GhcTc GhcTc -> IO [(Text, (Set Text, Set Text, Map Text Text, Maybe Text, Set Text, Set Text))]
#if __GLASGOW_HASKELL__ >= 900
    processBind (L l (PatFunBind id' matches)) = methodTags (locA l) id' matches
    processBind (L _ (PatAbsBinds binds)) = concat <$> mapM processBind (bagToList binds)
#else
    processBind (L l (PatFunBind id' matches)) = methodTags l id' matches
    processBind (L _ (PatAbsBinds binds)) = concat <$> mapM processBind (bagToList binds)
#endif
    processBind _ = pure []

    methodTags :: SrcSpan -> LIdP GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> IO [(Text, (Set Text, Set Text, Map Text Text, Maybe Text, Set Text, Set Text))]
    methodTags _ id' matches = do
      let methodName = occNameString (nameOccName (getName (unXRecTc id')))
          ty = idType (unXRecTc id')
          mTyKey = case methodAppliedType methodName ty of
                     Just t -> Just (keyOfType t)
                     Nothing -> Nothing
      case mTyKey of
        Nothing -> pure []
        Just tyKey -> do
          let matchAlts = unLoc (mg_alts matches :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
              exprs = matchAlts ^? biplateRef :: [LHsExpr GhcTc]
          case () of
            _ | methodName `elem` ["toJSON","toEncoding"] ->
                  let (encTags, encConToTag, encCons) = collectEncTags currentModName helperTagPos matchAlts
                  in pure [(tyKey, (encTags, Set.empty, encConToTag, Nothing, encCons, Set.empty))]
              | methodName == "parseJSON" ->
                  let (decTagsCase, mCatchAllCase) = collectDecTags exprs
                      (decTagsMatch, mCatchAllMatch) = collectDecMatchTags matchAlts
                      decTags = Set.union decTagsCase decTagsMatch
                      mCatchAllCon = mCatchAllCase <|> mCatchAllMatch
                      decCons = collectDecCons currentModName matchAlts
                  in pure [(tyKey, (Set.empty, decTags, Map.empty, mCatchAllCon, Set.empty, decCons))]
              | otherwise -> pure []

-- | Walk @tcg_binds@ to extract @<|>@ fallback key groups from parseJSON bodies.
-- For each type, returns a map from each decode key to the set of OTHER decode
-- keys that appear as alternatives in a @<|>@ expression (direct or via @liftA2@).
collectAltGroups :: TcGblEnv -> IO AltGroupMap
collectAltGroups tcg = do
  let currentModName = moduleNameString (moduleName (tcg_mod tcg))
  rows <- mapM (processBind currentModName) (bagToList (tcg_binds tcg))
  pure (Map.fromListWith (Map.unionWith Set.union) (concat rows))
  where
    processBind :: String -> LHsBindLR GhcTc GhcTc -> IO [(Text, Map Text (Set Text))]
    processBind modName (L _ (PatFunBind id' matches)) = pure (methodAlts modName id' matches)
    processBind modName (L _ (PatAbsBinds binds)) = concat <$> mapM (processBind modName) (bagToList binds)
    processBind _ _ = pure []

    methodAlts currentModName id' matches =
      let methodName = occNameString (nameOccName (getName (unXRecTc id')))
          ty = idType (unXRecTc id')
          mTyKey = case methodAppliedType methodName ty of
                     Just t -> Just (keyOfType t)
                     Nothing -> Nothing
      in case mTyKey of
           Nothing -> []
           Just tyKey | methodName == "parseJSON" ->
             let matchAlts = unLoc (mg_alts matches :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
                 exprs = matchAlts ^? biplateRef :: [LHsExpr GhcTc]
                 groups = concatMap (findAltGroups . unLoc) exprs
                            ++ caseFallbackGroups currentModName exprs
                 keyMap = buildAltMap groups
             in [(tyKey, keyMap)]
           _ -> []

-- | Fallback groups written as a @case@ over an already-parsed optional key
-- rather than with @\<|\>@:
--
-- @
--   mbBooking \<- v .:? "booking"
--   bData \<- case mbBooking of
--             Just b  -> pure (Just b)
--             Nothing -> v .:? "data"
-- @
--
-- \"booking\" and \"data\" fill the same field, so the encoder only has to write
-- one of them.  Recognised by binding the scrutinee: a @do@ statement
-- @v \<- \<expr reading exactly one key\>@ followed by a @case@ on that same @v@.
caseFallbackGroups :: String -> [LHsExpr GhcTc] -> [[Text]]
caseFallbackGroups currentModName exprs =
  [ nub (boundKey : altKeys)
  | (scrutName, boundKey) <- boundKeys
  , (caseName, altKeys) <- caseKeys
  , scrutName == caseName
  , not (null altKeys)
  ]
  where
    stmts = [ s | L _ e <- exprs, HsDo _ _ ss <- [peelWrap e], L _ s <- unLoc ss ]
    boundKeys =
      [ (v, k)
      | BindStmt _ pat rhs <- stmts
      , Just v <- [patVarName (unXRecTc pat)]
      , [k] <- [nub (map kiKey (deepDecKeys rhs))]
      ]
    caseKeys =
      [ (v, concatMap (map kiKey . deepDecKeys) (altBodies mg))
      | L _ e <- exprs
      , HsCase _ scrut mg <- [peelWrap e]
      , Just v <- [exprVarName (unXRecTc scrut)]
      ]
    altBodies mg = [ b | L _ m <- unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
                       , Just b <- [matchBodyL m] ]
    deepDecKeys b = [ k
                    | L _ sub <- (b ^? biplateRef :: [LHsExpr GhcTc])
                    , k <- extractKeysFromExpr (Just currentModName) DecodeSide sub
                    ]
    exprVarName e = case peelWrap e of
      HsVar _ (L _ v) -> Just (occNameString (nameOccName (getName v)))
      _ -> Nothing

-- | The variable a pattern binds, for simple @v <- ...@ binders.
patVarName :: Pat GhcTc -> Maybe String
patVarName p = case p of
  VarPat _ (L _ v) -> Just (occNameString (nameOccName (getName v)))
  PatParPat inner -> patVarName (unXRecTc inner)
  _ -> Nothing

-- | Find @<|>@ fallback groups in an expression. Returns a list of
--   key-groups, where each group is a list of decode keys that are
--   alternatives for each other.
findAltGroups :: HsExpr GhcTc -> [[Text]]
findAltGroups e0 = go 0 (peelWrap e0)
  where
    go depth e
      | depth > 10 = []
      | otherwise = case appSpineTc e of
      -- Chained <|>: spine has one or more <|> operators (possibly with
      -- dictionary arguments interspersed). Collect all non-operator
      -- elements that have decode keys.
      sp@(h : _) | isAltOp h ->
        let nonOps = filter (not . isAltOp) sp
            allKeys = concatMap decodeKeysIn nonOps
        in if length nonOps >= 2 && not (null allKeys)
             then [allKeys]
             else concatMap (go (depth + 1) . peelWrap) (take 2 (drop 1 nonOps))
      -- liftA2 (<|>) e1 e2 — spine is [liftA2, <|>, e1, e2]
      (h : altOp : a1 : a2 : _) | isLiftA2 h, isAltOp altOp ->
        let keys1 = decodeKeysIn a1
            keys2 = decodeKeysIn a2
        in if not (null keys1) && not (null keys2)
             then [keys1 ++ keys2]
             else []
      -- foldr1 (liftA2 (<|>)) [e1, e2, ...] or foldr1 (<|>) [e1, e2, ...]
      -- spine is [foldr1, opStuff..., ExplicitList [...]  (with dict args)
      (h : rest) | isFoldr1 h
        , hasAltOpInSpine (h : rest)
        , listExpr <- [x | x <- rest, isExplicitList x]
        , not (null listExpr)
        -> let allKeys = concatMap (decodeKeysIn . peelWrap) (concatMap listElements listExpr)
           in if not (null allKeys) then [allKeys] else []
      -- Recurse into first two args of the spine (skip head + dict args)
      (_ : a1 : rest) -> concatMap (go (depth + 1) . peelWrap) (take 2 (a1 : rest))
      _ -> []

    isLiftA2 h = case opName h of
      Just ("liftA2", mmod) -> maybe True (\m -> "Control.Applicative" `isInfixOf` m || "GHC.Base" `isInfixOf` m) mmod
      _ -> False

    isAltOp h = case opName h of
      Just ("<|>", mmod) -> maybe True (\m -> any (`isInfixOf` m) ["Control.Applicative", "Control.Alternative", "GHC.Base"]) mmod
      _ -> False

    isFoldr1 h = case opName h of
      Just ("foldr1", _) -> True
      _ -> False

    -- The combining function of a @foldr1@ arrives as an argument, so it is an
    -- application (@liftA2 (<|>)@) rather than a bare variable.  Look at the
    -- head of each element's own spine, or @foldr1 (liftA2 (<|>)) [...]@ is
    -- never recognised as an alternation.
    hasAltOpInSpine = any (\x -> isAltOp x || isLiftA2 x || altOpUnderHead x)
    altOpUnderHead x = case appSpineTc (peelWrap x) of
      (h : args) -> isLiftA2 h && any isAltOp args
      [] -> False

    isExplicitList x = case peelWrap x of
      ExplicitList _ _ -> True
      _ -> False

    listElements x = case peelWrap x of
      ExplicitList _ xs -> map unXRecTc xs
      _ -> []

    decodeKeysIn e = extractKeys e
      where
        extractKeys expr =
          let ks = [ kiKey ki | ki <- extractKeysFromExpr Nothing DecodeSide expr ]
          in if not (null ks) then ks
             else case peelWrap expr of
               HsApp _ f a -> extractKeys (unXRecTc f) ++ extractKeys (unXRecTc a)
               PatHsPar p -> extractKeys (unXRecTc p)
               _ -> []

-- | Build a map from each key to the set of its alternative keys.
buildAltMap :: [[Text]] -> Map Text (Set Text)
buildAltMap groups =
  Map.unionsWith Set.union
    [ Map.fromList [(k, Set.fromList (filter (/= k) ks)) | k <- ks]
    | ks <- groups
    ]

-- | Extract tag values from @"tag" .= value"@ in a toJSON body.
-- This is Pattern 1 (object-based encoding), searched via 'biplateRef'
-- so it finds @"tag" .= value@ anywhere in the body, including inside
-- @object [...]@.
extractEncTagObjValues :: HsExpr GhcTc -> [Text]
extractEncTagObjValues e = case appSpineTc e of
  (h : keyArg : valArgs) | Just (occ, Just mmod) <- opName h, isAesonMod (Just mmod), occ == ".=" ->
    case keyString keyArg of
      Just k | k == "tag" -> case [v | Just v <- map keyString valArgs] of
                              (v : _) -> [v]
                              [] -> []
      _ -> []
  _ -> []

-- | Like 'extractEncTagObjValues', but returns the 'Name' of the variable in
-- the tag position rather than a literal: @"tag" .= t@ yields @t@.  Used to
-- work out which of a helper's parameters ends up as the constructor tag.
encTagValueVars :: HsExpr GhcTc -> [Name]
encTagValueVars e = case appSpineTc e of
  (h : keyArg : valArgs)
    | Just (occ, Just mmod) <- opName h
    , isAesonMod (Just mmod)
    , occ == ".="
    , Just "tag" <- keyString keyArg
    -> [ getName v | a <- valArgs, HsVar _ (L _ v) <- [peelWrap a] ]
  _ -> []

-- | Which of a same-module function's parameters are written out as a
-- constructor tag by its body.  For
--
-- @
--   mkTagged tag payload = object [ "tag" .= tag, "payload" .= payload ]
-- @
--
-- this is @[0]@, which lets 'collectEncTags' read the tag value @"A"@ out of
-- @toJSON (HiddenA x) = mkTagged "A" (toJSON x)@ -- an encoder that mentions
-- no @"tag"@ key of its own.
helperTagParamPositions :: MatchGroup GhcTc (LHsExpr GhcTc) -> [Int]
helperTagParamPositions mg = nub
  [ i
  | L _ m <- alts
  , (i, Just v) <- zip [0 ..] (matchPatVarNames m)
  , v `elem` tagVars
  ]
  where
    alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
    tagVars = concatMap (encTagValueVars . unLoc) (alts ^? biplateRef :: [LHsExpr GhcTc])

-- | The 'Name' bound by each argument pattern of a match, in order
-- ('Nothing' for anything that is not a plain variable).
matchPatVarNames :: Match GhcTc (LHsExpr GhcTc) -> [Maybe Name]
#if __GLASGOW_HASKELL__ >= 900
matchPatVarNames (Match _ _ pats _) = map (patBoundName . unXRecTc) pats
#else
matchPatVarNames (Match _ pats _) = map (patBoundName . unLoc) pats
#endif

patBoundName :: Pat GhcTc -> Maybe Name
patBoundName p = case p of
  VarPat _ (L _ v) -> Just (getName v)
  PatParPat inner -> patBoundName (unXRecTc inner)
  _ -> Nothing

-- | Extract tag values from a call to a same-module helper that writes the
-- @"tag"@ key itself: the literal sitting in a tag-carrying parameter position
-- is the constructor tag.
extractEncTagHelperValues :: String -> Map String [Int] -> HsExpr GhcTc -> [Text]
extractEncTagHelperValues currentModName helperTagPos e = case appSpineTc e of
  (h : args)
    | Just (occ, Just mmod) <- opName h
    , mmod == currentModName
    , Just poss <- Map.lookup occ helperTagPos
    -> [ v | i <- poss, i < length args, Just v <- [keyString (args !! i)] ]
  _ -> []

-- | Extract tag values from @String "VALUE"@ at the top level of a
-- toJSON match body (Pattern 2, String-based enum encoding).
-- Only checks the match body directly (not sub-expressions) to avoid
-- false positives on field values like @object ["version" .= String "v1"]@.
extractEncTagStrValues :: HsExpr GhcTc -> [Text]
extractEncTagStrValues e = case appSpineTc e of
  (h : args) | Just (occ, mmod) <- conLikeOpName h, occ == "String", isAesonMod' mmod ->
    case [v | Just v <- map keyString args] of
      (v : _) -> [v]
      [] -> []
  _ -> []

-- | Like 'opName' but for 'HsConLikeOut' (data constructor applications
-- after typechecking). Returns (occNameString, moduleString) of the
-- constructor.
conLikeOpName :: HsExpr GhcTc -> Maybe (String, Maybe String)
conLikeOpName e0 = case peelWrap e0 of
  HsConLikeOut _ cl -> Just (occNameString (nameOccName (conLikeName cl)), moduleNameString . moduleName <$> nameModule_maybe (conLikeName cl))
  _ -> Nothing

-- | Extract tag values from a pattern (string-literal patterns only).
patTag :: Pat GhcTc -> [Text]
#if __GLASGOW_HASKELL__ >= 900
patTag (NPat _ (L _ ol) _ _) = case ol of
  OverLit{ol_val = HsIsString _ fs} -> [T.pack (unpackFS fs)]
  _ -> []
patTag (LitPat _ (HsString _ fs)) = [T.pack (unpackFS fs)]
#else
patTag (NPat _ (L _ ol) _ _) = case ol of
  OverLit{ol_val = HsIsString _ fs} -> [T.pack (unpackFS fs)]
  _ -> []
patTag (LitPat _ (HsString _ fs)) = [T.pack (unpackFS fs)]
#endif
patTag _ = []

-- | Extract the constructor name from a 'ConPat' pattern (typechecked AST).
-- Peels through 'ParPat' (parenthesized patterns). Returns 'Nothing' for
-- non-constructor patterns (wildcards, variables, etc.).
patConName :: Pat GhcTc -> Maybe Text
#if __GLASGOW_HASKELL__ >= 900
patConName (PatParPat p) = patConName (unXRecTc p)
patConName (ConPat _ lcon _) = Just (T.pack (occNameString (nameOccName (conLikeName (unLoc lcon)))))
#else
patConName (PatParPat p) = patConName (unLoc p)
patConName (ConPatOut _ lcon _ _ _ _ _) = Just (T.pack (occNameString (nameOccName (conLikeName (unLoc lcon)))))
#endif
patConName _ = Nothing

-- | Check whether a pattern is a wildcard ('WildPat').
-- Peels through 'ParPat' for robustness.
isWildPat :: Pat GhcTc -> Bool
#if __GLASGOW_HASKELL__ >= 900
isWildPat (PatParPat p) = isWildPat (unXRecTc p)
#else
isWildPat (PatParPat p) = isWildPat (unLoc p)
#endif
isWildPat (WildPat _) = True
isWildPat _ = False

-- | Find the first data-constructor name in an expression (via
-- 'HsConLikeOut').  Uses 'appSpineTc' (not @biplateRef@) because
-- @WrapExpr@ is opaque to Uniplate after typechecking.
-- Returns 'Nothing' when the RHS is @fail ...@ or otherwise contains
-- no constructor application.
rhsConName :: HsExpr GhcTc -> Maybe Text
rhsConName e = listToMaybe
  [ T.pack occ
  | h <- appSpineTc e
  , Just (occ, _) <- [conLikeOpName h]
  ]

-- | Get the body expression of a 'Match' (first GRHS, unguarded).
matchBody :: Match GhcTc (LHsExpr GhcTc) -> Maybe (HsExpr GhcTc)
#if __GLASGOW_HASKELL__ >= 900
matchBody (Match _ _ _ grhss) =
#else
matchBody (Match _ _ grhss) =
#endif
  case grhssGRHSs grhss of
    (L _ (GRHS _ _ body) : _) -> Just (unLoc body)
    [] -> Nothing

-- | Like 'matchBody' but returns the located expression (for biplateRef).
matchBodyL :: Match GhcTc (LHsExpr GhcTc) -> Maybe (LHsExpr GhcTc)
#if __GLASGOW_HASKELL__ >= 900
matchBodyL (Match _ _ _ grhss) =
#else
matchBodyL (Match _ _ grhss) =
#endif
  case grhssGRHSs grhss of
    (L _ (GRHS _ _ body) : _) -> Just body
    [] -> Nothing

-- | Walk an encoder body expression collecting JSON keys.
-- Descends into lambdas (to catch conditional keys like
-- @maybe [] (\fee -> ["flat_fee" .= fee]) val@), but when inside a lambda,
-- does NOT descend into 'ExplicitList' (to avoid false positives from
-- sub-object keys in @map (\(k,v) -> object ["key" .= ...])@).
walkEncKeys :: Maybe String -> HsExpr GhcTc -> [KeyInfo]
walkEncKeys mCurrentMod e0 = go False e0
  where
    go inLambda e =
      extractKeysFromExpr mCurrentMod EncodeSide e
      ++ case peelWrap e of
           HsLam _ mg ->
             let alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
             in concat [ go True body | L _ m <- alts, Just body <- [matchBody m] ]
           HsApp _ f a -> if inLambda then [] else go False (unXRecTc f) ++ go False (unXRecTc a)
           PatHsPar p -> go inLambda (unXRecTc p)
#if __GLASGOW_HASKELL__ >= 900
           ExplicitList _ xs -> if inLambda
                                  then concatMap (extractKeysFromExpr mCurrentMod EncodeSide . unXRecTc) xs
                                  else concatMap (go False . unXRecTc) xs
           HsIf _ a b c -> go False (unXRecTc a) ++ go False (unXRecTc b) ++ go False (unXRecTc c)
           HsDo _ _ stmts -> if inLambda then [] else concatMap (stmtKeys . unXRecTc) (unXRecTc stmts)
#else
           ExplicitList _ xs -> if inLambda
                                  then concatMap (extractKeysFromExpr mCurrentMod EncodeSide . unLoc) xs
                                  else concatMap (go False . unLoc) xs
           HsIf _ a b c -> go False (unLoc a) ++ go False (unLoc b) ++ maybe [] (go False . unLoc) c
           HsDo _ stmts -> if inLambda then [] else concatMap (stmtKeys . unLoc) stmts
#endif
           HsCase _ _ mg ->
             let alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
             in concat [ go inLambda body | L _ m <- alts, Just body <- [matchBody m] ]
           _ -> []

    stmtKeys (BodyStmt _ e _ _) = go False (unXRecTc e)
    stmtKeys _ = []

-- | Walk a decoder body expression collecting JSON keys.
-- Similar to 'walkEncKeys' but for 'DecodeSide': descends into
-- lambdas, do-blocks (including 'BindStmt'), case alternatives,
-- and if-then-else.  Does NOT descend into @where@-clause helpers
-- (those are local definitions, not part of the match body).
walkDecKeys :: Maybe String -> HsExpr GhcTc -> [KeyInfo]
walkDecKeys mCurrentMod e0 = go e0
  where
    go e =
      extractKeysFromExpr mCurrentMod DecodeSide e
      ++ case peelWrap e of
           HsLam _ mg ->
             let alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
             in concat [ go body | L _ m <- alts, Just body <- [matchBody m] ]
           HsApp _ f a -> go (unXRecTc f) ++ go (unXRecTc a)
           PatHsPar p -> go (unXRecTc p)
           OpApp _ a _ b -> go (unXRecTc a) ++ go (unXRecTc b)
           PatHsLet _ body -> go (unXRecTc body)
#if __GLASGOW_HASKELL__ >= 900
           HsIf _ a b c -> go (unXRecTc a) ++ go (unXRecTc b) ++ go (unXRecTc c)
           HsDo _ _ stmts -> concatMap (stmtKeys . unXRecTc) (unXRecTc stmts)
#else
           HsIf _ a b c -> go (unLoc a) ++ go (unLoc b) ++ maybe [] (go . unLoc) c
           HsDo _ stmts -> concatMap (stmtKeys . unLoc) stmts
#endif
           HsCase _ _ mg ->
             let alts = unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])
             in concat [ go body | L _ m <- alts, Just body <- [matchBody m] ]
           _ -> []
    stmtKeys (BodyStmt _ e _ _) = go (unXRecTc e)
    stmtKeys (BindStmt _ _ e) = go (unXRecTc e)
    stmtKeys _ = []

-- | Extract encoder keys from @where@-clause helper functions.
-- Helpers like @source' (Just v) = ["source" .= v]@ produce @.=@ calls
-- with string-literal keys that are invisible to 'walkEncKeys' because
-- they're in the @where@ binds, not the match body.
-- Uses 'biplateRef' on the local binds to find all expressions, then
-- calls 'extractKeysFromExpr' on each to find @.=@ calls.
whereClauseKeys :: Maybe String -> Match GhcTc (LHsExpr GhcTc) -> [KeyInfo]
whereClauseKeys mCurrentMod m =
  let lbs = grhssLocalBinds (matchGRHSs m)
  in [ k | L _ sub <- lbs ^? biplateRef :: [LHsExpr GhcTc]
         , ki <- extractKeysFromExpr mCurrentMod EncodeSide sub
         , let k = ki
     ]

-- | Extract decoder keys from @where@-clause helper functions.
-- Extracts keys from Aeson operators (@.:@, @.:?@, @.:!@, @.:|@), KeyMap
-- lookups, AND local helper calls with 2+ arguments where the first arg is a
-- variable (e.g., @parseNonEmptyField o "key"@).  This avoids false positives
-- like @missingFieldError "insertId"@ (single string arg, no Object).
whereClauseDecKeys :: Maybe String -> Match GhcTc (LHsExpr GhcTc) -> [KeyInfo]
whereClauseDecKeys mCurrentMod m =
  let lbs = grhssLocalBinds (matchGRHSs m)
  in [ k | L _ sub <- lbs ^? biplateRef :: [LHsExpr GhcTc]
         , ki <- extractWhereClauseDecKeys mCurrentMod sub
         , let k = ki
     ]

-- | Extract decoder keys from a where-clause expression.  Recognises:
-- 1. Aeson operators (@.:@, @.:?@, @.:!@, @.:|@) with string-literal keys
-- 2. KeyMap lookups (@AKM.lookup (AK.fromText "key")@)
-- 3. Local/same-module helper calls with 2+ args where the first arg is a
--    variable (likely an Object), e.g. @parseNonEmptyField o "key"@
-- 4. Cross-module helper calls with 2+ args where the second arg is a
--    variable, e.g. @readNumberAsMoney "amount" o@
extractWhereClauseDecKeys :: Maybe String -> HsExpr GhcTc -> [KeyInfo]
extractWhereClauseDecKeys mCurrentMod e0 = case appSpineTc e0 of
  (h : args) | Just (occ, mmod) <- opName h, occ == "lookup", isAesonMod mmod ->
    case [k | Just k <- map deepKeyString args] of
      (k : _) -> [KeyInfo k Nothing True]
      [] -> []
  (h : args) | Just (occ, Just mmod) <- opName h, isAesonMod (Just mmod)
            , occ `elem` [".:", ".:?", ".:!", ".:|"] ->
    let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
    in [KeyInfo k Nothing (occ `elem` [".:?",".:!",".:|"]) | k <- strArgs]
  (h : args) | Just (_, mmod) <- opName h
            , isLocalHelper mmod
            , length args >= 2
            , isVarArg (head args)
            -> let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
               in [KeyInfo k Nothing False | k <- strArgs]
  (h : args) | Just (_, mmod) <- opName h
            , not (isAesonMod mmod)
            , not (isTextUtilMod mmod)
            , not (isLocalHelper mmod)
            , length args >= 2
            , isVarArg (args !! 1)
            -> let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
               in [KeyInfo k Nothing False | k <- strArgs]
  _ -> []
  where
    isLocalHelper Nothing = True
    isLocalHelper (Just m) = maybe False (== m) mCurrentMod

-- | Extract keys from where-clause expressions that are likely false
-- positives: local/same-module helper calls with fewer than 2 arguments
-- or where the first arg is not a variable.  These are things like
-- @missingFieldError "insertId"@ (1 string arg, no Object) that are NOT
-- JSON key reads but get picked up by 'extractKeysFromExpr'.
extractBadWhereKeys :: Maybe String -> HsExpr GhcTc -> [KeyInfo]
extractBadWhereKeys mCurrentMod e0 = case appSpineTc e0 of
  (h : args) | Just (_, mmod) <- opName h
            , isLocalHelper mmod
            , not (length args >= 2 && isVarArg (head args))
            -> let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
               in [KeyInfo k Nothing False | k <- strArgs]
  _ -> []
  where
    isLocalHelper Nothing = True
    isLocalHelper (Just m) = maybe False (== m) mCurrentMod
-- (e.g., @parseJSON obj = (JweDeserialized <$> parseJSON obj) <|> ...@).
-- When the decoder delegates and has no @.:@/@.:?@ calls, the key set is
-- unknown (not empty).
hasDecDelegation :: HsExpr GhcTc -> Bool
hasDecDelegation e = case appSpineTc e of
  (h : _) | Just (occ, mmod) <- opName h, occ == "parseJSON", isAesonMod' mmod -> True
  _ -> case peelWrap e of
    HsApp _ f a -> hasDecDelegation (unXRecTc f) || hasDecDelegation (unXRecTc a)
    PatHsPar p -> hasDecDelegation (unXRecTc p)
    _ -> False

-- | Check whether an encoder body uses non-standard JSON construction
-- (e.g. @toJSON someValue@, @Object (insert ...)@) that produces keys
-- the plugin cannot statically extract.  When the encoder has zero @.=@
-- calls but uses these constructs, the key set is /unknown/ (not empty).
--
-- 'hasDelegation' fires for @toJSON@ applied to a variable/constructor
-- (delegation to another type's 'ToJSON'), NOT for @toJSON@ applied to a
-- list/tuple (value encoding that produces no keyed object).
--
-- 'hasObjectConstruction' fires for the @Object@ constructor from aeson.
--
-- Both recursively descend into 'HsLet', 'HsCase', 'HsApp', and 'HsPar'.
hasDelegation :: HsExpr GhcTc -> Bool
hasDelegation = hasNonStd checkDelegation
  where
    checkDelegation e = any check (appSpineTc e)
      where
        check h = case opName h of
          Just (occ, mmod) | occ `elem` ["toJSON", "toEncoding"]
                          , not (occ `elem` ["genericToJSON", "genericToEncoding"])
                          , isAesonMod' mmod ->
              case drop 1 (appSpineTc e) of
                (arg : _) -> isDelegation arg
                [] -> False
          _ -> False
    isDelegation arg = case peelWrap arg of
      HsVar _ _ -> True
      HsConLikeOut _ _ -> True
      PatHsPar p -> isDelegation (unXRecTc p)
      _ -> False

hasObjectConstruction :: HsExpr GhcTc -> Bool
hasObjectConstruction = hasNonStd checkObject
  where
    checkObject e = any check (appSpineTc e)
      where
        check h = case conLikeOpName h of
          Just ("Object", mmod) -> isAesonMod' mmod
          _ -> False

hasNonStdJSON :: HsExpr GhcTc -> Bool
hasNonStdJSON e = hasDelegation e || hasObjectConstruction e

-- | Detect composition with a recognized generic encoding/decoding function
-- where the OTHER side is a key-transforming function, e.g.:
--   camelCaseToSnakeCase <<< defaultEncode
--   snakeCaseToCamelCase >>> defaultDecode
-- In these cases the actual JSON keys are transformed by the outer function
-- and cannot be statically determined from the field labels alone.
hasCompositionGeneric :: HsExpr GhcTc -> Bool
hasCompositionGeneric e0 = case appSpineTc (peelWrap e0) of
  (h : args) | Just (occ, _) <- opName h
             , occ `elem` ["<<<", "."]
             , (_ : inner : _) <- args
             -> isGenericFn (peelWrap inner)
  (h : args) | Just (occ, _) <- opName h
             , occ `elem` [">>>"]
             , (inner : _) <- args
             -> isGenericFn (peelWrap inner)
  _ -> False
  where
    isGenericFn expr = case appSpineTc expr of
      (h' : _) | Just (occ', _) <- opName h' ->
        occ' `elem` ["genericToJSON","genericToEncoding","genericParseJSON",
                     "defaultEncode","defaultEncodeJSON","defaultDecode","defaultDecodeJSON",
                     "defaultEncodeOmitNothingOpts",
                     "genericEncode","genericDecode",
                     "genericEncodeModel","genericDecodeModel",
                     "genericEncodeJSON","genericDecodeJSON"]
      _ -> False

-- Shared recursive walker for the above checks.
hasNonStd :: (HsExpr GhcTc -> Bool) -> HsExpr GhcTc -> Bool
hasNonStd check e = check e || go (peelWrap e)
  where
    go expr = check expr || case peelWrap expr of
#if __GLASGOW_HASKELL__ >= 900
      PatHsLet lbs body -> any go (map unLoc (lbs ^? biplateRef :: [LHsExpr GhcTc])) || go (unXRecTc body)
      HsCase _ _ mg -> any (maybe False go . matchBody)
                          (map unLoc (unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])))
#else
      PatHsLet lbs body -> any go (map unLoc (lbs ^? biplateRef :: [LHsExpr GhcTc])) || go (unLoc body)
      HsCase _ _ mg -> any (maybe False go . matchBody)
                          (map unLoc (unLoc (mg_alts mg)))
#endif
      HsApp _ f a -> go (unXRecTc f) || go (unXRecTc a)
      PatHsPar p -> go (unXRecTc p)
      _ -> False

-- | Find the type key of the delegated type in a @toJSON <var>@ encoder body.
-- Walks the body (including let/case) looking for @toJSON@ applied to a
-- variable, then extracts the variable's type and converts to a type key.
-- Returns 'Nothing' if no such delegation is found.
findDelegatedTypeKey :: [LMatch GhcTc (LHsExpr GhcTc)] -> Maybe Text
findDelegatedTypeKey alts = listToMaybe
  [ delegatedKey
  | L _ m <- matchAlts
  , Just body <- [matchBody m]
  , Just delegatedKey <- [findInExpr body]
  ]
  where
    matchAlts = alts
    findInExpr e = case appSpineTc e of
      (h : args) | Just (occ, mmod) <- opName h, occ `elem` ["toJSON", "toEncoding"]
                 , isAesonMod' mmod ->
          case filter (not . isDict) args of
            (arg : _) -> case peelWrap arg of
              HsVar _ (L _ v) -> Just (keyOfType (idType v))
              _ -> Nothing
            [] -> Nothing
      _ -> go (peelWrap e)
    isDict _ = False  -- We can't easily distinguish dict args, so we try all non-head args

#if __GLASGOW_HASKELL__ >= 900
    go (PatHsLet lbs body) = case [ findInExpr sub | L _ sub <- lbs ^? biplateRef :: [LHsExpr GhcTc] ] ++ [findInExpr (unXRecTc body)] of
                              (Just k : _) -> Just k
                              _ -> Nothing
    go (HsCase _ _ mg) = listToMaybe [ k | L _ m <- unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)]), Just body <- [matchBody m], Just k <- [findInExpr body] ]
#else
    go (PatHsLet lbs body) = case [ findInExpr sub | L _ sub <- lbs ^? biplateRef :: [LHsExpr GhcTc] ] ++ [findInExpr (unLoc body)] of
                              (Just k : _) -> Just k
                              _ -> Nothing
    go (HsCase _ _ mg) = listToMaybe [ k | L _ m <- unLoc (mg_alts mg), Just body <- [matchBody m], Just k <- [findInExpr body] ]
#endif
    go (HsApp _ f a) = findInExpr (unXRecTc f) <|> findInExpr (unXRecTc a)
    go (PatHsPar p) = findInExpr (unXRecTc p)
    go _ = Nothing

-- | Collect encoder tag data from toJSON match alternatives.
-- Returns (tag values, constructor→tag map, all encoder constructors).
-- Pattern 1 (@"tag" .= value@): searched via 'biplateRef' anywhere in body.
-- Pattern 2 (@String "VALUE"@): only from match body directly (not sub-expressions).
-- Pattern 3 (@mkTagged "A" ...@): a call to a same-module helper that writes
-- the @"tag"@ key itself, with the tag value passed in as an argument.
collectEncTags :: String -> Map String [Int] -> [LMatch GhcTc (LHsExpr GhcTc)] -> (Set Text, Map Text Text, Set Text)
collectEncTags currentModName helperTagPos alts =
  let conTagPairs =
        [ (conName, tagVal)
        | alt <- alts
        , let m = unLoc alt
#if __GLASGOW_HASKELL__ >= 900
        , (Match _ _ pats _) <- [m]
#else
        , (Match _ pats _) <- [m]
#endif
        , let ps =
#if __GLASGOW_HASKELL__ >= 900
                  map unXRecTc pats
#else
                  map unLoc pats
#endif
        , not (null ps)
        , Just conName <- [patConName (head ps)]
        , tagVal <- concatMap (extractEncTagObjValues . unLoc) ([alt] ^? biplateRef :: [LHsExpr GhcTc])
                  ++ concatMap (extractEncTagHelperValues currentModName helperTagPos . unLoc) ([alt] ^? biplateRef :: [LHsExpr GhcTc])
                  ++ case matchBody m of
                       Just body -> extractEncTagStrValues body
                       Nothing -> []
        ]
      encConToTag = Map.fromList conTagPairs
      encTags = Set.fromList (Map.elems encConToTag)
      encCons = Set.fromList
        [ conName
        | alt <- alts
        , let m = unLoc alt
#if __GLASGOW_HASKELL__ >= 900
        , (Match _ _ pats _) <- [m]
#else
        , (Match _ pats _) <- [m]
#endif
        , let ps =
#if __GLASGOW_HASKELL__ >= 900
                  map unXRecTc pats
#else
                  map unLoc pats
#endif
        , not (null ps)
        , Just conName <- [patConName (head ps)]
        ]
  in (encTags, encConToTag, encCons)

-- | Collect decoder tag data from parseJSON body expressions.
-- Returns (tag values, catch-all constructor name if @_ -> Constructor@).
collectDecTags :: [LHsExpr GhcTc] -> (Set Text, Maybe Text)
collectDecTags exprs =
  let allAlts = concatMap (decCaseAlts . unLoc) exprs
      decTagSet = Set.fromList (concatMap fst allAlts)
      mCatchAll = listToMaybe [con | (_, Just con) <- allAlts]
  in (decTagSet, mCatchAll)
  where
    decCaseAlts :: HsExpr GhcTc -> [([Text], Maybe Text)]
    decCaseAlts e0 = case peelWrap e0 of
#if __GLASGOW_HASKELL__ >= 900
      HsCase _ _ mg -> map decAltData (map unLoc (unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])))
      PatHsLamCase mg -> map decAltData (map unLoc (unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])))
#else
      HsCase _ _ mg -> map decAltData (map unLoc (unLoc (mg_alts mg)))
      PatHsLamCase mg -> map decAltData (map unLoc (unLoc (mg_alts mg)))
#endif
      _ -> []

    decAltData :: Match GhcTc (LHsExpr GhcTc) -> ([Text], Maybe Text)
#if __GLASGOW_HASKELL__ >= 900
    decAltData m@(Match _ _ pats _) =
#else
    decAltData m@(Match _ pats _) =
#endif
      let ps =
#if __GLASGOW_HASKELL__ >= 900
              map unXRecTc pats
#else
              map unLoc pats
#endif
          tags = concatMap patTag ps
          mCatchCon = case ps of
            (pat0 : _) | isWildPat pat0 ->
              case matchBody m of
                Just body -> rhsConName body
                Nothing -> Nothing
            _ -> Nothing
      in (tags, mCatchCon)

-- | Collect decoder tag data from parseJSON's match-level patterns.
-- Handles direct pattern matching like @parseJSON (String "GPMF") = pure GPMF@
-- where the string value is in the function argument pattern, not in a @case@
-- expression inside the body.
-- Returns (tag values, catch-all constructor name if @_ -> Constructor@).
collectDecMatchTags :: [LMatch GhcTc (LHsExpr GhcTc)] -> (Set Text, Maybe Text)
collectDecMatchTags alts =
  let results = map (decMatchAltData . unLoc) alts
      decTagSet = Set.fromList (concatMap fst results)
      mCatchAll = listToMaybe [con | (_, Just con) <- results]
  in (decTagSet, mCatchAll)
  where
    decMatchAltData :: Match GhcTc (LHsExpr GhcTc) -> ([Text], Maybe Text)
#if __GLASGOW_HASKELL__ >= 900
    decMatchAltData m@(Match _ _ pats _) =
#else
    decMatchAltData m@(Match _ pats _) =
#endif
      let ps =
#if __GLASGOW_HASKELL__ >= 900
              map unXRecTc pats
#else
              map unLoc pats
#endif
          patTags = concatMap conPatStringTag ps
          guardTags = concatMap (grhsGuardStrings . unLoc) (grhssGRHSs (matchGRHSs m))
          mCatchCon = case ps of
            (pat0 : _) | isWildPat pat0 ->
              case matchBody m of
                Just body -> rhsConName body
                Nothing -> Nothing
            _ -> Nothing
      in (patTags ++ guardTags, mCatchCon)

-- | Collect decoder constructor names from parseJSON's match alternatives.
-- Uses 'biplateRef' to find all sub-expressions, then checks each one's
-- 'appSpineTc' head for 'HsConLikeOut' (which 'biplateRef' can't find directly
-- because 'WrapExpr' is opaque to Uniplate, but 'appSpineTc' peels it).
collectDecCons :: String -> [LMatch GhcTc (LHsExpr GhcTc)] -> Set Text
collectDecCons currentModName alts =
  let exprs = alts ^? biplateRef :: [LHsExpr GhcTc]
      cons = Set.fromList
        [ T.pack occ
        | L _ sub <- exprs
        , h <- take 1 (appSpineTc sub)
        , Just (occ, _) <- [conLikeOpName h]
        ]
  in if any (hasLocalFuncCall currentModName) exprs then Set.empty else cons

-- | Check if an expression is a call to a same-module function
-- (excluding 'parseJSON' itself).  Used to detect decoder delegation
-- to local helpers like @parsePaymentMethodInfo@.
hasLocalFuncCall :: String -> LHsExpr GhcTc -> Bool
hasLocalFuncCall currentModName (L _ e) = case appSpineTc e of
  (h' : _) | Just (occ', mmod') <- opName h' ->
    case mmod' of
      Just m -> m == currentModName && occ' /= "parseJSON"
      Nothing -> False
  _ -> False

-- | Extract string literals from a GRHS's guard statements.
-- Catches strings used in guard conditions like @| map toUpper s == "INFINITY"@.
grhsGuardStrings :: GRHS GhcTc (LHsExpr GhcTc) -> [Text]
#if __GLASGOW_HASKELL__ >= 900
grhsGuardStrings (GRHS _ guards _) =
#else
grhsGuardStrings (GRHS _ guards _) =
#endif
  concatMap (stmtStrings . unLoc) guards
  where
    stmtStrings (BodyStmt _ e _ _) = exprStrings (unXRecTc e)
    stmtStrings _ = []
    exprStrings e = case peelWrap e of
      HsLit _ (HsString _ fs) -> [T.pack (unpackFS fs)]
      HsOverLit _ OverLit{ol_val = HsIsString _ fs} -> [T.pack (unpackFS fs)]
      HsApp _ f a -> exprStrings (unXRecTc f) ++ exprStrings (unXRecTc a)
      PatHsPar p -> exprStrings (unXRecTc p)
      _ -> []

-- | Get the GRHSs from a Match.
matchGRHSs :: Match GhcTc (LHsExpr GhcTc) -> GRHSs GhcTc (LHsExpr GhcTc)
#if __GLASGOW_HASKELL__ >= 900
matchGRHSs (Match _ _ _ grhss) = grhss
#else
matchGRHSs (Match _ _ grhss) = grhss
#endif

-- | Extract a string tag from a @String "VALUE"@ constructor pattern.
-- Peels through 'ParPat'. Returns @[]@ for non-@String@ constructor patterns
-- or when the argument is not a string literal.
conPatStringTag :: Pat GhcTc -> [Text]
#if __GLASGOW_HASKELL__ >= 900
conPatStringTag (PatParPat p) = conPatStringTag (unXRecTc p)
conPatStringTag (ConPat _ lcon details)
#else
conPatStringTag (PatParPat p) = conPatStringTag (unLoc p)
conPatStringTag (ConPatOut _ lcon _ _ _ _ details)
#endif
  | Just ("String", mmod) <- conLikeInfo
  , isAesonMod' mmod
  = case details of
#if __GLASGOW_HASKELL__ >= 900
      PrefixCon _ args -> concatMap (patTag . unXRecTc) args
      InfixCon a1 a2 -> concatMap (patTag . unXRecTc) [a1, a2]
#else
      PrefixCon args -> concatMap (patTag . unLoc) args
      InfixCon a1 a2 -> concatMap (patTag . unLoc) [a1, a2]
#endif
      _ -> []
  | otherwise = []
  where
    conLikeInfo = case
#if __GLASGOW_HASKELL__ >= 900
      conLikeName (unLoc lcon)
#else
      conLikeName (unLoc lcon)
#endif
      of n -> Just (occNameString (nameOccName n), moduleNameString . moduleName <$> nameModule_maybe n)
conPatStringTag p@(NPat _ _ _ _) = patTag p
conPatStringTag p@(LitPat _ _) = patTag p
conPatStringTag _ = []

-- Walk an expression collecting JSON keys. On the typechecked AST, aeson
-- operators like @.=@/@.:@ are /applications/ (with dictionary arguments), so
-- we flatten the left-nested @HsApp@ spine and take the string-literal
-- argument as the JSON key.
--
-- We also handle /local helpers/ (e.g. @toE "key" val@ defined in a @where@
-- clause): when the head of the spine is a local variable (no associated
-- module), we collect its string-literal arguments as keys. This is generic
-- and catches any local function that wraps @.=@/@.:@ without hard-coding
-- specific names. Imported functions are excluded (they have modules).
-- Top-level functions defined in the /same module/ are also treated as local
-- helpers (their module matches @currentMod@).
extractKeysFromExpr :: Maybe String -> Side -> HsExpr GhcTc -> [KeyInfo]
extractKeysFromExpr mCurrentMod side e0 = case appSpineTc e0 of
  -- Aeson KeyMap lookup: AKM.lookup (AK.fromText "key") o (decode side).
  -- The key is inside a fromText wrapper, so we use deepKeyString.
  (h : args) | Just (occ, mmod) <- opName h, occ == "lookup", isAesonMod mmod, side == DecodeSide ->
    case [k | Just k <- map deepKeyString args] of
      (k : _) -> [KeyInfo k Nothing True]
      [] -> []
  -- Known aeson operators: .= (encode), .:/.:?/.:!/.:| (decode)
  (h : args) | Just (occ, Just mmod) <- opName h, isAesonMod (Just mmod) ->
    let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
    in if occ == ".=" && side == EncodeSide
         then [KeyInfo k Nothing False | k <- strArgs]
         else if occ `elem` [".:", ".:?", ".:!", ".:|"] && side == DecodeSide
           then [KeyInfo k Nothing (occ `elem` [".:?",".:!",".:|"]) | k <- strArgs]
           else []
  -- Local helpers (no module) or same-module top-level functions:
  -- collect string-literal arguments as keys.
  -- Also recognize cross-module helper functions on the decoder side
  -- that take a string key and an Object argument (e.g. readNumberAsMoney "amount" o).
  (h : args) | Just (occ, mmod) <- opName h
             , not (occ `elem` ["<>", "$", ".", "<<<", ">>>", "<|>", ">>=", ">>", "=<<", "++", ">", "<", ">=", "<=", "==", "/=", "&&", "||", "*"])
             , isWhereBoundHelper mmod
               || (isSameModuleHelper mmod && takesObjectArg args)
               || (not (isAesonMod mmod) && not (isTextUtilMod mmod) && length args >= 2 && objectArg (args !! 1))
             -> let strArgs = case [k | Just k <- map keyString args] of (k:_) -> [k]; [] -> []
                in [KeyInfo k Nothing False | k <- strArgs]
  -- Tuple syntax in encoder: ("key", value) inside object [...]
  [e] | side == EncodeSide, Just k <- tupleFirstKey e -> [KeyInfo k Nothing False]
  _ -> []
  where
    -- A @where@- or lambda-bound helper has no module.  These are written right
    -- next to the object they build (@toE "key" val@), so a string literal in
    -- one really is a key.
    isWhereBoundHelper Nothing = True
    isWhereBoundHelper (Just _) = False
    isSameModuleHelper Nothing = False
    isSameModuleHelper (Just m) = maybe False (== m) mCurrentMod
    -- A top-level helper is only reading or writing a JSON key if it is also
    -- handed the object (or record) to work on.  @mkTagged "A" (toJSON x)@ is
    -- passing a constructor tag *value*, not a key, and reading it as a key
    -- invents encoder keys that no decoder will ever ask for.
    takesObjectArg as = length as >= 2 && (objectArg (head as) || objectArg (as !! 1))
    -- On the decode side the companion argument really is the JSON object, and
    -- we can check that from its type.  On the encode side a helper is handed
    -- the record or the value itself, so any variable will do.
    objectArg = case side of
      DecodeSide -> isJsonObjectArg
      EncodeSide -> isVarArg

-- | Check if an expression is a variable reference (HsVar).
isVarArg :: HsExpr GhcTc -> Bool
isVarArg arg = case peelWrap arg of
  HsVar _ _ -> True
  _ -> False

-- | Is this argument the JSON object being read?  A decoder helper that pulls a
-- key out is always handed the 'Object'\/'Value'.  Without this test, any
-- two-argument call whose first argument is a string literal looks like a key
-- read -- @fromMaybe "request_failed" code@ being the classic false positive,
-- where the literal is a default *value*.
isJsonObjectArg :: HsExpr GhcTc -> Bool
isJsonObjectArg arg = case peelWrap arg of
  HsVar _ (L _ v) -> keyOfType (idType v) `elem` ["Object", "Value", "KeyMap", "Array"]
  _ -> False

-- | Keys read in an @\<|\>@ alternation where /another branch reads no key at
-- all/ -- @o .: "DataSet" \<|\> pure o@, or
-- @parseFields v \<|\> withObject "Wrapped" (\\o -> o .: "contents" >>= parseFields) v@.
-- The parse succeeds through the keyless branch, so the encoder is under no
-- obligation to write them; they are envelopes, not data.
--
-- Note this deliberately does /not/ cover an alternation between two keys
-- (@o .:? "a" \<|\> o .:? "b"@).  There one of the two really must be written,
-- and 'buildAltMap' is what decides whether the encoder wrote either.
defaultedAltKeys :: Maybe String -> [LHsExpr GhcTc] -> Set Text
defaultedAltKeys mCurrentMod es = Set.fromList
  [ k
  | L _ e <- es
  , Just branches <- [altBranches (peelWrap e)]
  , let branchKeys = map deepKeys branches
  , any null branchKeys          -- some branch parses without reading anything
  , any (not . null) branchKeys  -- ...and some other branch reads a key
  , k <- concat branchKeys
  ]
  where
    -- The branches of an alternation, if this expression is one.  Dictionary
    -- arguments ride in the wrapper rather than the spine, so what is left is
    -- exactly the operands.
    altBranches e = case appSpineTc e of
      sp@(h : _) | isAlt h -> case filter (not . isAlt) sp of
                                bs | length bs >= 2 -> Just bs
                                _ -> Nothing
      (h : altOp : a1 : a2 : _) | isLiftA2Head h, isAlt altOp -> Just [a1, a2]
      _ -> Nothing
    isAlt x = spineHeadName x == Just "<|>"
    isLiftA2Head x = spineHeadName x == Just "liftA2"
    -- 'biplateRef' yields a node's children, so the branch expression itself
    -- has to be included or a branch that /is/ the key read is seen as empty.
    deepKeys b = [ kiKey k
                 | sub <- b : [ s | L _ s <- (b ^? biplateRef :: [LHsExpr GhcTc]) ]
                 , k <- extractKeysFromExpr mCurrentMod DecodeSide sub
                 ]

-- | The name at the head of an expression's application spine, so that an
-- operator passed as an argument (@liftA2 (\<|\>)@, @foldr1 (liftA2 (\<|\>))@)
-- is still recognised.
spineHeadName :: HsExpr GhcTc -> Maybe String
spineHeadName e = case appSpineTc (peelWrap e) of
  (h : _) -> fst <$> opName h
  [] -> Nothing

-- | Check if a module is a text utility module (Data.Text, Text.Read,
-- Data.List, etc.) whose functions' string arguments are prefixes,
-- patterns, or delimiters — NOT JSON keys.
isTextUtilMod :: Maybe String -> Bool
isTextUtilMod Nothing = False
isTextUtilMod (Just m) = any (`isInfixOf` m)
  ["Data.Text", "Text.Read", "Data.List", "Data.Char", "Data.String", "Data.Time", "Data.Semigroup"]

-- | Like 'keyString' but also unwraps @fromText "key"@ applications.
-- Used to recognise keys passed as @AK.fromText "key"@ to 'lookup'.
deepKeyString :: HsExpr GhcTc -> Maybe Text
deepKeyString e = case keyString e of
  Just k -> Just k
  Nothing -> case appSpineTc e of
    (h : args) | Just (occ, mmod) <- opName h
               , occ == "fromText"
               , isAesonMod mmod
               , (arg : _) <- args
               -> keyString arg
    _ -> Nothing

-- Flatten a left-nested @HsApp@ into @[head, arg1, arg2, ...]@, peeling
-- typechecker wrappers at every step. The result elements are unwrapped
-- @HsExpr GhcTc@ values.
appSpineTc :: HsExpr GhcTc -> [HsExpr GhcTc]
appSpineTc e0 = go (peelWrap e0)
  where
    go (HsApp _ f a) = go (peelWrap (unXRecTc f)) ++ [unXRecTc a]
    go e = [e]

-- | Peel typechecker wrappers (@HsWrap@) so we can pattern match the
-- underlying expression. Representation differs between GHC 9 and 8.
#if __GLASGOW_HASKELL__ >= 900
peelWrap :: HsExpr GhcTc -> HsExpr GhcTc
peelWrap (XExpr (WrapExpr (HsWrap _ e))) = peelWrap e
peelWrap (XExpr (ExpansionExpr (HsExpanded _ e))) = peelWrap e
peelWrap (PatHsPar e) = peelWrap (unXRecTc e)
peelWrap (ExprWithTySig _ e _) = peelWrap (unXRecTc e)
peelWrap e = e
#else
peelWrap :: HsExpr GhcTc -> HsExpr GhcTc
peelWrap (HsWrap _ _ e) = peelWrap e
peelWrap (ExprWithTySig _ e _) = peelWrap (unLoc e)
peelWrap e = e
#endif

opName :: HsExpr GhcTc -> Maybe (String, Maybe String)
opName e0 = case peelWrap e0 of
  HsVar _ (L _ v) ->
    let n = getName v in Just (occNameString (nameOccName n), moduleNameString . moduleName <$> nameModule_maybe n)
  _ -> Nothing

isAesonMod :: Maybe String -> Bool
isAesonMod Nothing = True
isAesonMod (Just m) = "Aeson" `isInfixOf` m

keyString :: HsExpr GhcTc -> Maybe Text
keyString e0 = case peelWrap e0 of
  HsLit _ (HsString _ fs) -> Just (T.pack (unpackFS fs))
  HsOverLit _ ol -> case ol of
    OverLit{ol_val = HsIsString _ fs} -> Just (T.pack (unpackFS fs))
    _ -> Nothing
  _ -> Nothing

-- | Extract the first element of a tuple expression @("key", value)@
-- as a 'Text' key. Returns 'Nothing' if the expression is not a tuple
-- or the first element is not a string literal.
tupleFirstKey :: HsExpr GhcTc -> Maybe Text
tupleFirstKey e0 = case peelWrap e0 of
  ExplicitTuple _ args _ -> case args of
    (Present _ e : _) -> keyString (unXRecTc e)
    _ -> Nothing
  _ -> Nothing

-- | Detect calls to @genericToJSON@/@@genericToEncoding@/@@genericParseJSON@ in
-- the method body and return the pretty-printed 'Options' argument (if any).
-- Returns @Nothing@ if no generic-deriving function is called.
-- We can't use @biplateRef@ because @WrapExpr@ (an @XXExpr@ extension) is
-- opaque to Uniplate.  Instead, we walk the match RHSs manually.
detectGenericDeriving :: [LMatch GhcTc (LHsExpr GhcTc)] -> Maybe Text
detectGenericDeriving matches =
  case [opts | m <- matches, Just opts <- [checkMatch m]] of
    (opts : _) -> Just opts
    []         -> Nothing
  where
    checkMatch :: LMatch GhcTc (LHsExpr GhcTc) -> Maybe Text
#if __GLASGOW_HASKELL__ >= 900
    checkMatch m = case unLoc m of
      Match _ _ _ (GRHSs _ grhss _) -> case grhss of
        (L _ (GRHS _ _ body) : _) -> findGenericCall (unLoc body)
        [] -> Nothing
      _ -> Nothing
#else
    checkMatch m = case unLoc m of
      Match _ _ (GRHSs _ grhss _) -> case grhss of
        (L _ (GRHS _ _ body) : _) -> findGenericCall (unLoc body)
        [] -> Nothing
      _ -> Nothing
#endif

findGenericCall :: HsExpr GhcTc -> Maybe Text
findGenericCall e0 = go (peelWrap e0)
  where
    go expr = case appSpineTc expr of
      (h : args) | Just (occ, mmod) <- opName h
                 , occ `elem` ["genericToJSON","genericToEncoding","genericParseJSON",
                               "defaultEncode","defaultEncodeJSON","defaultDecode","defaultDecodeJSON",
                               "defaultEncodeOmitNothingOpts",
                               "genericEncode","genericDecode",
                               "genericEncodeModel","genericDecodeModel",
                               "genericEncodeJSON","genericDecodeJSON"]
                 , isAesonMod' mmod
                       || occ `elem` ["defaultEncode","defaultEncodeJSON","defaultDecode","defaultDecodeJSON",
                                     "defaultEncodeOmitNothingOpts",
                                     "genericEncode","genericDecode",
                                     "genericEncodeModel","genericDecodeModel",
                                     "genericEncodeJSON","genericDecodeJSON"]
                  -> case args of
                       (optsExpr : _) -> Just (pprText optsExpr)
                       [] -> Just "defaultOptions"
      -- Handle left composition: f <<< g  (i.e. f . g)
      -- The outer function (first arg) transforms the output of the inner;
      -- if it's a key-transforming function (e.g. camelCaseToSnakeCase),
      -- the actual keys are NOT the field labels.  Only the first argument
      -- is searched, so camelCaseToSnakeCase <<< defaultEncode returns
      -- Nothing (keys unknown), while defaultDecode <<< convertIntToStrings
      -- still finds defaultDecode.
      (h : args) | Just (occ, _) <- opName h
                 , occ `elem` ["<<<", "."]
                 , (firstArg : _) <- args
                 -> go (peelWrap firstArg)
      -- Handle right composition: f >>> g  (i.e. g . f)
      -- The outer function (last arg) is the generic decoder;
      -- the inner function (first arg) pre-processes the input.
      -- Only the last argument is searched.
      (h : args) | Just (occ, _) <- opName h
                 , occ `elem` [">>>"]
                 , not (null args)
                 -> go (peelWrap (last args))
      -- Handle alternative: f <|> g
      -- Both branches parse the same type, so we pick the first branch
      -- that yields a recognized generic function.
      (h : args) | Just (occ, _) <- opName h
                 , occ `elem` ["<|>"]
                 -> listToMaybe (mapMaybe (go . peelWrap) args)
      -- Handle ($) operator: after typechecking, @$@ is plain HsApp,
      -- so @genericParseJSON $ opts@ becomes appSpine [($), genericParseJSON, opts].
      -- If the second element is a recognized generic function, return the
      -- options (third element).
      (h : fn : optsExpr : _) | Just (occ, _) <- opName h
                             , occ == "$"
                             , isJust (go (peelWrap fn))
                             -> Just (pprText optsExpr)
      _ -> case peelWrap expr of
             HsCase _ _ mg ->
               case mapMaybe (\m -> case matchBody (unLoc m) of Just body -> go (peelWrap body); Nothing -> Nothing)
                             (unLoc (mg_alts mg :: XRec GhcTc [LMatch GhcTc (LHsExpr GhcTc)])) of
                 (opts : _) -> Just opts
                 [] -> Nothing
             OpApp _ f _ a ->
               case go (peelWrap (unXRecTc f)) of
                 Just _ -> Just (pprText a)
                 Nothing -> go (peelWrap (unXRecTc a))
             _ -> Nothing

isAesonMod' :: Maybe String -> Bool
isAesonMod' Nothing = False
isAesonMod' (Just m) = "Aeson" `isInfixOf` m

----------------------------------------------------------------------
-- | When the encoder delegates via @toJSON <var>@, look up the delegated
-- type's encoder keys in @localKeys@.  Returns @Just keys@ if the delegated
-- type's @toJSON@ is also defined in the same module (and has extractable
-- keys), or @Nothing@ if not found / keys are unknown.
resolveDelegatedKeys :: Maybe Text -> Map Text (SrcSpan,[KeyInfo],SrcSpan,[KeyInfo],Maybe Text,Maybe Text,Maybe Text) -> Maybe [KeyInfo]
resolveDelegatedKeys Nothing _ = Nothing
resolveDelegatedKeys (Just delegatedKey) localKeys =
  case Map.lookup delegatedKey localKeys of
    Just (_, delEncKeys, _, _, delGenOpts, _, _)
      | null delEncKeys -> Nothing  -- delegated type also has no keys
      | isJust delGenOpts -> Nothing  -- delegated type is generic, keys unknown
      | otherwise -> Just delEncKeys
    Nothing -> Nothing

-- Per-type checking.
----------------------------------------------------------------------

checkType :: SrcSpan -> Map Text ParsedSideInfo -> Map Text (SrcSpan,[KeyInfo],SrcSpan,[KeyInfo],Maybe Text,Maybe Text,Maybe Text) -> TagValueMap -> AltGroupMap -> Map Text TyConInfo -> Map Text (Bool,Bool) -> Text -> [(SrcSpan, JsonIdLawError)]
checkType moduleSpan parsedMap localKeys tagValues altGroups definedHere instPresence tyKey
  | not definedHereNow = []
  | not (encPresent && decPresent) = []
  | otherwise =
      let psi = fromMaybe emptyParsedSideInfo (Map.lookup tyKey parsedMap)
          (encSpan, locEncKeys, decSpan, locDecKeys, mEncGenOpts, mDecGenOpts, mDelegatedKey) = fromMaybe (noSrcSpan,[],noSrcSpan,[],Nothing,Nothing,Nothing) (Map.lookup tyKey localKeys)
          (encInInsts, decInInsts) = fromMaybe (False,False) (Map.lookup tyKey instPresence)
          tci = fromMaybe (emptyTyConInfo moduleSpan) (Map.lookup tyKey definedHere)
          typeSpan = tciSpan tci
          fields = tciFields tci
          nCons = tciNumCons tci
          -- Resolve the effective key sets per side, if computable.
          encKeysE = resolveKeys nCons (encSpan /= noSrcSpan) mEncGenOpts locEncKeys (psiOptsEnc psi) (psiViaEnc psi) (psiPlainEnc psi) encInInsts fields
          -- If encoder keys are unknown due to non-standard encoding (toJSON delegation),
          -- try to resolve keys from the delegated type's localKeys entry.
          encKeysResolved = encKeysE <|> resolveDelegatedKeys mDelegatedKey localKeys
          decKeysE = resolveKeys nCons (decSpan /= noSrcSpan) mDecGenOpts locDecKeys (psiOptsDec psi) (psiViaDec psi) (psiPlainDec psi) decInInsts fields
          keyErrs = case (encKeysResolved, decKeysE) of
            (Just ek, Just dk) -> keyChecks tyKey ek dk
              (effectiveSpan [encSpan, psiSpan psi, typeSpan, moduleSpan])
              (effectiveSpan [decSpan, psiSpan psi, typeSpan, moduleSpan])
              (Map.findWithDefault Map.empty tyKey altGroups)
            _ -> []
          optErrs = optionsChecks tyKey psi (effectiveSpan [psiSpan psi, moduleSpan])
          genOptErrs = genericOptionsChecks nCons tyKey mEncGenOpts mDecGenOpts (effectiveSpan [psiSpan psi, encSpan, decSpan, moduleSpan])
          (encTags, decTags, encConToTag, mCatchAllCon, encCons, decCons) = fromMaybe (Set.empty, Set.empty, Map.empty, Nothing, Set.empty, Set.empty) (Map.lookup tyKey tagValues)
          tagErrs = tagChecks tyKey encTags decTags encConToTag mCatchAllCon (effectiveSpan [encSpan, decSpan, typeSpan, moduleSpan])
          collapseErrs = collapseChecks tyKey encTags encCons decCons (effectiveSpan [encSpan, decSpan, typeSpan, moduleSpan])
      in keyErrs ++ optErrs ++ genOptErrs ++ tagErrs ++ collapseErrs
  where
    definedHereNow = Map.member tyKey definedHere
    effectiveSpan = foldr1 (\s acc -> if s /= noSrcSpan then s else acc)
    encPresent = let psi = fromMaybe emptyParsedSideInfo (Map.lookup tyKey parsedMap)
                     (encSpan, locEncKeys, _, _, _, _, _) = fromMaybe (noSrcSpan,[],noSrcSpan,[],Nothing,Nothing,Nothing) (Map.lookup tyKey localKeys)
                     (encInInsts, _) = fromMaybe (False,False) (Map.lookup tyKey instPresence)
                 in not (null locEncKeys) || encInInsts || isJust (psiOptsEnc psi) || psiPlainEnc psi || isJust (psiViaEnc psi)
    decPresent = let psi = fromMaybe emptyParsedSideInfo (Map.lookup tyKey parsedMap)
                     (_, _, _, locDecKeys, _, _, _) = fromMaybe (noSrcSpan,[],noSrcSpan,[],Nothing,Nothing,Nothing) (Map.lookup tyKey localKeys)
                     (_, decInInsts) = fromMaybe (False,False) (Map.lookup tyKey instPresence)
                 in not (null locDecKeys) || decInInsts || isJust (psiOptsDec psi) || psiPlainDec psi || isJust (psiViaDec psi)

-- Returns @Just keys@ if the side's keys are statically computable, @Nothing@ if
-- not.  When @mGenOpts@ is @Just opts@ the method delegates to
-- @genericToJSON@/@genericParseJSON@/@defaultEncode@/@defaultDecode@; if those
-- options cannot change a key (see 'isKeyPreservingOptionsText') the JSON keys
-- are the field labels (after 'decodeFieldKey' underscore stripping).  When
-- @hasLocalBind@ is True the side has a locally-defined method bind and an empty
-- key list means /genuinely empty/ (the method uses no @.=@/@.:@ operators)
-- rather than /unknown/ -- but that reading only holds once the /derived/ cases
-- have been ruled out, hence the guard order below.
resolveKeys :: Int -> Bool -> Maybe Text -> [KeyInfo] -> Maybe Text -> Maybe Text -> Bool -> Bool -> [Text] -> Maybe [KeyInfo]
resolveKeys nCons hasLocalBind mGenOpts locKeys mOpts mVia plain inInsts fields
  | isJust mGenOpts, keyPreserving (fromJust mGenOpts), not (null fields), not (null locKeys), all kiOptional locKeys
    = Just (genericKeys fields)
  | not (null locKeys) = Just locKeys
  -- Generic derivation only tells us the keys of a record.  A type with no
  -- record fields is encoded as a tag/contents envelope, which we do not model.
  | isJust mGenOpts, keyPreserving (fromJust mGenOpts), not (null fields) = Just (genericKeys fields)
  | isJust mGenOpts = Nothing
  | isJust mOpts = Nothing
  -- 'deriving via' and plain 'deriving' also produce a local method bind (GHC
  -- fills the instance in for you), so these must be consulted before falling
  -- back on "has a bind, therefore writes no keys".
  | isJust mVia = Nothing
  | plain, not (null fields) = Just (genericKeys fields)
  | plain = Nothing
  | hasLocalBind = Just []
  | inInsts = Nothing
  | otherwise = Nothing
  where
    keyPreserving = isKeyPreservingOptionsText nCons
    genericKeys fs = [KeyInfo (decodeFieldKey f) Nothing False | f <- fs]

-- | Replicate the @Nau.Utils.DecodeField@ underscore-stripping logic used by
-- the Presto framework's @defaultEncode@/@defaultDecode@.  Strips a leading
-- underscore when the field is one of the special names (\"_id\", \"_type\",
-- \"_class\", \"_data\", \"_default\") or when the character after the
-- underscore is uppercase.  Otherwise leaves the field unchanged.
decodeFieldKey :: Text -> Text
decodeFieldKey s
  | "__" `T.isPrefixOf` s = s  -- double underscore: not handled here
  | s `elem` ["_id","_type","_class","_data","_default"] = T.drop 1 s
  | Just rest <- T.stripPrefix "_" s, not (T.null rest), T.head rest `elem` ['A'..'Z'] = rest
  | otherwise = s

-- | Check if the pretty-printed options text represents plain @defaultOptions@
-- (no record extension, no @fieldLabelModifier@).  Matches both unqualified
-- (@defaultOptions@) and qualified (@A.defaultOptions@).
isDefaultOptionsText :: Text -> Bool
isDefaultOptionsText t =
  let s = T.unpack (T.strip t)
      base = reverse (takeWhile (/= '.') (reverse s))
  in base == "defaultOptions"

-- | The 'Options' fields that can change a JSON key /for this type/.
--
-- aeson only writes a constructor tag for a single-constructor type when
-- @tagSingleConstructors@ is set (see aeson's @D1 d (C1 c a)@ instance), so for
-- a one-constructor type without it, @sumEncoding@, @constructorTagModifier@ and
-- @allNullaryToStringTag@ are inert and must not be compared -- otherwise a
-- record whose encoder says @defaultOptions {sumEncoding = UntaggedValue}@ is
-- reported against a decoder that says @defaultOptions@, with no key ever
-- differing.
--
-- The field names below are matched as substrings, so the singular spellings
-- also match aeson's actual @tagSingleConstructors@ field.
keyAffectingFieldsFor :: Int -> Text -> Text -> [Text]
keyAffectingFieldsFor nCons encOpts decOpts
  | nCons == 1, not (mentionsTagSingle encOpts || mentionsTagSingle decOpts) = alwaysKeyAffecting
  | otherwise = alwaysKeyAffecting ++ tagRelated
  where
    mentionsTagSingle = T.isInfixOf "tagSingleConstructor"

alwaysKeyAffecting :: [Text]
alwaysKeyAffecting = ["fieldLabelModifier", "unwrapUnaryRecords", "tagSingleConstructor"]

tagRelated :: [Text]
tagRelated = ["constructorTagModifier", "allNullaryToStringTag", "sumEncoding"]

-- | Does this pretty-printed 'Options' expression leave the JSON keys equal to
-- the (underscore-stripped) record field labels?  True for plain
-- @defaultOptions@ and for record updates that only set fields which cannot
-- change a key for a type with @nCons@ constructors.
isKeyPreservingOptionsText :: Int -> Text -> Bool
isKeyPreservingOptionsText nCons t
  | isDefaultOptionsText t = True
  | isUnknownOptsText t = False
  | otherwise =
      let inert = ["omitNothingFields"]
                    ++ (if nCons == 1 && not (T.isInfixOf "tagSingleConstructor" t) then tagRelated else [])
          -- Every field assignment left after dropping the inert ones.
          remaining = [ p
                      | p <- T.splitOn "," (T.unwords (T.words t))
                      , T.isInfixOf "=" p
                      , not (any (`T.isInfixOf` p) inert)
                      ]
          -- What is left once the record-update braces and the base name go.
          base = T.strip (T.takeWhile (/= '{') t)
      in null remaining && isDefaultOptionsText base

keyChecks :: Text -> [KeyInfo] -> [KeyInfo] -> SrcSpan -> SrcSpan -> Map Text (Set Text) -> [(SrcSpan, JsonIdLawError)]
keyChecks ty enc dec encSpan decSpan altGroups =
     [ (decSpan, KEY_ONLY_IN_DECODE ty (kiKey d))
     | d <- dec
     , kiKey d `notElem` map kiKey enc
     , not (any (\alt -> alt `elem` map kiKey enc) (Set.toList (Map.findWithDefault Set.empty (kiKey d) altGroups)))
     ]
  ++ [ pickEncodeError ty e dec encSpan
     | e <- enc
     , kiKey e `notElem` map kiKey dec
     ]

pickEncodeError :: Text -> KeyInfo -> [KeyInfo] -> SrcSpan -> (SrcSpan, JsonIdLawError)
pickEncodeError ty e dec encSpan =
  case [ d | d <- dec, T.toLower (kiKey e) == T.toLower (kiKey d), kiKey e /= kiKey d ] of
    (d : _) -> (encSpan, KEY_CASE_MISMATCH ty (fromMaybe (kiKey e) (kiField e)) (kiKey e) (kiKey d))
    [] -> (encSpan, KEY_ONLY_IN_ENCODE ty (kiKey e))

optionsChecks :: Text -> ParsedSideInfo -> SrcSpan -> [(SrcSpan, JsonIdLawError)]
optionsChecks ty psi sp =
     case (psiOptsEnc psi, psiOptsDec psi) of
       (Just oe, Just od) | stripOmitNothingFields oe /= stripOmitNothingFields od
         -> [(sp, OPTIONS_MISMATCH ty oe od)]
       _ -> []
  ++ case (psiViaEnc psi, psiViaDec psi) of
       (Just ve, Just vd) | stripOmitNothingFields ve /= stripOmitNothingFields vd
         -> [(sp, OPTIONS_VIA_MISMATCH ty ve vd)]
       _ -> []

-- | Remove @omitNothingFields = <bool>@ from options text so this
-- safe-to-differ field does not cause a false options mismatch.
-- Also normalizes whitespace, commas, braces, and @defaultOptions@ prefix
-- to handle GHC pretty-printer line-wrapping differences.
stripOmitNothingFields :: Text -> Text
stripOmitNothingFields t =
  T.unwords (T.words (T.replace "," " " (removeDefaultOpts (fixBrace stripped))))
  where
    flat = T.unwords (T.words t)
    parts = T.splitOn ", " flat
    kept = filter (not . T.isInfixOf "omitNothingFields") parts
    stripped = T.intercalate ", " kept
    fixBrace s
      | "}" `T.isInfixOf` t && not ("}" `T.isSuffixOf` (T.strip s)) =
          T.append (T.strip s) "}"
      | otherwise = s
    removeDefaultOpts s = T.replace "defaultOptions" "" s

-- | Compare key-affecting 'Options' fields between @genericToJSON@ and
-- @genericParseJSON@ calls. Only fields that change JSON key or tag names
-- are compared; safe-to-differ fields like @omitNothingFields@ are ignored.
genericOptionsChecks :: Int -> Text -> Maybe Text -> Maybe Text -> SrcSpan -> [(SrcSpan, JsonIdLawError)]
genericOptionsChecks nCons ty mEncOpts mDecOpts sp =
  case (mEncOpts, mDecOpts) of
    (Just encOpts, Just decOpts) ->
      let encFiltered = filterKeyAffecting encOpts
          decFiltered = filterKeyAffecting decOpts
      in if encFiltered /= decFiltered
          then [(sp, OPTIONS_MISMATCH ty encOpts decOpts)]
          else []
    _ -> []
  where
    -- Fields that affect JSON key/tag names and must match for round-trip safety.
    keyAffectingFields = keyAffectingFieldsFor nCons (fromMaybe "" mEncOpts) (fromMaybe "" mDecOpts)
    -- Keep only the lines mentioning a key-affecting field, and normalize
    -- GHC internal variable names (e.g. x_aPhB -> x_) so that the same
    -- lambda with different internal names doesn't cause a false mismatch.
    filterKeyAffecting opts =
      normalizeVarNames (T.unlines (filter (\l -> any (`T.isInfixOf` l) keyAffectingFields) (T.lines (stripOmitNothingFields opts))))
    -- Replace GHC internal variable name suffixes like _aPhB, _aPfZ with _
    normalizeVarNames = T.pack . go . T.unpack
      where
        go [] = []
        go ('_':rest@(c:_)) | c >= 'a' && c <= 'z' =
          case dropWhile (\d -> d >= 'a' && d <= 'z' || d >= 'A' && d <= 'Z' || d >= '0' && d <= '9') rest of
            rest' -> '_' : go rest'
        go (c:rest) = c : go rest

-- | Compare constructor tag values between encoder and decoder.
-- Fires for any type that has tag values on the encoder side (per user's
-- choice of option b for Question 2). Two kinds of mismatch:
--   1. Case-insensitive match but different case (e.g. "leaf" vs "LEAF")
--      — suppressed when the encoder tag has an exact match in the decoder
--      (the case-variant is just an extra accepted spelling).
--   2. Encoder tag with no match at all in decoder (e.g. "RightCtor" missing)
--      — suppressed when the decoder catch-all @_@ returns the /same/
--      constructor that the encoder used (round-trip is correct).
tagChecks :: Text -> Set Text -> Set Text -> Map Text Text -> Maybe Text -> SrcSpan -> [(SrcSpan, JsonIdLawError)]
tagChecks ty encTags decTags encConToTag mCatchAllCon sp
  | Set.null encTags = []
  | otherwise =
      let encConForTag tag = listToMaybe [con | (con, t) <- Map.toList encConToTag, t == tag]
          catchAllOk tag = case (encConForTag tag, mCatchAllCon) of
            (Just encCon, Just catchCon) -> encCon == catchCon
            _ -> False
      in  [ (sp, TAG_VALUE_MISMATCH ty encTag decTag)
         | encTag <- Set.toList encTags
         , not (encTag `Set.member` decTags)
         , decTag <- Set.toList decTags
         , T.toLower encTag == T.toLower decTag
         , encTag /= decTag
         ]
      ++ [ (sp, TAG_VALUE_MISMATCH ty encTag "<not handled>")
         | encTag <- Set.toList encTags
         , not (any (\d -> T.toLower encTag == T.toLower d) (Set.toList decTags))
         , not (catchAllOk encTag)
         ]

-- | Detect constructor collapse: the encoder has multiple constructors
-- (sum type) but the decoder only produces one constructor. This catches
-- patterns like @toJSON (WrapperA inner) = toJSON inner; toJSON (WrapperB inner) = toJSON inner@
-- where the decoder always returns the same constructor.
-- Only fires when there are no encoder tag values (tag checks handle tagged sum types).
collapseChecks :: Text -> Set Text -> Set Text -> Set Text -> SrcSpan -> [(SrcSpan, JsonIdLawError)]
collapseChecks ty encTags encCons decCons sp
  | not (Set.null encTags) = []  -- tag checks handle tagged sum types
  | Set.size encCons > 1 =
      let decEncCons = Set.intersection decCons encCons
      in if Set.size decEncCons == 1
           then [ (sp, TAG_VALUE_MISMATCH ty encCon "<not handled>")
                | encCon <- Set.toList encCons
                , not (encCon `Set.member` decEncCons)
                ]
           else []
  | otherwise = []

----------------------------------------------------------------------
-- The universe of types to consider.
----------------------------------------------------------------------

allTypesOfInterest :: Map Text ParsedSideInfo -> Map Text (SrcSpan,[KeyInfo],SrcSpan,[KeyInfo],Maybe Text,Maybe Text,Maybe Text) -> Map Text TyConInfo -> Map Text (Bool,Bool) -> [Text]
allTypesOfInterest parsedMap localKeys definedHere instPresence =
  nub $ Map.keys definedHere
     ++ Map.keys parsedMap
     ++ Map.keys localKeys
     ++ Map.keys instPresence

----------------------------------------------------------------------
-- Helpers.
----------------------------------------------------------------------

parseCli :: [CommandLineOption] -> CliOptions
parseCli [] = idLawDefaultCliOptions
parseCli (local : _) = case A.decode (BL.fromStrict (encodeUtf8 (T.pack local)) :: BL.ByteString) of
  Just (val :: CliOptions) -> val
  Nothing -> idLawDefaultCliOptions

idLawDefaultCliOptions :: CliOptions
idLawDefaultCliOptions = CliOptions
  { path = "./.juspay/api-contract/"
  , port = 4444
  , host = "::1"
  , log = False
  , tc_funcs = Just False
  , api_contract = Just True
  , id_law_check = Just True
  , id_law_exceptions_path = Just "./.juspay/jsonIdLawExceptions.yaml"
  }

idLawEnabled :: CliOptions -> Bool
idLawEnabled opts = fromMaybe True (id_law_check opts) && not envDisabled
  where
    envDisabled = readBool (unsafePerformIO (lookupEnv "JSON_ID_LAW_CHECK"))
    readBool (Just "false") = True
    readBool (Just "False") = True
    readBool (Just "FALSE") = True
    readBool _ = False

-- | YAML config for type-level exceptions.
data ExceptionConfig = ExceptionConfig
  { exceptions     :: Maybe [Text]   -- ^ Module-qualified type names (e.g. "Mod.Type")
  , ignore_modules :: Maybe [Text]   -- ^ Module names to skip entirely
  } deriving (Generic, A.FromJSON)

-- | Load exception config from a YAML file. Returns (exception type set,
-- ignore module set). On file-not-found or parse error, returns empty sets
-- (graceful — no build failure).
loadExceptions :: Maybe FilePath -> IO (Set Text, Set Text)
loadExceptions mPath = do
  let path = fromMaybe "./.juspay/jsonIdLawExceptions.yaml" mPath
  result <- YAML.decodeFileEither path
  pure $ case result of
    Left _  -> (Set.empty, Set.empty)
    Right cfg -> (Set.fromList (fromMaybe [] (exceptions cfg)), Set.fromList (fromMaybe [] (ignore_modules cfg)))

-- | Build a module-qualified type key ("ModuleName.TypeName") for a type
-- defined in the current module. Since @checkType@ already guards on
-- @definedHereNow@, only types defined in the current module reach the
-- filter, so the module qualifier is always the current module.
qualifiedKey :: Text -> TcGblEnv -> Text
qualifiedKey tyKey tcg =
  T.pack (moduleNameString (moduleName (tcg_mod tcg))) <> "." <> tyKey

pprText :: (Outputable a) => a -> Text
pprText = T.pack . showSDocUnsafe . ppr

stripQuotes :: String -> String
stripQuotes = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse

unXPs :: LIdP GhcPs -> RdrName
#if __GLASGOW_HASKELL__ >= 900
unXPs = GHC.unXRec @GhcPs
#else
unXPs (L _ n) = n
#endif

mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan mod_loc = case ml_hs_file mod_loc of
  Just fp -> mkGeneralSrcSpan (mkFastString fp)
  Nothing -> interactiveSrcSpan

isSafeTyCon :: TyCon -> Bool
isSafeTyCon tc = not (isClassTyCon tc) && not (isPromotedDataCon tc) && not (isTcTyCon tc)
