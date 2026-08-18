{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : VariableTracer.Collect
Description : Build a variable provenance graph from the type-checked AST.

This is the AST half of the tracer. It walks the type-checked Haskell AST
(@GhcTc@, i.e. after type checking, so every occurrence carries a real 'Id' with
a unique and a type) and records, for every binder in the module:

  * the computation that produced it (application, record construction, case,
    monadic bind, pattern projection, …), and
  * the direct dependencies of that computation.

It additionally records every application it sees as a 'CallSite', which is what
lets the tracer walk /out/ of a function through its parameters and into its
callers — including callers in other modules, because top-level binders are
keyed by GHC's stable name.

The entry points are 'collectModuleGraph' (from a plugin) and 'collectFromBinds'
(if you already have the binds). Both are pure, so any other plugin in this
repository can call them and post-process the result without doing IO.
-}
module VariableTracer.Collect
  ( CollectEnv (..)
  , mkCollectEnv
  , collectModuleGraph
  , collectFromBinds
  , locToSrcSpan
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import VariableTracer.Types

#if __GLASGOW_HASKELL__ >= 900

import Data.Generics.Uniplate.Data (childrenBi)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

-- 'ModuleGraph' is ours (the provenance graph), not GHC's module dependency graph.
import GHC hiding (ModuleGraph)
import GHC.Core.ConLike (ConLike)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Types.SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Types.Name (nameModule_maybe, nameOccName, nameStableString)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Unique (getKey)
import GHC.Types.Var (Var, isTyVar, varName, varType, varUnique)
import GHC.Unit.Module.ModSummary (msHsFilePath)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)

#else

import GHC (ModSummary, SrcSpan, ms_mod, noSrcSpan)
import GhcPlugins (moduleName, moduleNameString)
import HscTypes (msHsFilePath)
import TcRnTypes (TcGblEnv)

#endif

-- | Everything the collector needs to know about the module being compiled.
data CollectEnv = CollectEnv
  { ceModule :: Text
  , cePackage :: Text
  , ceFile :: Text
  , ceOpts :: TracerOpts
  }

#if __GLASGOW_HASKELL__ >= 900

--------------------------------------------------------------------------------
-- Entry points
--------------------------------------------------------------------------------

mkCollectEnv :: TracerOpts -> ModSummary -> CollectEnv
mkCollectEnv opts modSummary =
  CollectEnv
    { ceModule = T.pack (moduleNameString (moduleName (ms_mod modSummary)))
    , cePackage = T.pack (showS (moduleUnit (ms_mod modSummary)))
    , ceFile = T.pack (msHsFilePath modSummary)
    , ceOpts = opts
    }

collectModuleGraph :: TracerOpts -> ModSummary -> TcGblEnv -> ModuleGraph
collectModuleGraph opts modSummary tcEnv =
  collectFromBinds (mkCollectEnv opts modSummary) (tcg_binds tcEnv)

-- | Build the provenance graph of a module from its type-checked bindings.
collectFromBinds :: CollectEnv -> LHsBinds GhcTc -> ModuleGraph
collectFromBinds env binds =
  (emptyModuleGraph (ceModule env) (cePackage env) (ceFile env))
    { mgNodes = dedupNodes (colNodes collected)
    , mgFunctions = dedupFunctions (colFunctions collected)
    , mgCallSites = colCalls collected
    , mgAliases = map aliasPair aliasList
    }
  where
    topLevel = bagToList binds
    aliasList = gatherAliases topLevel
    ctx0 =
      Ctx
        { ctxEnv = env
        , ctxOwner = Nothing
        , ctxOwnerName = Nothing
        , ctxAliases = Map.fromList [(getKey (varUnique mono), poly) | (mono, poly) <- aliasList]
        , ctxKind = TopLevelBind
        }
    collected = mconcat (map (collectBind ctx0) topLevel)

    aliasPair (mono, poly) = (rawVarKey env mono, rawVarKey env poly)

--------------------------------------------------------------------------------
-- Collector plumbing
--------------------------------------------------------------------------------

data Ctx = Ctx
  { ctxEnv :: CollectEnv
  , ctxOwner :: Maybe NodeKey
  , ctxOwnerName :: Maybe Text
  , ctxAliases :: Map.Map Int Var
  -- ^ monomorphic unique -> exported polymorphic 'Var' (from @AbsBinds@)
  , ctxKind :: BindKind
  -- ^ the binding kind to use for binders introduced at this level
  }

data Collected = Collected
  { colNodes :: [ProvenanceNode]
  , colFunctions :: [FunctionNode]
  , colCalls :: [CallSite]
  }

instance Semigroup Collected where
  a <> b = Collected (colNodes a <> colNodes b) (colFunctions a <> colFunctions b) (colCalls a <> colCalls b)

instance Monoid Collected where
  mempty = Collected [] [] []

nodesOnly :: [ProvenanceNode] -> Collected
nodesOnly ns = Collected ns [] []

-- | The result of looking at one expression: what it computes, what it depends
-- on, and any binders/call sites found inside it.
data Analysis = Analysis
  { anComp :: Computation
  , anDeps :: [Dep]
  , anCollected :: Collected
  }

mergeDeps :: [Analysis] -> [Dep]
mergeDeps = concatMap anDeps

mergeCollected :: [Analysis] -> Collected
mergeCollected = mconcat . map anCollected

-- | Give unlabelled ('DepUse') edges a more specific label, leaving edges that
-- already say something useful alone.
relabel :: DepKind -> [Dep] -> [Dep]
relabel k = map (\d -> if depKind d == DepUse then d {depKind = k} else d)

--------------------------------------------------------------------------------
-- Names, keys, locations
--------------------------------------------------------------------------------

showS :: Outputable a => a -> String
showS = showSDocUnsafe . ppr

renderCode :: Outputable a => Ctx -> a -> Text
renderCode ctx = clip (codeLimit (ceOpts (ctxEnv ctx))) . T.pack . showS

-- | @GRHSs@ and @MatchGroup@ have no 'Outputable' instance of their own, so
-- render the right-hand sides instead.
renderGRHSs :: Ctx -> GRHSs GhcTc (LHsExpr GhcTc) -> Text
renderGRHSs ctx = renderBodies ctx . grhssBodies

renderMatchGroup :: Ctx -> MatchGroup GhcTc (LHsExpr GhcTc) -> Text
renderMatchGroup ctx = renderBodies ctx . matchGroupBodies

renderBodies :: Ctx -> [LHsExpr GhcTc] -> Text
renderBodies ctx =
  clip (codeLimit (ceOpts (ctxEnv ctx))) . T.intercalate " | " . map (renderCode ctx)

grhssBodies :: GRHSs GhcTc (LHsExpr GhcTc) -> [LHsExpr GhcTc]
grhssBodies (GRHSs _ grhss _) = [body | L _ (GRHS _ _ body) <- grhss]
grhssBodies _ = []

matchGroupBodies :: MatchGroup GhcTc (LHsExpr GhcTc) -> [LHsExpr GhcTc]
matchGroupBodies (MG _ (L _ matches) _) =
  concat [grhssBodies grhss | L _ (Match _ _ _ grhss) <- matches]
matchGroupBodies _ = []

clip :: Int -> Text -> Text
clip limit t =
  let flat = T.unwords (T.words t)
   in if T.length flat > limit then T.take limit flat <> " ..." else flat

-- | Turn a graph location back into a GHC span, so a plugin can report an
-- error at a position that came out of the provenance graph.
locToSrcSpan :: Loc -> SrcSpan
locToSrcSpan l
  | isEmptyLoc l = noSrcSpan
  | otherwise =
      RealSrcSpan
        (mkRealSrcSpan (mkRealSrcLoc file (locStartLine l) (locStartCol l))
                       (mkRealSrcLoc file (locEndLine l) (locEndCol l)))
        Nothing
  where
    file = mkFastString (T.unpack (locFile l))

locOf :: SrcSpan -> Loc
locOf (RealSrcSpan s _) =
  Loc
    { locFile = T.pack (unpackFS (srcSpanFile s))
    , locStartLine = srcSpanStartLine s
    , locStartCol = srcSpanStartCol s
    , locEndLine = srcSpanEndLine s
    , locEndCol = srcSpanEndCol s
    }
locOf _ = emptyLoc

resolveVar :: Ctx -> Var -> Var
resolveVar ctx v = fromMaybe v (Map.lookup (getKey (varUnique v)) (ctxAliases ctx))

-- | Graph key of a variable, after resolving @AbsBinds@ aliasing.
varKey :: Ctx -> Var -> NodeKey
varKey ctx = rawVarKey (ctxEnv ctx) . resolveVar ctx

rawVarKey :: CollectEnv -> Var -> NodeKey
rawVarKey env v
  | isExternalName nm = T.pack (nameStableString nm)
  | otherwise =
      ceModule env
        <> ":"
        <> T.pack (occNameString (nameOccName nm))
        <> ":"
        <> T.pack (show (getKey (varUnique v)))
  where
    nm = varName v

varOccText :: Var -> Text
varOccText = T.pack . occNameString . nameOccName . varName

-- | Occurrence name of anything GHC considers named (constructors, selectors).
namedText :: NamedThing a => a -> Text
namedText = T.pack . occNameString . nameOccName . getName

conLikeText :: ConLike -> Text
conLikeText = namedText

mkVarRef :: Ctx -> SrcSpan -> Var -> VarRef
mkVarRef ctx sp var =
  VarRef
    { vrKey = varKey ctx var
    , vrName = varOccText v
    , vrStableName = stable
    , vrModule = modName
    , vrPackage = packageOfStable stable (cePackage env)
    , vrType = clip 200 (T.pack (showS (varType v)))
    , vrScope = if isExternalName nm then GlobalVar else LocalVar
    , vrLoc = locOf sp
    }
  where
    env = ctxEnv ctx
    v = resolveVar ctx var
    nm = varName v
    stable = T.pack (nameStableString nm)
    modName = case nameModule_maybe nm of
      Just m -> T.pack (moduleNameString (moduleName m))
      Nothing -> ceModule env

-- | GHC stable names look like @package$Module$occurrence@ for external names
-- and @$_in$occurrence@ for internal ones.
packageOfStable :: Text -> Text -> Text
packageOfStable stable fallbackPkg =
  case filter (not . T.null) (T.splitOn "$" stable) of
    (pkg : _ : _ : _) -> pkg
    _ -> fallbackPkg

-- | Dictionaries, evidence and other compiler-generated binders are noise for a
-- data-flow question, so they are dropped unless the user asks for them.
isIgnorableVar :: Ctx -> Var -> Bool
isIgnorableVar ctx v
  | isTyVar v = True
  | not (ignoreDictionaries (ceOpts (ctxEnv ctx))) = False
  | otherwise = any (`T.isPrefixOf` name) generatedPrefixes
  where
    name = varOccText v
    generatedPrefixes =
      ["$d", "$f", "$c", "$w", "$s", "$p", "$tc", "$tr", "$krep", "$dm", "$sel", "ipv_", "ipv1", "$cont"]

-- | Bindings the user asked not to collect at all — by default the class
-- methods that @deriving@ generates bodies for. Calls to them are unaffected.
isSkippedBinding :: Ctx -> Var -> Bool
isSkippedBinding ctx v =
  any (`matchesPattern` varOccText v) (skipBindings (ceOpts (ctxEnv ctx)))

--------------------------------------------------------------------------------
-- AbsBinds aliasing
--------------------------------------------------------------------------------

-- | Collect the monomorphic-to-polymorphic mapping @AbsBinds@ introduces, so
-- that a recursive self-call (which uses the monomorphic 'Id') and an external
-- call (which uses the exported polymorphic 'Id') land on the same key.
gatherAliases :: [LHsBindLR GhcTc GhcTc] -> [(Var, Var)]
gatherAliases = concatMap go
  where
    go (L _ bind) = case bind of
      AbsBinds {abs_exports = exports, abs_binds = inner} ->
        mapMaybe exportPair exports <> gatherAliases (bagToList inner)
      _ -> []

    exportPair ABE {abe_poly = poly, abe_mono = mono} = Just (mono, poly)
    exportPair _ = Nothing

--------------------------------------------------------------------------------
-- Bindings
--------------------------------------------------------------------------------

collectBind :: Ctx -> LHsBindLR GhcTc GhcTc -> Collected
collectBind ctx (L _ AbsBinds {abs_exports = exports, abs_binds = inner}) =
  mconcat (map (collectBind ctx') (bagToList inner))
  where
    ctx' =
      ctx
        { ctxAliases =
            Map.union
              (Map.fromList (mapMaybe pair exports))
              (ctxAliases ctx)
        }
    pair ABE {abe_poly = poly, abe_mono = mono} = Just (getKey (varUnique mono), poly)
    pair _ = Nothing
collectBind ctx (L l FunBind {fun_id = funId, fun_matches = matches}) =
  collectFunction ctx (locA l) funId matches
collectBind ctx (L _ PatBind {pat_lhs = pat, pat_rhs = grhss}) =
  let bodyAn = analyseGRHSs ctx grhss
      sourceCode = renderGRHSs ctx grhss
      binders = patBinders "" pat
      deps = relabel DepPatternSource (anDeps bodyAn)
      nodes =
        [ mkNode ctx (ctxKind ctx) Nothing (pbVar b) (pbSpan b) sourceCode (computationFor b sourceCode) deps False
        | b <- binders
        , not (isIgnorableVar ctx (pbVar b))
        , not (isSkippedBinding ctx (pbVar b))
        ]
   in nodesOnly nodes <> anCollected bodyAn
  where
    computationFor b src
      | T.null (pbPath b) = CompAlias (mkVarRef ctx (pbSpan b) (pbVar b))
      | otherwise = CompPatternProjection src (pbPath b)
collectBind ctx (L l VarBind {var_id = var, var_rhs = rhs})
  | isIgnorableVar ctx var || isSkippedBinding ctx var = mempty
  | otherwise =
      let an = analyseExpr ctx rhs
          node = mkNode ctx (ctxKind ctx) Nothing var (locA l) (renderCode ctx rhs) (anComp an) (anDeps an) False
       in nodesOnly [node] <> anCollected an
collectBind _ _ = mempty

-- | A function binding: one node for the function itself, one node per
-- parameter binder, plus everything found in the body.
collectFunction :: Ctx -> SrcSpan -> LIdP GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> Collected
collectFunction ctx sp (L idSpan funVar) matches
  | isIgnorableVar ctx funVar || isSkippedBinding ctx funVar = mempty
  | otherwise = nodesOnly (funNode : paramNodes) <> Collected [] [funInfo] [] <> bodyCollected
  where
    key = varKey ctx funVar
    name = varOccText (resolveVar ctx funVar)
    innerCtx =
      ctx
        { ctxOwner = Just key
        , ctxOwnerName = Just name
        , ctxKind = WhereBind
        }

    (paramNodes, paramKeysByIndex, bodyAn) = analyseMatches innerCtx (Just (key, name)) FunctionParam matches

    arity = length paramKeysByIndex

    funNode =
      (mkNode ctx (ctxKind ctx) Nothing funVar (locA idSpan) (renderMatchGroup ctx matches) bodyComputation (anDeps bodyAn) (arity > 0))
        { pnLoc = if isEmptyLoc (locOf (locA idSpan)) then locOf sp else locOf (locA idSpan)
        }

    bodyComputation = if arity > 0 then CompFunctionBody else anComp bodyAn

    bodyCollected = anCollected bodyAn

    funInfo =
      FunctionNode
        { fnKey = key
        , fnName = name
        , fnModule = ceModule (ctxEnv ctx)
        , fnArity = arity
        , fnParams = paramKeysByIndex
        , fnLoc = locOf (locA idSpan)
        }

mkNode :: Ctx -> BindKind -> Maybe Int -> Var -> SrcSpan -> Text -> Computation -> [Dep] -> Bool -> ProvenanceNode
mkNode ctx kind paramIdx var sp code comp deps isFn =
  ProvenanceNode
    { pnKey = varKey ctx var
    , pnVar = mkVarRef ctx sp var
    , pnBindKind = kind
    , pnOwner = ctxOwner ctx
    , pnOwnerName = ctxOwnerName ctx
    , pnLoc = locOf sp
    , pnCode = code
    , pnComputation = comp
    , pnDeps = deps
    , pnParamIndex = paramIdx
    , pnIsFunction = isFn
    }

--------------------------------------------------------------------------------
-- Matches, guards, local binds
--------------------------------------------------------------------------------

-- | Analyse every equation of a function or lambda.
--
-- Returns the parameter binders, the binder keys grouped by parameter position
-- (several equations can bind different names in the same position) and the
-- analysis of the bodies.
analyseMatches
  :: Ctx
  -> Maybe (NodeKey, Text)
  -- ^ owning function, when these are function parameters
  -> BindKind
  -> MatchGroup GhcTc (LHsExpr GhcTc)
  -> ([ProvenanceNode], [[NodeKey]], Analysis)
analyseMatches ctx owner kind (MG _ (L _ matches) _) =
  ( concatMap (\(ns, _, _) -> ns) results
  , mergeByIndex (map (\(_, ks, _) -> ks) results)
  , Analysis bodyComp (mergeDeps bodyAns) (mergeCollected bodyAns)
  )
  where
    results = map (analyseMatch ctx owner kind) matches
    bodyAns = map (\(_, _, a) -> a) results
    bodyComp = case bodyAns of
      [a] -> anComp a
      _ -> CompGuards
analyseMatches _ _ _ _ = ([], [], Analysis (CompOther "no matches") [] mempty)

analyseMatch
  :: Ctx
  -> Maybe (NodeKey, Text)
  -> BindKind
  -> LMatch GhcTc (LHsExpr GhcTc)
  -> ([ProvenanceNode], [[NodeKey]], Analysis)
analyseMatch ctx owner kind (L _ (Match _ _ pats grhss)) =
  (concat paramNodes, paramKeys, bodyAn)
  where
    ownerName = maybe (fromMaybe "<anonymous>" (ctxOwnerName ctx)) snd owner
    indexed = zip [0 ..] pats
    paramNodes = map (uncurry mkParamNodes) indexed
    paramKeys = map (map pnKey) paramNodes
    bodyAn = analyseGRHSs ctx grhss

    mkParamNodes idx pat =
      [ mkNode ctx kind (Just idx) (pbVar b) (pbSpan b) (renderCode ctx pat) (computationFor idx b) [] False
      | b <- patBinders "" pat
      , not (isIgnorableVar ctx (pbVar b))
      ]

    computationFor idx b
      | T.null (pbPath b) = CompParameter ownerName idx
      | otherwise = CompPatternProjection ("argument #" <> T.pack (show idx) <> " of " <> ownerName) (pbPath b)
analyseMatch _ _ _ _ = ([], [], Analysis (CompOther "unsupported match") [] mempty)

analyseGRHSs :: Ctx -> GRHSs GhcTc (LHsExpr GhcTc) -> Analysis
analyseGRHSs ctx (GRHSs _ grhss localBinds) =
  Analysis comp (mergeDeps bodyAns) (mergeCollected bodyAns <> whereCollected)
  where
    whereCollected = collectLocalBinds ctx WhereBind localBinds
    bodyAns = map (analyseGRHS ctx) grhss
    comp = case bodyAns of
      [a] -> anComp a
      _ -> CompGuards
analyseGRHSs _ _ = Analysis (CompOther "unsupported right-hand side") [] mempty

analyseGRHS :: Ctx -> LGRHS GhcTc (LHsExpr GhcTc) -> Analysis
analyseGRHS ctx (L _ (GRHS _ guards body)) =
  Analysis (anComp bodyAn) (guardDeps <> anDeps bodyAn) (mconcat guardCols <> anCollected bodyAn)
  where
    bodyAn = analyseExpr ctx body
    guardResults = map (analyseStmt ctx CaseBind) guards
    guardDeps = relabel DepGuard (concatMap snd guardResults)
    guardCols = map fst guardResults
analyseGRHS _ _ = Analysis (CompOther "unsupported guard") [] mempty

collectLocalBinds :: Ctx -> BindKind -> HsLocalBinds GhcTc -> Collected
collectLocalBinds ctx kind = \case
  HsValBinds _ (ValBinds _ binds _) -> mconcat (map (collectBind ctx {ctxKind = kind}) (bagToList binds))
  HsValBinds _ (XValBindsLR (NValBinds groups _)) ->
    mconcat [collectBind ctx {ctxKind = kind} b | (_, bag) <- groups, b <- bagToList bag]
  _ -> mempty

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

-- | Analyse a @do@/comprehension/guard statement.
--
-- Binders introduced by the statement become their own nodes; the returned
-- dependencies are the ones the /enclosing/ block depends on.
analyseStmt :: Ctx -> BindKind -> ExprLStmt GhcTc -> (Collected, [Dep])
analyseStmt ctx kind (L _ stmt) = case stmt of
  BindStmt xbs pat body ->
    let bodyAn = analyseExpr ctx body
        actionCode = renderCode ctx body
        bindOp = syntaxAnalyses ctx [xbstc_bindOp xbs]
        deps = relabel DepMonadicAction (anDeps bodyAn) <> relabel DepAppliedFunction (mergeDeps bindOp)
        nodes =
          [ mkNode ctx kind Nothing (pbVar b) (pbSpan b) actionCode (computationFor b actionCode) deps False
          | b <- patBinders "" pat
          , not (isIgnorableVar ctx (pbVar b))
          , not (isSkippedBinding ctx (pbVar b))
          ]
     in (nodesOnly nodes <> anCollected bodyAn <> mergeCollected bindOp, [])
  LastStmt _ body _ retOp ->
    let an = analyseExpr ctx body
        ops = syntaxAnalyses ctx [retOp]
     in ( anCollected an <> mergeCollected ops
        , relabel DepBody (anDeps an) <> relabel DepAppliedFunction (mergeDeps ops)
        )
  BodyStmt _ body thenOp guardOp ->
    let an = analyseExpr ctx body
        ops = syntaxAnalyses ctx [thenOp, guardOp]
     in ( anCollected an <> mergeCollected ops
        , relabel DepBody (anDeps an) <> relabel DepAppliedFunction (mergeDeps ops)
        )
  LetStmt _ binds -> (collectLocalBinds ctx LetBind binds, [])
  other ->
    let ans = map (analyseExpr ctx) (childrenBi other :: [LHsExpr GhcTc])
        binderNodes =
          [ mkNode ctx kind Nothing (pbVar b) (pbSpan b) (renderCode ctx other) (CompMonadicBind (renderCode ctx other)) (relabel DepMonadicAction (mergeDeps ans)) False
          | p <- (childrenBi other :: [LPat GhcTc])
          , b <- patBinders "" p
          , not (isIgnorableVar ctx (pbVar b))
          , not (isSkippedBinding ctx (pbVar b))
          ]
     in (mergeCollected ans <> nodesOnly binderNodes, relabel DepBody (mergeDeps ans))
  where
    computationFor b actionCode
      | T.null (pbPath b) = CompMonadicBind actionCode
      | otherwise = CompPatternProjection actionCode (pbPath b)

-- | The operators behind @do@ notation. With 'RebindableSyntax' or a custom
-- monad these are user code, so they are part of the answer; with the standard
-- ones they are noise, hence the flag.
syntaxAnalyses :: Ctx -> [SyntaxExpr GhcTc] -> [Analysis]
syntaxAnalyses ctx ops
  | not (includeSyntaxOps (ceOpts (ctxEnv ctx))) = []
  | otherwise = map (analyseExpr ctx . wrapXRec @GhcTc) (concatMap syntaxOp ops)
  where
    syntaxOp SyntaxExprTc {syn_expr = e} = [e]
    syntaxOp _ = []

analyseStmts :: Ctx -> BindKind -> [ExprLStmt GhcTc] -> Analysis
analyseStmts ctx kind stmts =
  Analysis CompDoBlock (concatMap snd results) (mconcat (map fst results))
  where
    results = map (analyseStmt ctx kind) stmts

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

-- | A variable bound by a pattern, together with the path that reaches it.
data PatBinder = PatBinder
  { pbVar :: Var
  , pbSpan :: SrcSpan
  , pbPath :: Text
  -- ^ empty for a plain variable pattern, otherwise something like
  -- @Just _@, @(_, #1)@ or @Order{amount}@
  }

patBinders :: Text -> LPat GhcTc -> [PatBinder]
patBinders path (L l pat) = case pat of
  VarPat _ (L vl v) -> [PatBinder v (locA vl) path]
  AsPat _ (L vl v) sub -> PatBinder v (locA vl) path : patBinders path sub
  LazyPat _ sub -> patBinders path sub
  BangPat _ sub -> patBinders path sub
  ParPat _ sub -> patBinders path sub
  SigPat _ sub _ -> patBinders path sub
  ViewPat _ _ sub -> patBinders (extend path "view") sub
  ListPat _ ps -> concat (zipWith (\i p -> patBinders (extend path ("[" <> tshow i <> "]")) p) [0 :: Int ..] ps)
  TuplePat _ ps _ -> concat (zipWith (\i p -> patBinders (extend path ("#" <> tshow i)) p) [0 :: Int ..] ps)
  SumPat _ sub _ _ -> patBinders (extend path "sum") sub
  ConPat {pat_con = conL, pat_args = args} -> conBinders (conNameText conL) args
  _ -> genericPatBinders (locA l) pat
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

    extend p more = if T.null p then more else p <> "." <> more

    conNameText c = conLikeText (unXRec @GhcTc c)

    conBinders conName = \case
      PrefixCon _ ps ->
        concat (zipWith (\i p -> patBinders (extend path (conName <> ".#" <> tshow i)) p) [0 :: Int ..] ps)
      InfixCon p1 p2 ->
        patBinders (extend path (conName <> ".#0")) p1 <> patBinders (extend path (conName <> ".#1")) p2
      RecCon (HsRecFields {rec_flds = flds}) ->
        concatMap
          (\fld -> let (lbl, sub) = recFieldParts fld in patBinders (extend path (conName <> "." <> lbl)) sub)
          flds

-- | Last-resort binder extraction for pattern forms we do not model
-- structurally (coercion patterns, splices, …).
genericPatBinders :: SrcSpan -> Pat GhcTc -> [PatBinder]
genericPatBinders sp pat =
  [PatBinder v sp "" | L _ (VarPat _ (L _ v)) <- (childrenBi pat :: [LPat GhcTc])]
    <> [PatBinder v sp "" | VarPat _ (L _ v) <- (childrenBi pat :: [Pat GhcTc])]

-- | Works for both record construction and record patterns; deliberately left
-- without a signature because the field type differs between GHC versions.
recFieldParts (L _ HsRecField {hsRecFieldLbl = lbl, hsRecFieldArg = arg}) =
  (T.pack (showS lbl), arg)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | Strip away the wrappers that carry no data-flow information.
stripExpr :: LHsExpr GhcTc -> LHsExpr GhcTc
stripExpr le@(L l e) = case e of
  HsPar _ inner -> stripExpr inner
  ExprWithTySig _ inner _ -> stripExpr inner
  HsAppType _ inner _ -> stripExpr inner
  HsTick _ _ inner -> stripExpr inner
  HsBinTick _ _ _ inner -> stripExpr inner
  HsPragE _ _ inner -> stripExpr inner
  XExpr (WrapExpr (HsWrap _ inner)) -> stripExpr (L l inner)
  XExpr (ExpansionExpr (HsExpanded _ inner)) -> stripExpr (L l inner)
  _ -> le

-- | Peel an application spine: @f a b c@ becomes @(f, [a, b, c])@.
splitApp :: LHsExpr GhcTc -> (LHsExpr GhcTc, [LHsExpr GhcTc])
splitApp = go []
  where
    go acc e = case stripExpr e of
      L _ (HsApp _ f a) -> go (a : acc) f
      other -> (other, acc)

analyseExpr :: Ctx -> LHsExpr GhcTc -> Analysis
analyseExpr ctx expr = case stripped of
  L l (HsVar _ (L vl var))
    | isIgnorableVar ctx var -> Analysis (CompOther "evidence") [] mempty
    | otherwise ->
        let ref = mkVarRef ctx (locA vl) var
         in Analysis (CompAlias ref) [Dep DepUse (TargetVar ref) (locOf (locA l)) (renderCode ctx stripped)] mempty
  L l (HsConLikeOut _ con) ->
    let name = conLikeText con
     in Analysis (CompConstructor name []) [Dep DepUse (TargetConstructor name) (locOf (locA l)) name] mempty
  L l (HsLit _ lit) -> literal l (renderCode ctx lit)
  L l (HsOverLit _ lit) -> literal l (renderCode ctx lit)
  L l (HsOverLabel _ fs) -> literal l (T.pack (showS fs))
  L _ HsApp {} -> uncurry (analyseApplication ctx stripped False) (splitApp stripped)
  L _ (OpApp _ left op right) -> analyseApplication ctx stripped True op [left, right]
  L _ (NegApp _ inner negOp) ->
    let an = analyseExpr ctx inner
        ops = syntaxAnalyses ctx [negOp]
     in Analysis
          (CompApply Nothing "negate" [renderCode ctx inner] False)
          (relabel (DepArgument 0) (anDeps an) <> relabel DepAppliedFunction (mergeDeps ops))
          (anCollected an <> mergeCollected ops)
  L _ (SectionL _ left op) -> analyseApplication ctx stripped True op [left]
  L _ (SectionR _ op right) -> analyseApplication ctx stripped True op [right]
  L _ (HsCase _ scrutinee matches) -> analyseCase ctx scrutinee matches
  L _ (HsIf _ cond thenE elseE) ->
    let condAn = analyseExpr ctx cond
        branchAns = map (analyseExpr ctx) [thenE, elseE]
     in Analysis
          (CompIf (renderCode ctx cond))
          (relabel DepCondition (anDeps condAn) <> relabel DepBranch (mergeDeps branchAns))
          (anCollected condAn <> mergeCollected branchAns)
  L _ (HsMultiIf _ grhss) ->
    let ans = map (analyseGRHS ctx) grhss
     in Analysis CompGuards (relabel DepBranch (mergeDeps ans)) (mergeCollected ans)
  L _ (HsLet _ binds body) ->
    let bodyAn = analyseExpr ctx body
     in Analysis CompLet (anDeps bodyAn) (collectLocalBinds ctx LetBind binds <> anCollected bodyAn)
  L _ (HsDo _ _ (L _ stmts)) -> analyseStmts ctx DoBind stmts
  L _ (HsLam _ matches) -> analyseLambda ctx matches
  L _ (HsLamCase _ matches) -> analyseLambda ctx matches
  L _ (ExplicitList _ items) ->
    let ans = map (analyseExpr ctx) items
     in Analysis
          (CompList (length items))
          (concat (zipWith (\i a -> relabel (DepElement i) (anDeps a)) [0 ..] ans))
          (mergeCollected ans)
  L _ (ExplicitTuple _ args _) ->
    let items = [e | Present _ e <- args]
        ans = map (analyseExpr ctx) items
     in Analysis
          (CompTuple (length args))
          (concat (zipWith (\i a -> relabel (DepElement i) (anDeps a)) [0 ..] ans))
          (mergeCollected ans)
  L _ (ExplicitSum _ _ _ inner) ->
    let an = analyseExpr ctx inner in Analysis (CompTuple 1) (anDeps an) (anCollected an)
  L _ (RecordCon _ conL flds) -> analyseRecordCon ctx (conLikeText (unXRec @GhcTc conL)) flds
  L _ (RecordUpd _ base flds) -> analyseRecordUpd ctx base flds
  L _ (HsGetField _ record fld) ->
    let an = analyseExpr ctx record
     in Analysis
          (CompFieldAccess (renderCode ctx record) (T.pack (showS fld)))
          (relabel DepRecordBase (anDeps an))
          (anCollected an)
  L l (HsRecFld _ fld) -> analyseFieldSelector ctx (locA l) fld
  L _ (HsProc _ pat cmd) -> analyseProc ctx pat cmd
  L _ (HsSpliceE _ splice) ->
    let ans = map (analyseExpr ctx) (splicedExprs splice)
     in Analysis (CompOther "template haskell splice") (mergeDeps ans) (mergeCollected ans)
  other -> fallbackExpr ctx other
  where
    stripped = stripExpr expr

    literal l code =
      Analysis (CompLiteral code) [Dep DepUse (TargetLiteral code) (locOf (locA l)) code] mempty

fallbackExpr :: Ctx -> LHsExpr GhcTc -> Analysis
fallbackExpr ctx (L _ e) =
  Analysis (CompOther (T.pack (constructorLabel e))) (mergeDeps ans) (mergeCollected ans)
  where
    ans = map (analyseExpr ctx) (childrenBi e :: [LHsExpr GhcTc])

-- | @proc pat -> cmd@: the pattern binds like a lambda, and the command is
-- walked properly so that arguments and bindings inside it are not lost.
analyseProc :: Ctx -> LPat GhcTc -> LHsCmdTop GhcTc -> Analysis
analyseProc ctx pat cmdTop =
  Analysis (CompLambda (map (pbPath') binders)) (anDeps cmdAn) (nodesOnly paramNodes <> anCollected cmdAn)
  where
    binders = patBinders "" pat
    pbPath' b = varOccText (pbVar b)
    paramNodes =
      [ mkNode ctx LambdaParam (Just 0) (pbVar b) (pbSpan b) (renderCode ctx pat) (CompParameter "proc" 0) [] False
      | b <- binders
      , not (isIgnorableVar ctx (pbVar b))
      , not (isSkippedBinding ctx (pbVar b))
      ]
    cmdAn = analyseCmdTop ctx cmdTop

analyseCmdTop :: Ctx -> LHsCmdTop GhcTc -> Analysis
analyseCmdTop ctx (L _ (HsCmdTop _ cmd)) = analyseCmd ctx cmd
analyseCmdTop _ _ = Analysis (CompOther "arrow procedure") [] mempty

analyseCmd :: Ctx -> LHsCmd GhcTc -> Analysis
analyseCmd ctx (L _ cmd) = case cmd of
  HsCmdArrApp _ f arg _ _ -> exprs [f, arg]
  HsCmdArrForm _ e _ _ tops ->
    let an = analyseExpr ctx e
        ans = map (analyseCmdTop ctx) tops
     in Analysis (CompOther "arrow form") (anDeps an <> mergeDeps ans) (anCollected an <> mergeCollected ans)
  HsCmdApp _ inner e ->
    let cmdAn = analyseCmd ctx inner
        argAn = analyseExpr ctx e
     in Analysis
          (CompOther "arrow application")
          (anDeps cmdAn <> relabel (DepArgument 0) (anDeps argAn))
          (anCollected cmdAn <> anCollected argAn)
  HsCmdLam _ mg -> cmdMatches mg
  HsCmdPar _ inner -> analyseCmd ctx inner
  HsCmdCase _ scrutinee mg ->
    let an = analyseExpr ctx scrutinee
        bodyAn = cmdMatches mg
     in Analysis
          (CompCase (renderCode ctx scrutinee))
          (relabel DepScrutinee (anDeps an) <> relabel DepBranch (anDeps bodyAn))
          (anCollected an <> anCollected bodyAn)
  HsCmdLamCase _ mg -> cmdMatches mg
  HsCmdIf _ _ cond thenCmd elseCmd ->
    let condAn = analyseExpr ctx cond
        branchAns = map (analyseCmd ctx) [thenCmd, elseCmd]
     in Analysis
          (CompIf (renderCode ctx cond))
          (relabel DepCondition (anDeps condAn) <> relabel DepBranch (mergeDeps branchAns))
          (anCollected condAn <> mergeCollected branchAns)
  HsCmdLet _ binds inner ->
    let an = analyseCmd ctx inner
     in Analysis CompLet (anDeps an) (collectLocalBinds ctx LetBind binds <> anCollected an)
  HsCmdDo _ (L _ stmts) ->
    let ans = map (analyseCmd ctx) [body | L _ (BodyStmt _ body _ _) <- stmts]
        collected = mconcat [collectLocalBinds ctx LetBind binds | L _ (LetStmt _ binds) <- stmts]
     in Analysis CompDoBlock (relabel DepBody (mergeDeps ans)) (mergeCollected ans <> collected)
  _ -> Analysis (CompOther "arrow command") [] mempty
  where
    exprs es =
      let ans = map (analyseExpr ctx) es
       in Analysis (CompOther "arrow command") (mergeDeps ans) (mergeCollected ans)

    cmdMatches (MG _ (L _ matches) _) =
      let ans = [analyseCmd ctx body | L _ (Match _ _ _ grhss) <- matches, body <- cmdBodies grhss]
       in Analysis (CompOther "arrow command") (mergeDeps ans) (mergeCollected ans)
    cmdMatches _ = Analysis (CompOther "arrow command") [] mempty

    cmdBodies (GRHSs _ grhss _) = [body | L _ (GRHS _ _ body) <- grhss]
    cmdBodies _ = []

-- | Expressions carried by a Template Haskell splice that survived to the
-- type-checked tree.
splicedExprs :: HsSplice GhcTc -> [LHsExpr GhcTc]
splicedExprs = \case
  HsTypedSplice _ _ _ e -> [e]
  HsUntypedSplice _ _ _ e -> [e]
  HsSpliced _ _ (HsSplicedExpr e) -> [wrapXRec @GhcTc e]
  _ -> []

constructorLabel :: HsExpr GhcTc -> String
constructorLabel = \case
  ArithSeq {} -> "arithmetic sequence"
  HsStatic {} -> "static pointer"
  HsIPVar {} -> "implicit parameter"
  HsUnboundVar {} -> "unbound variable"
  _ -> "expression"

-- | Application of a function, operator or data constructor.
analyseApplication :: Ctx -> LHsExpr GhcTc -> Bool -> LHsExpr GhcTc -> [LHsExpr GhcTc] -> Analysis
analyseApplication ctx whole isOperator headExpr args =
  Analysis computation (headDeps <> argDeps) collected
  where
    headAn = analyseExpr ctx (stripExpr headExpr)
    argAns = map (analyseExpr ctx) args
    headDeps = relabel DepAppliedFunction (anDeps headAn)
    argDeps = concat (zipWith (\i a -> relabel (DepArgument i) (anDeps a)) [0 ..] argAns)
    argCodes = map (renderCode ctx) args

    headVar = headVarOf ctx headExpr

    computation = case stripExpr headExpr of
      L _ (HsConLikeOut _ con) ->
        CompConstructor (conLikeText con) argCodes
      _ ->
        CompApply
          (fmap (\(v, sp) -> mkVarRef ctx sp v) headVar)
          (renderCode ctx headExpr)
          argCodes
          isOperator

    callSites = case headVar of
      Nothing -> []
      Just (v, _) ->
        [ CallSite
            { csCalleeKey = varKey ctx v
            , csCalleeName = varOccText (resolveVar ctx v)
            , csArgs = zipWith mkArg [0 ..] (zip args argAns)
            , csLoc = locOf (exprSpan whole)
            , csEnclosing = ctxOwner ctx
            , csModule = ceModule (ctxEnv ctx)
            }
        ]

    mkArg i (argExpr, an) =
      CallArg
        { caIndex = i
        , caCode = renderCode ctx argExpr
        , caDeps = anDeps an
        , caLoc = locOf (exprSpan argExpr)
        }

    collected = anCollected headAn <> mergeCollected argAns <> Collected [] [] callSites

exprSpan :: LHsExpr GhcTc -> SrcSpan
exprSpan (L l _) = locA l

-- | The variable being applied, if the head of an application is one.
--
-- A record selector reaches the type checker as 'HsRecFld' rather than
-- 'HsVar', so @grossAmount order@ has to be recognised here too — otherwise no
-- call site is recorded for it and a field access becomes a dead end.
headVarOf :: Ctx -> LHsExpr GhcTc -> Maybe (Var, SrcSpan)
headVarOf ctx expr = case stripExpr expr of
  L vl (HsVar _ (L _ v)) | usable v -> Just (v, locA vl)
  L vl (HsRecFld _ fld) | Just v <- fieldSelectorId fld, usable v -> Just (v, locA vl)
  _ -> Nothing
  where
    usable v = not (isIgnorableVar ctx v)

fieldSelectorId :: AmbiguousFieldOcc GhcTc -> Maybe Var
fieldSelectorId = \case
  Unambiguous selector _ -> Just selector
  Ambiguous selector _ -> Just selector
  _ -> Nothing

analyseCase :: Ctx -> LHsExpr GhcTc -> MatchGroup GhcTc (LHsExpr GhcTc) -> Analysis
analyseCase ctx scrutinee matches =
  Analysis (CompCase scrutineeCode) (scrutineeDeps <> bodyDeps) collected
  where
    scrutAn = analyseExpr ctx scrutinee
    scrutineeCode = renderCode ctx scrutinee
    scrutineeDeps = relabel DepScrutinee (anDeps scrutAn)
    patternDeps = relabel DepPatternSource (anDeps scrutAn)

    (binderNodes, bodyAn) = analyseCaseAlts ctx scrutineeCode patternDeps matches
    bodyDeps = relabel DepBranch (anDeps bodyAn)
    collected = anCollected scrutAn <> nodesOnly binderNodes <> anCollected bodyAn

analyseCaseAlts
  :: Ctx
  -> Text
  -> [Dep]
  -> MatchGroup GhcTc (LHsExpr GhcTc)
  -> ([ProvenanceNode], Analysis)
analyseCaseAlts ctx scrutineeCode patternDeps (MG _ (L _ matches) _) =
  (concatMap fst results, Analysis CompGuards (mergeDeps bodyAns) (mergeCollected bodyAns))
  where
    results = map alt matches
    bodyAns = map snd results

    alt (L _ (Match _ _ pats grhss)) =
      ( [ mkNode ctx CaseBind Nothing (pbVar b) (pbSpan b) scrutineeCode (CompPatternProjection scrutineeCode (pbPath b)) patternDeps False
        | p <- pats
        , b <- patBinders "" p
        , not (isIgnorableVar ctx (pbVar b))
        ]
      , analyseGRHSs ctx grhss
      )
    alt _ = ([], Analysis (CompOther "unsupported alternative") [] mempty)
analyseCaseAlts _ _ _ _ = ([], Analysis (CompOther "unsupported alternatives") [] mempty)

analyseLambda :: Ctx -> MatchGroup GhcTc (LHsExpr GhcTc) -> Analysis
analyseLambda ctx matches =
  Analysis (CompLambda (map (T.intercalate "|") (map (map keyName) paramKeys))) (anDeps bodyAn) collected
  where
    (paramNodes, paramKeys, bodyAn) = analyseMatches ctx Nothing LambdaParam matches
    collected = nodesOnly paramNodes <> anCollected bodyAn
    keyName = T.takeWhileEnd (/= ':')

analyseRecordCon :: Ctx -> Text -> HsRecFields GhcTc (LHsExpr GhcTc) -> Analysis
analyseRecordCon ctx conName (HsRecFields {rec_flds = flds}) =
  Analysis (CompRecordCon conName (zip labels codes)) fieldDeps (mergeCollected ans)
  where
    parts = map recFieldParts flds
    labels = map fst parts
    fieldExprs = map snd parts
    ans = map (analyseExpr ctx) fieldExprs
    codes = map (renderCode ctx) fieldExprs
    conDep = [Dep DepUse (TargetConstructor conName) emptyLoc conName | not (T.null conName)]
    fieldDeps =
      conDep
        <> concat (zipWith (\lbl a -> relabel (DepRecordField lbl) (anDeps a)) labels ans)

analyseRecordUpd
  :: Ctx
  -> LHsExpr GhcTc
  -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc]
  -> Analysis
analyseRecordUpd ctx base flds =
  Analysis
    (CompRecordUpdate (renderCode ctx base) (zip labels codes))
    (relabel DepRecordBase (anDeps baseAn) <> fieldDeps)
    (anCollected baseAn <> mergeCollected ans)
  where
    baseAn = analyseExpr ctx base
    parts = case flds of
      Left updates -> map recFieldParts updates
      Right projections -> map recFieldParts projections
    labels = map fst parts
    fieldExprs = map snd parts
    ans = map (analyseExpr ctx) fieldExprs
    codes = map (renderCode ctx) fieldExprs
    fieldDeps = concat (zipWith (\lbl a -> relabel (DepRecordField lbl) (anDeps a)) labels ans)

-- | A record selector used as a value, e.g. @map amount orders@.
analyseFieldSelector :: Ctx -> SrcSpan -> AmbiguousFieldOcc GhcTc -> Analysis
analyseFieldSelector ctx sp fld = case fieldSelectorId fld of
  Just selector
    | not (isIgnorableVar ctx selector) ->
        let ref = mkVarRef ctx sp selector
         in Analysis
              (CompFieldAccess "" (vrName ref))
              [Dep DepUse (TargetVar ref) (locOf sp) (vrName ref)]
              mempty
  _ ->
    let name = T.pack (showS fld)
     in Analysis (CompFieldAccess "" name) [Dep DepUse (TargetField name) (locOf sp) name] mempty

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | Combine per-equation parameter binders into one list indexed by position.
mergeByIndex :: [[[NodeKey]]] -> [[NodeKey]]
mergeByIndex = foldl' step []
  where
    step acc keys = zipLong acc keys
    zipLong xs [] = xs
    zipLong [] ys = ys
    zipLong (x : xs) (y : ys) = (x <> y) : zipLong xs ys

dedupNodes :: [ProvenanceNode] -> [ProvenanceNode]
dedupNodes = Map.elems . Map.fromListWith (\_new old -> old) . map (\n -> (pnKey n, n))

dedupFunctions :: [FunctionNode] -> [FunctionNode]
dedupFunctions = Map.elems . Map.fromListWith (\_new old -> old) . map (\f -> (fnKey f, f))

#else

--------------------------------------------------------------------------------
-- GHC 8.10 and older
--
-- The collector matches on GHC 9 AST shapes (ConPat, XXExprGhcTc, the 9.x
-- statement forms). Rather than ship an untested second traversal, older
-- compilers get an empty graph carrying a note, so that a package depending on
-- this library still builds.
--------------------------------------------------------------------------------

mkCollectEnv :: TracerOpts -> ModSummary -> CollectEnv
mkCollectEnv opts modSummary =
  CollectEnv
    { ceModule = T.pack (moduleNameString (moduleName (ms_mod modSummary)))
    , cePackage = T.pack ""
    , ceFile = T.pack (msHsFilePath modSummary)
    , ceOpts = opts
    }

collectModuleGraph :: TracerOpts -> ModSummary -> TcGblEnv -> ModuleGraph
collectModuleGraph opts modSummary _ =
  let env = mkCollectEnv opts modSummary
   in (emptyModuleGraph (ceModule env) (cePackage env) (ceFile env))
        { mgNotes = ["variableTracer requires GHC >= 9.0; no graph was collected"]
        }

locToSrcSpan :: Loc -> SrcSpan
locToSrcSpan _ = noSrcSpan

collectFromBinds :: CollectEnv -> a -> ModuleGraph
collectFromBinds env _ =
  (emptyModuleGraph (ceModule env) (cePackage env) (ceFile env))
    { mgNotes = ["variableTracer requires GHC >= 9.0; no graph was collected"]
    }

#endif
