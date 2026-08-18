{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : VariableTracer.Trace
Description : Walk a provenance graph backwards from a variable to its roots.

Given a binder, this answers "what computations built this value, all the way
down". It follows

  * dependency edges (arguments, scrutinees, record fields, monadic actions, …),
  * function parameters through every recorded call site — across modules once
    the graphs are linked, and
  * applied functions into their bodies,

until it reaches a root: a literal, a constructor, a function defined outside
the graph, or a parameter of an entry point that is never called inside the
analysed program.

Cycles (recursion), depth and total node count are all bounded, so a trace
always terminates.
-}
module VariableTracer.Trace
  ( traceVariable
  , traceTargets
    -- * Tracing arbitrary dependencies
    --
    -- | For callers that hold a dependency list rather than a binder — the
    -- arguments of one call site, say — and want to know what those values are
    -- built from. This is the entry point rule-checking plugins want.
  , traceDeps
  , traceLeaves
  , pathsTo
  , literalRoots
    -- * Rendering
  , renderTraceText
  , renderTracesText
  , renderTraceDot
  ) where

import Data.List (foldl')
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import VariableTracer.Graph
import VariableTracer.Types

--------------------------------------------------------------------------------
-- A very small state monad, so we can thread the budget without pulling in mtl
--------------------------------------------------------------------------------

data TState = TState
  { stBudget :: !Int
  , stPath :: !(Set NodeKey)
  , stExpanded :: !(Set NodeKey)
  }

newtype TM a = TM {runTM :: TState -> (a, TState)}

instance Functor TM where
  fmap f (TM g) = TM $ \s -> let (a, s') = g s in (f a, s')

instance Applicative TM where
  pure a = TM $ \s -> (a, s)
  TM f <*> TM g = TM $ \s ->
    let (h, s') = f s
        (a, s'') = g s'
     in (h a, s'')

instance Monad TM where
  TM g >>= f = TM $ \s -> let (a, s') = g s in runTM (f a) s'

get' :: TM TState
get' = TM $ \s -> (s, s)

modify' :: (TState -> TState) -> TM ()
modify' f = TM $ \s -> ((), f s)

mapM'' :: (a -> TM b) -> [a] -> TM [b]
mapM'' f = foldr step (pure [])
  where
    step x acc = do
      y <- f x
      ys <- acc
      pure (y : ys)

--------------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------------

traceTargets :: ProgramGraph -> TraceOpts -> [TargetSpec] -> [VariableTrace]
traceTargets pg opts specs = map (traceVariable pg opts) (matchTargets pg specs)

-- | Build the full computation tree for one binder.
traceVariable :: ProgramGraph -> TraceOpts -> ProvenanceNode -> VariableTrace
traceVariable pg opts node =
  let initial = TState (toMaxNodes opts) Set.empty Set.empty
      (root, finalState) = runTM (expandNode pg opts 0 Nothing node) initial
      leaves = collectLeaves root
   in VariableTrace
        { vtTarget = vrName (pnVar node)
        , vtTargetKey = pnKey node
        , vtModule = vrModule (pnVar node)
        , vtRoot = root
        , vtRoots = leaves
        , vtComputations = collectComputations root
        , vtNodeCount = toMaxNodes opts - stBudget finalState
        , vtTruncated = stBudget finalState <= 0
        }

-- | Expand a dependency list — the arguments of a call site, the fields of a
-- record, whatever you have — into the trees of what those values are built
-- from. One tree per dependency.
traceDeps :: ProgramGraph -> TraceOpts -> [Dep] -> [TraceNode]
traceDeps pg opts deps =
  fst (runTM (mapM'' (expandDep pg opts 0) deps) (TState (toMaxNodes opts) Set.empty Set.empty))

-- | Where a trace bottomed out: literals, constructors, external functions and
-- uncalled parameters.
traceLeaves :: TraceNode -> [TraceNode]
traceLeaves = collectLeaves

-- | Every path from this node down to a node satisfying the predicate.
--
-- Use it to answer "does this value derive from X, and how did it get here" —
-- the returned path reads as the explanation.
pathsTo :: (TraceNode -> Bool) -> TraceNode -> [[TraceNode]]
pathsTo p = go []
  where
    go acc n =
      let path = acc <> [n]
       in [path | p n] <> concatMap (go path) (tnChildren n)

-- | The literal values a trace bottoms out in, with their quoting removed.
literalRoots :: [TraceNode] -> [Text]
literalRoots =
  map (T.dropAround (== '"') . tnCode)
    . filter ((== RootLiteral) . tnStatus)
    . concatMap traceLeaves

expandNode :: ProgramGraph -> TraceOpts -> Int -> Maybe DepKind -> ProvenanceNode -> TM TraceNode
expandNode pg opts depth edge node = do
  st <- get'
  let key = pnKey node
      stop status = pure (nodeToTrace pg depth edge node status [])
  if stBudget st <= 0
    then stop NodeBudgetExhausted
    else
      if depth > toMaxDepth opts
        then stop DepthLimitReached
        else
          if key `Set.member` stPath st
            then stop CycleDetected
            else
              if key `Set.member` stExpanded st && not (toReexpandShared opts)
                then stop AlreadyExpanded
                else do
                  modify' $ \s ->
                    s
                      { stBudget = stBudget s - 1
                      , stPath = Set.insert key (stPath s)
                      , stExpanded = Set.insert key (stExpanded s)
                      }
                  depKids <- mapM'' (expandDep pg opts (depth + 1)) (relevantDeps opts node)
                  paramKids <- expandParameter pg opts (depth + 1) node
                  modify' $ \s -> s {stPath = Set.delete key (stPath s)}
                  let kids = depKids <> paramKids
                      status
                        | not (null kids) = Expanded
                        | pnBindKind node == FunctionParam = RootParameter
                        | otherwise = Expanded
                  pure (nodeToTrace pg depth edge node status kids)

relevantDeps :: TraceOpts -> ProvenanceNode -> [Dep]
relevantDeps opts node
  | toIncludeLiterals opts = pnDeps node
  | otherwise = filter (not . isLiteral) (pnDeps node)
  where
    isLiteral d = case depTarget d of
      TargetLiteral _ -> True
      _ -> False

expandDep :: ProgramGraph -> TraceOpts -> Int -> Dep -> TM TraceNode
expandDep pg opts depth dep = case depTarget dep of
  TargetLiteral lit -> pure (leafTrace depth (Just (depKind dep)) ("literal " <> lit) lit RootLiteral (depLoc dep))
  TargetConstructor con -> pure (leafTrace depth (Just (depKind dep)) con con RootConstructor (depLoc dep))
  TargetField fld -> pure (leafTrace depth (Just (depKind dep)) ("field " <> fld) fld RootConstructor (depLoc dep))
  TargetVar var ->
    case lookupNode pg (vrKey var) of
      Nothing -> pure (unresolvedTrace depth (Just (depKind dep)) var)
      Just target
        | pnIsFunction target && not (toFollowIntoFunctions opts) ->
            pure (externalTrace depth (Just (depKind dep)) var)
        | otherwise -> expandNode pg opts depth (Just (depKind dep)) target

-- | Interprocedural step: a function parameter is built out of whatever every
-- caller passed in that position.
expandParameter :: ProgramGraph -> TraceOpts -> Int -> ProvenanceNode -> TM [TraceNode]
expandParameter pg opts depth node
  | not (toFollowCallSites opts) = pure []
  | otherwise = case (pnBindKind node, pnParamIndex node, pnOwner node) of
      (FunctionParam, Just idx, Just ownerKey) -> do
        let sites = take (toMaxCallSites opts) (callSitesOf pg ownerKey)
            argsAt = mapMaybe (argAt idx) sites
        concat <$> mapM'' (uncurry (expandCallArg pg opts depth idx)) argsAt
      _ -> pure []
  where
    argAt idx site =
      fmap ((,) site) (listToMaybe (filter ((== idx) . caIndex) (csArgs site)))

expandCallArg :: ProgramGraph -> TraceOpts -> Int -> Int -> CallSite -> CallArg -> TM [TraceNode]
expandCallArg pg opts depth idx site arg = do
  kids <- mapM'' (expandDep pg opts (depth + 1)) (caDeps arg)
  pure
    [ TraceNode
        { tnKey = csCalleeKey site <> "@" <> renderLoc (csLoc site) <> "#" <> T.pack (show idx)
        , tnLabel = "call site: " <> csCalleeName site <> " argument #" <> T.pack (show idx)
        , tnEdge = Just (DepCallSiteArg idx)
        , tnBindKind = Nothing
        , tnComputation = Nothing
        , tnDescription = "value passed as argument #" <> T.pack (show idx) <> " at " <> renderLoc (csLoc site)
        , tnCode = caCode arg
        , tnType = ""
        , tnModule = csModule site
        , tnLoc = csLoc site
        , tnDepth = depth
        , tnStatus = if null kids then RootUnresolved else Expanded
        , tnChildren = kids
        }
    ]

--------------------------------------------------------------------------------
-- Trace node construction
--------------------------------------------------------------------------------

nodeToTrace :: ProgramGraph -> Int -> Maybe DepKind -> ProvenanceNode -> TraceStatus -> [TraceNode] -> TraceNode
nodeToTrace pg depth edge node status kids =
  TraceNode
    { tnKey = pnKey node
    , tnLabel = label
    , tnEdge = edge
    , tnBindKind = Just (pnBindKind node)
    , tnComputation = Just (pnComputation node)
    , tnDescription = describeComputation (pnComputation node)
    , tnCode = pnCode node
    , tnType = vrType (pnVar node)
    , tnModule = vrModule (pnVar node)
    , tnLoc = pnLoc node
    , tnDepth = depth
    , tnStatus = status
    , tnChildren = kids
    }
  where
    owner = ownerFunctionName pg node
    name = vrName (pnVar node)
    label
      | owner == name = name
      | otherwise = owner <> "." <> name

leafTrace :: Int -> Maybe DepKind -> Text -> Text -> TraceStatus -> Loc -> TraceNode
leafTrace depth edge label code status loc =
  TraceNode
    { tnKey = label
    , tnLabel = label
    , tnEdge = edge
    , tnBindKind = Nothing
    , tnComputation = Nothing
    , tnDescription = label
    , tnCode = code
    , tnType = ""
    , tnModule = ""
    , tnLoc = loc
    , tnDepth = depth
    , tnStatus = status
    , tnChildren = []
    }

externalTrace :: Int -> Maybe DepKind -> VarRef -> TraceNode
externalTrace depth edge var =
  (leafTrace depth edge (vrName var) (vrName var) RootExternal (vrLoc var))
    { tnKey = vrKey var
    , tnType = vrType var
    , tnModule = vrModule var
    , tnDescription = "defined outside the analysed graph: " <> vrStableName var
    }

unresolvedTrace :: Int -> Maybe DepKind -> VarRef -> TraceNode
unresolvedTrace depth edge var =
  (leafTrace depth edge (vrName var) (vrName var) status (vrLoc var))
    { tnKey = vrKey var
    , tnType = vrType var
    , tnModule = vrModule var
    , tnDescription = case vrScope var of
        GlobalVar -> "defined outside the analysed graph: " <> vrStableName var
        LocalVar -> "no binding recorded for " <> vrName var
    }
  where
    status = case vrScope var of
      GlobalVar -> RootExternal
      LocalVar -> RootUnresolved

--------------------------------------------------------------------------------
-- Summaries
--------------------------------------------------------------------------------

-- | The leaves where the trace actually bottomed out.
--
-- Nodes that were merely expanded elsewhere (or closed a recursive cycle) are
-- not roots: their real roots appear under their first expansion.
collectLeaves :: TraceNode -> [TraceNode]
collectLeaves = dedupOn tnKey . filter isRoot . leaves
  where
    leaves n
      | null (tnChildren n) = [n]
      | otherwise = concatMap leaves (tnChildren n)

    isRoot n = case tnStatus n of
      AlreadyExpanded -> False
      CycleDetected -> False
      _ -> True

    dedupOn key = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | key x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert (key x) seen) xs

-- | Every computation in the tree, deepest first, deduplicated.
collectComputations :: TraceNode -> [Text]
collectComputations = dedup . reverse . go
  where
    go n =
      maybe [] (const [tnLabel n <> " = " <> tnDescription n <> loc n]) (tnComputation n)
        <> concatMap go (tnChildren n)

    loc n = if isEmptyLoc (tnLoc n) then "" else "  (" <> renderLoc (tnLoc n) <> ")"

    dedup = foldl' (\acc x -> if x `elem` acc then acc else acc <> [x]) []

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderTracesText :: [VariableTrace] -> Text
renderTracesText = T.intercalate "\n\n" . map renderTraceText

-- | Human readable tree, the way you would want it in a terminal.
renderTraceText :: VariableTrace -> Text
renderTraceText vt =
  T.unlines $
    [ "variable : " <> vtTarget vt
    , "module   : " <> vtModule vt
    , "key      : " <> vtTargetKey vt
    , "nodes    : " <> T.pack (show (vtNodeCount vt)) <> (if vtTruncated vt then " (truncated: node budget exhausted)" else "")
    , ""
    ]
      <> renderTree (vtRoot vt)
      <> [""]
      <> ["roots:"]
      <> map (("  - " <>) . renderRoot) (vtRoots vt)

renderTree :: TraceNode -> [Text]
renderTree n = headline n : renderChildren "" (tnChildren n)

renderChildren :: Text -> [TraceNode] -> [Text]
renderChildren indent kids = concat (zipWith render [1 :: Int ..] kids)
  where
    total = length kids
    render i k =
      let isLast = i == total
          branch = if isLast then "`- " else "|- "
          childIndent = indent <> (if isLast then "   " else "|  ")
       in (indent <> branch <> headline k) : renderChildren childIndent (tnChildren k)

headline :: TraceNode -> Text
headline n =
  edgeLabel
    <> tnLabel n
    <> " :: "
    <> shortType
    <> "  -- "
    <> tnDescription n
    <> statusLabel
    <> locLabel
  where
    edgeLabel = maybe "" (\e -> "[" <> describeDepKind e <> "] ") (tnEdge n)
    shortType = if T.null (tnType n) then "?" else T.take 60 (oneLine (tnType n))
    locLabel = if isEmptyLoc (tnLoc n) then "" else "  @ " <> renderLoc (tnLoc n)
    statusLabel = case tnStatus n of
      Expanded -> ""
      other -> "  <" <> T.pack (show other) <> ">"

renderRoot :: TraceNode -> Text
renderRoot n =
  tnLabel n
    <> " <"
    <> T.pack (show (tnStatus n))
    <> ">"
    <> (if isEmptyLoc (tnLoc n) then "" else " @ " <> renderLoc (tnLoc n))

oneLine :: Text -> Text
oneLine = T.unwords . T.words

-- | Graphviz output, handy for eyeballing a large trace.
renderTraceDot :: VariableTrace -> Text
renderTraceDot vt =
  T.unlines $
    ["digraph variable_trace {", "  rankdir=RL;", "  node [shape=box, fontname=\"monospace\"];"]
      <> uniq (nodes (vtRoot vt))
      <> uniq (edges (vtRoot vt))
      <> ["}"]
  where
    uniq = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs

    nodes n =
      ("  " <> quoted (tnKey n) <> " [label=" <> quoted (tnLabel n <> "\\n" <> tnDescription n) <> style n <> "];")
        : concatMap nodes (tnChildren n)

    edges n =
      [ "  " <> quoted (tnKey n) <> " -> " <> quoted (tnKey c) <> " [label=" <> quoted (maybe "" describeDepKind (tnEdge c)) <> "];"
      | c <- tnChildren n
      ]
        <> concatMap edges (tnChildren n)

    style n = case tnStatus n of
      Expanded -> ""
      RootLiteral -> ", style=filled, fillcolor=lightgrey"
      RootConstructor -> ", style=filled, fillcolor=lightgrey"
      RootExternal -> ", style=filled, fillcolor=lightblue"
      RootParameter -> ", style=filled, fillcolor=lightyellow"
      _ -> ", style=dashed"

    quoted t = "\"" <> T.replace "\"" "\\\"" (oneLine t) <> "\""
