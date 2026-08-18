{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : VariableTracer.Graph
Description : Indexing, linking and querying of provenance graphs.

Pure code: it turns the per-module graphs emitted by the plugin into one
whole-program graph and provides the lookups the tracer needs.

Linking works because top-level binders are keyed by GHC's stable name
(@$package$Module$name@), so a call recorded in module @A@ and the parameter
binders recorded in module @B@ agree on the callee key without any extra
bookkeeping.
-}
module VariableTracer.Graph
  ( -- * Building
    fromModuleGraphs
  , addModuleGraph
    -- * Lookups
  , resolveKey
  , lookupNode
  , lookupFunction
  , callSitesOf
  , nodesNamed
  , ownerFunctionName
  , outermostOwner
    -- * Target selection
  , matchTargets
  , matchTarget
    -- | Re-exported from "VariableTracer.Types" for convenience.
  , matchesPattern
  ) where

import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

import VariableTracer.Types

--------------------------------------------------------------------------------
-- Building
--------------------------------------------------------------------------------

fromModuleGraphs :: [ModuleGraph] -> ProgramGraph
fromModuleGraphs = foldl' (flip addModuleGraph) emptyProgramGraph

-- | Merge one module's graph into the program graph.
--
-- Node keys collide across modules only for external names, and then the two
-- descriptions are of the same binder; we keep the first one that carries a
-- real source location so that a definition wins over a mere reference.
addModuleGraph :: ModuleGraph -> ProgramGraph -> ProgramGraph
addModuleGraph mg pg =
  pg
    { pgNodes = foldl' insertNode (pgNodes pg) (mgNodes mg)
    , pgFunctions = foldl' insertFunction (pgFunctions pg) (mgFunctions mg)
    , pgCallSites = foldl' insertCall (pgCallSites pg) (mgCallSites mg)
    , pgAliases = foldl' insertAlias (pgAliases pg) (mgAliases mg)
    , pgNameIndex = foldl' insertName (pgNameIndex pg) (mgNodes mg)
    , pgModules = nub (pgModules pg <> [mgModule mg])
    }
  where
    insertNode acc n = Map.insertWith preferDefined (pnKey n) n acc

    preferDefined new old
      | isEmptyLoc (pnLoc old) && not (isEmptyLoc (pnLoc new)) = new
      | otherwise = old

    insertFunction acc f = Map.insertWith (\new old -> if fnArity old >= fnArity new then old else new) (fnKey f) f acc

    insertCall acc c = Map.insertWith (<>) (csCalleeKey c) [c] acc

    insertAlias acc (from, to) = Map.insert from to acc

    insertName acc n = Map.insertWith (\new old -> nub (old <> new)) (vrName (pnVar n)) [pnKey n] acc

--------------------------------------------------------------------------------
-- Lookups
--------------------------------------------------------------------------------

-- | Follow @AbsBinds@ monomorphic-to-polymorphic aliases (bounded, so a broken
-- graph cannot loop forever).
resolveKey :: ProgramGraph -> NodeKey -> NodeKey
resolveKey pg = go (16 :: Int)
  where
    go 0 k = k
    go n k = case Map.lookup k (pgAliases pg) of
      Just k' | k' /= k -> go (n - 1) k'
      _ -> k

lookupNode :: ProgramGraph -> NodeKey -> Maybe ProvenanceNode
lookupNode pg k =
  case Map.lookup k (pgNodes pg) of
    Just n -> Just n
    Nothing -> Map.lookup (resolveKey pg k) (pgNodes pg)

lookupFunction :: ProgramGraph -> NodeKey -> Maybe FunctionNode
lookupFunction pg k =
  case Map.lookup k (pgFunctions pg) of
    Just f -> Just f
    Nothing -> Map.lookup (resolveKey pg k) (pgFunctions pg)

-- | Every recorded application of the given callee, from any linked module.
callSitesOf :: ProgramGraph -> NodeKey -> [CallSite]
callSitesOf pg k =
  fromMaybe [] (Map.lookup k (pgCallSites pg))
    <> (if resolved /= k then fromMaybe [] (Map.lookup resolved (pgCallSites pg)) else [])
  where
    resolved = resolveKey pg k

nodesNamed :: ProgramGraph -> Text -> [ProvenanceNode]
nodesNamed pg name =
  mapMaybe (lookupNode pg) (fromMaybe [] (Map.lookup name (pgNameIndex pg)))

-- | Name of the function a binder lives in (its own name for a top-level bind).
ownerFunctionName :: ProgramGraph -> ProvenanceNode -> Text
ownerFunctionName pg n = case pnOwnerName n of
  Just owner -> owner
  Nothing -> case pnOwner n >>= lookupNode pg of
    Just o -> vrName (pnVar o)
    Nothing -> vrName (pnVar n)

-- | Walk the owner chain up to the outermost enclosing binder — the top-level
-- function a nested @where@ helper ultimately lives in. Bounded, so a
-- malformed graph cannot loop.
outermostOwner :: ProgramGraph -> NodeKey -> Maybe ProvenanceNode
outermostOwner pg = go (32 :: Int)
  where
    go 0 k = lookupNode pg k
    go n k = do
      node <- lookupNode pg k
      case pnOwner node of
        Nothing -> Just node
        Just parent -> case go (n - 1) parent of
          Just outer -> Just outer
          Nothing -> Just node

--------------------------------------------------------------------------------
-- Target selection
--------------------------------------------------------------------------------

matchTargets :: ProgramGraph -> [TargetSpec] -> [ProvenanceNode]
matchTargets pg = concatMap (matchTarget pg)

-- | Resolve a 'TargetSpec' to the binders it names.
--
-- A target can be given as a bare variable name (@"amount"@), as a stable name
-- or node key (matched exactly), or narrowed by function and module. All
-- patterns accept a trailing @*@.
matchTarget :: ProgramGraph -> TargetSpec -> [ProvenanceNode]
matchTarget pg spec =
  case Map.lookup (tsVariable spec) (pgNodes pg) of
    Just n | matchesRest n -> [n]
    _ -> filter matches (Map.elems (pgNodes pg))
  where
    matches n =
      matchesPattern (tsVariable spec) (vrName (pnVar n))
        && matchesRest n

    matchesRest n =
      maybe True (\f -> matchesPattern f (ownerFunctionName pg n)) (tsFunction spec)
        && maybe True (\m -> matchesPattern m (vrModule (pnVar n))) (tsModule spec)
