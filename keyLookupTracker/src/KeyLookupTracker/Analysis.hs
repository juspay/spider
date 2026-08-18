{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : KeyLookupTracker.Analysis
Description : Find the keys looked up in a module, via variable provenance.

Pure: it takes a provenance graph (from @variableTracer@) and answers "which
keys does this module look up, and from which function".

The point of going through the graph rather than pattern matching on the call
expression is that a key is very often not a literal at the point of the
lookup. All of these report the key they actually use:

@
HM.lookup "AB" hm                    -- literal argument
"BC" \`HM.lookup\` hm                  -- infix
Prelude.map ("CD" \`HM.lookup\`) [hm]  -- left section
(HM.lookup "DE") \<$\> [hm]            -- partial application
getKeyFromHM "EF" hm                 -- key arrives as a parameter, resolved
                                     -- through the call site
@

Keys that cannot be resolved to literals (built at runtime, or coming from
outside the analysed modules) are reported separately in 'lsUnresolved' rather
than silently dropped, so coverage is visible.
-}
module KeyLookupTracker.Analysis
  ( LookupSite (..)
  , collectLookupSites
  , lookupsByFunction
  , isEligibleLookup
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import VariableTracer

-- | One resolved lookup call.
data LookupSite = LookupSite
  { lsFunction :: Text
  -- ^ the top-level function the lookup happens in
  , lsCallee :: Text
  -- ^ the lookup function that was called
  , lsKeys :: [Text]
  -- ^ keys the key argument resolves to (a lookup inside a helper called with
  -- two different keys legitimately reports both)
  , lsUnresolved :: [Text]
  -- ^ key expressions that did not bottom out in a literal
  , lsLoc :: Loc
  }
  deriving (Show, Eq)

-- | Does this call site call one of the configured lookup functions?
--
-- Matches on the bare name (@lookup@, trailing @*@ allowed) so that
-- @Data.HashMap.Strict.lookup@ and @Data.Map.lookup@ both hit, and on the
-- stable name so a rule can be qualified (@Data.HashMap.Internal.lookup@)
-- when only one of them should.
isEligibleLookup :: [Text] -> CallSite -> Bool
isEligibleLookup eligible site =
  any matches eligible
  where
    matches spec =
      matchesPattern spec (csCalleeName site)
        || (T.isInfixOf "." spec && spec `T.isInfixOf` T.replace "$" "." (csCalleeKey site))

-- | Every eligible lookup in the graph, with its key argument resolved.
collectLookupSites
  :: [Text]
  -- ^ eligible lookup function names
  -> Map.Map Text Int
  -- ^ key argument position per function name; anything not listed uses 0,
  -- which is right for @Data.Map.lookup@ and @Data.HashMap.lookup@
  -> ProgramGraph
  -> [LookupSite]
collectLookupSites eligible keyArgIndexes program =
  [ site
  | callSite <- concat (Map.elems (pgCallSites program))
  , isEligibleLookup eligible callSite
  , Just site <- [resolve callSite]
  ]
  where
    resolve callSite = do
      let idx = fromMaybe 0 (Map.lookup (csCalleeName callSite) keyArgIndexes)
      keyArg <- listToMaybe' [a | a <- csArgs callSite, caIndex a == idx]
      let forest = traceDeps program traceCfg (caDeps keyArg)
          keys = nub (literalRoots forest)
      pure
        LookupSite
          { lsFunction = enclosingFunction callSite
          , lsCallee = csCalleeName callSite
          , lsKeys = keys
          , lsUnresolved = if null keys then [caCode keyArg] else []
          , lsLoc = csLoc callSite
          }

    -- Resolving a key means following it out of helpers into their callers,
    -- but there is no point walking into the body of a called function: a key
    -- is a value, not a computation we care about the internals of.
    traceCfg = defaultTraceOpts {toFollowIntoFunctions = False, toMaxDepth = 15}

    enclosingFunction callSite =
      case csEnclosing callSite >>= outermostOwner program of
        Just owner -> vrName (pnVar owner)
        Nothing -> "<top level>"

    listToMaybe' xs = case xs of
      (x : _) -> Just x
      [] -> Nothing

-- | The plugin's on-disk shape: top-level function name -> keys it looks up.
--
-- Every top-level function is listed, with an empty list when it looks nothing
-- up, matching what the plugin emitted before it used the provenance graph.
lookupsByFunction :: ProgramGraph -> [LookupSite] -> [(String, [String])]
lookupsByFunction program sites =
  [ (T.unpack fn, map T.unpack (nub keys))
  | (fn, keys) <- Map.toList (Map.unionWith (<>) found emptyEntries)
  ]
  where
    found = Map.fromListWith (flip (<>)) [(lsFunction s, lsKeys s) | s <- sites]

    emptyEntries =
      Map.fromList
        [ (vrName (pnVar n), [])
        | n <- Map.elems (pgNodes program)
        , pnBindKind n == TopLevelBind
        ]
