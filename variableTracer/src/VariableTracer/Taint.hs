{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : VariableTracer.Taint
Description : "Must this value never reach there" checks over a provenance graph.

Rule-checking plugins in this repository ask questions of the form "argument N
of this call must not be X". Matching on the call expression answers that only
when X is written at the call site. This module answers the value-flow version:
/does anything this argument was built from match a forbidden source/, and by
what path did it get there.

@
findTaint program defaultTraceOpts
  [ TaintRule
      "no-card-in-logs"
      [SourceField "Card" "cardNumber"]  -- sources
      [TaintSink "logInfo*" Nothing]     -- sinks
      []                                 -- exempt enclosing functions
      ["maskCardNumber"]                 -- sanitizers
  ]
@

Each 'TaintFinding' carries the path from the sink argument down to the source,
so the diagnostic can show the whole flow instead of just the endpoint.

Accuracy follows the graph: it is an over-approximation (a parameter resolves
to every recorded call site), it does not resolve type-class dispatch, and it
only sees modules that were compiled with the tracer. A finding means "there is
a path in the analysed code", not "this executes".
-}
module VariableTracer.Taint
  ( -- * Rules
    TaintSource (..)
  , TaintSink (..)
  , TaintRule (..)
  , anySink
    -- * Findings
  , TaintFinding (..)
  , findTaint
  , renderFinding
    -- * Building blocks
  , matchesSource
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import VariableTracer.Graph
import VariableTracer.Trace
import VariableTracer.Types

-- | What must not flow.
data TaintSource
  = -- | A record field: type name and field name. @"*"@ as the type name
    -- matches the field on any type.
    SourceField Text Text
  | -- | Anything produced by this function (trailing @*@ allowed).
    SourceFunction Text
  | -- | A binder with this name.
    SourceVariable Text
  deriving (Show, Eq)

-- | Where it must not flow to.
data TaintSink = TaintSink
  { sinkFunction :: Text
  -- ^ callee name, trailing @*@ allowed
  , sinkArgs :: Maybe [Int]
  -- ^ argument positions to check; 'Nothing' checks every argument
  }
  deriving (Show, Eq)

-- | A sink that rejects the value in any argument position.
anySink :: Text -> TaintSink
anySink fn = TaintSink fn Nothing

data TaintRule = TaintRule
  { trName :: Text
  , trSources :: [TaintSource]
  , trSinks :: [TaintSink]
  , trExemptFunctions :: [Text]
  -- ^ enclosing functions where the flow is allowed. Matched against the
  -- outermost enclosing binder of the sink call.
  , trSanitizers :: [Text]
  -- ^ functions that make the value safe. A flow that passes through one of
  -- these on its way to the sink is not reported — that is what a masking or
  -- hashing helper is for. Matched against every step of the path.
  }
  deriving (Show, Eq)

data TaintFinding = TaintFinding
  { tfRule :: Text
  , tfSinkFunction :: Text
  , tfSinkArg :: Int
  , tfSinkLoc :: Loc
  , tfSinkCode :: Text
  , tfEnclosing :: Text
  , tfSource :: Text
  -- ^ the matched source, as it appears in the code
  , tfSourceLoc :: Loc
  , tfPath :: [TraceNode]
  -- ^ sink argument first, matched source last
  }
  deriving (Show, Eq)

-- | Every way a forbidden source reaches a sink argument.
findTaint :: ProgramGraph -> TraceOpts -> [TaintRule] -> [TaintFinding]
findTaint program opts rules =
  [ finding
  | rule <- rules
  , callSite <- allCallSites
  , sink <- trSinks rule
  , matchesPattern (sinkFunction sink) (csCalleeName callSite)
  , not (exempt rule callSite)
  , arg <- csArgs callSite
  , wantedArg sink (caIndex arg)
  , tree <- traceDeps program opts (caDeps arg)
  , path <- pathsTo (\n -> any (matchesSource n) (trSources rule)) tree
  , not (sanitized rule path)
  , finding <- [mkFinding rule callSite arg path]
  ]
  where
    allCallSites = concat (Map.elems (pgCallSites program))

    wantedArg sink idx = maybe True (idx `elem`) (sinkArgs sink)

    -- A masking helper anywhere between the sink and the source clears the flow.
    sanitized rule path =
      any (\n -> any (`matchesPattern` nodeName n) (trSanitizers rule)) path

    nodeName n = case T.splitOn "." (tnLabel n) of
      [] -> tnLabel n
      parts -> last parts

    exempt rule callSite =
      let enclosing = enclosingName callSite
       in any (`matchesPattern` enclosing) (trExemptFunctions rule)

    enclosingName callSite =
      case csEnclosing callSite >>= outermostOwner program of
        Just owner -> vrName (pnVar owner)
        Nothing -> "<top level>"

    mkFinding rule callSite arg path =
      let matched = last path
       in TaintFinding
            { tfRule = trName rule
            , tfSinkFunction = csCalleeName callSite
            , tfSinkArg = caIndex arg
            , tfSinkLoc = csLoc callSite
            , tfSinkCode = caCode arg
            , tfEnclosing = enclosingName callSite
            , tfSource = tnLabel matched
            , tfSourceLoc = tnLoc matched
            , tfPath = path
            }

-- | Does this step of a trace match a forbidden source?
matchesSource :: TraceNode -> TaintSource -> Bool
matchesSource node = \case
  SourceField typeName fieldName -> case tnComputation node of
    -- Reading a field of a record, however it was written: `r.field`,
    -- `field r`, or a pattern match that projects it out.
    Just (CompPatternProjection _ path) -> pathMentions typeName fieldName path
    Just (CompFieldAccess _ field) -> field == fieldName
    _ -> False
  SourceFunction fn -> matchesPattern fn (bareName node)
  SourceVariable v -> matchesPattern v (bareName node)
  where
    pathMentions typeName fieldName path =
      let segments = T.splitOn "." path
       in fieldName `elem` segments
            && (typeName == "*" || typeName `elem` segments)

    -- tnLabel is "enclosingFunction.binder" for a nested binder.
    bareName n = case T.splitOn "." (tnLabel n) of
      [] -> tnLabel n
      parts -> last parts

-- | A finding as a diagnostic: what reached where, and how.
renderFinding :: TaintFinding -> Text
renderFinding f =
  T.unlines $
    [ tfRule f
        <> ": "
        <> tfSource f
        <> " reaches argument #"
        <> T.pack (show (tfSinkArg f))
        <> " of "
        <> tfSinkFunction f
        <> " in "
        <> tfEnclosing f
    , "  at " <> renderLoc (tfSinkLoc f) <> ": " <> tfSinkCode f
    , "  flow:"
    ]
      <> zipWith step [0 :: Int ..] (tfPath f)
  where
    step i n =
      "    "
        <> T.replicate i "  "
        <> (if i == 0 then "" else "<- ")
        <> tnLabel n
        <> " ("
        <> tnDescription n
        <> ")"
        <> (if isEmptyLoc (tnLoc n) then "" else " @ " <> renderLoc (tnLoc n))
