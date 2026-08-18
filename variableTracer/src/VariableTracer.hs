{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : VariableTracer
Description : Trace a variable back to every computation that builds it.

This is the entry point for consumers. Two ways to use the library:

* __From a GHC plugin__ (your own, or 'VariableTracer.Plugin.plugin'): call
  'collectModuleGraph' in @typeCheckResultAction@ to get a pure 'ModuleGraph'
  for the module being compiled, then either dump it or query it in place with
  'traceInModule'.

* __Offline, across the whole program__: read the per-module graphs the plugin
  dumped, 'link' them into a 'ProgramGraph' and call 'traceTargets'. Only this
  path can follow a value out of a function into its callers in other modules.

@
graph   <- collectModuleGraph opts modSummary tcEnv        -- one module
program <- pure (link [graphA, graphB, ...])               -- whole program
let traces = traceTargets program defaultTraceOpts [defaultTargetSpec "finalAmount"]
putStrLn (T.unpack (renderTracesText traces))
@
-}
module VariableTracer
  ( -- * Collecting (needs the GHC API)
    module VariableTracer.Collect

    -- * Linking and querying
  , module VariableTracer.Graph

    -- * Tracing and rendering
  , module VariableTracer.Trace

    -- * Value-flow rules
  , module VariableTracer.Taint

    -- * Data model and configuration
  , module VariableTracer.Types

    -- * Convenience
  , link
  , traceInModule
  , traceNamed
  ) where

import Data.Text (Text)

import VariableTracer.Collect
import VariableTracer.Graph
import VariableTracer.Taint
import VariableTracer.Trace
import VariableTracer.Types

-- | Stitch per-module graphs into one whole-program graph.
link :: [ModuleGraph] -> ProgramGraph
link = fromModuleGraphs

-- | Trace targets using only a single module's graph.
--
-- Cheap and available during compilation, but a function parameter can only be
-- resolved through call sites that live in the same module; use 'link' over all
-- modules for the complete picture.
traceInModule :: TraceOpts -> ModuleGraph -> [TargetSpec] -> [VariableTrace]
traceInModule opts graph specs = traceTargets (link [graph]) opts specs

-- | Trace every binder with the given name in a linked program.
traceNamed :: ProgramGraph -> TraceOpts -> Text -> [VariableTrace]
traceNamed program opts name = traceTargets program opts [defaultTargetSpec name]
