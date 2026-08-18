{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : VariableTracer.Plugin
Description : Ready-made GHC plugin that dumps variable provenance graphs.

Enable it with

@
ghc-options: -fplugin=VariableTracer.Plugin
             -fplugin-opt=VariableTracer.Plugin:{"path":"./.juspay/variableTracer/","targets":[{"variable":"finalAmount"}]}
@

For every compiled module it writes @\<path\>\/\<module path\>.variable-graph.json@.
When @targets@ are configured it additionally writes the traces it can already
resolve from this module alone, as JSON and as a readable tree.

Whole-program tracing is a second step: run @variable-tracer link@ over the
directory of graphs and then @variable-tracer trace@, which resolves parameters
through call sites in /other/ modules too.

If you are writing your own plugin, skip this module and call
'VariableTracer.Collect.collectModuleGraph' directly — it is pure.
-}
module VariableTracer.Plugin
  ( plugin
  , variableTracerAction
  , parsePluginOpts
  ) where

import qualified Control.Exception as E
import Control.Monad (unless, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude hiding (log)

import VariableTracer.Collect (collectModuleGraph)
import VariableTracer.Graph (fromModuleGraphs)
import VariableTracer.Trace (renderTracesText, traceTargets)
import VariableTracer.Types

#if __GLASGOW_HASKELL__ >= 900
-- GHC.Plugins re-exports ModSummary, msHsFilePath and liftIO on GHC 9.
import GHC.Plugins hiding ((<>), getHscEnv, purePlugin)
import GHC.Tc.Types (TcGblEnv, TcM)
#else
-- GhcPlugins re-exports ModSummary, msHsFilePath and liftIO on GHC 8.
import GhcPlugins hiding ((<>), getHscEnv, purePlugin)
import TcRnTypes (TcGblEnv, TcM)
#endif

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = variableTracerAction
    , pluginRecompile = \_ -> pure NoForceRecompile
    }

-- | The type-checker pass: collect the graph, write it out, optionally trace.
variableTracerAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
variableTracerAction cliOpts modSummary tcEnv = do
  let opts = parsePluginOpts cliOpts
      graph = collectModuleGraph opts modSummary tcEnv
      modulePath = path opts <> msHsFilePath modSummary
  result :: Either E.SomeException () <- liftIO . E.try $ do
    createDirectoryIfMissing True (takeDirectory modulePath)
    when (dumpGraph opts) $
      BL.writeFile (modulePath <> ".variable-graph.json") (A.encode graph)
    when (dumpTraces opts && not (null (targets opts))) $ do
      let traces = traceTargets (fromModuleGraphs [graph]) (traceOpts opts) (targets opts)
      unless (null traces) $ do
        BL.writeFile (modulePath <> ".variable-trace.json") (A.encode traces)
        TIO.writeFile (modulePath <> ".variable-trace.txt") (renderTracesText traces)
    when (log opts) $
      putStrLn $
        "[variableTracer] "
          <> intercalate
            ", "
            [ T.unpack (mgModule graph)
            , show (length (mgNodes graph)) <> " binders"
            , show (length (mgCallSites graph)) <> " call sites"
            ]
  case result of
    Left err ->
      when (log opts) . liftIO . putStrLn $ "[variableTracer] failed: " <> show err
    Right () -> pure ()
  pure tcEnv

-- | Plugin options are a single JSON object; anything missing falls back to
-- 'defaultTracerOpts'.
parsePluginOpts :: [CommandLineOption] -> TracerOpts
parsePluginOpts [] = defaultTracerOpts
parsePluginOpts (raw : _) =
  fromMaybe defaultTracerOpts (A.decode (BL.fromStrict (encodeUtf8 (T.pack raw))))
