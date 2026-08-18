{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @variable-tracer@ — offline, whole-program half of the tracer.
--
-- The plugin drops one @*.variable-graph.json@ per module while the project
-- compiles. This tool links those into a single program graph and answers
-- "where did this variable come from" across module boundaries.
--
-- @
-- variable-tracer link  -d ./.juspay/variableTracer -o program.json
-- variable-tracer trace -g program.json -v finalAmount -f settleOrder --format tree
-- variable-tracer trace -d ./.juspay/variableTracer -v finalAmount --format dot > trace.dot
-- variable-tracer list  -g program.json -m Payments.Settlement
-- @
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import VariableTracer

--------------------------------------------------------------------------------
-- Command line
--------------------------------------------------------------------------------

data Command
  = Link LinkOpts
  | Trace TraceCmdOpts
  | List ListOpts

data LinkOpts = LinkOpts
  { linkDir :: FilePath
  , linkOut :: FilePath
  }

-- | Either a pre-linked program graph or a directory of module graphs.
data GraphSource
  = FromProgram FilePath
  | FromDir FilePath

data OutputFormat = FormatTree | FormatJson | FormatDot
  deriving (Eq)

data TraceCmdOpts = TraceCmdOpts
  { traceSource :: GraphSource
  , traceVar :: T.Text
  , traceFn :: Maybe T.Text
  , traceMod :: Maybe T.Text
  , traceFormat :: OutputFormat
  , traceDepth :: Int
  , traceMaxNodes :: Int
  , traceCallSites :: Bool
  , traceIntoFunctions :: Bool
  , traceMaxCallSites :: Int
  , traceReexpandShared :: Bool
  }

data ListOpts = ListOpts
  { listSource :: GraphSource
  , listModule :: Maybe T.Text
  , listPattern :: Maybe T.Text
  }

graphSourceParser :: Parser GraphSource
graphSourceParser =
  (FromProgram <$> strOption (long "graph" <> short 'g' <> metavar "PROGRAM.JSON" <> help "linked program graph produced by `link`"))
    <|> (FromDir <$> strOption (long "dir" <> short 'd' <> metavar "DIR" <> help "directory of per-module *.variable-graph.json files"))

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "link" (info (Link <$> linkParser) (progDesc "link per-module graphs into one program graph"))
        <> command "trace" (info (Trace <$> traceParser) (progDesc "trace a variable back to its roots"))
        <> command "list" (info (List <$> listParser) (progDesc "list the binders in a graph"))
    )
  where
    linkParser =
      LinkOpts
        <$> strOption (long "dir" <> short 'd' <> metavar "DIR" <> help "directory to scan recursively")
        <*> strOption (long "out" <> short 'o' <> metavar "FILE" <> value "program.json" <> showDefault <> help "output file")

    traceParser =
      TraceCmdOpts
        <$> graphSourceParser
        <*> (T.pack <$> strOption (long "var" <> short 'v' <> metavar "NAME" <> help "variable to trace (name, or a node key; trailing * allowed)"))
        <*> optional (T.pack <$> strOption (long "function" <> short 'f' <> metavar "NAME" <> help "restrict to binders inside this function"))
        <*> optional (T.pack <$> strOption (long "module" <> short 'm' <> metavar "NAME" <> help "restrict to this module"))
        <*> formatParser
        <*> option auto (long "depth" <> metavar "N" <> value (toMaxDepth defaultTraceOpts) <> showDefault <> help "maximum trace depth")
        <*> option auto (long "max-nodes" <> metavar "N" <> value (toMaxNodes defaultTraceOpts) <> showDefault <> help "maximum nodes to expand")
        <*> flag True False (long "no-call-sites" <> help "do not resolve parameters through call sites")
        <*> flag True False (long "no-functions" <> help "do not trace into the bodies of called functions")
        <*> option auto (long "max-call-sites" <> metavar "N" <> value (toMaxCallSites defaultTraceOpts) <> showDefault <> help "call sites to follow per parameter")
        <*> switch (long "reexpand-shared" <> help "expand a shared binder under every path instead of marking it AlreadyExpanded")

    formatParser =
      option
        (eitherReader parseFormat)
        (long "format" <> metavar "tree|json|dot" <> value FormatTree <> help "output format (default: tree)")

    parseFormat = \case
      "tree" -> Right FormatTree
      "json" -> Right FormatJson
      "dot" -> Right FormatDot
      other -> Left ("unknown format: " <> other)

    listParser =
      ListOpts
        <$> graphSourceParser
        <*> optional (T.pack <$> strOption (long "module" <> short 'm' <> metavar "NAME"))
        <*> optional (T.pack <$> strOption (long "pattern" <> short 'p' <> metavar "PATTERN" <> help "variable name pattern, trailing * allowed"))

main :: IO ()
main = do
  cmd <-
    execParser $
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Trace a variable back to every computation that builds it" <> header "variable-tracer")
  case cmd of
    Link opts -> runLink opts
    Trace opts -> runTrace opts
    List opts -> runList opts

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

runLink :: LinkOpts -> IO ()
runLink opts = do
  graphs <- loadModuleGraphs (linkDir opts)
  failIfEmpty (linkDir opts) graphs
  let program = link graphs
  BL.writeFile (linkOut opts) (A.encode program)
  putStrLn $
    "linked "
      <> show (length graphs)
      <> " modules, "
      <> show (Map.size (pgNodes program))
      <> " binders, "
      <> show (sum (map length (Map.elems (pgCallSites program))))
      <> " call sites -> "
      <> linkOut opts

runTrace :: TraceCmdOpts -> IO ()
runTrace opts = do
  program <- loadProgram (traceSource opts)
  let spec = TargetSpec {tsVariable = traceVar opts, tsFunction = traceFn opts, tsModule = traceMod opts}
      traceCfg =
        defaultTraceOpts
          { toMaxDepth = traceDepth opts
          , toMaxNodes = traceMaxNodes opts
          , toFollowCallSites = traceCallSites opts
          , toFollowIntoFunctions = traceIntoFunctions opts
          , toMaxCallSites = traceMaxCallSites opts
          , toReexpandShared = traceReexpandShared opts
          }
      traces = traceTargets program traceCfg [spec]
  if null traces
    then do
      hPutStrLn stderr $ "no binder matched " <> T.unpack (traceVar opts)
      hPutStrLn stderr "try `variable-tracer list` to see what is in the graph"
      exitFailure
    else case traceFormat opts of
      FormatTree -> TIO.putStrLn (renderTracesText traces)
      FormatJson -> BL.putStr (A.encode traces)
      FormatDot -> forM_ traces (TIO.putStrLn . renderTraceDot)

runList :: ListOpts -> IO ()
runList opts = do
  program <- loadProgram (listSource opts)
  let nodes =
        [ n
        | n <- Map.elems (pgNodes program)
        , maybe True (\m -> matchesPattern m (vrModule (pnVar n))) (listModule opts)
        , maybe True (\p -> matchesPattern p (vrName (pnVar n))) (listPattern opts)
        ]
  forM_ (sortOn (\n -> (vrModule (pnVar n), locStartLine (pnLoc n))) nodes) $ \n ->
    TIO.putStrLn $
      T.justifyLeft 40 ' ' (vrModule (pnVar n) <> "." <> vrName (pnVar n))
        <> " "
        <> T.justifyLeft 16 ' ' (T.pack (show (pnBindKind n)))
        <> " "
        <> describeComputation (pnComputation n)
        <> "  @ "
        <> renderLoc (pnLoc n)

--------------------------------------------------------------------------------
-- Loading
--------------------------------------------------------------------------------

loadProgram :: GraphSource -> IO ProgramGraph
loadProgram = \case
  FromProgram file -> do
    exists <- doesFileExist file
    unless exists $ die' ("no such file: " <> file)
    contents <- BL.readFile file
    case A.eitherDecode contents of
      Right program -> pure program
      Left err -> die' ("could not read program graph " <> file <> ": " <> err)
  FromDir dir -> do
    graphs <- loadModuleGraphs dir
    failIfEmpty dir graphs
    pure (link graphs)

loadModuleGraphs :: FilePath -> IO [ModuleGraph]
loadModuleGraphs dir = do
  files <- findGraphFiles dir
  fmap catMaybes . forM files $ \file -> do
    contents <- BL.readFile file
    case A.eitherDecode contents of
      Right graph -> pure (Just graph)
      Left err -> do
        hPutStrLn stderr ("skipping " <> file <> ": " <> err)
        pure Nothing

findGraphFiles :: FilePath -> IO [FilePath]
findGraphFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure [root | ".variable-graph.json" `isSuffixOf` root]
    else do
      entries <- listDirectory root
      fmap concat . forM entries $ \entry -> findGraphFiles (root </> entry)

failIfEmpty :: FilePath -> [ModuleGraph] -> IO ()
failIfEmpty dir graphs =
  when (null graphs) $
    die' ("no *.variable-graph.json files found under " <> dir)

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitFailure
