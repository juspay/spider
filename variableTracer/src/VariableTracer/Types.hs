{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : VariableTracer.Types
Description : Data model of the variable provenance graph.

Everything in this module is plain Haskell (no GHC API), so it can be shared
between the compiler plugin, the offline linker/CLI and any consumer that just
wants to read the emitted JSON.

The model is a graph:

  * a 'ProvenanceNode' is /one binder/ (a variable) together with the
    computation that produced it and its direct dependencies;
  * a 'Dep' is an edge from a binder to something it was built out of
    (another variable, a literal, a constructor, a record field);
  * a 'CallSite' records an application so that a 'FunctionParam' binder can be
    resolved to the actual argument expressions at every call — this is what
    makes the trace interprocedural, and (after linking) cross-module.
-}
module VariableTracer.Types
  ( -- * Locations
    Loc(..)
  , emptyLoc
  , isEmptyLoc
  , renderLoc
    -- * Variables
  , NodeKey
  , VarScope(..)
  , VarRef(..)
    -- * Computations and dependencies
  , Computation(..)
  , DepKind(..)
  , DepTarget(..)
  , Dep(..)
  , depVar
  , describeComputation
  , describeDepKind
    -- * Nodes
  , BindKind(..)
  , ProvenanceNode(..)
  , FunctionNode(..)
    -- * Call sites
  , CallArg(..)
  , CallSite(..)
    -- * Graphs
  , ModuleGraph(..)
  , emptyModuleGraph
  , ProgramGraph(..)
  , emptyProgramGraph
    -- * Traces
  , TraceStatus(..)
  , TraceNode(..)
  , VariableTrace(..)
    -- * Configuration
  , TargetSpec(..)
  , defaultTargetSpec
  , TraceOpts(..)
  , defaultTraceOpts
  , TracerOpts(..)
  , defaultTracerOpts
  , defaultSkipBindings
    -- * Internal helpers (re-used by other modules)
  , matchesPattern
  , jsonOpts
  , graphFormatVersion
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | @show@ into 'Text'.
tshow :: Show a => a -> Text
tshow = T.pack . show

commaSep :: [Text] -> Text
commaSep = T.intercalate ", "

-- | Exact match, or prefix match when the pattern ends in @*@.
matchesPattern :: Text -> Text -> Bool
matchesPattern pat value
  | pat == "*" = True
  | Just prefix <- T.stripSuffix "*" pat = prefix `T.isPrefixOf` value
  | otherwise = pat == value

-- | Bumped whenever the on-disk JSON shape changes incompatibly.
graphFormatVersion :: Int
graphFormatVersion = 1

-- | Aeson options that drop a record-field prefix and lower-case the first
-- remaining character, so @pnBindKind@ becomes @bindKind@ in JSON.
jsonOpts :: String -> Options
jsonOpts prefix =
  defaultOptions
    { fieldLabelModifier = dropPrefix
    , omitNothingFields = True
    }
  where
    dropPrefix field = case stripPrefix prefix field of
      Just (c : cs) -> toLower c : cs
      _ -> field

--------------------------------------------------------------------------------
-- Locations
--------------------------------------------------------------------------------

-- | A source span, flattened so it survives a JSON round trip.
data Loc = Loc
  { locFile :: Text
  , locStartLine :: Int
  , locStartCol :: Int
  , locEndLine :: Int
  , locEndCol :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Loc where toJSON = genericToJSON (jsonOpts "loc")
instance FromJSON Loc where parseJSON = genericParseJSON (jsonOpts "loc")

emptyLoc :: Loc
emptyLoc = Loc "<no location>" 0 0 0 0

isEmptyLoc :: Loc -> Bool
isEmptyLoc l = locStartLine l == 0 && locEndLine l == 0

renderLoc :: Loc -> Text
renderLoc l
  | isEmptyLoc l = "<no location>"
  | otherwise =
      locFile l
        <> ":"
        <> tshow (locStartLine l)
        <> ":"
        <> tshow (locStartCol l)
        <> "-"
        <> tshow (locEndLine l)
        <> ":"
        <> tshow (locEndCol l)

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

-- | Graph-wide identity of a binder.
--
-- For anything with an external GHC 'Name' (top-level and exported binders,
-- imported functions) this is @nameStableString@, which is stable across
-- modules and compilations — that is what makes cross-module linking work.
--
-- For local binders (let/where/lambda/do/case) it is
-- @\<module\>:\<occurrence name\>:\<unique\>@, which is unique inside a single
-- module graph.
type NodeKey = Text

data VarScope
  = GlobalVar
  | LocalVar
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | A reference to a variable as it appears somewhere in the program.
data VarRef = VarRef
  { vrKey :: NodeKey
  , vrName :: Text
  , vrStableName :: Text
  , vrModule :: Text
  , vrPackage :: Text
  , vrType :: Text
  , vrScope :: VarScope
  , vrLoc :: Loc
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VarRef where toJSON = genericToJSON (jsonOpts "vr")
instance FromJSON VarRef where parseJSON = genericParseJSON (jsonOpts "vr")

--------------------------------------------------------------------------------
-- Computations
--------------------------------------------------------------------------------

-- | A structured summary of /how/ a binder was computed. This is the "what
-- computation was done" part of the trace; 'Dep' edges are the "out of what".
data Computation
  = -- | Function or operator application.
    CompApply
      { compFn :: Maybe VarRef
      , compFnText :: Text
      , compArgs :: [Text]
      , compOperator :: Bool
      }
  | -- | @y = x@ — a plain alias.
    CompAlias VarRef
  | CompLiteral Text
  | CompConstructor Text [Text]
  | CompRecordCon Text [(Text, Text)]
  | CompRecordUpdate Text [(Text, Text)]
  | CompFieldAccess {compRecord :: Text, compField :: Text}
  | CompCase {compScrutinee :: Text}
  | CompIf {compCondition :: Text}
  | CompGuards
  | CompLet
  | CompDoBlock
  | -- | @x <- action@
    CompMonadicBind {compAction :: Text}
  | -- | A binder projected out of a pattern, e.g. @Just x@ or @(a, b)@.
    CompPatternProjection {compSource :: Text, compPath :: Text}
  | CompLambda {compParams :: [Text]}
  | CompList Int
  | CompTuple Int
  | CompArithSeq
  | -- | A function parameter; resolved through call sites.
    CompParameter {compFunction :: Text, compIndex :: Int}
  | CompFunctionBody
  | CompOther Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

describeComputation :: Computation -> Text
describeComputation = \case
  CompApply _ fn args isOp
    | isOp -> "operator " <> fn <> " applied to " <> commaSep args
    | otherwise -> "call " <> fn <> (if null args then "" else " with " <> commaSep args)
  CompAlias v -> "alias of " <> vrName v
  CompLiteral l -> "literal " <> l
  CompConstructor c args -> "constructor " <> c <> (if null args then "" else " " <> commaSep args)
  CompRecordCon c flds -> "record " <> c <> " { " <> commaSep (map fst flds) <> " }"
  CompRecordUpdate b flds -> "record update of " <> b <> " { " <> commaSep (map fst flds) <> " }"
  CompFieldAccess r f -> "field " <> f <> " of " <> r
  CompCase s -> "case on " <> s
  CompIf c -> "if " <> c
  CompGuards -> "guarded right-hand sides"
  CompLet -> "let block"
  CompDoBlock -> "do block"
  CompMonadicBind a -> "monadic bind of " <> a
  CompPatternProjection src p -> "pattern projection " <> p <> " out of " <> src
  CompLambda ps -> "lambda \\" <> commaSep ps
  CompList n -> "list literal of " <> tshow n <> " elements"
  CompTuple n -> "tuple of " <> tshow n <> " components"
  CompArithSeq -> "arithmetic sequence"
  CompParameter fn i -> "parameter #" <> tshow i <> " of " <> fn
  CompFunctionBody -> "function body"
  CompOther t -> t

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

-- | Why a binder depends on something — the label on a graph edge.
data DepKind
  = DepAppliedFunction
  | DepArgument Int
  | DepScrutinee
  | DepCondition
  | DepBranch
  | DepGuard
  | DepRecordField Text
  | DepRecordBase
  | DepElement Int
  | DepMonadicAction
  | DepPatternSource
  | DepAlias
  | DepBody
  | -- | Added by the tracer: the value flowed in from an argument at a call site.
    DepCallSiteArg Int
  | DepUse
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

describeDepKind :: DepKind -> Text
describeDepKind = \case
  DepAppliedFunction -> "applied function"
  DepArgument i -> "argument #" <> tshow i
  DepScrutinee -> "scrutinee"
  DepCondition -> "condition"
  DepBranch -> "branch"
  DepGuard -> "guard"
  DepRecordField f -> "field " <> f
  DepRecordBase -> "record base"
  DepElement i -> "element #" <> tshow i
  DepMonadicAction -> "monadic action"
  DepPatternSource -> "pattern source"
  DepAlias -> "alias"
  DepBody -> "body"
  DepCallSiteArg i -> "call-site argument #" <> tshow i
  DepUse -> "use"

data DepTarget
  = TargetVar VarRef
  | TargetLiteral Text
  | TargetConstructor Text
  | TargetField Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Dep = Dep
  { depKind :: DepKind
  , depTarget :: DepTarget
  , depLoc :: Loc
  , depCode :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Dep where toJSON = genericToJSON (jsonOpts "dep")
instance FromJSON Dep where parseJSON = genericParseJSON (jsonOpts "dep")

depVar :: Dep -> Maybe VarRef
depVar d = case depTarget d of
  TargetVar v -> Just v
  _ -> Nothing

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

data BindKind
  = TopLevelBind
  | LetBind
  | WhereBind
  | DoBind
  | LambdaParam
  | FunctionParam
  | CaseBind
  | PatternBind
  | ComprehensionBind
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | One binder plus everything known about how it is built.
data ProvenanceNode = ProvenanceNode
  { pnKey :: NodeKey
  , pnVar :: VarRef
  , pnBindKind :: BindKind
  , pnOwner :: Maybe NodeKey
  -- ^ the enclosing binder (usually the enclosing function)
  , pnOwnerName :: Maybe Text
  , pnLoc :: Loc
  , pnCode :: Text
  -- ^ pretty-printed defining expression (possibly truncated)
  , pnComputation :: Computation
  , pnDeps :: [Dep]
  , pnParamIndex :: Maybe Int
  -- ^ set when this binder is (or is projected out of) a function parameter
  , pnIsFunction :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProvenanceNode where toJSON = genericToJSON (jsonOpts "pn")
instance FromJSON ProvenanceNode where parseJSON = genericParseJSON (jsonOpts "pn")

-- | A function definition, used to map call-site arguments onto parameters.
data FunctionNode = FunctionNode
  { fnKey :: NodeKey
  , fnName :: Text
  , fnModule :: Text
  , fnArity :: Int
  , fnParams :: [[NodeKey]]
  -- ^ binders per parameter position; a position can have several binders
  -- because a function may have several equations and patterns can bind more
  -- than one variable.
  , fnLoc :: Loc
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON FunctionNode where toJSON = genericToJSON (jsonOpts "fn")
instance FromJSON FunctionNode where parseJSON = genericParseJSON (jsonOpts "fn")

--------------------------------------------------------------------------------
-- Call sites
--------------------------------------------------------------------------------

data CallArg = CallArg
  { caIndex :: Int
  , caCode :: Text
  , caDeps :: [Dep]
  , caLoc :: Loc
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CallArg where toJSON = genericToJSON (jsonOpts "ca")
instance FromJSON CallArg where parseJSON = genericParseJSON (jsonOpts "ca")

data CallSite = CallSite
  { csCalleeKey :: NodeKey
  , csCalleeName :: Text
  , csArgs :: [CallArg]
  , csLoc :: Loc
  , csEnclosing :: Maybe NodeKey
  , csModule :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CallSite where toJSON = genericToJSON (jsonOpts "cs")
instance FromJSON CallSite where parseJSON = genericParseJSON (jsonOpts "cs")

--------------------------------------------------------------------------------
-- Graphs
--------------------------------------------------------------------------------

-- | What the plugin emits for a single module.
data ModuleGraph = ModuleGraph
  { mgVersion :: Int
  , mgModule :: Text
  , mgPackage :: Text
  , mgFile :: Text
  , mgNodes :: [ProvenanceNode]
  , mgFunctions :: [FunctionNode]
  , mgCallSites :: [CallSite]
  , mgAliases :: [(NodeKey, NodeKey)]
  -- ^ monomorphic-binder key -> exported polymorphic key (from @AbsBinds@)
  , mgNotes :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ModuleGraph where toJSON = genericToJSON (jsonOpts "mg")
instance FromJSON ModuleGraph where parseJSON = genericParseJSON (jsonOpts "mg")

emptyModuleGraph :: Text -> Text -> Text -> ModuleGraph
emptyModuleGraph modName pkg file =
  ModuleGraph
    { mgVersion = graphFormatVersion
    , mgModule = modName
    , mgPackage = pkg
    , mgFile = file
    , mgNodes = []
    , mgFunctions = []
    , mgCallSites = []
    , mgAliases = []
    , mgNotes = []
    }

-- | Many 'ModuleGraph's stitched together — the whole-program view.
data ProgramGraph = ProgramGraph
  { pgNodes :: Map NodeKey ProvenanceNode
  , pgFunctions :: Map NodeKey FunctionNode
  , pgCallSites :: Map NodeKey [CallSite]
  -- ^ keyed by callee
  , pgAliases :: Map NodeKey NodeKey
  , pgNameIndex :: Map Text [NodeKey]
  -- ^ occurrence name -> every binder with that name
  , pgModules :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProgramGraph where toJSON = genericToJSON (jsonOpts "pg")
instance FromJSON ProgramGraph where parseJSON = genericParseJSON (jsonOpts "pg")

emptyProgramGraph :: ProgramGraph
emptyProgramGraph = ProgramGraph Map.empty Map.empty Map.empty Map.empty Map.empty []

--------------------------------------------------------------------------------
-- Traces
--------------------------------------------------------------------------------

-- | Why a branch of the trace stopped (or did not).
data TraceStatus
  = -- | children were followed
    Expanded
  | RootLiteral
  | RootConstructor
  | -- | defined outside the graph we have (an imported/unanalysed function)
    RootExternal
  | -- | a parameter with no known call site — an entry point
    RootParameter
  | -- | referenced but no node found
    RootUnresolved
  | -- | already on the current path
    CycleDetected
  | -- | expanded elsewhere in this trace; not repeated
    AlreadyExpanded
  | DepthLimitReached
  | NodeBudgetExhausted
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TraceNode = TraceNode
  { tnKey :: NodeKey
  , tnLabel :: Text
  , tnEdge :: Maybe DepKind
  -- ^ how the parent depends on this node
  , tnBindKind :: Maybe BindKind
  , tnComputation :: Maybe Computation
  , tnDescription :: Text
  , tnCode :: Text
  , tnType :: Text
  , tnModule :: Text
  , tnLoc :: Loc
  , tnDepth :: Int
  , tnStatus :: TraceStatus
  , tnChildren :: [TraceNode]
  }
  deriving (Show, Eq, Generic)

instance ToJSON TraceNode where toJSON = genericToJSON (jsonOpts "tn")
instance FromJSON TraceNode where parseJSON = genericParseJSON (jsonOpts "tn")

data VariableTrace = VariableTrace
  { vtTarget :: Text
  , vtTargetKey :: NodeKey
  , vtModule :: Text
  , vtRoot :: TraceNode
  , vtRoots :: [TraceNode]
  -- ^ the leaves — where the trace bottomed out
  , vtComputations :: [Text]
  -- ^ every computation that contributes, deepest first
  , vtNodeCount :: Int
  , vtTruncated :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON VariableTrace where toJSON = genericToJSON (jsonOpts "vt")
instance FromJSON VariableTrace where parseJSON = genericParseJSON (jsonOpts "vt")

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Which variable(s) to trace. Every field supports a trailing @*@ wildcard;
-- 'Nothing' matches anything.
data TargetSpec = TargetSpec
  { tsVariable :: Text
  , tsFunction :: Maybe Text
  , tsModule :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TargetSpec where toJSON = genericToJSON (jsonOpts "ts")

instance FromJSON TargetSpec where
  parseJSON = withObject "TargetSpec" $ \o ->
    TargetSpec
      <$> o .: "variable"
      <*> o .:? "function"
      <*> o .:? "module"

defaultTargetSpec :: Text -> TargetSpec
defaultTargetSpec v = TargetSpec v Nothing Nothing

data TraceOpts = TraceOpts
  { toMaxDepth :: Int
  , toMaxNodes :: Int
  , toFollowCallSites :: Bool
  -- ^ resolve function parameters through call sites (interprocedural)
  , toFollowIntoFunctions :: Bool
  -- ^ when a dependency is a function, keep tracing into its body
  , toIncludeLiterals :: Bool
  , toMaxCallSites :: Int
  -- ^ per parameter, to keep hot helpers from exploding the trace
  , toReexpandShared :: Bool
  -- ^ show a shared binder in full under every path that reaches it, instead
  -- of expanding it once and marking later occurrences 'AlreadyExpanded'.
  -- Still bounded by 'toMaxNodes'.
  }
  deriving (Show, Eq, Generic)

instance ToJSON TraceOpts where toJSON = genericToJSON (jsonOpts "to")

instance FromJSON TraceOpts where
  parseJSON = withObject "TraceOpts" $ \o ->
    TraceOpts
      <$> o .:? "maxDepth" .!= toMaxDepth defaultTraceOpts
      <*> o .:? "maxNodes" .!= toMaxNodes defaultTraceOpts
      <*> o .:? "followCallSites" .!= toFollowCallSites defaultTraceOpts
      <*> o .:? "followIntoFunctions" .!= toFollowIntoFunctions defaultTraceOpts
      <*> o .:? "includeLiterals" .!= toIncludeLiterals defaultTraceOpts
      <*> o .:? "maxCallSites" .!= toMaxCallSites defaultTraceOpts
      <*> o .:? "reexpandShared" .!= toReexpandShared defaultTraceOpts

defaultTraceOpts :: TraceOpts
defaultTraceOpts =
  TraceOpts
    { toMaxDepth = 25
    , toMaxNodes = 5000
    , toFollowCallSites = True
    , toFollowIntoFunctions = True
    , toIncludeLiterals = True
    , toMaxCallSites = 20
    , toReexpandShared = False
    }

-- | Plugin configuration, passed as a JSON blob in @-fplugin-opt@.
data TracerOpts = TracerOpts
  { path :: FilePath
  , targets :: [TargetSpec]
  , traceOpts :: TraceOpts
  , dumpGraph :: Bool
  , dumpTraces :: Bool
  , includeLiterals :: Bool
  , ignoreDictionaries :: Bool
  , includeSyntaxOps :: Bool
  -- ^ record the operators behind @do@ notation (@>>=@, @>>@, @return@,
  -- @fail@, @negate@). Off by default because for ordinary code that is one
  -- extra @GHC.Base.>>=@ leaf per statement; turn it on for custom monads or
  -- @RebindableSyntax@, where which bind runs is part of the answer.
  , skipBindings :: [Text]
  -- ^ binder names whose definitions are not collected at all, to keep derived
  -- and instance boilerplate out of the graph. Trailing @*@ allowed. Calls to
  -- them are still recorded — only their bodies are skipped. See
  -- 'defaultSkipBindings'; set to @[]@ to collect everything.
  , codeLimit :: Int
  -- ^ maximum length of a pretty-printed code snippet
  , log :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON TracerOpts where
  parseJSON = withObject "TracerOpts" $ \o ->
    TracerOpts
      <$> o .:? "path" .!= path defaultTracerOpts
      <*> o .:? "targets" .!= targets defaultTracerOpts
      <*> o .:? "traceOpts" .!= traceOpts defaultTracerOpts
      <*> o .:? "dumpGraph" .!= dumpGraph defaultTracerOpts
      <*> o .:? "dumpTraces" .!= dumpTraces defaultTracerOpts
      <*> o .:? "includeLiterals" .!= includeLiterals defaultTracerOpts
      <*> o .:? "ignoreDictionaries" .!= ignoreDictionaries defaultTracerOpts
      <*> o .:? "includeSyntaxOps" .!= includeSyntaxOps defaultTracerOpts
      <*> o .:? "skipBindings" .!= skipBindings defaultTracerOpts
      <*> o .:? "codeLimit" .!= codeLimit defaultTracerOpts
      <*> o .:? "log" .!= VariableTracer.Types.log defaultTracerOpts

defaultTracerOpts :: TracerOpts
defaultTracerOpts =
  TracerOpts
    { path = "./.juspay/variableTracer/"
    , targets = []
    , traceOpts = defaultTraceOpts
    , dumpGraph = True
    , dumpTraces = True
    , includeLiterals = True
    , ignoreDictionaries = True
    , includeSyntaxOps = False
    , skipBindings = defaultSkipBindings
    , codeLimit = 400
    , log = False
    }

-- | Class methods that a @deriving@ clause generates bodies for.
--
-- Skipping them removes the bulk of instance boilerplate from a module graph.
-- The cost is that a /hand-written/ instance of the same method is skipped too,
-- so drop an entry (or set the list to @[]@) when you want to trace inside one.
--
-- Deliberately excluded: names that are class methods but also plausible names
-- for ordinary functions — @from@, @to@, @min@, @max@, @enumFrom…@. Add them
-- yourself if your codebase never uses those as regular bindings.
defaultSkipBindings :: [Text]
defaultSkipBindings =
  [ "showsPrec"
  , "show"
  , "showList"
  , "readsPrec"
  , "readPrec"
  , "readListPrec"
  , "readList"
  , "compare"
  , "=="
  , "/="
  , "<"
  , "<="
  , ">"
  , ">="
  , "minBound"
  , "maxBound"
  , "toEnum"
  , "fromEnum"
  , "succ"
  , "pred"
  , "rnf"
  , "toJSON"
  , "toEncoding"
  , "toJSONList"
  , "parseJSON"
  , "parseJSONList"
  , "toConstr"
  , "gfoldl"
  , "gunfold"
  , "dataTypeOf"
  ]
