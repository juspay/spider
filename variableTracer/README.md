# variableTracer

Find **every computation that goes into building a variable**, down to the roots.

Given a binder anywhere in a program, the tracer answers: which expression
produced it, what did that expression read, where did *those* values come from —
recursing through `let`/`where`/`do`/`case` binders, out of functions through
their call sites, and across module boundaries, until it bottoms out at
literals, data constructors, imported functions or the parameters of an entry
point that nothing in the program calls.

It works at the **AST level** on GHC's type-checked tree (`GhcTc`), so every
occurrence carries a real `Id` with a unique and a type — no name-based
guessing, no confusion between two different `x`es.

This is a **library first**. The plugin and the CLI are thin wrappers; other
plugins in this repo (or anywhere else) can depend on `variableTracer` and call
the collector directly.

---

## The model

| Type | Meaning |
| --- | --- |
| `ProvenanceNode` | one binder + the `Computation` that produced it + its direct `Dep`s |
| `Dep` | an edge to a variable, literal, constructor or field, labelled with *why* (`DepArgument 1`, `DepScrutinee`, `DepRecordField "amount"`, `DepMonadicAction`, …) |
| `CallSite` | one recorded application: callee key + the argument expressions and their deps |
| `ModuleGraph` | what the plugin emits per module |
| `ProgramGraph` | many module graphs linked together |
| `VariableTrace` | the answer: a tree of computations from a target down to its roots |

Binders are keyed so that graphs from separately compiled modules join up:

* external names (top-level, exported, imported) use GHC's stable name,
  `package$Module$name`;
* local binders use `Module:name:unique`.

`AbsBinds` monomorphic/polymorphic pairs are resolved, so a recursive self-call
and an external call land on the same key.

## What the collector understands

Applications and operator sections (with the applied function recorded
separately from each argument), data constructor applications, record
construction and record update (per field), record field access including
`OverloadedRecordDot`, `case` (scrutinee + per-alternative pattern projections),
`if`/multi-way if/guards, `let` and `where` groups, `do` blocks (`x <- action`
becomes a monadic-bind node), lambdas, list/tuple literals, pattern bindings
with the projection path (`Just _`, `#0`, `Order.amount`), and function
parameters per equation and position.

Anything not modelled structurally (Template Haskell splices, arrows, brackets)
falls back to a generic traversal that still collects the referenced variables,
so a trace is never silently empty.

Dictionaries, evidence and other compiler-generated binders (`$d…`, `$f…`,
`$c…`, type variables) are dropped by default — set `"ignoreDictionaries":false`
to keep them.

## Use it from your own plugin

```haskell
import VariableTracer

myPass :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
myPass _ modSummary tcEnv = do
  let graph  = collectModuleGraph defaultTracerOpts modSummary tcEnv  -- pure
      traces = traceInModule defaultTraceOpts graph [defaultTargetSpec "finalAmount"]
  liftIO $ mapM_ (putStrLn . T.unpack . renderTraceText) traces
  pure tcEnv
```

`collectModuleGraph` does no IO and never throws on unexpected AST shapes, so it
is safe to call inside another analysis.

## Use the bundled plugin

```
ghc-options:
  -fplugin=VariableTracer.Plugin
  -fplugin-opt=VariableTracer.Plugin:{"path":"./.juspay/variableTracer/","targets":[{"variable":"finalAmount","function":"computeFinalAmount"}]}
```

Options (all optional):

| Key | Default | Meaning |
| --- | --- | --- |
| `path` | `./.juspay/variableTracer/` | output prefix |
| `targets` | `[]` | `{"variable":…,"function":…,"module":…}`, trailing `*` allowed |
| `traceOpts` | see below | `maxDepth`, `maxNodes`, `followCallSites`, `followIntoFunctions`, `includeLiterals`, `maxCallSites` |
| `dumpGraph` | `true` | write the module graph |
| `dumpTraces` | `true` | write traces for `targets` |
| `ignoreDictionaries` | `true` | drop compiler-generated binders (`$d…`, `$f…`, type vars) |
| `skipBindings` | derived class methods | binder names whose *bodies* are not collected — keeps `deriving`-generated `showsPrec`/`==`/`toJSON` out of the graph. Trailing `*` allowed; `[]` collects everything. Calls to them are still recorded |
| `includeSyntaxOps` | `false` | record the operators behind `do` (`>>=`, `>>`, `return`, `fail`, `negate`). Worth turning on for `RebindableSyntax`; for standard code it is one extra `GHC.Base.>>=` leaf per statement |
| `codeLimit` | `400` | truncate pretty-printed snippets |
| `log` | `false` | print a per-module summary |

`traceOpts` additionally takes `reexpandShared` (default `false`): expand a
shared binder in full under every path that reaches it instead of showing it
once and marking later occurrences `AlreadyExpanded`. The CLI exposes it as
`--reexpand-shared`.

Per module it writes `<path>/<module path>.variable-graph.json`, plus
`.variable-trace.json` / `.variable-trace.txt` when targets are configured.

Traces written during compilation only see *one* module. For the whole-program
answer, link first.

## Whole-program tracing

```bash
# after a build with the plugin enabled
variable-tracer link  -d ./.juspay/variableTracer -o program.json

variable-tracer trace -g program.json -v finalAmount
variable-tracer trace -g program.json -v netAmount -f formatAmount        # resolves via call sites
variable-tracer trace -g program.json -v receipt --format dot > trace.dot
variable-tracer list  -g program.json -m Payments.Settlement
```

`trace` also accepts `-d DIR` directly and links on the fly.

Real output, tracing `base` (a parameter of `applyFee` in module `Lib`) after
linking `Lib` and `Main` — note that it leaves the module and keeps going:

```
variable : base
module   : Lib
key      : Lib:base:6989586621679013735
nodes    : 2

applyFee.base :: Int  -- parameter #1 of applyFee  @ Lib.hs:8:14-8:18
`- [call-site argument #1] call site: applyFee argument #1  -- value passed as argument #1 at App.hs:8:17-8:35
   `- [use] chargeAmount.gross :: Int  -- parameter #1 of chargeAmount  @ App.hs:6:19-6:24
      `- [call-site argument #1] call site: chargeAmount argument #1  -- value passed as argument #1 at App.hs:12:15-12:41
         `- [use] literal 250000 :: ?  -- literal 250000  <RootLiteral>  @ App.hs:12:35-12:41

roots:
  - literal 250000 <RootLiteral> @ App.hs:12:35-12:41
```

And a computed binder, `total = base + pctPart + feeFlat fee`, where the field
access resolves through the record selector into whichever `Fee` was built:

```
applyFee.total :: Int  -- call (+) with base + pctPart, feeFlat fee  @ Lib.hs:10:7-10:12
|- [applied function] + :: forall a. Num a => a -> a -> a  -- defined outside the analysed graph  <RootExternal>
|- [argument #0] applyFee.base :: Int  -- parameter #1 of applyFee  @ Lib.hs:8:14-8:18
|  `- ... literal 250000 <RootLiteral>
|- [argument #1] applyFee.pctPart :: Int  -- call div with (base * feePct fee), 100  @ Lib.hs:9:7-9:14
|  |- [applied function] feePct :: Fee -> Int  -- function body  @ Lib.hs:3:18-3:24
|  |  `- [use] feePct :: Int  -- pattern projection Fee.feePct out of argument #0 of feePct
|  |     `- [call-site argument #0] call site: feePct argument #0  -- value passed at Lib.hs:9:25-9:35
|  |        `- [use] applyFee.fee :: Fee  -- parameter #0 of applyFee
|  |           `- [call-site argument #0] call site: applyFee argument #0  -- at App.hs:8:17-8:35
|  |              `- [use] chargeAmount.fee :: Fee  -- call feeFor with tier
|  |                 `- [applied function] feeFor :: String -> Fee  -- function body
|  |                    |- [argument #0] literal 2 <RootLiteral>  @ Lib.hs:14:21-14:22
|  |                    `- [argument #1] literal 500 <RootLiteral>  @ Lib.hs:14:23-14:26
```

## Limits worth knowing

* **Type classes are not devirtualized.** Instance bodies are collected and can
  be traced, but a call to a class method is recorded against the *class
  selector*, so `basePrice item` stops at `basePrice <RootExternal>` rather
  than descending into the `Book` or `Toy` instance. The value flowing *into*
  the polymorphic function is tracked normally. Instance methods also carry
  internal names, so they are module-local and never link across modules.
  Resolving this means following the dictionary evidence (`WpEvApp`,
  `tcg_ev_binds`) to `$fClassType` and its methods — not implemented.
* **Higher-order flow is one-directional.** In `applyTwice f x = f (f x)` the
  applications are recorded against the parameter `f`, so you can see
  `f ← bump` from the call site, but the argument never reaches `bump`'s own
  parameter. Same for callbacks and functions stored in fields.

* **Context-insensitive.** A parameter resolves to *every* recorded call site,
  so a helper called from twenty places shows twenty inbound branches
  (`maxCallSites` bounds it). The trace is an over-approximation, not a slice of
  one execution.
* **Arity is by source position.** Partial application and point-free
  composition are recorded as they appear in the source; a value threaded
  through `.` or `$` is followed as an argument of that operator, not
  beta-reduced.
* **Nesting follows binders, not sub-expressions.** In `a - b + c` the tree
  shows `+`, `-`, `a`, `b` and `c` as children of the same binder rather than a
  nested expression tree — every contributor is present, but the operator
  grouping inside one right-hand side is flattened. Introduce a `let` if you
  want that step to be its own node.
* **Only what was compiled with the plugin.** Anything else is a
  `RootExternal` leaf, named by its stable name.
* **GHC ≥ 9.0.** The collector matches GHC 9 AST shapes (developed against
  9.2.8). On GHC 8.10 the library still builds, but `collectModuleGraph`
  returns an empty graph carrying a note instead of a partial, untested
  traversal.
* **`ApplicativeDo`, `mdo` and parallel comprehensions** bind through statement
  forms that are not modelled structurally. Their binders *do* get nodes, but
  each one is attributed to the whole statement group rather than to its own
  action — an over-approximation, visible as an action snippet like
  `(left <- fetchAmount | right <- fetchAmount)`.
* Recursion, depth and total node count are bounded; a truncated trace says so
  (`vtTruncated`, and `<DepthLimitReached>` / `<NodeBudgetExhausted>` markers).
* **Types are text.** `vrType` is a pretty-printed signature, not structured
  type information, so there is no type-level reasoning in a trace.

## Layout

| Module | Role |
| --- | --- |
| `VariableTracer.Types` | data model + JSON, no GHC API |
| `VariableTracer.Collect` | the AST traversal (the actual tracer logic) |
| `VariableTracer.Graph` | linking, indexing, target matching |
| `VariableTracer.Trace` | the backwards walk + text/DOT rendering |
| `VariableTracer.Plugin` | ready-made plugin |
| `VariableTracer` | umbrella re-export + convenience helpers |
| `app/Main.hs` | `variable-tracer` CLI |
