# sql-schema

A GHC plugin + companion CLI that extract the SQL schema implied by Beam
table definitions in a Haskell package, into a YAML contract that
downstream tooling can diff against a live production database.

The YAML is the **single source of truth for what the Haskell side
believes the database looks like**. A release pipeline can compare it
against the production schema and refuse to deploy on mismatch.

## What it extracts

For every Beam table (`data XxxT (f :: Type -> Type) = Xxx { … } deriving
(…, Beamable)`) the plugin captures:

- The table type's name and its source module.
- `modelTableName`, `modelTableType` from the `ModelMeta` instance.
- `primaryKey` from the `Table` instance — the key constructor and the Haskell field names it applies (not a column list).
- For each `Columnar f T` field: Haskell field name, the column name
  applied via `fieldNamed` (defaults to Beam's `defaultDbSettings` name if
  no override), pretty-printed Haskell type, nullability (any `Maybe _`).
- Any `setEntityName` overrides found in a `defaultDbSettings
  ``withDbModification`` …` binding — cross-checked at merge time
  against the referenced table's `modelTableName`.

## How to wire it into a downstream package

In the consuming package's `.cabal` (CI-only branch — keep out of the
`if flag(Local)` dev branch so dev `cabal build` skips the plugin
overhead):

```
build-depends:
  …
  , sql-schema

ghc-options:
  -fplugin=SqlSchema.Plugin
  -fplugin-opt=SqlSchema.Plugin:{"fragmentsDir":"dist/build/sql-schema-fragments","blacklistModules":["Test.","Spec."]}
```

In the package's `flake.nix` haskell-flake settings, run the merger as
part of `postInstall` so every `nix build .#default` publishes the
contract:

```nix
settings.<pkg>.postInstall = ''
  mkdir -p $out/share/sql-schema
  sql-schema-merge \
    --fragments dist/build/sql-schema-fragments \
    --source-dirs src \
    --out $out/share/sql-schema/sql-schema.yaml
'';
```

For a package that consumes pre-built dep contracts (the typical
service case where a dependency's tables must compose into the service's
yaml):

```nix
settings.<service>.postInstall = ''
  mkdir -p $out/share/sql-schema
  sql-schema-merge \
    --fragments dist/build/sql-schema-fragments \
    --source-dirs src \
    --include-merged ${inputs.app-db.packages.${system}.app-db}/share/sql-schema/sql-schema.yaml \
    --out $out/share/sql-schema/sql-schema.yaml
'';
```

Pass `--generator-rev ${inputs.sql-schema-src.rev}` (only if the plugin source
is pinned by a rev) to record the plugin revision in the contract's
`generator` stamp; without it `rev` is `null`. The flag needs sql-schema
0.4.3 or later: an older `sql-schema-merge` rejects it as an unrecognised
argument and exits 2.

The merge CLI exits non-zero on duplicate-type **mismatch** (identical
duplicates are silently deduped), on `setEntityName`/`modelTableName`
disagreement, or on argument errors.  Two different Haskell types
mapping to the same SQL `tableName` produces a stderr warning, not an
error.

## Plugin options (JSON in `-fplugin-opt`)

| Key                         | Type         | Default                              | Notes |
|-----------------------------|--------------|--------------------------------------|-------|
| `fragmentsDir`              | string       | `./.juspay/sql-schema-fragments`     | Where per-module fragments are written. For incremental builds that restore the build directory, keep it inside that directory (e.g. `dist/build/sql-schema-fragments`), or the fragments of modules that were not recompiled are lost. Components that share one directory must not define modules with the same name: their fragments share a file. |
| `blacklistModules`          | [string]     | `[]`                                 | Module-name prefixes to skip entirely. |
| `allowUnknownColumnWrapper` | bool         | `false`                              | If `true`, fields whose type isn't `Columnar f T` / `C f T` are silently left out of the table (no warning). Use only for migrations — the default refusal is the safe stance. |
| `allowNonStandardModifier`  | bool         | `false`                              | If `true`, a modifier the plugin can't read is silently treated as "no overrides" (Beam default names): one that isn't `tableModification { … }` (guarded definitions included), or a binding that isn't a plain top-level `x = …` of this module with one unguarded right-hand side (imported, qualified, or bound in `where` or by a pattern). Same caveat as above. |
| `tableNameOverrides`        | {str: str}   | `{}`                                 | Per-type-name SQL-name override; applied only when `modelTableName` is not a static literal (e.g. `modelTableName = SomeConst.fooName`). Lets you assert the resolved value in plugin config instead of editing storage source. |

## Safety policy

The contract drives a production gate. A false-pass (YAML says fine,
prod isn't) is the worst outcome, so the plugin refuses to extract any
shape it can't read precisely:

| Hard compile error             | What it means |
|--------------------------------|---------------|
| `UNRESOLVABLE_TABLE_NAME`      | `modelTableName` isn't a literal. Inline the literal, or use `tableNameOverrides`. |
| `UNRESOLVABLE_COLUMN_NAME`     | The RHS inside `xxxTMod` isn't `fieldNamed "lit"`. Either rewrite or drop the override. |
| `UNRESOLVABLE_PRIMARY_KEY`     | `primaryKey` isn't in a recognised shape. Rewrite into one of the supported forms. |
| `UNRECOGNIZED_FIELD_WRAPPER`   | A field's type isn't `Columnar f T` / `C f T`. Override via `allowUnknownColumnWrapper` only if you accept that field being omitted (the table is still emitted). |
| `UNRECOGNIZED_MODIFIER_SHAPE`  | `modelFieldModification`, or the `xxxTMod` binding it names, isn't a single unguarded `tableModification { … }`. Override via `allowNonStandardModifier`. |
| `UNRESOLVABLE_MODIFIER_BINDING`| `modelFieldModification` names a binding the plugin can't read: it reads only a plain top-level `x = …` binding of this module, referenced unqualified, with one unguarded right-hand side. Define it that way, or use `allowNonStandardModifier`. |
| `INVALID_PLUGIN_OPTIONS`       | The `-fplugin-opt` JSON is malformed, has an unknown key or a wrong-typed value, or is given more than once. |

## Supported `primaryKey` shapes

```haskell
primaryKey x = Con (f1 x) (f2 x) …      -- function equation, any arity
primaryKey   = \x -> Con (f1 x) …        -- explicit lambda
primaryKey   = Con . field               -- single-column composition
primaryKey   = Con . (.field)            -- composition with record-dot section
primaryKey x = Con x.f1 x.f2             -- native OverloadedRecordDot,
                                         --   or the left-associative `.`
                                         --   chain GHC emits for `x.f`
                                         --   without that extension
primaryKey   = Con <$> f1 <*> f2 <*> …   -- applicative composite
primaryKey _ = ConNullary                -- no-column PK
```

## Architecture notes

- Per-module fragments mean incremental builds stay correct: an
  unchanged module's fragment on disk is still authoritative. Stale
  fragments (whose source file has been deleted) are pruned at merge
  time, and a recompiled module that no longer defines a table or
  override, or is blacklisted, has its fragment deleted.
- File writes (both fragment and final YAML) are atomic — tmp file +
  `renameFile`.
- The plugin runs at `parsedResultAction` time and inspects only what
  the parser produces, so it does not depend on type-class resolution.
  In particular, `Columnar` / `Beamable` / `Database` are matched by
  the syntactic tail of the qualifier; no GHC API typecheck is
  performed.
