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

- Fully-qualified Haskell type name and source file.
- `modelTableName`, `modelTableType` from the `ModelMeta` instance.
- `primaryKey` from the `Table` instance — constructor + column list.
- For each `Columnar f T` field: Haskell field name, the column name
  applied via `fieldNamed` (defaults to the Haskell field name if no
  override), pretty-printed Haskell type, nullability (any `Maybe _`).
- Any `setEntityName` overrides found in a `defaultDbSettings
  ``withDbModification`` …` binding — cross-checked at merge time
  against the referenced table's `modelTableName`.

## How to wire it into a downstream package

In the consuming package's `.cabal`:

```
build-depends:
  …
  , sql-schema

ghc-options:
  -fplugin=SqlSchema.Plugin
  -fplugin-opt=SqlSchema.Plugin:{"fragmentsDir":"./.juspay/sql-schema-fragments","blacklistModules":["Test.","Spec."]}
```

After building, combine fragments into the final contract:

```
cabal build
cabal run sql-schema:exe:sql-schema-merge -- \
  --fragments .juspay/sql-schema-fragments \
  --out      .juspay/sql-schema.yaml \
  --projectRoot .
```

The merge CLI exits non-zero on duplicate Haskell types, on
`setEntityName`/`modelTableName` disagreement, or on argument errors.

## Plugin options (JSON in `-fplugin-opt`)

| Key                         | Type         | Default                              | Notes |
|-----------------------------|--------------|--------------------------------------|-------|
| `fragmentsDir`              | string       | `./.juspay/sql-schema-fragments`     | Where per-module fragments are written. |
| `blacklistModules`          | [string]     | `[]`                                 | Module-name prefixes to skip entirely. |
| `allowUnknownColumnWrapper` | bool         | `false`                              | If `true`, fields whose type isn't `Columnar f T` / `C f T` are skipped silently. Use only for migrations — the default refusal is the safe stance. |
| `allowNonStandardModifier`  | bool         | `false`                              | If `true`, `xxxTMod` bindings that aren't `tableModification { … }` are treated as "no overrides". Same caveat as above. |
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
| `UNRECOGNIZED_FIELD_WRAPPER`   | A field's type isn't `Columnar f T` / `C f T`. Override via `allowUnknownColumnWrapper` only if you accept the table being omitted. |
| `UNRECOGNIZED_MODIFIER_SHAPE`  | `xxxTMod` isn't `tableModification { … }`. Override via `allowNonStandardModifier`. |

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
  time.
- File writes (both fragment and final YAML) are atomic — tmp file +
  `renameFile`.
- The plugin runs at `parsedResultAction` time and inspects only what
  the parser produces, so it does not depend on type-class resolution.
  In particular, `Columnar` / `Beamable` / `Database` are matched by
  the syntactic tail of the qualifier; no GHC API typecheck is
  performed.
