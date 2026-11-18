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
- Each `setEntityName` override, cross-checked at merge time against the
  referenced table's `modelTableName`. The plugin reads
  `field = setEntityName "sql_name"` in a `dbModification { … }` that is the
  right operand of `withDbModification` (infix or prefix, any qualifier) in
  the result of a top-level binding, such as
  `db = defaultDbSettings ``withDbModification`` dbModification { … }`. The
  value may stand alone or in a `<>` chain (`P.<>` too) whose other operands
  are `modifyTableFields x` or `setEntitySchema x`. `field` must have one
  type, `f (TableEntity T)`, across all of the module's records, one record
  of the module must declare every label of the update, and no label may be
  qualified with another module (this module's own qualifier is fine).
  Later operands of `withDbModification` must be `dbModification { … }`
  updates. Neither the `dbModification` nor the settings expression may
  carry a type annotation or a type application, no record wildcard `{..}`
  may be in the binding's declaration, a top-level pattern binding or a
  top-level splice, and the module must not bind `setEntityName`,
  `dbModification`, `withDbModification`, `(<>)`, `modifyTableFields` or
  `setEntitySchema` itself. Any other `setEntityName` in the module is a
  compile error (`UNCAPTURED_SET_ENTITY_NAME`), Template Haskell quotes,
  name quotes (`'setEntityName`) and punned record fields
  (`R { setEntityName }`) included: a rename the plugin doesn't read would
  never be cross-checked.

### DB-settings renames

Queries built from the DB settings take column names from them, while the
contract reads them from `modelFieldModification`, so a column rename
applied in the DB settings must be the contract's. The plugin accepts
`modifyTableFields xxxTMod` applied directly, where `xxxTMod` is unqualified
and is the binding named in `modelFieldModification` of a table of the same
module: a top-level `xxxTMod = tableModification { … }` with at least one
field, bound nowhere else in the module, with no record wildcard `{..}` in
the use's declaration, a top-level pattern binding or a top-level splice, in
a module that binds none of `modifyTableFields`, `tableModification`,
`fieldNamed` and `(<>)` itself. Passing that binding is enough,
because
`modifyTableFields :: tbl (FieldModification (TableField tbl)) ->
EntityModification (DatabaseEntity be db) be (TableEntity tbl)` and the
binding's record fields fix `tbl` to its table: it applies exactly the
contract's renames, to that table only. Any other `modifyTableFields` is a
compile error (`UNCAPTURED_MODIFY_TABLE_FIELDS`). So is any use of
`modifyTable`, `modifyEntityName`, `withTableModification` or
`renamingFields`, or of the entity internals from which a modification can
be written by hand, `EntityModification`, `DatabaseEntity`, `dbEntityName`
and `dbTableCurrentName`, as a variable, a constructor or a constructor
pattern (`UNSUPPORTED_ENTITY_RENAME`): each can rename in a way the plugin
can't read. Types may name them, as the usual EMod signature
`EntityModification (DatabaseEntity be db) be (TableEntity T)` does. If no
column is renamed, drop `modifyTableFields`.

### What the rename checks cannot see

The plugin reads the parsed source, before names and types are resolved:

- A `dbModification` updates the record GHC selects from its labels and
  annotations, and the plugin assumes this module's records. It refuses an
  update with an annotation, with a label qualified with another module, or
  with labels that no single record of the module declares (a record inside
  a Template Haskell declaration quote doesn't count). An unannotated update
  whose labels all name fields of one local record is read as that record's
  update, even if its type were to come from elsewhere. Under
  `DuplicateRecordFields`, GHC 9.2.8 and 9.8.4 reject such an update as
  ambiguous in every shape tried: a binding signature, an annotated or
  type-applied settings operand, and a pattern signature.
- A record label naming an entity internal, such as
  `d { dbTableCurrentName = … }`, `DatabaseTable { … }` or the same labels
  in a pattern, is not refused. The changed value renames a table only once
  it is wrapped back into the refused `DatabaseEntity`.
- An instance can change what a class method does at one type without
  binding its name: an `OVERLAPPING` `Semigroup` instance at
  `EntityModification … (TableEntity T)` changes how a `<>` chain combines.
  An instance method defines its class's method, so it is not refused as a
  binding of `(<>)`.
- `coerce` can build an `EntityModification` without naming it, for example
  by substituting another `Database` record's entity for a table's. That
  rename is not seen.
- With `RebindableSyntax` and `OverloadedStrings`, every string literal goes
  through the `fromString` in scope. A local or imported `fromString` can
  change each literal the plugin reads: table names, column names and
  renames.
- Only uses of the names count. A record field of that name
  (`x.setEntityName`, `(.setEntityName)`), an overloaded label
  (`#setEntityName`), a rename built in Template Haskell with `mkName`, in a
  quasi-quote or in a splice's output, and a renaming function defined in a
  package without the plugin are not seen.

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
disagreement, or on argument errors.  It also fails rather than drop
tables: on a fragment that doesn't parse (unless `--source-dirs` shows its
module is gone), a missing or malformed `--include-merged` file, a
directory it can't read, or `--source-dirs` that prune every fragment.
It refuses a contract with no tables unless `--allow-empty` is passed, and
one with none of the package's own fragments unless the package declares
`--no-own-tables` (it defines no tables itself and only composes
`--include-merged` contracts); with that flag, own fragments that do define
a table fail the merge, so the declaration can't go stale.  A `--source-dirs`
entry that doesn't exist only warns, so a mistyped one prunes the
fragments of its modules; each pruned module is named on stderr.  Two
different Haskell types mapping to the same SQL `tableName` produces a
stderr warning, not an error.

## Plugin options (JSON in `-fplugin-opt`)

| Key                         | Type         | Default                              | Notes |
|-----------------------------|--------------|--------------------------------------|-------|
| `fragmentsDir`              | string       | `./.juspay/sql-schema-fragments`     | Where per-module fragments are written; it must hold only this plugin's files. For incremental builds that restore the build directory, keep it inside that directory (e.g. `dist/build/sql-schema-fragments`), or the fragments of modules that were not recompiled are lost. Components that share one directory must not define same-named modules with tables: their fragments share a file, and the last compile wins. Same-named modules without tables (several test suites' `Main`) must not be compiled at the same time: they race on that file, and the losing compile fails with `STAGED_FRAGMENT_MISSING`, never with a wrong fragment. Give such components their own `fragmentsDir`, or no plugin. An empty value or the working directory (`.`, `./`) is an invalid option. |
| `blacklistModules`          | [string]     | `[]`                                 | Modules to skip entirely. An entry ending in `.` (e.g. `Test.`) skips every module it prefixes; any other entry skips only the module of exactly that name (`App.Db` skips only `App.Db`; `App.Db.` skips `App.Db.Pool`, but not `App.Db`). An empty entry, or one starting with `.`, is an invalid option. |
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
| `UNCAPTURED_SET_ENTITY_NAME`   | A `setEntityName` the plugin doesn't capture as an override: one outside the shape under "What it extracts" (in an EMod or other separate binding, a `where` or `let` binding, an instance method, a splice, or a `dbModification` that `withDbModification` does not apply to a top-level binding's result), with a non-literal name, in a `<>` chain with two of them or with another kind of operand, under a type annotation, a label qualified with another module or a record wildcard `{..}`, in an update whose labels no single record of the module declares, before a later `withDbModification` operand that isn't a `dbModification { … }` update, on a field with no table type or several across the module's records, or in a module that binds one of the names the shape relies on. The message names the line and the reason. Rewrite it into the shape that is read. No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `UNCAPTURED_MODIFY_TABLE_FIELDS` | A `modifyTableFields` outside the shape under "DB-settings renames": not applied directly to an argument, an argument that isn't an unqualified variable or isn't the `modelFieldModification` binding of a table of this module, a binding name also bound elsewhere in the module (a parameter or a `where` binding, say) or rebound by a record wildcard `{..}`, a module that binds `modifyTableFields`, `tableModification`, `fieldNamed` or `(<>)` itself, a type application, or a binding that isn't a `tableModification { … }` with at least one field. The message names the line and the reason. Pass the binding named in `modelFieldModification`; if no column is renamed, drop `modifyTableFields`. No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `UNSUPPORTED_ENTITY_RENAME`    | A use of `modifyTable`, `modifyEntityName`, `withTableModification` or `renamingFields`, or of `EntityModification`, `DatabaseEntity`, `dbEntityName` or `dbTableCurrentName` as a variable, a constructor or a constructor pattern (a signature or a record label may name them): each can rename a table or its columns in a way the plugin can't read. Rename the table with `setEntityName "sql_name"` and its columns with `modifyTableFields` as under "DB-settings renames". No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `INVALID_PLUGIN_OPTIONS`       | The `-fplugin-opt` JSON is malformed, has an unknown key, a wrong-typed value, an invalid `blacklistModules` entry or `fragmentsDir`, or is given more than once. |
| `STAGED_FRAGMENT_MISSING`      | The module's staged fragment was gone when the plugin went to publish it (see Architecture notes): something else removed it from `fragmentsDir`, such as a compile of a same-named module at the same time (see `fragmentsDir` above). Rebuild the module. |

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
- A compile stages its module's fragment when it parses the module, as
  `<Module>.yaml.pending`: the fragment's YAML, or an empty file meaning
  "delete the fragment". A pass the plugin appends to the end of the Core
  pipeline publishes it: it renames the staged file to `<Module>.yaml`, or
  deletes the staged file and then `<Module>.yaml`. GHC runs the Core
  pipeline only after type checking, desugaring and the `-Werror` check
  pass, so a compile that fails leaves the published fragment as it was.
  This matters when the failed edit is then reverted: GHC 9.8 recompiles
  by source hash, so it doesn't recompile the reverted module. GHCi's
  prompt declarations, which GHC compiles without the parse step, are
  skipped.
- A compile that generates no code only stages: `-fno-code`, haddock, and
  HLS when it only type checks. Two cases still publish, after the
  `-Werror` check: under `-fno-code`, the modules GHC compiles to code for
  Template Haskell, and on GHC 9.8 every module under `-fno-code
  -fwrite-if-simplified-core`, which runs the Core pipeline. The merge
  reads only `*.yaml`, so run it after a build that generates code.
- `fragmentsDir` must hold only this plugin's files: it creates, renames
  and deletes `<Module>.yaml`, `<Module>.yaml.pending` and temp files named
  `<Module>.yaml.pending*.tmp` there, and fails a compile with
  `STAGED_FRAGMENT_MISSING` when a staged file is gone at publish time.
- Staged files and the merged YAML are written to a temp file and renamed
  into place.
- The extractor runs at `parsedResultAction` time and inspects only what
  the parser produces, so it does not depend on type-class resolution.
  In particular, `Columnar` / `Beamable` / `Database` are matched by
  the syntactic tail of the qualifier; no GHC API typecheck is
  performed.

## Known limits

- A failure after the publish pass (a later Core pass, tidy, code
  generation, a killed build) followed by a revert can leave the failed
  edit's fragment, or its deletion, published in a local incremental build
  on GHC 9.8, which does not recompile the reverted module (GHC 9.2 does, by
  timestamp). A published deletion silently drops the module's tables from
  the contract. Nix builds are unaffected: they start from scratch or from a
  successful build's intermediates.
- An editor session (HLS, ghcide) with the plugin on may publish fragments
  of unsaved buffers, so keep the plugin flag off there.
