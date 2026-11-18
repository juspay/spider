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
- `modelTableName` from the `ModelMeta` instance.
- `modelTableType` and `modelSchemaName` from the `ModelMeta` instance, in
  these shapes only (a field the instance leaves out is `Nothing`):
  - `modelTableType`: `Nothing` or `Just` of a `TableType` constructor
    (`DOMAIN`, `CONFIG`, `COMMON_CONFIG`, `TRACKER`, `COMMON_TRACKER`),
    recorded unqualified (`Just Se.TRACKER` gives `TRACKER`);
  - `modelSchemaName`: `Nothing`, `Just "literal"` (recorded as
    `SchemaLiteral`) or `Just <value name>`, qualified or not (recorded as
    `SchemaConfig`, qualifier kept).

  Parentheses and type signatures around the `Just …` or its argument are
  read. The binding must be written once, as a plain equation with no
  arguments, guards or `where` bindings. Any other shape, or a derived
  `ModelMeta` instance, is a compile error (`UNRESOLVABLE_MODEL_META_FIELD`):
  recorded as `Nothing`, it would route the table to the wrong database.

  The names are matched as written, not resolved, so a pattern synonym of
  this module that reuses `Just`, `Nothing` or the name of a `TableType`
  constructor is refused (`REDEFINED_NAME`); an imported one is refused after
  type checking (`MODEL_META_FIELD_MISMATCH`, see "Type-check cross-check").
- `primaryKey` from the `Table` instance — the key constructor and the Haskell field names it applies (not a column list).
  Each must be a column field the plugin extracts for the table
  (`UNKNOWN_PRIMARY_KEY_FIELD`).
- For each `Columnar f T` field: Haskell field name, the column name
  applied via `fieldNamed` (defaults to Beam's `defaultDbSettings` name if
  no override), pretty-printed Haskell type, nullability (any `Maybe _`).
  The modifier is read only as `tableModification { field = fieldNamed "…", … }`
  in plain record update syntax: a punned field (`{ fooName }`) is
  `UNRESOLVABLE_COLUMN_NAME`, and `OverloadedRecordUpdate` syntax
  `UNRECOGNIZED_MODIFIER_SHAPE`.
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
  `setEntitySchema` itself, nor declare a type named `TableEntity`. Any other
  `setEntityName` in the module is a
  compile error (`UNCAPTURED_SET_ENTITY_NAME`), Template Haskell quotes,
  name quotes (`'setEntityName`) and punned record fields
  (`R { setEntityName }`) included: a rename the plugin doesn't read would
  never be cross-checked.


Each definition the plugin reads (`modelTableName`, `modelTableType`,
`modelSchemaName`, `modelFieldModification`, the top-level binding it names,
and `primaryKey`) must be written once, as one equation with no guards and no
`where` bindings: a `where` binding could shadow a name the plugin reads
there. Only `primaryKey` may take an argument. The module must not define,
itself, a name the plugin reads a table through (`REDEFINED_NAME`):
`tableModification` or `fieldNamed` in any form, a name `primaryKey`
applies (`.`, `<$>`, `<*>`, `getField`, a field name) in any form but a
record field, a pattern synonym named `Just`, `Nothing` or a `TableType`
constructor, or a type, type synonym or family named `C`, `Columnar`, `Maybe`
or `TableEntity`. A table's `Generic` instance must be derived in its module
with the stock strategy, by its deriving clause (no strategy written, or
`stock`) or by `deriving instance Generic (XxxT f)` with the table itself in
the head, or, for a large record, come from the large-records plugin, which
`{-# ANN type XxxT largeRecord #-}` (any qualifier) asks for. A table with
neither, or with another `Generic` instance in the module that could be its
(written, derived `via`, `newtype` or `anyclass`, also for a type synonym of
the module or a head the plugin can't read), is refused (`UNEXTRACTED_TABLE`):
Beam names the columns from the Generic representation, which only a stock
derivation, or that plugin, takes from the record's fields. A `Generic`
instance the source doesn't show (from Template Haskell, an orphan in another
module, or behind a module class named `Generic`) is checked after type
checking (`GENERIC_FIELD_MISMATCH`, see "Type-check cross-check"). A table's
`Beamable` instance, and its key's, may be derived (not `via` another type) or
written with no methods (`instance Beamable XxxT`); one written with a method,
or derived `via` another type, is refused (`UNEXTRACTED_TABLE`), since
`zipBeamFieldsM` decides the field each column rename of the modifier goes to.

A module that defines a table or a `setEntityName` override must not enable
`RebindableSyntax` (`UNSUPPORTED_EXTENSION`: record-dot syntax and string
literals then call the module's own `getField` and `fromString`), enable
`OverlappingInstances` or `IncoherentInstances` (`UNSUPPORTED_EXTENSION`: an
instance then overlaps a library's without a pragma, also through a type
synonym), have a
`RULES` pragma (`UNSUPPORTED_RULES`: GHC applies a rule when it optimises),
or bind a record wildcard pattern at top level (`UNSUPPORTED_RECORD_WILDCARD`:
`R{..} = …` binds names the parser does not show). Other modules are
unaffected.

**Limit:** the plugin reads the parsed module, so it cannot see a
redefinition it would have to resolve: an imported one (a project module
that exports its own `fieldNamed` or `tableModification`, or a Prelude
replacement with its own `.`), a qualified operator (`P.<$>`), or a name a
Template Haskell splice binds at top level. Each is read as Beam's or base's
name. Nor does it see a `RULES` pragma in another module, which GHC applies in
any module that imports it, directly or not. Model code must not import such
redefinitions or rules. The type-check cross-check below resolves the names the
contract's routing, nullability and table list rest on: `Just`, `Nothing`, the
`TableType` constructor, `Columnar`/`C`, `Maybe` and the three classes. Type
synonyms are read as written too, so a column typed through one that hides
`Maybe` (`type Opt = Maybe`, then `C f (Opt Text)`) is read as not nullable;
the cross-check expands the synonyms and refuses it (`COLUMN_TYPE_MISMATCH`):
a column passes only when its type inside beam-core's `Columnar` is headed by
base's `Maybe` exactly when the parse step recorded it nullable.

### Type-check cross-check

After GHC type checks a module (`typeCheckResultAction`), the plugin compares
what it staged at parse time with what GHC resolved, which the parse step
cannot see. It runs in every build that type checks the module, `-fno-code`,
haddock and HLS included, but not for a blacklisted module, an `hs-boot` or
`hsig` file, or GHCi's prompt declarations. Each mismatch is a compile error,
located at the instance, the table's declaration or the column's field when
GHC gives it a place in the module, else at the module; there is no opt-out.

- Every `Table`, `ModelMeta` or `Beamable` instance GHC reports for the
  module, Template Haskell's and other plugins' included, must be for a table
  the parse step staged, declared in the module (a key's instance, for
  beam-core's `PrimaryKey` family by package and module, is for none), or the
  compile fails with `UNEXTRACTED_TABLE`: a splice's instance for an imported
  type named like one of the module's tables is refused, not taken for the
  table's. One whose head has no type constructor is
  `UNREADABLE_INSTANCE_HEAD`.
- The class of each such instance must be beam-core's `Table` or `Beamable`
  (package `beam-core`, module `Database.Beam.Schema.Tables`) or sequelize's
  `ModelMeta` (package `sequelize`, module `Sequelize`): `UNEXPECTED_CLASS`.
  The package is checked too: a module of the package being compiled named
  `Sequelize` shadows the package's without a word.
- A staged table's `modelTableType` and `modelSchemaName`, as written, must
  resolve to base's `Just` and `Nothing`, a data constructor (not a pattern
  synonym) of sequelize's `TableType` with the recorded name, and the recorded
  string literal or value name: `MODEL_META_FIELD_MISMATCH`.
- Each staged column's field, with type synonyms expanded, must be
  beam-core's `Columnar` applied to the table's last type parameter and a type
  that is base's `Maybe t` exactly when the column was recorded as nullable,
  or else `COLUMN_TYPE_MISMATCH`. So `type Opt = Maybe` (then
  `C f (Opt Text)`), an imported `C` that adds a `Maybe`, another module's
  `Columnar` or `Maybe`, and a type family in the column type are refused.
  For a large-records table (below), whose fields that plugin types as
  existential type variables, each fixed by an equality constraint of the
  constructor (`forall f1. (f1 ~ τ) => …`), the type checked is that `τ`; a
  field no one such constraint fixes is refused.
- A staged table's `{-# ANN type T largeRecord #-}` must name the large-records
  package's `largeRecord`, whose type is that package's options type
  (`UNEXTRACTED_TABLE`). The large-records plugin consumes the annotation
  before GHC renames the module, so this sees one only where that plugin is
  not loaded.
- Each staged table's GHC.Generics `Generic` instance, looked up by class among
  every instance GHC sees in the module (imported and orphan ones included),
  must be the only one that matches or could match. Its representation, with
  type families reduced, must be one constructor whose selectors are all the
  record's fields in order: Beam names the columns after those selectors, by
  position. A large-records table is one whose representation is instead
  large-generics' `ThroughLRGenerics` of the table (package `large-generics`,
  module `Data.Record.Generic.GHC`), which the large-records plugin generates
  with large-generics' `Generic` instance. Beam is taken to reach its columns
  through beam-large-records' instances for that representation (unverified
  offline: Hackage's beam-large-records 0.1.3 needs beam-core 0.10.3 or later,
  and the corpus builds with 0.9.0.0, so its copy is another build), so
  large-generics' `MetadataOf` of the table, reduced, must name all the
  record's fields in order; a package named large-generics without that
  `MetadataOf` is refused too. Either way the staged columns must be those
  fields in order: all of them, or with `allowUnknownColumnWrapper` those it
  does not leave out. Otherwise `GENERIC_FIELD_MISMATCH`.
- Once the module uses beam-core's `setEntityName` or `modifyTableFields`, the
  only `Semigroup` or `Monoid` instance that could apply at the
  `EntityModification` of a table it can rename (one staged here, or the table
  of a record field here, a large record's field read at the type its equality
  constraint fixes), for any database and backend, must be beam-core's
  own (`UNEXPECTED_INSTANCE`). Instances of every module GHC sees count,
  incoherent ones included. A record field typed by an existential type
  variable of its constructor that equality constraints mention but fix to no
  one type (two constraints, or one to another such variable) could hold a
  table unseen, so it is refused at the field (`UNCHECKABLE_RENAMES`); a
  variable no equality constraint mentions, as in
  `data AnyShow = forall a. Show a => AnyShow a`, holds no table and is
  skipped. A beam-core whose `Database.Beam.Schema.Tables` defines no
  `EntityModification` or `TableEntity` beside `setEntityName` is refused
  too (`UNCHECKABLE_RENAMES`), located at the module rather than a line: the
  name the check starts from is beam-core's `setEntityName` or
  `modifyTableFields`, which has no span in the module.

It reads the renamed source, which GHC keeps only on request, so the plugin
asks for it (`keepRenamedSource`); on GHC 9.8 a build without `-haddock` then
writes the module's documentation structure into its `.hi` file, as `-haddock`
does. A table counts as a large-records table by its representation, not by
its annotation: one annotated but compiled without that plugin has another
representation, checked by its selectors. The large-records checks are written
against large-records 0.4.4 and large-generics 0.2.3. The pinned fork
(`infinitumkiran/large-records` rev 35005b63) is not available offline; if it
differs from 0.4.4 in how it encodes fields or `MetadataOf`, CI fails loudly
rather than passing. The check does not cover a name the parse step reads
elsewhere (the modifier, the key, `setEntityName`).

### What the type-check cross-check cannot see

It checks the instances GHC resolves in the table's module, by type. Each of
these defeats it, and each is deliberate code by someone with commit access,
which is outside the threat model:

- A written or Template Haskell `Generic` instance (GHC.Generics' or
  large-generics') whose selectors or `MetadataOf` names are the record's
  fields in order, but whose `to` and `from`, or large-generics' value-level
  metadata, permute or rename the fields.
- An orphan `OVERLAPPING` `Generic` instance, or an overlapping instance of
  beam-core's generic plumbing (`GDefaultTableFieldSettings`,
  `GAutoDbSettings`, `IsDatabaseEntity`, `GZipDatabase`, …), in a module
  imported only where the database is built: GHC selects it there, not in the
  table's module.
- A record field typed by an existential type variable whose equality
  constraint the rename check does not read as one: written behind a
  synonym of a constraint tuple
  (`type Is g f = (g ~ f (TableEntity T), g ~ Maybe Int)`), as a class's
  superclass (`class (g ~ Maybe Int) => C g`), at the head of a type family,
  or through `Coercible`. The check reads each `~` or `~~` of the
  constructor's context, a synonym of a single one expanded, so such a
  variable counts as one no equality constraint mentions: its field is skipped, as `AnyShow`'s is, not
  refused (`UNCHECKABLE_RENAMES`), and the `EntityModification` instances of
  a table it holds go unchecked.

### DB-settings renames

Queries built from the DB settings take column names from them, while the
contract reads them from `modelFieldModification`, so a column rename
applied in the DB settings must be the contract's. The plugin accepts
`modifyTableFields xxxTMod` applied directly, where `xxxTMod` is unqualified
and is the binding named in `modelFieldModification` of a table of the same
module: a top-level `xxxTMod = tableModification { … }` with at least one
field, written as one equation with no guards or `where` bindings and bound
nowhere else in the module, with no record wildcard `{..}` in
the use's declaration, a top-level pattern binding or a top-level splice, in
a module that binds none of `modifyTableFields`, `tableModification`,
`fieldNamed` and `(<>)` itself. Passing that binding is enough,
because
`modifyTableFields :: tbl (FieldModification (TableField tbl)) ->
EntityModification (DatabaseEntity be db) be (TableEntity tbl)` and the
binding's record fields fix `tbl` to its table: with the table's `Beamable`
methods taken from its Generic representation (a written method is refused,
above), it applies exactly the contract's renames, to that table only. Any other `modifyTableFields` is a
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
  binding its name, as an `OVERLAPPING` `Semigroup` instance at
  `EntityModification … (TableEntity T)` changes how a `<>` chain combines.
  So the methods of an instance whose type names `EntityModification` or
  `DatabaseEntity`, or that has an overlap pragma, count as names the module
  binds, and such a `(<>)` refuses the module's renames; other instances'
  methods don't. A derived instance under the same rule, standalone or in a
  deriving clause, binds its class's methods the same way when the class is
  `Semigroup` (`<>`, `sconcat`, `stimes`) or `Monoid` (`mempty`, `mappend`,
  `mconcat`). An instance that overlaps by the `OverlappingInstances` or
  `IncoherentInstances` extension instead of a pragma can't arise: those
  extensions are refused (above). Such an instance in another module, or one
  that names those types only through a type synonym, is refused after type
  checking (`UNEXPECTED_INSTANCE`).
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
- A `tableNameOverrides` entry is not checked against the code: it exists
  because the plugin can't read the value `modelTableName` names. If that
  value changes and the override keeps the old name, the contract records the
  old table; while that table still exists, the validator checks it instead of
  the one the code uses.
- Beam's database settings name each table by its default entity name, taken
  from the database record's field, unless `setEntityName` renames it; the
  contract records `modelTableName`. A query through those settings whose
  default name differs from `modelTableName`, with no `setEntityName`, uses a
  table the contract does not name, and nothing checks that.

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

### Plugin order

GHC runs a module's plugins in an order that depends on its version: 9.8.4
in command-line order, 9.2.8 in reverse (9.2's `addPluginModuleName` puts each
`-fplugin` at the head of the list and its `loadPlugins` uses the list as is;
9.8's `loadPlugins` reverses it). A plugin that rewrites the parse tree
(large-records, record-dot-preprocessor) changes what this plugin reads if it
runs first: a large-records table is refused as `UNEXTRACTED_TABLE` (observed
on 9.8.4 with large-records 0.4.4: that plugin adds a written `Generic`
instance, and `allowUnknownColumnWrapper` does not change that); a rewritten
modifier is a compile error too. The order on 9.2 is from GHC's source and a
two-plugin probe on 9.2.8; large-records on 9.2 was not run. So list `-fplugin=SqlSchema.Plugin` before
such plugins on 9.8 and after them on 9.2, and for a package built with both,
set it per compiler, with every `-fplugin` flag in the branches (`-fplugin-opt`
may stay outside):

```
if impl(ghc >= 9.8)
  ghc-options: -fplugin=SqlSchema.Plugin -fplugin=Other.Plugin
else
  ghc-options: -fplugin=Other.Plugin -fplugin=SqlSchema.Plugin
```

Check the order in the compiler's command line (`cabal build -v2`).

### `fragmentsDir`

The directory is dedicated to the plugin, which creates, renames and deletes
files in it (see Architecture notes). It must be unique per set of components
compiled at the same time that share module names (a library and test suites
that each have a `Main`, say): give each its own `fragmentsDir`, or no plugin.
A concurrent compile of a same-named module that loses the race fails with
`STAGED_FRAGMENT_MISSING`, never with a wrong fragment. These are invalid
options (`INVALID_PLUGIN_OPTIONS`), because the plugin could delete an
unrelated file in the working directory: `""`, `.`, `./` and `./.`; any value
with a `..` component (`a/..`, and `dist/../frags` too, though that one is
safe); and a value that resolves to the working directory or a directory above
it, as an absolute path or through a symbolic link. This is checked when the
module is parsed, before staging, and again in the publish pass, before
publishing; a chain of links past the system's limit can't be resolved, passes
the check, and fails at the first write with `FRAGMENT_IO_ERROR`.

### Nix

In the package's `flake.nix` haskell-flake settings, run the merger as part of
`postInstall` so every `nix build .#default` publishes the contract. haskell-flake
has no `postInstall` setting: `settings.<pkg>.drvAttrs.postInstall` would replace
the package's `postInstall` (`drvAttrs` is applied as `oldAttrs // attrs`) and
would not put `sql-schema-merge` on `PATH`. This package's setting module adds
`sqlSchemaPostInstall`, which appends the snippet and adds `sql-schema` to the
build tools:

```nix
settings.<pkg> = {
  imports = [ (inputs.sql-schema-src + "/sqlSchema/nix/postInstall.nix") ];
  sqlSchemaPostInstall = ''
    mkdir -p $out/share/sql-schema
    sql-schema-merge \
      --fragments dist/build/sql-schema-fragments \
      --source-dirs src --source-dirs app \
      --out $out/share/sql-schema/sql-schema.yaml
  '';
};
```

Here the package's components compiled with the plugin have
`hs-source-dirs: src` and `hs-source-dirs: app`: list every entry of every
such component (see "`sql-schema-merge` command line"), and run the merge from
the package root, as `postInstall` does.

When the plugin is enabled by a cabal flag, cabal2nix resolves the conditional
`sql-schema` dependency with the flag's default, so a build with the flag on
lacks it: add `extraBuildDepends = [ self.sql-schema ];` to the same settings,
which then take the module argument `self` (the Haskell package set):
`settings.<pkg> = { self, ... }: { … };`.

For a package that consumes pre-built dep contracts (the typical
service case where a dependency's tables must compose into the service's
yaml):

```nix
settings.<service> = {
  imports = [ (inputs.sql-schema-src + "/sqlSchema/nix/postInstall.nix") ];
  sqlSchemaPostInstall = ''
    mkdir -p $out/share/sql-schema
    sql-schema-merge \
      --fragments dist/build/sql-schema-fragments \
      --source-dirs src --source-dirs app \
      --include-merged ${inputs.app-db.packages.${system}.app-db}/share/sql-schema/sql-schema.yaml \
      --out $out/share/sql-schema/sql-schema.yaml
  '';
};
```

Pass `--generator-rev ${inputs.sql-schema-src.rev}` (only if the plugin source
is pinned by a rev) to record the plugin revision in the contract's
`generator` stamp; without it `rev` is `null`. The flag needs sql-schema
0.4.3 or later: an older `sql-schema-merge` rejects it as an unrecognised
argument and exits 2.

### `sql-schema-merge` command line

| Flag | Given | Meaning |
|------|-------|---------|
| `--fragments DIR` | once, required | The directory of per-module `*.yaml` fragments (the plugin's `fragmentsDir`). |
| `--out FILE` | once, required | Where the contract is written, only when every check passes. |
| `--source-dirs DIR` | repeatable | Source directories: a fragment whose module has no source file in any of them is pruned, unless its recorded source file still exists (`UNLISTED_SOURCE_FILE`). List every `hs-source-dirs` entry of every component compiled into the fragments directory (see below). Without the flag nothing is pruned. |
| `--include-merged FILE` | repeatable | A pre-merged contract whose tables and overrides join the output. |
| `--no-own-tables` | flag | The package defines no tables and only composes included contracts. |
| `--allow-empty` | flag | Permit a contract with no tables. |
| `--allow-empty-include` | flag | Accept an included contract with no tables, with a warning. |
| `--allow-unmatched-override` | flag | Accept an override that matches no table, with a warning. |
| `--allow-columnless-table` | flag | Accept a table with no columns, with a warning. |
| `--generator-rev REV` | once | The plugin revision recorded in the `generator` stamp; `null` without it. |

Exit codes:

| Exit | Meaning |
|------|---------|
| 0 | The contract was written; a summary line is on stdout. |
| 1 | The merge failed, was refused, or hit an I/O error. Nothing was written to `--out`. |
| 2 | An argument error, including a missing `--fragments` or `--out`, an unknown flag (`--help` too) and the cases listed under "Argument errors" below. Nothing was read. |

The walk of a `--source-dirs` entry has a fixed step cap, in the library
`moWalkEntryCap` (200000 steps by default, `walkEntryCap`), and a depth limit of
256 levels (`maxWalkDepth`); neither has a flag.

The merge CLI exits non-zero on duplicate-type **mismatch** (identical
duplicates are silently deduped), on `setEntityName`/`modelTableName`
disagreement, or on argument errors.  It also fails rather than drop
tables: on a fragment that doesn't parse (unless `--source-dirs` shows its
module is gone, and the source path it records, if any, is gone too), a
missing or malformed `--include-merged` file, a
directory it can't read, a `--fragments` or `--source-dirs` path it can't
check (table below), or `--source-dirs` that prune every fragment. A
fragment or include is malformed with a key this version does not write, a
key it writes but the file lacks (`null` is allowed where the value is
optional; only the `generator` stamp and `fragmentSourceFile` may be absent,
as in files written before each was added), or a key given twice in one YAML
mapping.
It refuses a contract with no tables unless `--allow-empty` is passed, and
one with none of the package's own fragments unless the package declares
`--no-own-tables` (it defines no tables itself and only composes
`--include-merged` contracts); with that flag, own fragments that do define
a table fail the merge, so the declaration can't go stale.  Each pruned
module is named on stderr, in quotes.  Two different Haskell types mapping
to the same SQL `tableName` produces a stderr warning, not an error.

Three more inputs fail the merge, because each means a check could not run
or a plugin did not.  Each has an opt-out flag that turns its error into a
warning:

| Input | Why it fails | Opt-out |
|-------|--------------|---------|
| an `--include-merged` contract with no tables | the plugin most likely did not run when that package was built | `--allow-empty-include` |
| a `setEntityName` override that matches no table in any fragment or included contract | the table is missing from the contract, and the override could not be checked against its `modelTableName` | `--allow-unmatched-override` |
| a table with no columns, own or included, counted once after duplicates are collapsed | the plugin read none of its fields: for example another plugin rewrote the record first, and `allowUnknownColumnWrapper` dropped them | `--allow-columnless-table` |

An opt-out relaxes only its own check.  An empty include is an input error,
like a missing or malformed include file, and input errors stop the merge
before the table checks run, so the errors of those checks (conflicting
duplicate tables, overrides, tables with no columns) show only once the input
errors are fixed, or the empty include accepted with `--allow-empty-include`.
The exit is 1 either way.

The opt-outs are flags of one merge, and the contract records none of them, so
they are not inherited through an include.  The tables and overrides of an
included contract are checked again by this merge, so a dependency built with
`--allow-columnless-table` or `--allow-unmatched-override` needs the same flag
here, and the flag then also covers this package's own tables and overrides.  An
upstream `--allow-empty-include` leaves no trace in its contract.  One override
key (module, field, table type) with two different SQL names is an error
whatever the flags, in a fragment, in a contract or across them, so an
override can't be lost when equal entries collapse into one.  Every name read
from a fragment, an included contract or a directory (modules, tables,
overrides, files, YAML and I/O errors) is printed quoted and escaped, so a
name holding a newline can't forge a `sql-schema-merge:` line.

What warns and what fails, for the two kinds of path:

| The path | `--fragments` | each `--source-dirs` entry |
|----------|---------------|----------------------------|
| a directory, or a symlink to one | read | walked |
| missing, or a dangling symlink | warning; reads as no fragments, which the merge refuses (exit 1) unless `--no-own-tables` is given and the includes hold tables, or `--allow-empty` is given and the contract is empty | warning; exit 1 instead if a fragment was pruned |
| exists but isn't a directory (a file, say) | exit 1 | warning; exit 1 instead if a fragment was pruned |
| can't be checked: below a file (`file/x`, or `file/` with a trailing slash), a symlink loop, or no search permission on it or a parent | exit 1 | exit 1 |

A `--source-dirs` entry that is missing or isn't a directory holds no modules,
so a fragment pruned while one is listed may belong to a module that lives
there: the exit 1 names the entries and the pruned modules (fix the path, or
delete the stale fragments).  Inside a listed directory, an entry that can't
be inspected (a symlink into a directory without search permission, say)
exits 1.  A symlink back to a directory on the walk's path from the entry is
skipped with a warning naming it: every name through it would repeat one found
on that path, unless a file there declares its module by the name through the
link, so a fragment pruned in a merge that skipped such a link exits 1, naming
the links and the pruned modules.  Every other symlink is followed under its
own name, an alias of a directory reached another way included, so both names
are live (over-inclusive, which is safe).  The system stops following a chain
of symlinks after a few dozen, which says nothing about a loop, so such a
chain is followed one link at a time: one that ends at a directory is an
alias, one that leads back to one of its own links has no directory at its end
and is skipped with a warning, and one of more than 1000 links, or a link that
can't be read, exits 1.  A dangling symlink is no error and, like a file,
counts as a module only if its name has a module extension.  A walk takes at
most 200000 steps per `--source-dirs` entry, and exits 1, naming the entry,
when it would take more: each directory entry listed is one step, and each
symlink followed (to a directory or a file, a dangling one too), or hop of a
chain followed one link at a time, is one step per path component its
resolution walks (the link's path, then its target's).  That bounds the
file-system work, not only the entries: a directory that several symlinks lead
to is listed once per symlink, and resolving a link deep in a tree walks every
component above it.  A tree with no symlinks takes one step per entry.  A
directory is read whole before its entries are counted, so one listing can pass
the cap.  The cap applies to each `--source-dirs` entry on its own, so the
worst case of one merge is that many steps times the number of entries, which
the caller chooses.  A walk also descends at most 256 directory levels below its
entry, a link to a directory counting as a level, and exits 1, naming the limit
and the entry, past that (the deepest directory in the trees it serves is 7
levels down).  The
warnings of an entry's walk are written 1000 lines at a time, and also when
the walk then fails.

`--source-dirs` must list every `hs-source-dirs` entry of every component
compiled into the fragments directory: read them from the `.cabal` file
rather than writing `src` alone. A module in an entry
left out has no source file in any listed one, so its fragment is pruned, and a
pruned fragment is only a stderr line: the merge still exits 0, without that
module's tables. Every component that defines tables must load the plugin too,
or its tables never reach a fragment. A from-scratch build, one that restores
no earlier build's directory or intermediates, has no stale fragments and can
omit the flag, which prunes nothing. A build that reuses intermediates, a nix
build among them, can have stale fragments.

Before it prunes a fragment, the merge checks the module's source file the
plugin recorded in it (`fragmentSourceFile`, the path as GHC was given it,
relative or absolute). If that file still exists, the module is
most likely still compiled from a directory `--source-dirs` leaves out, so the
merge fails with `UNLISTED_SOURCE_FILE` (exit 1), naming the module and the
path, instead of pruning it. A recorded file that is gone is a removed module,
pruned as before. A fragment that records no path is pruned unchecked: the
check does not cover it. Such a fragment comes from a plugin before 0.4.3, and
a build that keeps intermediates across a plugin upgrade can keep it for a
module that is still compiled (see "Known limits": build from scratch after
upgrading the plugin). A fragment that no longer decodes gets the same check,
its path read from it as plain YAML. A file that is no YAML mapping (not YAML,
a list, bytes that are not UTF-8), or whose key is missing or holds no string,
records no path, so its fragment is pruned unchecked; a key given twice keeps
its last value there. The merge only checks whether something
is at the path (a symbolic link is followed, a dangling one counts as gone).
A path it can't check fails the merge with `UNCHECKABLE_SOURCE_FILE` (exit 1),
naming the module and the fragment file: an empty path; one holding U+FFFD,
which the plugin writes for bytes of a file name that are not UTF-8, so the
file is not at that path; and one it can't stat, such as one under a directory
without search permission. A relative recorded path is relative to the
compile's working directory, so run the merge from there, the package root,
as `postInstall` does. Run from elsewhere, every relative path reads as gone
and the check passes everything, as before 0.4.3.

The walk names a module by its file's path, so the fragment of `module Main`
of an executable whose `main-is` is not `Main.hs` (say `app/Server.hs`) never
matches a live module: the merge refuses it (`UNLISTED_SOURCE_FILE`), where an
earlier merge pruned its tables silently. Keep tables out of `Main` modules.

The contract is written only after every check has passed, atomically: to a
new temporary file next to `--out` (an existing `<out>.tmp` is never touched),
which is then renamed over it.  A merge that exits 1 (failed, refused, or an
I/O error) never creates or replaces `--out`, so an earlier contract stays as
it was; one that is killed mid-write can leave its temporary file behind, which
nothing reads.  A refusal states the fragment, pruned and included counts the
summary line would have given.
"Argument errors" exit 2: a repeated `--fragments`, `--out` or
`--generator-rev`; an empty value (an unset shell variable); a value that
starts with `--` (an unset, unquoted variable leaves the next option in its
place); a flag without its value; an unknown argument.

The same checks run in the library: `SqlSchema.Merge.runMerge` returns
`Left` before writing when `publishProblem` refuses (`moAllowEmpty` and
`moNoOwnTables` are its two opt-outs), and `SqlSchema.MergeCli.mergeMain`
is the CLI as a function from arguments to an exit code.
`moAllowEmptyInclude`, `moAllowUnmatchedOverride` and `moAllowColumnlessTable`
relax the three checks above.

## Plugin options (JSON in `-fplugin-opt`)

| Key                         | Type         | Default                              | Notes |
|-----------------------------|--------------|--------------------------------------|-------|
| `fragmentsDir`              | string       | `./.juspay/sql-schema-fragments`     | Where per-module fragments are written; it must hold only this plugin's files. For incremental builds that restore the build directory, keep it inside that directory (e.g. `dist/build/sql-schema-fragments`), or the fragments of modules that were not recompiled are lost. Components that share one directory must not define same-named modules with tables: their fragments share a file, and the last compile wins. Same-named modules without tables (several test suites' `Main`) must not be compiled at the same time: they race on that file, and the losing compile fails with `STAGED_FRAGMENT_MISSING`, never with a wrong fragment. Give such components their own `fragmentsDir`, or no plugin. An empty value, the working directory (`.`, `./`, `./.`), a value with a `..` component, and one that resolves to the working directory or a directory above it (an absolute path, or through a symbolic link) are invalid options (see `fragmentsDir` under the wiring section). |
| `blacklistModules`          | [string]     | `[]`                                 | Modules to skip entirely. An entry ending in `.` (e.g. `Test.`) skips every module it prefixes; any other entry skips only the module of exactly that name (`App.Db` skips only `App.Db`; `App.Db.` skips `App.Db.Pool`, but not `App.Db`). An empty entry, or one starting with `.`, is an invalid option. |
| `allowUnknownColumnWrapper` | bool         | `false`                              | If `true`, fields whose type isn't `Columnar f T` / `C f T` are silently left out of the table (no warning). Use only for migrations — the default refusal is the safe stance. |
| `allowNonStandardModifier`  | bool         | `false`                              | If `true`, a modifier the plugin can't read is silently treated as "no overrides" (Beam default names): one that isn't `tableModification { … }` (guarded, repeated or `where`-bound definitions and `OverloadedRecordUpdate` syntax included), or a binding that isn't a plain top-level `x = …` of this module, bound once, with one unguarded right-hand side and no `where` bindings (imported, qualified, or bound by a pattern). It does not cover `REDEFINED_NAME` or the `UNSUPPORTED_` errors. Same caveat as above. |
| `tableNameOverrides`        | {str: str}   | `{}`                                 | Per-type-name SQL-name override; applied only when `modelTableName` is not a static literal (e.g. `modelTableName = SomeConst.fooName`). Lets you assert the resolved value in plugin config instead of editing storage source. |

## Safety policy

The contract drives a production gate. A false-pass (YAML says fine,
prod isn't) is the worst outcome, so the plugin refuses to extract any
shape it can't read precisely. Each error's first line starts with exactly
`sql-schema: <CODE>: `, where `<CODE>` matches `[A-Z_]+` and is the code in
the table below; any further lines of the message start with two spaces. Every
code is listed:

| Hard compile error             | What it means |
|--------------------------------|---------------|
| `COLUMN_TYPE_MISMATCH`         | After type checking: a column's field, type synonyms expanded, isn't beam-core's `Columnar` of the table's last type parameter with a base `Maybe` exactly when the column was recorded as nullable (a synonym or family that hides or adds `Maybe`, another `Columnar` or `Maybe`), or the record has no such field. Write `Maybe` out in the field's type. There is no opt-out. |
| `DUPLICATE_INSTANCE`           | The module has two `Table`, `ModelMeta` or `Beamable` instances, written or derived, for one type name: same-named classes of different modules, or one parameterised type at two arguments (`ModelMeta (Wrap A)` and `ModelMeta (Wrap B)`). Keep one per table type. |
| `FRAGMENT_IO_ERROR`            | A file operation the plugin does to stage, read back or publish a module's fragment failed: the system's error is quoted with the path (see Architecture notes). Nothing is published, so the module's previous fragment stays and the compile fails instead. Typical causes are a `fragmentsDir` that is not writable, is a file, or lies behind a chain of symbolic links longer than the system follows. |
| `GENERIC_FIELD_MISMATCH`       | After type checking: a table has no GHC.Generics `Generic` instance, more than one that matches or could match, or one whose representation is not one constructor whose selectors are all the record's fields in order (a Template Haskell or orphan instance, one behind a module class named `Generic`); for a large-records table, large-generics' `MetadataOf` that does not name all the record's fields in order; or staged columns that are not those fields in order. Beam names the columns after those selectors, or that metadata. Derive it with the stock strategy or, for a table annotated with large-records' `largeRecord`, leave both `Generic` instances to that plugin. There is no opt-out. |
| `INTERNAL_ERROR`              | The extractor refused a module without giving a reason, which cannot happen: a bug in the plugin, not in the module. Report it with the module's source. Also after type checking, when GHC kept no renamed source for a module with tables: the plugin asks GHC to keep it, so another plugin of the compile dropped it; compile without the other `-fplugin` flags to find which. There is no opt-out. |
| `INVALID_PLUGIN_OPTIONS`       | The `-fplugin-opt` JSON is malformed, has an unknown key, a wrong-typed value, an invalid `blacklistModules` entry or `fragmentsDir`, or is given more than once. |
| `MISSING_MODEL_META`           | A table has no `ModelMeta` instance in its module. `tableNameOverrides` doesn't stand in for one. |
| `MODEL_META_FIELD_MISMATCH`    | After type checking: a table's `modelTableType` or `modelSchemaName` doesn't resolve to what the plugin recorded: an imported pattern synonym or another module's `Just`, `Nothing` or `TableType` constructor. Write it with base's `Just`/`Nothing` and a constructor of sequelize's `TableType`. There is no opt-out. |
| `REDEFINED_NAME`               | The module defines, itself, a name the plugin reads a table through: `tableModification` or `fieldNamed`, or a name `primaryKey` applies (`.`, `<$>`, `<*>`, `getField`, or a field name the module also binds another way, as `NoFieldSelectors` allows), or a pattern synonym named `Just`, `Nothing` or a `TableType` constructor, or a type, type synonym or family named `C`, `Columnar`, `Maybe` or `TableEntity`. Names are matched as written, so the plugin can't tell the module's definition from Beam's or base's. Rename it. There is no opt-out. |
| `STAGED_FRAGMENT_MISSING`      | The module's staged fragment was gone when the plugin went to cross-check it after type checking or to publish it (see Architecture notes): something else removed it from `fragmentsDir`, such as a compile of a same-named module at the same time (see `fragmentsDir` above). Rebuild the module. |
| `STAGED_FRAGMENT_UNREADABLE`   | After type checking, the module's staged fragment does not decode as a fragment: the decode error is quoted with the path. The parse step writes the file whole, through a rename, so something other than the plugin wrote it. Keep `fragmentsDir` for this plugin's files only, and rebuild the module. |
| `UNCAPTURED_MODIFY_TABLE_FIELDS` | A `modifyTableFields` outside the shape under "DB-settings renames": not applied directly to an argument, an argument that isn't an unqualified variable or isn't the `modelFieldModification` binding of a table of this module, a binding name also bound elsewhere in the module (a parameter or a `where` binding, say) or rebound by a record wildcard `{..}`, a module that binds `modifyTableFields`, `tableModification`, `fieldNamed` or `(<>)` itself, a type application, or a binding that isn't one equation with no arguments, guards or `where` bindings, a `tableModification { … }` with at least one field. The message names the line and the reason. Pass the binding named in `modelFieldModification`; if no column is renamed, drop `modifyTableFields`. No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `UNCAPTURED_SET_ENTITY_NAME`   | A `setEntityName` the plugin doesn't capture as an override: one outside the shape under "What it extracts" (in an EMod or other separate binding, a `where` or `let` binding, an instance method, a splice, or a `dbModification` that `withDbModification` does not apply to a top-level binding's result), with a non-literal name, in a `<>` chain with two of them or with another kind of operand, under a type annotation, a label qualified with another module or a record wildcard `{..}`, in an update whose labels no single record of the module declares, before a later `withDbModification` operand that isn't a `dbModification { … }` update, on a field with no table type or several across the module's records, or in a module that binds one of the names the shape relies on or declares a type named `TableEntity`. The message names the line and the reason. Rewrite it into the shape that is read. No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `UNCHECKABLE_RENAMES`          | After type checking, in a module that uses `setEntityName` or `modifyTableFields`: the plugin cannot check the `Semigroup` and `Monoid` instances that combine its renames, because a record field is typed by an existential type variable that equality constraints mention but fix to no one type (located at the field), or because the beam-core in use defines no `EntityModification` or `TableEntity` in `Database.Beam.Schema.Tables` (located at the module). Give the field a type GHC can read, or build with beam-core 0.9's layout. There is no opt-out. |
| `UNCHECKABLE_SOURCE_FILE`      | The merge, not a compile: a fragment whose module is in no `--source-dirs` entry would be pruned, but the source path the plugin recorded for it can't be checked: it is empty, holds U+FFFD (a file name that is not UTF-8), or can't be stat-ed (e.g. a directory without search permission). The error names the module, the fragment file and why. Run the merge where the compile ran, able to search the path's directories, and give source files UTF-8 names (exit 1). |
| `UNEXPECTED_CLASS`             | After type checking: the class of a `Table`, `Beamable` or `ModelMeta` instance is a same-named class, not beam-core's (`Database.Beam.Schema.Tables`) or sequelize's (`Sequelize`), by package and module. Make it an instance of the library's class, or rename the other class. There is no opt-out. |
| `UNEXPECTED_INSTANCE`          | After type checking, in a module that uses `setEntityName` or `modifyTableFields`: a `Semigroup` or `Monoid` instance other than beam-core's could apply at a renamed table's `EntityModification` (an `OVERLAPPING` or incoherent one, in any module GHC sees). It can drop or change a rename the contract records. Remove it. There is no opt-out. |
| `UNEXTRACTED_TABLE`            | A type with a `Beamable`, `Table` or `ModelMeta` instance in the module (written, derived inline or derived standalone) isn't a table the plugin can read: it has no data declaration in the module; its declaration isn't a single-constructor record (Haskell 98 syntax) with the column functor as last type parameter; nothing in the module derives or declares `Beamable` for it (an instance Template Haskell generates doesn't count); it derives `Database`; the module neither derives its `Generic` instance with the stock strategy nor annotates it `{-# ANN type T largeRecord #-}`, or has another `Generic` instance that could be its (written, derived `via`, `newtype` or `anyclass`, also for a type synonym of the module); or the module writes a `Beamable` instance with a method for it, its key or a type synonym of it, or derives one `via` another type. After type checking: an instance the parse step did not see, from Template Haskell or another plugin; an instance whose type GHC resolves to one the module does not declare, such as a splice's instance for an imported type of a table's name; or a table's `largeRecord` annotation that is not the large-records package's value of its options type. The message gives the fix for its reason. |
| `UNKNOWN_PRIMARY_KEY_FIELD`    | A field `primaryKey` applies isn't a column the plugin extracts for the table: a helper function, another module's name (`M.fooId`), or a field `allowUnknownColumnWrapper` left out. There is no opt-out. |
| `UNLISTED_SOURCE_FILE`         | The merge, not a compile: a fragment whose module is in no `--source-dirs` entry would be pruned, but the source file the plugin recorded for it still exists, relative to the merge's working directory. Add the directory the file is under to `--source-dirs`, or, if the module is no longer compiled with the plugin, delete the fragments directory and rebuild (exit 1). |
| `UNREADABLE_INSTANCE_HEAD`     | A `Table`, `ModelMeta` or `Beamable` instance isn't for one unqualified type constructor, so the plugin can't tell which table it belongs to: e.g. `instance Table t => ModelMeta t`, `instance ModelMeta M.FooT`, a promoted `'FooT`, `instance Table (PrimaryKey FooT)` (a key's `Beamable` instance is fine), or a class applied to two types (`instance X.Table Int FooT`). After type checking: an instance the parse step did not see whose head has no type constructor. |
| `UNRECOGNIZED_FIELD_WRAPPER`   | A field's type isn't `Columnar f T` / `C f T`. Override via `allowUnknownColumnWrapper` only if you accept that field being omitted (the table is still emitted). |
| `UNRECOGNIZED_MODIFIER_SHAPE`  | `modelFieldModification`, or the `xxxTMod` binding it names, isn't a single unguarded `tableModification { … }` in plain record update syntax (`OverloadedRecordUpdate` syntax and a derived `ModelMeta` instance included), or `modelFieldModification` is bound twice, by a pattern, or with `where` bindings. Override via `allowNonStandardModifier`. |
| `UNRESOLVABLE_COLUMN_NAME`     | The RHS inside `xxxTMod` isn't `fieldNamed "lit"`, a punned field (`{ fooName }`) included. Either rewrite or drop the override. |
| `UNRESOLVABLE_MODEL_META_FIELD` | `modelTableType` or `modelSchemaName` isn't in a shape listed under "What it extracts" (e.g. `Just $ x`, `pure x`, `Just (f x)`, `Just someVar` for `modelTableType`, a guarded binding), or comes from a derived `ModelMeta` instance. There is no opt-out. |
| `UNRESOLVABLE_MODIFIER_BINDING`| `modelFieldModification` names a binding the plugin can't read: it reads only a plain top-level `x = …` binding of this module, referenced unqualified, bound once, with one unguarded right-hand side and no `where` bindings. Define it that way, or use `allowNonStandardModifier`. |
| `UNRESOLVABLE_PRIMARY_KEY`     | `primaryKey` is missing, or isn't written once, as one equation with no guards or `where` bindings, in a recognised shape. Rewrite into one of the supported forms. |
| `UNRESOLVABLE_TABLE_NAME`      | `modelTableName` isn't a literal written once, as one equation with no arguments, guards or `where` bindings. Inline the literal, or use `tableNameOverrides`. |
| `UNSUPPORTED_ENTITY_RENAME`    | A use of `modifyTable`, `modifyEntityName`, `withTableModification` or `renamingFields`, or of `EntityModification`, `DatabaseEntity`, `dbEntityName` or `dbTableCurrentName` as a variable, a constructor or a constructor pattern (a signature or a record label may name them): each can rename a table or its columns in a way the plugin can't read. Rename the table with `setEntityName "sql_name"` and its columns with `modifyTableFields` as under "DB-settings renames". No option turns the check off; a module in `blacklistModules` is skipped entirely, this check included. |
| `UNSUPPORTED_EXTENSION`        | A module that defines a table or a `setEntityName` override enables `RebindableSyntax`, `OverlappingInstances` or `IncoherentInstances`, whether by a pragma or a flag. Under `RebindableSyntax`, record-dot syntax and string literals call the `getField` and `fromString` in scope; under the other two, an instance overlaps a library's without a pragma, so one at an `EntityModification` type, also through a type synonym, can change how a `<>` chain combines the renames. Either way the plugin could record a key, name or column the code does not use. Turn the extension off in that module (`{-# LANGUAGE NoRebindableSyntax #-}`, with `import Prelude`: `RebindableSyntax` also turned the implicit Prelude import off; for the other two, mark each instance that must overlap with a pragma). There is no opt-out. |
| `UNSUPPORTED_RECORD_WILDCARD`  | A module that defines a table or a `setEntityName` override binds a record wildcard pattern at top level (`R{..} = …`), which binds names the parser does not show. Bind the fields by name, or move it to another module. There is no opt-out. |
| `UNSUPPORTED_RULES`            | A module that defines a table or a `setEntityName` override has a `RULES` pragma. A rule can rewrite `fieldNamed` or another name the plugin reads, and GHC applies it when it optimises. Move the rules to a module this module does not import, directly or not: GHC applies a rule in every module that imports its module, however indirectly. There is no opt-out. |

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

`primaryKey` is written once, with no guards or `where` bindings, and each
field it applies is one of the table's own columns, unqualified.

## Architecture notes

- Per-module fragments mean incremental builds stay correct: an
  unchanged module's fragment on disk is still authoritative, as long as
  the plugin is the same. After a plugin upgrade on a build that keeps
  intermediates it is not: see "Known limits" (build from scratch). Stale
  fragments (whose module has no source file in `--source-dirs`) are pruned at merge
  time, and a recompiled module that no longer defines a table or
  override, or is blacklisted, has its fragment deleted.
- A compile stages its module's fragment when it parses the module, as
  `<Module>.yaml.pending`: the fragment's YAML, with the module's source file
  as GHC gives it (`fragmentSourceFile`, which the merge checks before it
  prunes), or an empty file meaning "delete the fragment". A pass the plugin appends to the end of the Core
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
  `<Module>.yaml.pending*.tmp` there. A compile fails with
  `STAGED_FRAGMENT_MISSING` when a staged file is gone at publish time, and
  with `FRAGMENT_IO_ERROR` when one of these file operations fails, a
  `fragmentsDir` behind a chain of symbolic links past the system's limit
  included, or when the type-check step cannot read a staged file back, and
  with `STAGED_FRAGMENT_UNREADABLE` when the staged file it reads does not
  decode as a fragment; the error names the path and the module.
- Staged files and the merged YAML are written to a temp file and renamed
  into place.
- The extractor runs at `parsedResultAction` time and inspects only what
  the parser produces, so it does not depend on type-class resolution.
  In particular, `Columnar` / `Beamable` / `Database` are matched by
  the syntactic tail of the qualifier. The type-check cross-check then
  compares what it read with GHC's resolution (see above).

## Known limits

- A failure after the publish pass (a later Core pass, tidy, code
  generation, a killed build) followed by a revert can leave the failed
  edit's fragment, or its deletion, published in a local incremental build
  on GHC 9.8, which does not recompile the reverted module (GHC 9.2 does, by
  timestamp). A published deletion silently drops the module's tables from
  the contract. Nix builds are unaffected: they start from scratch or from a
  successful build's intermediates.
- The check that `fragmentsDir` is neither the working directory nor above it
  runs twice per compile, each time before that step's own writes: when the
  module is parsed, before staging, and in the publish pass, before
  publishing (the type-check step between them only reads). Someone who can
  write to the build tree can swap a link between a check and the writes after
  it and defeat it. That is outside the threat model: it takes a deliberate
  action by someone with write access to the tree, who could delete the file
  directly.
- On GHC 9.2 the type-check step's scenarios against the real beam-core,
  sequelize and large-records packages run only against stand-ins of them:
  9.2's package set here has none of the three.
- An `--include-merged` file is parsed whole, with YAML aliases expanded, so a
  crafted include of 40 KB expands to 3.8 MB, growing quadratically. Crafting
  one takes build access to a dependency, whose code and contract that access
  could change anyway, so this is outside the threat model. There is no size
  cap: a cap on the file would not bound the expansion, which happens while it
  is parsed, and the yaml package has no limit on aliases. Likewise, a crafted
  file can set a key both through a YAML merge key (`<<: *anchor`) and
  explicitly: YAML lets the explicit value win, with no repeated key to
  refuse. The plugin and the merge never write an alias or a merge key.
- After upgrading the plugin, build from scratch: discard the build's
  intermediates. GHC 9.2.8 and 9.8.4 recompile no module for a plugin rebuilt
  under the same unit id and options while the old plugin library is still
  readable at its old path, as an earlier nix store path stays (probed; they
  recompile once that library is gone). The unchanged modules then keep the
  fragments the old plugin wrote, and the new plugin's checks never run on
  them; a fragment from a plugin before 0.4.3 also records no source path, so
  the `--source-dirs` check can't hold it (above).
- A merge in a build directory restored from an earlier build can refuse a
  removed module whose source file was generated (`.x`, `.y`, `.hsc`, …): the
  plugin records the generated `.hs` file, which the restored directory may
  still hold (`UNLISTED_SOURCE_FILE`). Delete the fragments directory and
  rebuild.
- An editor session (HLS, ghcide) with the plugin on may publish fragments
  of unsaved buffers, so keep the plugin flag off there. With it on,
  re-type-checking a module from a cached parse after a cabal build has
  published the module's fragment can show `STAGED_FRAGMENT_MISSING` in the
  editor; a rebuild clears it. This is unverified: no HLS was run.
