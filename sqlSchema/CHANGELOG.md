# sql-schema

## 0.4.3

* `sql-schema-merge` writes a top-level `generator: {name, version, rev}`
  stamp into the contract so consumers can tell which plugin version produced
  it. `rev` comes from the new `--generator-rev REV` flag and is `null` when
  the flag is omitted. Contracts without the key (written by 0.4.2 and
  earlier) still decode, including as `--include-merged` inputs; the stamps of
  included contracts are ignored.
* **Fix:** when a recompiled module no longer defines a table or a
  `setEntityName` override, or is newly listed in `blacklistModules`, its old
  fragment is deleted. Before, a restored build directory kept that fragment,
  so the contract still listed a table the module no longer defines or the
  options now exclude. A module's `.hs-boot` or `.hsig` file is skipped
  entirely: it is not checked and never touches the module's fragment.
* **Breaking:** every `setEntityName` in a module must be captured as an
  override, or the compile fails with `UNCAPTURED_SET_ENTITY_NAME`, naming
  the line and the reason. Before, a rename the plugin didn't read was left
  out of the contract without a word, so the merge never checked it against
  `modelTableName`: one in an EMod or other separate binding, a `where` or
  `let` binding, an instance method or a splice, one with a non-literal
  name, and one whose field is in no `Database` record of the module. A
  rename in a `<>` chain (`setEntityName "x" <> modifyTableFields m`) is now
  read when the chain's other operands are `modifyTableFields x` or
  `setEntitySchema x`. A rename could also be credited to the wrong table, or
  read though it was never applied. These are now refused:
  - a `dbModification` that isn't the right operand of `withDbModification`
    in a top-level binding's result;
  - a label of the update qualified with another module;
  - a type annotation on the `dbModification` or the settings expression;
  - a field whose name has another type in any record of the module (before,
    only records deriving `Database` were read, and the one whose name sorts
    first supplied the type);
  - a `<>` chain with any other operand;
  - an update whose labels no single record of the module declares (GHC
    resolves an update to the record that has all its labels); a record
    inside a Template Haskell declaration quote doesn't count;
  - a later `withDbModification` operand that isn't a `dbModification { … }`
    update;
  - a record wildcard `{..}` in the binding's declaration, a top-level pattern
    binding or a top-level splice;
  - a module that binds `setEntityName`, `dbModification`,
    `withDbModification`, `(<>)`, `modifyTableFields` or `setEntitySchema`
    itself.

  A punned record field named like a checked function
  (`R { setEntityName }`) counts as a use, and under `OverloadedRecordUpdate`
  it is reported at its field. Still undetected: an unannotated update whose
  labels all name fields of one local record, read as that record's even if
  its type were to come from elsewhere (GHC 9.2.8 and 9.8.4 reject every such
  shape tried as ambiguous).
* **Fix:** under `-haddock`, which nixpkgs passes to library builds, a
  deriving clause with a doc comment, such as
  `deriving (Generic, Database -- ^ doc)`, is read. GHC wraps a documented
  type, and the wrapper hid the class: such a record's `setEntityName` renames
  were dropped, and a table whose `Beamable` deriving had a doc comment was
  left out of the contract.
* **Breaking:** a `modifyTableFields` whose argument isn't the table's own
  modifier binding, or any
  `modifyTable`/`modifyEntityName`/`withTableModification`, is now a compile
  error. So is `renamingFields`, and any use, as a variable, a constructor or
  a constructor pattern, of `EntityModification`, `DatabaseEntity`,
  `dbEntityName` or `dbTableCurrentName`, from which a modification can be
  written by hand; signatures and record labels may still name them. The
  errors, `UNCAPTURED_MODIFY_TABLE_FIELDS` and `UNSUPPORTED_ENTITY_RENAME`,
  name the line. Before, column renames
  applied through the DB settings were never compared with the contract's,
  which come from `modelFieldModification`. The accepted shape is
  `modifyTableFields xxxTMod` with `xxxTMod` the unqualified, top-level binding
  named in `modelFieldModification` of a table of the module, setting at least
  one field and bound nowhere else in the module, with no record wildcard
  `{..}` covering the use, in a module that binds none of `modifyTableFields`,
  `tableModification`, `fieldNamed` and `(<>)` itself; if no column is
  renamed, drop `modifyTableFields`.
* **Fix:** a `--source-dirs` entry with a trailing slash (`src/`) shifted every
  module name it produced, so all of its fragments were pruned as stale.
* **Breaking:** `sql-schema-merge` fails instead of dropping tables. Each of
  these used to be skipped with at most a warning, and the merge still exited
  0:
  - a fragment that doesn't parse, unless `--source-dirs` shows its module is
    gone;
  - a missing or malformed `--include-merged` file;
  - a fragments or source directory it can't read or search;
  - `--source-dirs` that prune every fragment.

  A `--source-dirs` entry that doesn't exist still only warns; each pruned
  module is now named on stderr.
* **Breaking:** the CLI refuses a contract with none of the package's own
  fragments, as it already refused one with no tables: included tables used
  to hide that the package's plugin never ran. A package that defines no
  tables of its own and only composes `--include-merged` contracts passes the
  new `--no-own-tables` instead of `--allow-empty`; the merge then fails if
  its own fragments do define a table. `--allow-empty` permits only a contract
  with no tables.
* **Fix:** modules in `.lhs`, `.hsc`, `.chs`, `.x`, `.y` and `.ly` files count as
  live, so their fragments are no longer pruned as stale.
* **Fix:** a compile that fails after parsing (a type error, or a warning made
  an error by `-Werror`) no longer publishes the module's fragment. The plugin
  stages the fragment as `<Module>.yaml.pending` when it parses the module, and
  a pass at the end of the Core pipeline, which GHC reaches only after type
  checking, desugaring and the `-Werror` check, publishes it. Before, the
  fragment was written at parse time; when the failed edit was then reverted,
  GHC 9.8, which recompiles by source hash, did not recompile the module, and
  the contract kept the edit. A staged file missing at publish time is the new
  compile error `STAGED_FRAGMENT_MISSING`.
* **Breaking:** a compile that generates no code (`-fno-code`, haddock) no
  longer writes `<Module>.yaml`: it leaves `<Module>.yaml.pending`, which the
  merge ignores, so run the merge after a build that generates code. Two
  cases still publish, after the `-Werror` check: the modules GHC compiles
  to code for Template Haskell, and on GHC 9.8 `-fno-code
  -fwrite-if-simplified-core`, which runs the Core pipeline.
  `fragmentsDir` must hold only this plugin's files, and same-named modules
  without tables (several test suites' `Main`) must not be compiled into it
  at the same time: the losing compile fails with `STAGED_FRAGMENT_MISSING`.
  Give such components their own `fragmentsDir`, or no plugin.
* Drops unused dependencies: `references`, `uniplate`, `extra`,
  `ordered-containers` and `unordered-containers` from the library, and
  `directory` and `filepath` from `sql-schema-merge`.
* **Breaking:** a `blacklistModules` entry that doesn't end in `.` matches only
  the module of exactly that name: `App.Db` no longer also skips `App.DbUtil`
  or `App.Db.Pool`. An entry ending in `.` (e.g. `Test.`) still matches every
  module it prefixes, so `App.Db.` skips `App.Db.Pool` but not `App.Db`. An
  empty entry, which used to skip every module, and an entry starting with `.`
  are now invalid options (`INVALID_PLUGIN_OPTIONS`).
* **Breaking:** an empty `fragmentsDir`, or one that normalises to `.` (e.g.
  `./`), is an invalid option (`INVALID_PLUGIN_OPTIONS`). It meant the working
  directory, where deleting a table-less module's stale `<Module>.yaml` could
  delete an unrelated file: on a case-insensitive file system, compiling
  `module Stack` deleted `stack.yaml`.
* **Fix:** plugin error messages escape every control, format and line- or
  paragraph-separator character, newlines included, in each value they quote
  (source text such as a quasi-quote, an option value, a path), cut each such
  value at 500 characters, and are capped at 4000 characters, so source text
  can no longer add lines or terminal escapes to a build log.

## 0.4.2

* **Fix:** a field with no `fieldNamed` entry in its `tableModification`
  now gets the column name Beam's `defaultDbSettings` gives it
  (`widgetEnabled` → `enabled`) instead of its Haskell field name.
* Changing the plugin options now recompiles every module (the options are
  fingerprinted into `pluginRecompile`), so each fragment is rewritten under
  the new options. Not yet handled: a module newly added to `blacklistModules`
  keeps its old fragment until stale fragments are pruned (fixed in 0.4.3).
* **Breaking:** plugin options are strict: malformed JSON, a wrong-typed value
  (e.g. `"blacklistModules":"Test."`), an unknown key, or more than one
  `-fplugin-opt` string is a compile error (`INVALID_PLUGIN_OPTIONS`). Before,
  malformed JSON or a wrong-typed value silently reset every option to its
  default, an unknown key was ignored, and only the first string was used.
* **Breaking:** a `modelFieldModification` the plugin can't read is an error
  instead of silently meaning "no overrides". `UNRECOGNIZED_MODIFIER_SHAPE`
  covers a guarded `modelFieldModification` and any expression other than a
  bare binding or `tableModification { … }` (e.g. `a <> b`, `f $ x`,
  `let … in`). `UNRESOLVABLE_MODIFIER_BINDING` covers a named binding that is
  imported, qualified (which used to read a same-named local binding), bound
  in `where`, guarded, or bound by a pattern such as `(x) = …`. An inline
  `tableModification { … }` is now read like an `xxxTMod` binding, so a field
  whose RHS isn't `fieldNamed "lit"` is `UNRESOLVABLE_COLUMN_NAME` (it used to
  be ignored). `allowNonStandardModifier` still opts out of the two modifier
  errors.

## 0.4.1

* Builds on GHC 9.8 as well as 9.2: CPP shims cover the GHC 9.4+ parser AST
  (token fields, `HsFieldBind`, `DotFieldOcc`, `DataDefnCons`, `LHsRecUpdFields`)
  and the 9.4+ diagnostics API. Extraction output is unchanged.
* **Fix:** `sql-schema-merge` no longer aborts when a `setEntityName` override's
  table type matches several tables that all already use the overridden name
  (an upstream package's `JobT` re-modelled in a downstream package, which
  broke that package's merge). It still aborts when the candidates disagree.

## 0.4.0

* **Breaking (contract shape).** Each `TableSchema` now carries `codeName` (the
  Haskell table type constructor, e.g. `WidgetT`) and `modelTableName` (the
  `ModelMeta` SQL name) in place of the old `tableName` field, and the redundant
  `haskellType` field is dropped — it was always `sourceModule <> "." <> codeName`
  and is now exposed as the `qualifiedType` helper. Consumers reading
  `haskellType` / `tableName` must switch to `sourceModule`+`codeName` /
  `modelTableName`.
* **Fix: `setEntityName` overrides are now extracted.** A Beam DB-record
  (`data Db f = … deriving Database`) is shaped like a Beam table (single-
  constructor record with a type variable), so `visitDecl` claimed it as a
  (non-Beamable, ultimately dropped) table candidate *before* the DB-record
  branch ran — leaving `rmDbRecords` empty and `mergedDbEntityOverrides`
  perpetually `[]`. The `deriving Database` branch is now checked first, so
  `withDbModification` / `setEntityName` renames are captured (and cross-checked
  against each table's `modelTableName`).

## 0.3.0

* `sql-schema-merge` now **exits non-zero when it would emit a contract with
  zero tables**, unless `--allow-empty` is passed. A zero-table contract almost
  always means the `SqlSchema` plugin did not run during compilation (e.g. the
  package was built with the plugin disabled), and silently publishing an empty
  contract is a downstream false-pass against the production DB. Packages that
  genuinely define no Beam tables (and only compose dependency contracts via
  `--include-merged`) should pass `--allow-empty`. The library `runMerge` and
  its return type are unchanged; the gate lives in the CLI.

## 0.1.0.0

* Initial release. GHC plugin that extracts the SQL schema implied by Beam
  table definitions (column name, Haskell type, nullability, primary key)
  into per-module YAML fragments under
  `.juspay/sql-schema-fragments/<Module>.yaml`. A companion
  `sql-schema-merge` executable combines fragments into a single contract
  YAML, prunes stale fragments (source file gone), rejects duplicate
  Haskell types, and cross-checks `setEntityName` overrides against each
  table's `modelTableName`.
* Regenerate-only — validation against a live database is the
  responsibility of downstream release tooling, not this package.
