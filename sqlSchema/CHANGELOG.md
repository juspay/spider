# sql-schema

## 0.4.3

* **Breaking:** `tested-with: GHC == 9.2.8, GHC == 9.8.4`, and the library's
  `ghc` dependency is bounded to `^>=9.2 || ^>=9.8`. The parser's syntax tree
  changes with every major GHC release and only these two are tested, so an
  untested compiler now fails at the solver instead of extracting wrongly.
* Documentation: the README covers the plugin order (GHC 9.8 runs plugins in
  command-line order, 9.2 in reverse, so `-fplugin=SqlSchema.Plugin` goes
  first on 9.8 and last on 9.2), the dedicated `fragmentsDir`, the haskell-flake
  wiring through `sqlSchemaPostInstall` (haskell-flake has no `postInstall`
  setting), `extraBuildDepends` for a flag-conditional dependency, every
  `sql-schema-merge` flag and exit code, the error-line format and every error
  code.
* Drops unused dependencies: `references`, `uniplate`, `extra`,
  `ordered-containers` and `unordered-containers` from the library, and
  `directory` and `filepath` from `sql-schema-merge`.
* **Breaking:** a type with a `Beamable` deriving or instance, or a `Table` or
  `ModelMeta` instance, that the plugin can't extract as a table is a compile
  error (`UNEXTRACTED_TABLE`); before, it was silently left out of the
  contract. This covers a positional or GADT-syntax record, an instance for a
  type declared in another module, a table whose `Beamable` comes from another
  module or from Template Haskell, and a `Database` record. Standalone
  `deriving instance Beamable XxxT` is now read like an inline deriving clause.
  A derived `Table` or `ModelMeta` instance, inline or standalone, counts too,
  but has no methods to read: a derived `ModelMeta`'s modifier is
  `UNRECOGNIZED_MODIFIER_SHAPE` unless `allowNonStandardModifier` is set.
* **Breaking:** a table with no `ModelMeta` instance in its module is a compile
  error (`MISSING_MODEL_META`). `tableNameOverrides` no longer stands in for
  the instance, because the table's column names would be guessed.
* **Breaking:** a `Table`, `ModelMeta` or `Beamable` instance that isn't for one
  unqualified type constructor is a compile error (`UNREADABLE_INSTANCE_HEAD`):
  a type variable, as in `instance Table t => ModelMeta t`, a qualified or
  promoted name, a `Table` or `ModelMeta` instance for a key (`PrimaryKey t`),
  or a class applied to two types (`X.Table Int XxxT`). A qualified or promoted
  name used to be taken for the module's own type of the same name, and a
  two-type class for the one-parameter one.
* **Breaking:** two `Table`, `ModelMeta` or `Beamable` instances for one type
  name in a module are a compile error (`DUPLICATE_INSTANCE`): same-named
  classes of different modules, or one parameterised type at two arguments.
  The later one used to win, so a table could take another class's name and
  key, or one of two instantiations was silently missing.
* **Fix:** an instance with a context (`instance C => ModelMeta XxxT`) or with a
  kind signature around its head is read. A `ModelMeta` instance with a context
  used to be skipped, so with `tableNameOverrides` the table was emitted with
  Beam's default column names.
* **Fix:** a `Table`, `ModelMeta` or `Beamable` instance whose head nests a
  context or a `forall` (`instance C => (D => Table T)`) is read like any
  other; it used to be skipped silently. GHC rejects such a head, but only
  after the plugin has run.
* **Breaking:** a table's `Generic` instance must be derived in its module
  with the stock strategy, by its deriving clause (no strategy written, or
  `stock`) or by `deriving instance Generic (XxxT f)` with the table itself in
  the head, or come from the large-records plugin, which
  `{-# ANN type XxxT largeRecord #-}` (any qualifier) asks for. A table with
  neither, or with another `Generic` instance that could be its (written,
  derived `via`, `newtype` or `anyclass`, also for a type synonym of the module
  or a head the plugin can't read), is a compile error (`UNEXTRACTED_TABLE`).
  Beam names the columns from the Generic representation, which only a stock
  derivation, or that plugin, takes from the record's fields; the contract used
  to name the fields whatever the instance, so one that Template Haskell or
  another plugin generated could rename the columns unseen. Likewise a table
  whose `Beamable` instance, its key's or a type synonym's, is written with a
  method or derived `via` another type is refused: `zipBeamFieldsM` decides the
  field each column rename of the modifier goes to, so such an instance could
  apply a rename to another field.
* **Breaking:** `modelTableType` must be `Nothing` or `Just` of a `TableType`
  constructor, and `modelSchemaName` must be `Nothing`, `Just "literal"` or
  `Just <value name>`, each also in parentheses or with a type signature, as
  one equation with no arguments, guards or `where` bindings. Any other shape
  is a compile error (`UNRESOLVABLE_MODEL_META_FIELD`), with no opt-out.
  Before, `Just $ x`, `pure x`, `Just (f x)`, a guarded binding or several
  equations were recorded as `null`, which routes the table to the wrong
  database downstream; `modelTableType = Just someVar` (or a pattern synonym)
  was recorded under that name, and `modelSchemaName = Just s where s = …` as
  the name `s`. A derived `ModelMeta` instance is refused too, even under
  `allowNonStandardModifier` and `tableNameOverrides`: deriving via can take
  both fields from another type, and they used to be recorded as `null`. The
  names themselves are matched as written at parse time; an imported pattern
  synonym that reuses `Just`, `Nothing` or a `TableType` constructor name is
  refused after type checking (`MODEL_META_FIELD_MISMATCH`, below).
* **Fix:** `modelTableType = Just CON :: Maybe TableType`, with the signature
  around the whole value, is read; it used to be recorded as `null`.
* **Breaking:** a punned field in a modifier's `tableModification { … }`
  (`{ fooName }`) is `UNRESOLVABLE_COLUMN_NAME`, and a modifier in
  `OverloadedRecordUpdate` syntax is `UNRECOGNIZED_MODIFIER_SHAPE`
  (`allowNonStandardModifier` still opts out of the second). Both used to be
  skipped silently, so the column kept Beam's default name. A
  `dbModification { … }` in `OverloadedRecordUpdate` syntax is not read as a
  rename either.
* **Breaking:** `modelTableName`, `modelFieldModification`, the top-level
  binding it names and `primaryKey` must each be written once, as one equation
  with no guards or `where` bindings. A `where` binding used to be ignored, so
  one that shadowed the modifier binding, `tableModification`, `fieldNamed` or a
  key field was read as the outer name; a pattern-bound
  `modelFieldModification` was read as no overrides; and of two bindings, the
  last was read. Such a definition is now unreadable: `UNRESOLVABLE_TABLE_NAME`
  (`tableNameOverrides` still applies), `UNRECOGNIZED_MODIFIER_SHAPE`,
  `UNRESOLVABLE_MODIFIER_BINDING` or `UNRESOLVABLE_PRIMARY_KEY`. An empty
  `where`, which binds nothing, is accepted, for `modelTableType` and
  `modelSchemaName` too.
* **Breaking:** each field `primaryKey` applies must be a column the plugin
  extracts for the table (`UNKNOWN_PRIMARY_KEY_FIELD`): a helper function,
  another module's name (`M.fooId`) or a field that `allowUnknownColumnWrapper`
  leaves out is refused. Before, the contract named such a key column
  (`pkColumns: [someHelper]`) and marked none of the table's columns as the key.
* **Fix:** a punned field of a pattern binding in an instance
  (`Mods{modelFieldModification} = mods`) binds its label, as the parser leaves
  that out of the binders it lists. Such a method is unreadable like any other
  pattern-bound one; before, it was read as absent, or the other binding of it
  was read. GHC rejects the module, but only after the plugin has run.
* **Breaking:** a module that defines, itself, a name the plugin reads a
  table through is a compile error (`REDEFINED_NAME`): `tableModification` or
  `fieldNamed`, a name `primaryKey` applies (`.`, `<$>`, `<*>`, `getField`, or a
  field name the module also binds another way), a pattern synonym named `Just`,
  `Nothing` or a `TableType` constructor, or a type, type synonym or family
  named `C`, `Columnar`, `Maybe` or `TableEntity`. They used to be read as
  Beam's or base's names: a `type C f a = Columnar f (Maybe a)` made a nullable
  column read as not nullable. An imported redefinition is not seen at parse
  time; after type checking, one of `Just`, `Nothing`, a `TableType`
  constructor, `C`, `Columnar` or `Maybe` that changes what was read is
  refused (below), while one of `tableModification`, `fieldNamed` or a name
  `primaryKey` applies is still not seen.
* **Breaking:** a module that defines a table or a `setEntityName` override is a
  compile error if it enables `RebindableSyntax`, `OverlappingInstances` or
  `IncoherentInstances` (`UNSUPPORTED_EXTENSION`), has a
  `RULES` pragma (`UNSUPPORTED_RULES`) or binds a record wildcard pattern at top
  level (`UNSUPPORTED_RECORD_WILDCARD`), with no opt-out; other modules are
  unaffected. These used to be read as written: under `RebindableSyntax`,
  `x.f` and string literals call the module's own `getField` and `fromString`;
  under the other two, an instance overlaps a library's without a pragma, so one
  at an `EntityModification` type, also through a type synonym, changes how a
  `<>` chain combines the renames unseen; a
  rule can rewrite `fieldNamed` and GHC applies it when it optimises; and a
  record wildcard stands for names the parser does not show.
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
    itself; an instance method counts only in an instance whose type names
    `EntityModification` or `DatabaseEntity`, or that has an overlap pragma,
    and a derived `Semigroup` or `Monoid` instance under that rule, standalone
    or in a deriving clause, binds its class's methods;
  - a module that declares a type named `TableEntity`.

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
  one field, written as one equation with no arguments, guards or `where`
  bindings and bound nowhere else in the module, with no record wildcard
  `{..}` covering the use, in a module that binds none of `modifyTableFields`,
  `tableModification`, `fieldNamed` and `(<>)` itself; if no column is
  renamed, drop `modifyTableFields`.
* **Breaking:** after GHC type checks a module, the plugin cross-checks what it
  read at parse time against GHC's resolution, in every build that type checks
  (`-fno-code`, haddock and HLS included; blacklisted modules, `hs-boot` and
  `hsig` files excepted), with no opt-out. Each error is located at the
  instance, the table's declaration or the column's field where GHC places it
  in the module. These compiled before and are now compile errors:
  - a `Table`, `ModelMeta` or `Beamable` instance the parse step did not see,
    such as one Template Haskell generates, for a type it did not extract, or
    for a type the module does not declare, such as an imported one named like
    a table of the module (`UNEXTRACTED_TABLE`; one whose head has no type
    constructor is `UNREADABLE_INSTANCE_HEAD`; a key's instance is matched as
    beam-core's `PrimaryKey` family, by package and module): the table was
    left out of the contract;
  - such an instance of a same-named class that is not beam-core's or
    sequelize's, by package and module (`UNEXPECTED_CLASS`): it was read as the
    real one;
  - a `modelTableType` or `modelSchemaName` whose `Just`, `Nothing` or
    `TableType` constructor is an imported pattern synonym or another module's
    name (`MODEL_META_FIELD_MISMATCH`): the contract recorded the name written,
    not the value the code routes by;
  - a column whose type, with type synonyms expanded, has a base `Maybe` where
    the plugin read none or none where it read one, or that is not beam-core's
    `Columnar` (`COLUMN_TYPE_MISMATCH`): `type Opt = Maybe` made a nullable
    column read as not nullable. A large-records table's field is checked at
    the type its equality constraint fixes;
  - a table's `{-# ANN type T largeRecord #-}` whose `largeRecord` is not the
    large-records package's, or not of that package's options type
    (`UNEXTRACTED_TABLE`);
  - a table whose GHC.Generics `Generic` instance, looked up by class among
    every instance GHC sees (imported and orphan ones included), is missing,
    not the only one that matches or could match, or has a representation
    whose selectors are not all the record's fields in order, or, for a
    large-records table (representation large-generics' `ThroughLRGenerics`),
    whose large-generics `MetadataOf` is missing or does not name them in
    order; or whose staged columns are not all those fields in order (with
    `allowUnknownColumnWrapper`, those it does not leave out)
    (`GENERIC_FIELD_MISMATCH`):
    Beam names the columns after those selectors or that metadata, so a
    Template Haskell or orphan instance, or one behind a module class named
    `Generic`, could rename a column the contract records under its field
    name;
  - in a module that uses `setEntityName` or `modifyTableFields`, a
    `Semigroup` or `Monoid` instance other than beam-core's that could apply at
    the `EntityModification` of a table the module can rename (a large-records
    database record's tables included), from any module GHC sees
    (`UNEXPECTED_INSTANCE`): it could drop or change a rename.

  A staged file missing at that point is `STAGED_FRAGMENT_MISSING`, one it
  cannot read is `FRAGMENT_IO_ERROR`, and one that does not decode as a
  fragment is `STAGED_FRAGMENT_UNREADABLE`. What the check cannot see (a
  `Generic` instance whose `to`/`from` permute the fields; an overlapping
  `Generic` or beam-core generic-plumbing instance imported only where the
  database is built), and its large-records assumptions, are in the README.
  The check reads the renamed source, which the plugin now asks GHC to keep;
  on GHC 9.8 a build without `-haddock` then writes the module's
  documentation structure into its `.hi` file.
* The `UNRESOLVABLE_PRIMARY_KEY` message lists all seven accepted shapes. The
  `UNRESOLVABLE_COLUMN_NAME` message no longer speaks of a right-hand side for a
  punned field, and the `UNRESOLVABLE_TABLE_NAME` fix names `tableNameOverrides`
  and fits a literal that is where-bound or written twice.
* **Breaking:** a `blacklistModules` entry that doesn't end in `.` matches only
  the module of exactly that name: `App.Db` no longer also skips `App.DbUtil`
  or `App.Db.Pool`. An entry ending in `.` (e.g. `Test.`) still matches every
  module it prefixes, so `App.Db.` skips `App.Db.Pool` but not `App.Db`. An
  empty entry, which used to skip every module, and an entry starting with `.`
  are now invalid options (`INVALID_PLUGIN_OPTIONS`).
* **Breaking:** an empty `fragmentsDir`, one that normalises to `.` (e.g.
  `./`), one with a `..` component, and one that resolves to the working
  directory or a directory above it (an absolute path, or through a symbolic
  link; checked before the first write) are invalid options
  (`INVALID_PLUGIN_OPTIONS`). The first two meant the working
  directory, where deleting a table-less module's stale `<Module>.yaml` could
  delete an unrelated file: on a case-insensitive file system, compiling
  `module Stack` deleted `stack.yaml`.
* **Fix:** plugin error messages escape every control, format and line- or
  paragraph-separator character, newlines included, in each value they quote
  (source text such as a quasi-quote, an option value, a path), and each
  backslash, so a value's own `\n` text reads apart from an escaped newline;
  they cut each such value at 500 characters, and are capped at 4000
  characters, so source text can no longer add lines or terminal escapes to a
  build log.
* **Fix:** a compile that fails after parsing (a type error, or a warning made
  an error by `-Werror`) no longer publishes the module's fragment. The plugin
  stages the fragment as `<Module>.yaml.pending` when it parses the module, and
  a pass at the end of the Core pipeline, which GHC reaches only after type
  checking, desugaring and the `-Werror` check, publishes it. Before, the
  fragment was written at parse time; when the failed edit was then reverted,
  GHC 9.8, which recompiles by source hash, did not recompile the module, and
  the contract kept the edit. A staged file missing at publish time is the new
  compile error `STAGED_FRAGMENT_MISSING`, and a failed file operation of the
  plugin (a `fragmentsDir` that is a file or is not writable, or that lies
  behind a chain of symbolic links past the system's limit) is the new
  `FRAGMENT_IO_ERROR`, naming the path and the module; it used to fail the
  compile with an uncoded I/O error.
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
* **Fix:** when a recompiled module no longer defines a table or a
  `setEntityName` override, or is newly listed in `blacklistModules`, its old
  fragment is deleted. Before, a restored build directory kept that fragment,
  so the contract still listed a table the module no longer defines or the
  options now exclude. A module's `.hs-boot` or `.hsig` file is skipped
  entirely: it is not checked and never touches the module's fragment.
* `sql-schema-merge` writes a top-level `generator: {name, version, rev}`
  stamp into the contract so consumers can tell which plugin version produced
  it. `rev` comes from the new `--generator-rev REV` flag and is `null` when
  the flag is omitted. Contracts without the key (written by 0.4.2 and
  earlier) still decode, including as `--include-merged` inputs; the stamps of
  included contracts are ignored.
* **Breaking:** `sql-schema-merge` fails instead of dropping tables. Each of
  these used to be skipped with at most a warning, and the merge still exited
  0:
  - a fragment that doesn't parse, unless `--source-dirs` shows its module is
    gone;
  - a missing or malformed `--include-merged` file;
  - a fragments or source directory it can't read or search, or an entry
    inside a source directory that it can't inspect (a symlink into a
    directory it can't search, say);
  - `--source-dirs` that prune every fragment.

  Each pruned module is now named on stderr, in quotes.
* **Breaking:** a `--source-dirs` entry that doesn't exist, or isn't a
  directory, fails the merge when a fragment is pruned. Before, it only warned
  (an existing file as "does not exist"), so a mistyped entry pruned the
  fragments of the modules that live there, and the merge exited 0 without their
  tables. The error names the entries and the pruned modules; fix the path, or
  delete the stale fragments. With nothing pruned such an entry still only
  warns. A `--fragments` path that exists but isn't a directory fails too: it
  used to read as an empty directory, which `--allow-empty` or `--no-own-tables`
  then accepted. A `--fragments` path that doesn't exist, or is a dangling
  symlink, still reads as no fragments, now with a warning. A symlink back to a
  directory on the walk's path from the entry is skipped, with a warning.
  Library: `liveModulesFromDirs` returns these entries (`SourceDirProblem`) with
  the modules instead of warning about them, and prints the warnings of its own
  walk itself; `loadFragments` throws on a path that isn't a directory. New
  exports: `PathKind(..)`, `pathKind` and `SourceDirProblem(..)`.
* **Fix:** a `--source-dirs` entry with a trailing slash (`src/`) shifted every
  module name it produced, so all of its fragments were pruned as stale.
* **Fix:** the walk of a `--source-dirs` entry no longer follows links round in
  circles. Two symlinks back to their own directory (`src/a -> .`, `src/b -> .`)
  made it follow every sequence of them until the system's symlink limit, about
  2^32 directory listings, and invented live module names through them
  (`a.Lib.B`), which could keep the fragment of a deleted module. A symlink back
  to a directory on the walk's path from the entry is now skipped, with a
  warning naming it, and a merge that skips one and prunes a fragment exits 1,
  naming both: a file under the link that declares its module by the name
  through it has no other name in the walk. Every other symlink is followed
  under its own name, an alias included, so `src/Compat -> Legacy/Compat` keeps
  `Compat.X` live, and `Legacy.Compat.X` too (over-inclusive, which is safe). A
  chain of symlinks past the system's limit is followed one link at a time,
  since that error says nothing about a loop: a chain that ends at a directory
  is an alias, one that leads back to itself is skipped with a warning, and one
  of more than 1000 links, or an unreadable link, exits 1. A walk takes at most
  200000 steps per entry, past which the merge exits 1: each directory entry
  listed is one step, and each link followed (to a directory or a file, a
  dangling one too), or hop of a chain, one step per path component its
  resolution walks (the link's path, then its target's), so the cap bounds the
  file-system work of links fanned out deep in a tree too. A tree with no
  links takes one step per entry; a directory is read whole before its entries
  are counted. The cap is per `--source-dirs` entry, so one merge takes at most
  that many steps times the number of entries, which the caller chooses. A
  walk descends at most 256 directory levels below its entry, a link to a
  directory counting as a level, and exits 1 past that. Measured on one
  machine: a chain of 10000 directories, each linking to the next, took 80 s
  and now stops at the depth limit in 0.1 s; 150000 links to one directory,
  250 levels down, took 245 s and now stop at the cap in 1.5 s; 190000 links
  to one file at that depth took 26 s (a link to a file was charged one step)
  and now stop at the cap in 1.4 s. The warnings of a walk are written 1000
  lines at a time, also when the walk fails: stderr is unbuffered, so
  thousands of them took over a minute.
  Library: `liveModulesFromDirs` takes the entry cap as its first argument and
  returns each skipped link as a `SourceDirLinkSkipped`; new `walkEntryCap`,
  `maxWalkDepth` and `maxLinkHops`, and `MergeOptions` gains `moWalkEntryCap`
  (`walkEntryCap` in `defaultMergeOptions`). The merge's own walk errors print
  as written, each path in them shown escaped, not as `user error (…)`.
* **Fix:** modules in `.lhs`, `.hsc`, `.chs`, `.x`, `.y` and `.ly` files count as
  live, so their fragments are no longer pruned as stale.
* **Breaking:** the CLI refuses a contract with none of the package's own
  fragments, as it already refused one with no tables: included tables used
  to hide that the package's plugin never ran. A package that defines no
  tables of its own and only composes `--include-merged` contracts passes the
  new `--no-own-tables` instead of `--allow-empty`; the merge then fails if
  its own fragments do define a table. `--allow-empty` permits only a contract
  with no tables.
* **Breaking:** a merge the CLI refuses no longer writes `--out`. Before, the
  contract was written first and the refusal (0 tables, no own fragments,
  `--no-own-tables` with own tables) came after, so a refused merge replaced the
  previous contract with the one it then rejected. A refusal now states the
  fragment, pruned and included counts instead of the summary line.
* **Fix:** the contract is written to a new temporary file next to `--out`
  (`<out>`, a unique suffix, `.tmp`), created exclusively with the default
  permissions, and renamed over `--out`. Before, it went to the fixed name
  `<out>.tmp`: a file already there was overwritten, a symlink there was written
  through to its target, and two merges to the same `--out` shared one file. A
  failed write now removes only the temporary file it created.
* **Breaking:** argument errors (exit 2) are stricter. Each of these used to be
  accepted: a repeated `--fragments`, `--out` or `--generator-rev` (the last one
  won silently), an empty value (an empty `--fragments` read as a missing
  directory, which `--no-own-tables` then accepted), and a value that starts
  with `--` (an unset, unquoted variable let the next option become the value:
  `--generator-rev --no-own-tables` recorded `--no-own-tables` as the revision).
  A flag without its value is now reported as that, not as an unrecognised
  argument.
* **Breaking:** the merge fails on three more inputs, each with an opt-out flag
  that turns its error into a warning. Before, the second was only a warning and
  the other two passed silently:
  - an `--include-merged` contract with no tables (`--allow-empty-include`): the
    plugin most likely did not run when that package was built;
  - a `setEntityName` override that matches no table in any fragment or included
    contract (`--allow-unmatched-override`): the table is missing from the
    contract, and the override could not be checked against its `modelTableName`;
  - a table with no columns, own or included, counted once after duplicates are
    collapsed (`--allow-columnless-table`): the plugin read none of its fields,
    for example because another plugin rewrote the record first and
    `allowUnknownColumnWrapper` dropped them.

  An opt-out relaxes only its own check. The flags are not recorded in the
  contract, so they are not inherited: a dependency's contract is checked again,
  and one built with `--allow-columnless-table` or `--allow-unmatched-override`
  needs the same flag in the merge that includes it, which then also covers that
  package's own tables and overrides. An empty include is an input error, and
  input errors stop the merge before the table checks, whose errors show once
  it is fixed. Library: `MergeOptions` gains
  `moAllowEmptyInclude`, `moAllowUnmatchedOverride` and `moAllowColumnlessTable`,
  and `loadIncludedMerged` returns the included files with no tables (overrides
  or not) as a fifth component.
* **Breaking:** one override key (module, field, table type) with two different
  SQL names fails the merge, whatever the flags. Before, under
  `--allow-unmatched-override` the entries collapsed and one of them silently
  left the contract, so a merge that later included it could not check it
  against the table. Entries with one name still collapse.
* **Fix:** every name a merge message prints from a fragment, an included
  contract or a directory (module, table, override and source names, file
  names, YAML and I/O error text) goes through `show`, and so does every path
  the command line names (`--fragments`, `--out`, `--source-dirs` and
  `--include-merged` entries, in the warnings, the errors, the refusal and the
  summary line), and every argument the command line refuses. Before, a name
  holding a newline and an escape character could forge a `sql-schema-merge:`
  line. The warning for two Haskell types with one SQL name now quotes both.
* **Breaking (library):** `MergeOptions` gains `moAllowEmpty` and
  `moNoOwnTables`, and `runMerge` applies `publishProblem` itself, returning
  `Left` before it writes. New: `defaultMergeOptions`, `refusalLines` and
  `SqlSchema.MergeCli` (`mergeMain`, `parseArgs`, `usage`), the CLI as a
  function from arguments to an exit code.

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
