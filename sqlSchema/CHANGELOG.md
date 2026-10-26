# sql-schema

## 0.4.1

* Builds on GHC 9.8 as well as 9.2: CPP shims cover the GHC 9.4+ parser AST
  (token fields, `HsFieldBind`, `DotFieldOcc`, `DataDefnCons`, `LHsRecUpdFields`)
  and the 9.4+ diagnostics API. Extraction output is unchanged.

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
