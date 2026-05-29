# sql-schema

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
