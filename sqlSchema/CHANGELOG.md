# sql-schema

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
