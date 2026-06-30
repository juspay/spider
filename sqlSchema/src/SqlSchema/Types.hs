{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data types and error categories for the sql-schema plugin.
--
-- Every export here is part of the on-disk contract: it ends up either in
-- per-module fragment YAMLs or in the merged final YAML, so changes to
-- constructor names, field names, or default ordering will break consumers
-- that read those files.
module SqlSchema.Types
  ( -- * On-disk contract
    TableSchema(..)
  , qualifiedType
  , ColumnInfo(..)
  , PrimaryKeyInfo(..)
  , ModelSchemaName(..)
  , DbEntityOverride(..)
  , Fragment(..)
  , MergedSchema(..)
    -- * Plugin CLI options
  , CliOptions(..)
  , defaultCliOptions
    -- * Extraction errors
  , SqlSchemaError(..)
  , generateErrorMessage
  ) where

import           Data.Aeson   (FromJSON (..), ToJSON, (.!=), (.:?), withObject)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

-- | One @TableSchema@ per Beam table.  Keyed in the merged YAML by its
-- fully-qualified type ('qualifiedType' = @sourceModule '.' codeName@) so
-- renaming a SQL table shows up as a 'modelTableName' diff rather than as a
-- table-drop + table-add.
data TableSchema = TableSchema
  { codeName       :: String
    -- ^ Code-side identity: the Beam table's Haskell type constructor, e.g.
    --   @WidgetT@.  With 'sourceModule' this gives the fully-qualified
    --   type ('qualifiedType'), which is the merge identity key.
  , sourceModule   :: String
    -- ^ Module that declared the table.  @sourceModule '.' codeName@ is the
    --   fully-qualified type, so two fragments with matching
    --   ('sourceModule','codeName') are equal iff every other field is equal
    --   too — the merger relies on this for safe dedup of transitively-
    --   included contracts.
  , modelTableName :: String
    -- ^ Value of @modelTableName@ (the ModelMeta SQL name), e.g.
    --   @"widget"@.  Must be a string literal in the source (or supplied
    --   via @tableNameOverrides@).  This is the name validation matches the
    --   live DB against; a @setEntityName@ override, when present, supersedes
    --   it (see 'DbEntityOverride').
  , modelTableType :: Maybe String
    -- ^ Value of @modelTableType@ if present, e.g. @"TRACKER"@.
  , modelSchemaName :: Maybe ModelSchemaName
    -- ^ Source-level form of the table's @modelSchemaName@ ModelMeta
    --   binding when it is @Just <something>@.  @Nothing@ here covers
    --   both "field not declared" and "@modelSchemaName = Nothing@" —
    --   downstream validation treats both the same (no schema → MySQL).
    --
    --   The two recognised RHS forms map to distinct constructors so the
    --   validator can split PG tables (which need a PG @information_schema@
    --   lookup) from MySQL ones at YAML-read time:
    --
    --       modelSchemaName = Just "public"
    --         → 'SchemaLiteral' "public"
    --       modelSchemaName = Just Config.getAppDbSchema
    --         → 'SchemaConfig' "Config.getAppDbSchema"
  , primaryKey     :: PrimaryKeyInfo
  , columns        :: [ColumnInfo]
    -- ^ Columns in source order from the record definition.
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Fully-qualified Haskell type, reconstructed from the module and the
-- code-side type name.  Replaces the former standalone @haskellType@ field
-- (which was exactly @sourceModule <> "." <> codeName@), so the redundant
-- string no longer rides in every fragment; the merge identity key uses it.
qualifiedType :: TableSchema -> String
qualifiedType t = sourceModule t <> "." <> codeName t

data ColumnInfo = ColumnInfo
  { hsField      :: String
  , column       :: String
    -- ^ Database column name.  Defaults to 'hsField'; overridden if the
    --   table's @xxxTMod@ has @<hsField> = fieldNamed \"…\"@.
  , hsType       :: String
  , nullable     :: Bool
  , isPrimaryKey :: Bool
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PrimaryKeyInfo = PrimaryKeyInfo
  { pkConstructor :: String
  , pkColumns     :: [String]
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Captures the two source-level shapes of @modelSchemaName = Just _@
-- that the codebase uses today.  The qualifier on a 'SchemaConfig' value
-- is preserved verbatim from the source (e.g. @"Config.getAppDbSchema"@,
-- @"C.getAppDbSchema"@) so the validator can recognise the PG dynamic-
-- schema marker without having to resolve imports.
--
-- A 'SchemaLiteral' carries the unquoted SQL schema text (e.g. @"public"@).
-- Forms other than @Just "lit"@ or @Just <var>@ are not currently
-- extracted — they become 'Nothing' on the table.
data ModelSchemaName
  = SchemaLiteral String
  | SchemaConfig   String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | A @setEntityName@-style override discovered in a @withDbModification@
-- block on a @defaultDbSettings@ binding.  At Beam runtime this overrides
-- whatever @modelTableName@ says, so we extract them and have the merge
-- CLI cross-check that the two agree.
data DbEntityOverride = DbEntityOverride
  { dbField      :: String
    -- ^ The DB-record field name being overridden, e.g. @\"gadgets\"@.
  , dbTableType  :: String
    -- ^ The unqualified table-type constructor name, e.g. @\"GadgetT\"@,
    --   read from the DB-record field's @f (TableEntity GadgetT)@ type.
  , sqlName      :: String
    -- ^ The literal passed to @setEntityName@.
  , overrideSourceModule :: String
  , overrideSourceFile   :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | A single module's contribution.  Each module that the plugin processes
-- writes exactly one fragment file under the configured fragments
-- directory; the merge CLI combines all fragments into the final YAML.
--
-- Staleness is detected by the merger walking @--source-dirs@: any
-- fragment whose @fragmentModule@ is not backed by an @.hs@ file in any
-- listed source dir is pruned.  No path lives on the fragment itself,
-- which is what lets fragments from different repositories (e.g. a
-- pinned dependency's contract) compose into a single merged YAML without
-- the per-package CWD juggling that file-relative paths would require.
data Fragment = Fragment
  { fragmentModule   :: String
  , fragmentTables   :: [TableSchema]
  , fragmentDbEntityOverrides :: [DbEntityOverride]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Final merged on-disk contract.  This is what downstream prod-DB
-- validation reads.
data MergedSchema = MergedSchema
  { mergedTables             :: [TableSchema]
    -- ^ Sorted by 'haskellType' for stable diffs.
  , mergedDbEntityOverrides  :: [DbEntityOverride]
    -- ^ All overrides found; the merge CLI has already cross-checked them
    --   against each table's 'tableName' before emitting.  Kept here for
    --   downstream auditability.
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Parsed from the JSON plugin option string.  All fields are optional
-- in the on-disk JSON; missing ones use the defaults from
-- 'defaultCliOptions'.  Defaulting matters because the cabal stanza
-- usually only sets the path-y bits and relies on safety defaults.
data CliOptions = CliOptions
  { fragmentsDir              :: FilePath
    -- ^ Directory where per-module fragment YAMLs are written.
    --   Default: @./.juspay/sql-schema-fragments@.
  , blacklistModules          :: [String]
    -- ^ Modules to skip entirely.  Each entry matches as a prefix of the
    --   module name (e.g. @\"Test.\"@ matches @\"Test.KV.…\"@).
  , allowUnknownColumnWrapper :: Bool
    -- ^ If True, fields whose type isn't @Columnar f T@ / @C f T@ produce
    --   a stderr warning instead of a hard error.  Default False.
  , allowNonStandardModifier  :: Bool
    -- ^ If True, @xxxTMod@ bindings that aren't
    --   @tableModification { … }@ produce a stderr warning instead of an
    --   error.  Default False.
  , tableNameOverrides        :: Map String String
    -- ^ Per-type-name override for @modelTableName@.  Applied ONLY when
    --   the table's @modelTableName@ is not a static string literal (so
    --   the extractor would otherwise emit UNRESOLVABLE_TABLE_NAME).
    --   Keyed by unqualified type name, e.g. @\"SettingsT\"@.
    --   Acts as an explicit, reviewed escape hatch for cases where the
    --   source uses a value reference like
    --   @modelTableName = SomeConst.theName@ — the engineer asserts the
    --   SQL name in the plugin config rather than the plugin guessing.
  } deriving (Show, Eq, Generic, ToJSON)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
  { fragmentsDir              = "./.juspay/sql-schema-fragments"
  , blacklistModules          = []
  , allowUnknownColumnWrapper = False
  , allowNonStandardModifier  = False
  , tableNameOverrides        = Map.empty
  }

instance FromJSON CliOptions where
  parseJSON = withObject "CliOptions" $ \o -> CliOptions
    <$> o .:? "fragmentsDir"              .!= fragmentsDir defaultCliOptions
    <*> o .:? "blacklistModules"          .!= blacklistModules defaultCliOptions
    <*> o .:? "allowUnknownColumnWrapper" .!= allowUnknownColumnWrapper defaultCliOptions
    <*> o .:? "allowNonStandardModifier"  .!= allowNonStandardModifier defaultCliOptions
    <*> o .:? "tableNameOverrides"        .!= tableNameOverrides defaultCliOptions

-- | Errors the extractor can hit on a table that genuinely looks like a
-- Beam table.  These are hard errors because the YAML would otherwise be
-- silently incomplete or wrong on data the prod schema depends on.
data SqlSchemaError
  = -- | @modelTableName@ was not a string literal.
    UNRESOLVABLE_TABLE_NAME String
  | -- | RHS of @<field> = …@ inside the modifier was not @fieldNamed "lit"@.
    UNRESOLVABLE_COLUMN_NAME String String
  | -- | @primaryKey@ wasn't in a shape the extractor recognises.
    UNRESOLVABLE_PRIMARY_KEY String
  | -- | A field's type isn't @Columnar f T@ / @C f T@.  Silently dropping
    --   the table would be a false-pass in prod validation; silently
    --   dropping the field would be a wrong column count.
    UNRECOGNIZED_FIELD_WRAPPER String String String
    --                          typeName  field   pretty-printed type
  | -- | The @xxxTMod@ binding's RHS isn't @tableModification { … }@.
    --   Silently treating it as "no overrides" would emit Haskell field
    --   names as DB column names — wrong YAML, no signal.
    UNRECOGNIZED_MODIFIER_SHAPE String String
    --                          typeName  modifier-binding-name
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Human-readable rendering for a compile error.  Always includes the
-- fragments directory so the engineer knows what is being generated.
generateErrorMessage :: FilePath -> SqlSchemaError -> String
generateErrorMessage fragDir = \case
  UNRESOLVABLE_TABLE_NAME t ->
    "modelTableName for '" <> t <> "' is not a string literal — the sql-schema plugin cannot statically extract it.\n" <>
    "  Replace the computed value with a literal string.\n" <>
    "  (Fragments dir: " <> fragDir <> ")"
  UNRESOLVABLE_COLUMN_NAME t f ->
    "The RHS of '" <> f <> "' inside the *TMod for '" <> t <> "' is not 'fieldNamed \"…\"' with a literal.\n" <>
    "  Either rewrite it as 'fieldNamed \"explicit_name\"' or drop the override so the column defaults to '" <> f <> "'."
  UNRESOLVABLE_PRIMARY_KEY t ->
    "primaryKey for '" <> t <> "' is not in a recognised shape.\n" <>
    "  Accepted forms:\n" <>
    "    primaryKey x = Con (f1 x) (f2 x) …      (function equation, any arity)\n" <>
    "    primaryKey   = \\x -> Con (f1 x) …       (explicit lambda)\n" <>
    "    primaryKey   = Con . field               (single-column composition)\n" <>
    "    primaryKey   = Con <$> f1 <*> f2 <*> …   (applicative composite)\n" <>
    "    primaryKey _ = ConNullary                (no-column PK)"
  UNRECOGNIZED_FIELD_WRAPPER t f ty ->
    "Field '" <> f <> "' on table '" <> t <> "' has type '" <> ty <> "', which the plugin does not recognise as a Beam column wrapper.\n" <>
    "  Expected 'Columnar f T' or 'C f T'.\n" <>
    "  Silently dropping this table from the YAML would cause a downstream false-pass against the prod DB; refusing to extract instead.\n" <>
    "  Either rewrite the field to use Columnar, or pass --allowUnknownColumnWrapper to the plugin to opt out per-build."
  UNRECOGNIZED_MODIFIER_SHAPE t binding ->
    "Modifier binding '" <> binding <> "' for table '" <> t <> "' is not 'tableModification { … }'.\n" <>
    "  The plugin cannot statically read column-name overrides from any other shape.\n" <>
    "  Silently defaulting to Haskell field names would produce a wrong YAML; refusing to extract instead.\n" <>
    "  Either rewrite the binding, or pass --allowNonStandardModifier to opt out per-build."
