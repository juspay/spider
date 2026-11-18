{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | Data types and error categories for the sql-schema plugin.
--
-- Every export under "On-disk contract" ends up in per-module fragment YAMLs
-- or in the merged final YAML, so changes to constructor names, field names,
-- or default ordering will break consumers that read those files.
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
  , Generator(..)
    -- * Column naming
  , beamDefaultColumnName
    -- * Extraction rules
  , tableTypeConstructors
    -- * Plugin CLI options
  , CliOptions(..)
  , defaultCliOptions
  , isBlacklistedModule
    -- * Extraction errors
  , SqlSchemaError(..)
  , errorCode
  , generateErrorMessage
  , reasonNoDeclaration
  , reasonNotARecord
  , reasonNoBeamable
  , reasonDatabaseRecord
  , reasonGenericNotStock
    -- * Message backstop, exported for the test-suite
  , escapeUnsafe
  , capMessage
  ) where

import           Control.Monad (unless, when)
import           Data.Aeson   (FromJSON (..), ToJSON (..), Value, (.!=), (.:?), withObject,
                               Options, defaultOptions, fieldLabelModifier, genericParseJSON,
                               genericToJSON)
import           Data.Aeson.Types (Parser)
import           Data.Char    (GeneralCategory (Control, Format, LineSeparator, ParagraphSeparator),
                               generalCategory, isSpace, isUpper, showLitChar, toLower)
import           Data.List    (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text    as T
import           GHC.Generics (C1, Constructor (conName), D1, Generic (from), M1 (..), (:+:) (..))
import           System.FilePath (normalise)

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
    -- ^ The constructor in @modelTableType = Just CON@, unqualified, e.g.
    --   @"TRACKER"@ for @Just Se.TRACKER@.
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
    -- ^ Database column name.  Falls back to Beam's @defaultDbSettings@ name
    --   unless the table's @xxxTMod@ has @<hsField> = fieldNamed \"…\"@.
  , hsType       :: String
  , nullable     :: Bool
  , isPrimaryKey :: Bool
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Port of beam-core's @unCamelCaseSel@ (Database/Beam/Schema/Tables.hs, juspay/beam 25f1bcd960).
beamDefaultColumnName :: String -> String
beamDefaultColumnName original =
  let symbolLeft = dropWhile (== '_') original
  in if null symbolLeft then original
     else if '_' `elem` symbolLeft then symbolLeft
     else case unCamelCase symbolLeft of
            []     -> symbolLeft
            [xs]   -> xs
            _ : xs -> intercalate "_" xs
  where
    unCamelCase "" = []
    unCamelCase s
      | (comp, next) <- break isUpper s, not (null comp) = lowerComp comp : unCamelCase (lowerHead next)
      | otherwise = let (comp, next) = span isUpper s in lowerComp comp : unCamelCase (lowerHead next)
    -- Via Text on purpose: beam's T.toLower differs from Data.Char.toLower on U+0130.
    lowerComp = T.unpack . T.toLower . T.pack
    lowerHead (c : cs) = toLower c : cs
    lowerHead []       = []

data PrimaryKeyInfo = PrimaryKeyInfo
  { pkConstructor :: String
  , pkColumns     :: [String]
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | The two shapes of @modelSchemaName = Just _@ the plugin reads:
-- @Just "lit"@ and @Just <value name>@.  The qualifier on a 'SchemaConfig'
-- value is preserved verbatim from the source (e.g.
-- @"Config.getAppDbSchema"@, @"C.getAppDbSchema"@) so the validator can
-- recognise the PG dynamic-schema marker without having to resolve imports.
--
-- A 'SchemaLiteral' carries the unquoted SQL schema text (e.g. @"public"@).
-- Any other shape except @Nothing@ is a compile error
-- ('UNRESOLVABLE_MODEL_META_FIELD'), never 'Nothing' on the table.
data ModelSchemaName
  = SchemaLiteral String
  | SchemaConfig   String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | The five constructors of the ModelMeta library's @TableType@.
tableTypeConstructors :: [String]
tableTypeConstructors = ["DOMAIN", "CONFIG", "COMMON_CONFIG", "TRACKER", "COMMON_TRACKER"]

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
  , generator                :: Maybe Generator
    -- ^ Absent in contracts written before 0.4.3.
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Which plugin build wrote a contract.  Fields are prefixed because
-- @version@ would clash with @Paths_sql_schema.version@.
data Generator = Generator
  { generatorName    :: String
  , generatorVersion :: String
  , generatorRev     :: Maybe String
  } deriving (Show, Eq, Generic)

generatorOptions :: Options
generatorOptions = defaultOptions { fieldLabelModifier = lowerFirst . drop (length ("generator" :: String)) }
  where
    lowerFirst (c:cs) = toLower c : cs
    lowerFirst []     = []

instance ToJSON Generator where
  toJSON = genericToJSON generatorOptions

instance FromJSON Generator where
  parseJSON = genericParseJSON generatorOptions

-- | Parsed from the JSON plugin option string.  All fields are optional
-- in the on-disk JSON; missing ones use the defaults from
-- 'defaultCliOptions'.  Defaulting matters because the cabal stanza
-- usually only sets the path-y bits and relies on safety defaults.
data CliOptions = CliOptions
  { fragmentsDir              :: FilePath
    -- ^ Directory where per-module fragment YAMLs are written.
    --   Default: @./.juspay/sql-schema-fragments@.  For incremental builds
    --   that restore the build directory, put it inside that directory (e.g.
    --   @dist/build/sql-schema-fragments@); otherwise the fragments of
    --   modules that are not recompiled are lost.  It must not be empty or
    --   the working directory (@.@, @./@, …): an invalid option.
  , blacklistModules          :: [String]
    -- ^ Modules to skip entirely.  An entry ending in @.@ matches every module
    --   it prefixes (e.g. @\"Test.\"@ matches @\"Test.KV.…\"@); any other entry
    --   matches only the module of exactly that name.  An empty entry, or one
    --   starting with @.@, is an invalid option.
  , allowUnknownColumnWrapper :: Bool
    -- ^ If True, fields whose type isn't @Columnar f T@ / @C f T@ are
    --   silently left out of the table's columns instead of being a hard
    --   error.  Default False.
  , allowNonStandardModifier  :: Bool
    -- ^ If True, a modifier the plugin cannot read is silently treated as
    --   "no overrides" (Beam default names) instead of an error: one that
    --   isn't @tableModification { … }@ (guarded, repeated or where-bound
    --   definitions and OverloadedRecordUpdate syntax included), or a binding
    --   that isn't a plain top-level @x = …@ of this module, bound once, with
    --   one unguarded right-hand side and no where bindings (imported,
    --   qualified, or bound by a pattern).  Default False.
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

-- | No entry skips a module it doesn't name exactly or as a prefix ending in
-- @.@: skipping is silent, so a near miss must not widen it.
isBlacklistedModule :: CliOptions -> String -> Bool
isBlacklistedModule cli modName = any covers (blacklistModules cli)
  where
    covers entry
      | "." `isSuffixOf` entry = entry `isPrefixOf` modName
      | otherwise              = modName == entry

instance FromJSON CliOptions where
  parseJSON v = do
    keys <- Map.keys <$> (parseJSON v :: Parser (Map T.Text Value))
    let unknown = filter (`notElem` knownKeys) keys
    unless (null unknown) $
      fail ("unknown plugin option key(s): " <> intercalate ", " (map (show . T.unpack) unknown)
            <> "; known keys: " <> intercalate ", " (map T.unpack knownKeys))
    opts <- parseKnown v
    when (workingDir (fragmentsDir opts)) $
      fail ("invalid fragmentsDir " <> show (fragmentsDir opts) <> ": it is the working directory,"
            <> " where deleting a module's stale <Module>.yaml can delete an unrelated file;"
            <> " use a directory of its own, e.g. dist/build/sql-schema-fragments")
    case filter invalidEntry (blacklistModules opts) of
      []  -> pure opts
      bad -> fail ("invalid blacklistModules entries " <> intercalate ", " (map show bad)
                   <> ": an empty entry or one starting with '.' names no module; use a module"
                   <> " name, or a prefix ending in '.' for the modules under it")
    where
    invalidEntry e = null e || "." `isPrefixOf` e
    workingDir dir = null dir || normalise dir `elem` [".", "./"]
    knownKeys =
      [ "fragmentsDir", "blacklistModules", "allowUnknownColumnWrapper"
      , "allowNonStandardModifier", "tableNameOverrides" ]
    parseKnown = withObject "CliOptions" $ \o -> CliOptions
      <$> o .:? "fragmentsDir"              .!= fragmentsDir defaultCliOptions
      <*> o .:? "blacklistModules"          .!= blacklistModules defaultCliOptions
      <*> o .:? "allowUnknownColumnWrapper" .!= allowUnknownColumnWrapper defaultCliOptions
      <*> o .:? "allowNonStandardModifier"  .!= allowNonStandardModifier defaultCliOptions
      <*> o .:? "tableNameOverrides"        .!= tableNameOverrides defaultCliOptions

-- | Errors the extractor can hit on a table that genuinely looks like a
-- Beam table.  These are hard errors because the YAML would otherwise be
-- silently incomplete or wrong on data the prod schema depends on.
data SqlSchemaError
  = -- | @modelTableName@ was not a string literal in one plain equation.
    UNRESOLVABLE_TABLE_NAME String
  | -- | RHS of @<field> = …@ inside the modifier was not @fieldNamed "lit"@.
    UNRESOLVABLE_COLUMN_NAME String String
  | -- | @primaryKey@ was missing or not in a shape the extractor reads.
    UNRESOLVABLE_PRIMARY_KEY String
  | -- | A field's type isn't @Columnar f T@ / @C f T@.  Silently dropping
    --   the table would be a false-pass in prod validation; silently
    --   dropping the field would be a wrong column count.
    UNRECOGNIZED_FIELD_WRAPPER String String String
    --                          typeName  field   pretty-printed type
  | -- | The modifier (@modelFieldModification@ or the @xxxTMod@ binding it
    --   names) isn't a single unguarded @tableModification { … }@ in one
    --   equation with no where bindings.
    --   Silently treating it as "no overrides" would emit Beam's default
    --   names for overridden columns — wrong YAML, no signal.
    UNRECOGNIZED_MODIFIER_SHAPE String String
    --                          typeName  modifier-binding-name
  | -- | @modelFieldModification@ names something other than a plain top-level
    --   @x = …@ binding of this module, referenced unqualified, bound once, with
    --   one unguarded right-hand side and no where bindings.
    UNRESOLVABLE_MODIFIER_BINDING String String
    --                            typeName  binding-name
  | -- | A @setEntityName@ that is not captured as an override: the contract
    --   would not record a rename Beam applies.
    UNCAPTURED_SET_ENTITY_NAME String String
    --                         line:col  reason
  | -- | A @modifyTableFields@ not applied to the modifier binding of a table
    --   extracted from this module: the column renames Beam applies could
    --   differ from the contract's.
    UNCAPTURED_MODIFY_TABLE_FIELDS String String
    --                             line:col  reason
  | -- | A use of a Beam function, constructor or field that can rename a
    --   table or its columns in a way the plugin cannot read, e.g.
    --   @modifyEntityName@.
    UNSUPPORTED_ENTITY_RENAME String String
    --                        line:col  name
  | -- | The @-fplugin-opt@ JSON was malformed, had an unknown key, a
    --   wrong-typed value, an invalid @blacklistModules@ entry or
    --   @fragmentsDir@, or was given more than once.
    INVALID_PLUGIN_OPTIONS String
  | -- | A type with a Beamable deriving or instance, or a Table or ModelMeta
    --   instance, in the module is not one the extractor reads as a table.
    UNEXTRACTED_TABLE String String
    --                typeName  reason
  | -- | An extracted table has no ModelMeta instance in its module.
    MISSING_MODEL_META String
  | -- | A Table, ModelMeta or Beamable instance is not for an unqualified type
    --   constructor, so it cannot be attributed to one of the module's types.
    UNREADABLE_INSTANCE_HEAD String String
    --                       class  rendered head type
  | -- | @modelTableType@ or @modelSchemaName@ is not in a shape the plugin
    --   reads, or comes from a derived instance.  Recorded as 'Nothing', it
    --   would route the table to the wrong database.
    UNRESOLVABLE_MODEL_META_FIELD String String String
    --                            typeName  field  rendered right-hand side or binding
  | -- | The module has two instances of a Table, ModelMeta or Beamable class
    --   (by name, any qualifier) for one type name.
    DUPLICATE_INSTANCE String String
    --                 class  typeName
  | -- | A field @primaryKey@ applies is not a column the plugin extracted for the table.
    UNKNOWN_PRIMARY_KEY_FIELD String String
    --                        typeName  field as written
  | -- | The module defines a name the plugin reads a table through: its
    --   modifier, key, meta fields, column types or database records.
    REDEFINED_NAME String String
    --             typeName  name
  | -- | A module that defines tables or database settings enables an extension
    --   that changes what the names and literals the plugin reads mean.
    UNSUPPORTED_EXTENSION String
    --                    extension
  | -- | A module that defines tables or database settings has a @RULES@ pragma.
    UNSUPPORTED_RULES String
    --                line:col
  | -- | A module that defines tables or database settings binds a record
    --   wildcard pattern at top level.
    UNSUPPORTED_RECORD_WILDCARD String
    --                          line:col
  | -- | The publish pass found the file the parse step staged gone:
    --   publishing nothing would leave the module's old fragment in place.
    STAGED_FRAGMENT_MISSING String
    --                      staged-file path
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | The constructor name, e.g. @UNRESOLVABLE_TABLE_NAME@.
errorCode :: SqlSchemaError -> String
errorCode = constructorName . from

-- Generic metadata, not 'Show', so neither a 'Show' instance nor an infix
-- constructor can put other text where the code goes.
class ConstructorName f where
  constructorName :: f p -> String

instance ConstructorName f => ConstructorName (D1 c f) where
  constructorName (M1 x) = constructorName x

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  constructorName (L1 x) = constructorName x
  constructorName (R1 x) = constructorName x

instance Constructor c => ConstructorName (C1 c f) where
  constructorName = conName

-- | Human-readable rendering for a compile error, starting with
-- @sql-schema: <code>: @ so a log scan finds every error by its code.  Only the
-- UNRESOLVABLE_TABLE_NAME message names the fragments directory.
generateErrorMessage :: FilePath -> SqlSchemaError -> String
generateErrorMessage fragDir e =
  capMessage ("sql-schema: " <> errorCode e <> ": " <> escapeUnsafe (errorText fragDir e))

-- The backstop for template text: each value a message quotes has been through
-- escapeField, and only a template's "\n  " continuation keeps a real newline.
escapeUnsafe :: String -> String
escapeUnsafe = \case
  '\n' : rest@(' ' : ' ' : _) -> '\n' : escapeUnsafe rest
  c : rest
    | unsafeChar c -> showLitChar c (escapeUnsafe rest)
    | otherwise    -> c : escapeUnsafe rest
  []               -> []

-- Generous: the longest message with empty values is 1116 characters, so three
-- values cut at fieldCap stay under the 4000-character cap unless they are full
-- of escapes.
capMessage :: String -> String
capMessage msg
  | length msg <= 4000 = msg
  | otherwise          = dropWhileEnd isSpace (take (4000 - length truncatedMarker) msg) <> truncatedMarker

-- Each value a message quotes: escaping every unsafe character, newlines included,
-- keeps it from forging a "sql-schema:" line or a terminal escape, and the cut
-- keeps a long one from crowding the template's fix out of the capped message.
escapeField :: String -> String
escapeField s = case splitAt fieldCap s of
  (kept, [])    -> escapeAll kept
  (kept, _ : _) -> escapeAll kept <> truncatedMarker
  where
    escapeAll = foldr (\c rest -> if unsafeChar c then showLitChar c rest else c : rest) ""

fieldCap :: Int
fieldCap = 500

unsafeChar :: Char -> Bool
unsafeChar c = generalCategory c `elem` [Control, Format, LineSeparator, ParagraphSeparator]

truncatedMarker :: String
truncatedMarker = " … [truncated]"

errorText :: FilePath -> SqlSchemaError -> String
errorText fragDir = \case
  UNRESOLVABLE_TABLE_NAME t ->
    "modelTableName for '" <> escapeField t <> "' is not a string literal written once, as one plain equation with no arguments, and with no guards or where bindings — the sql-schema plugin cannot statically extract it.\n" <>
    "  Write it as one equation 'modelTableName = \"sql_name\"', or give the table's SQL name in the tableNameOverrides plugin option, which applies only when modelTableName is not such a literal.\n" <>
    "  (Fragments dir: " <> escapeField fragDir <> ")"
  UNRESOLVABLE_COLUMN_NAME t f ->
    "The column name of '" <> escapeField f <> "' inside the *TMod for '" <> escapeField t <> "' is not 'fieldNamed \"…\"' with a literal.\n" <>
    "  It is computed, or the field is punned ('{ " <> escapeField f <> " }'), which takes its value from a variable of the same name; the plugin reads only a string literal.\n" <>
    "  Either write '" <> escapeField f <> " = fieldNamed \"explicit_name\"' or drop the override so the column falls back to Beam's default name '" <> escapeField (beamDefaultColumnName f) <> "'."
  UNRESOLVABLE_PRIMARY_KEY t ->
    "primaryKey for '" <> escapeField t <> "' is missing or not in a shape the plugin reads.\n" <>
    "  It reads the primaryKey of the Table instance of '" <> escapeField t <> "' in this module, written once as one equation, with no guards or where bindings, in one of these forms:\n" <>
    "    primaryKey x = Con (f1 x) (f2 x) …      (function equation, any arity)\n" <>
    "    primaryKey   = \\x -> Con (f1 x) …       (explicit lambda)\n" <>
    "    primaryKey   = Con . field               (single-column composition)\n" <>
    "    primaryKey   = Con . (.field)            (composition with a record-dot section)\n" <>
    "    primaryKey x = Con x.f1 x.f2             (record-dot fields, with or without OverloadedRecordDot)\n" <>
    "    primaryKey   = Con <$> f1 <*> f2 <*> …   (applicative composite)\n" <>
    "    primaryKey _ = ConNullary                (no-column key)\n" <>
    "  A guessed key could mark the wrong columns as the key; refusing to extract instead."
  UNRECOGNIZED_FIELD_WRAPPER t f ty ->
    "Field '" <> escapeField f <> "' on table '" <> escapeField t <> "' has type '" <> escapeField ty <> "', which the plugin does not recognise as a Beam column wrapper.\n" <>
    "  Expected 'Columnar f T' or 'C f T'.\n" <>
    "  Silently dropping this field from the YAML would cause a downstream false-pass against the prod DB; refusing to extract instead.\n" <>
    "  Either rewrite the field to use Columnar, or set allowUnknownColumnWrapper in the plugin options to opt out per-build."
  UNRECOGNIZED_MODIFIER_SHAPE t binding ->
    "Modifier binding '" <> escapeField binding <> "' for table '" <> escapeField t <> "' is not in a shape the plugin reads: 'tableModification { field = fieldNamed \"…\", … }' as a plain record update (not OverloadedRecordUpdate syntax), or for modelFieldModification the name of a top-level binding, written once, as one plain equation with no arguments, and with no guards or where bindings (one could shadow a name the plugin reads).\n" <>
    "  The plugin cannot statically read column-name overrides from any other shape.\n" <>
    "  Silently falling back to Beam's default (defaultDbSettings) names would produce a wrong YAML; refusing to extract instead.\n" <>
    "  Either rewrite the binding, or set allowNonStandardModifier in the plugin options to opt out per-build."
  UNRESOLVABLE_MODIFIER_BINDING t binding ->
    "Modifier binding '" <> escapeField binding <> "' for table '" <> escapeField t <> "' cannot be read: the plugin reads only a top-level binding of this module, referenced unqualified and bound once, as one plain equation 'x = …' with no guards or where bindings (one could shadow a name the plugin reads).\n" <>
    "  Define the modifier that way, or pass allowNonStandardModifier in the plugin options to treat it as no overrides."
  UNCAPTURED_SET_ENTITY_NAME loc reason ->
    "setEntityName at " <> escapeField loc <> ": " <> escapeField reason <> ".\n" <>
    "  A rename the plugin does not read is never checked against modelTableName, so the contract could name a table the code does not use; refusing to compile instead.\n" <>
    "  Write the rename as 'field = setEntityName \"sql_name\"' in a dbModification { … } that is the right operand of withDbModification in the result of a top-level binding (not in a where or let binding, a case or if branch, an instance method or another function's argument), alone or in a '<>' chain whose other operands are 'modifyTableFields x' or 'setEntitySchema x'.\n" <>
    "  'field' must have one type, 'f (TableEntity T)', across this module's records, and one record of this module must declare every label of the update, none qualified with another module; later withDbModification operands must be dbModification { … } updates; neither the dbModification nor the settings expression may carry a type annotation; no record wildcard '{..}' may cover the binding; and the module must not bind setEntityName, dbModification, withDbModification, (<>), modifyTableFields or setEntitySchema itself."
  UNCAPTURED_MODIFY_TABLE_FIELDS loc reason ->
    "modifyTableFields at " <> escapeField loc <> ": " <> escapeField reason <> ".\n" <>
    "  The contract reads column names from the table's modelFieldModification binding, so a rename the DB settings apply from anything else could make it name a column the code does not use; refusing to compile instead.\n" <>
    "  The accepted shape is 'modifyTableFields xxxTMod' applied directly, where 'xxxTMod' is unqualified and is the binding named in modelFieldModification of a table of this module: a top-level 'xxxTMod = tableModification { … }' with at least one field, written as one equation with no guards or where bindings and bound nowhere else in the module, with no record wildcard '{..}' covering the use, in a module that binds none of modifyTableFields, tableModification, fieldNamed and (<>) itself.\n" <>
    "  To fix it, pass the binding named in modelFieldModification; if no column is renamed, drop modifyTableFields."
  UNSUPPORTED_ENTITY_RENAME loc name ->
    escapeField name <> " at " <> escapeField loc <> ": it can rename a table or its columns in a way the plugin cannot read.\n" <>
    "  A rename the plugin does not read is never checked against the contract, so the contract could name a table or column the code does not use; refusing to compile instead.\n" <>
    "  Rename a table with setEntityName \"sql_name\" as the value of its dbModification { … } field, and its columns with 'modifyTableFields xxxTMod': pass the binding named in modelFieldModification; if no column is renamed, drop modifyTableFields."
  INVALID_PLUGIN_OPTIONS msg ->
    "Invalid sql-schema plugin options: " <> escapeField msg <> "\n" <>
    "  Pass one -fplugin-opt=SqlSchema.Plugin:{...} JSON object using only the documented keys."
  UNEXTRACTED_TABLE t reason ->
    "'" <> escapeField t <> "' has a Beamable deriving or instance, or a Table or ModelMeta instance, in this module, but the plugin cannot extract it as a table: " <> escapeField reason <> ".\n" <>
    "  Leaving it out of the contract would let a downstream schema check pass without it; refusing to extract instead.\n" <>
    "  " <> unextractedFix t reason
  MISSING_MODEL_META t ->
    "Table '" <> escapeField t <> "' has no ModelMeta instance in this module, so the plugin cannot read its table name or its column-name modifier.\n" <>
    "  tableNameOverrides does not stand in for it: the column names would be guessed.\n" <>
    "  Define 'instance ModelMeta " <> escapeField t <> "' in the module that declares the table."
  UNREADABLE_INSTANCE_HEAD cls ty ->
    "The " <> escapeField cls <> " instance for '" <> escapeField ty <> "' is not for one unqualified type constructor, so the plugin cannot tell which of this module's tables it belongs to: a type variable, a qualified or promoted name, a table's key (PrimaryKey t) and a class applied to more than one type are not read.\n" <>
    "  Skipping it could leave a table out of the contract, or let it take another type's instance; refusing to extract instead.\n" <>
    "  Write the instance for the table's own type, unqualified: 'instance " <> escapeField cls <> " XxxT'."
  UNRESOLVABLE_MODEL_META_FIELD t field rhs ->
    "The " <> escapeField field <> " of '" <> escapeField t <> "' is not in a shape the plugin reads: '" <> escapeField rhs <> "'.\n" <>
    "  It reads only " <> metaFieldShapes field <> ", also in parentheses or with a type signature, written once as a plain equation '" <> escapeField field <> " = …' with no arguments, guards or where bindings.\n" <>
    "  Recorded as Nothing, it would route the table to the wrong database downstream; refusing to extract instead.\n" <>
    "  " <> metaFieldFix t field rhs
  DUPLICATE_INSTANCE cls t ->
    "This module has more than one " <> escapeField cls <> " instance for '" <> escapeField t <> "', so the plugin cannot tell which one describes the table.\n" <>
    "  They may belong to same-named classes of different modules, or to one parameterised type at different arguments; reading either could record the wrong table name, key or columns, so the plugin refuses to extract instead.\n" <>
    "  Keep one " <> escapeField cls <> " instance per table type in the module; give each table its own type rather than instantiating one type twice."
  UNKNOWN_PRIMARY_KEY_FIELD t f ->
    "The primaryKey of '" <> escapeField t <> "' applies '" <> escapeField f <> "', which is not one of the column fields the plugin extracted for '" <> escapeField t <> "'.\n" <>
    "  Recorded as is, the contract would name a key column the table does not have and mark none of its columns as the key; refusing to extract instead.\n" <>
    "  Make primaryKey apply the table's own Columnar fields, unqualified; a field that allowUnknownColumnWrapper leaves out cannot be part of the key."
  REDEFINED_NAME t n ->
    "This module defines '" <> escapeField n <> "' itself (a top-level binding, a pattern synonym, a type or type family, a class method, a record field or a foreign import), and the plugin reads the table '" <> escapeField t <> "' through that name: its modifier, its primaryKey, its modelTableType and modelSchemaName, or its column types.\n" <>
    "  The plugin matches names as written, so it cannot tell whether '" <> escapeField n <> "' still means Beam's or base's, or the table's field.\n" <>
    "  Reading it as those could record a column name, key, table type or nullability the table does not use; refusing to extract instead.\n" <>
    "  Rename this module's own '" <> escapeField n <> "'."
  UNSUPPORTED_EXTENSION ext ->
    "This module defines tables or database settings and enables " <> escapeField ext <> ", which changes what the names and literals the plugin reads mean: with RebindableSyntax, record-dot syntax ('x.f', '(.f)') calls the getField in scope and a string literal the fromString in scope.\n" <>
    "  The plugin reads names and literals as written, so it could record a key, table name or column the code does not use; refusing to extract instead.\n" <>
    "  Turn " <> escapeField ext <> " off in this module ('{-# LANGUAGE No" <> escapeField ext <> " #-}', with 'import Prelude': RebindableSyntax also turned the implicit Prelude import off), or move its tables and database settings to a module without it."
  UNSUPPORTED_RULES loc ->
    "RULES pragma at " <> escapeField loc <> " in a module that defines tables or database settings: a rewrite rule can replace a name the plugin reads (fieldNamed, tableModification, a key's field, setEntityName, …), and GHC applies rules when it optimises.\n" <>
    "  The plugin does not read rules, so the code could run with a column name, key or table name it did not record; refusing to extract instead.\n" <>
    "  Move the rules to a module this module does not import, directly or not: GHC applies a rule in every module that imports its module, however indirectly."
  UNSUPPORTED_RECORD_WILDCARD loc ->
    "Record wildcard pattern at " <> escapeField loc <> " in a top-level binding of a module that defines tables or database settings: 'C{..} = …' binds every field of its record that is in scope as a top-level name, and the parser does not show which.\n" <>
    "  Any of them could replace a name the plugin reads (fieldNamed, tableModification, a key's field, …), so the plugin could record a column name or key the code does not use; refusing to extract instead.\n" <>
    "  Bind only the fields you need, by name ('C{f1, f2} = …' with NamedFieldPuns), or move the binding to a module that defines no tables or database settings."

  STAGED_FRAGMENT_MISSING pending ->
    "Nothing is staged at '" <> escapeField pending <> "' to publish: the plugin stages a module's fragment there when it parses the module, and publishes it once the module has passed type checking, desugaring and -Werror.\n" <>
    "  Publishing nothing would keep the module's previous fragment in the contract, so the compile fails instead.\n" <>
    "  Keep fragmentsDir for this plugin's files only, compile each module name into it once at a time, then rebuild the module."

-- | The reasons 'UNEXTRACTED_TABLE' gives; 'unextractedFix' names the fix for each.
reasonNoDeclaration, reasonNotARecord, reasonNoBeamable, reasonDatabaseRecord, reasonGenericNotStock :: String
reasonNoDeclaration  = "no data declaration for it in this module"
reasonNotARecord     = "its data declaration is not a single-constructor record with the column functor as last type parameter"
reasonNoBeamable     = "nothing in this module derives or declares Beamable for it"
reasonDatabaseRecord = "it derives Database, so its data declaration is read as a database record, not as a table"
reasonGenericNotStock = "this module does not derive its Generic instance with the stock strategy or annotate it as a large record, or has another Generic instance that could be its, so the column names Beam takes from it need not be its field names"

unextractedFix :: String -> String -> String
unextractedFix t reason
  | reason == reasonNoDeclaration =
      "Declare '" <> escapeField t <> "' in this module, beside its instances, or move the instances to the module that declares it."
  | reason == reasonNotARecord =
      "Write its declaration as a record whose last type parameter is the column functor: 'data " <> escapeField t <> " f = Con { field :: Columnar f T, … }'."
  | reason == reasonNoBeamable =
      "Derive Beamable for it in this module, inline or by standalone deriving; an instance that Template Haskell generates is not seen."
  | reason == reasonDatabaseRecord =
      "A database record is not a table: drop its Beamable deriving and any Table or ModelMeta instance for it, and declare each table as its own record."
  | reason == reasonGenericNotStock =
      "Derive Generic for '" <> escapeField t <> "' in this module with the stock strategy, in its deriving clause or by 'deriving instance Generic (" <> escapeField t <> " f)' with '" <> escapeField t <> "' itself in the head (a large record keeps its '{-# ANN type " <> escapeField t <> " largeRecord #-}'), and drop each other Generic instance that could be its: a written one, or one derived via, newtype or anyclass, also for a type synonym of this module or a head the plugin cannot read. Beam then names its columns from its record fields."
  | otherwise =
      "Keep the table's record declaration, its Beamable deriving and its Table and ModelMeta instances, all written out, in one module."

metaFieldShapes :: String -> String
metaFieldShapes = \case
  "modelTableType" -> "Nothing or Just <TableType constructor> (one of " <> intercalate ", " tableTypeConstructors <> "; qualified or not)"
  _                -> "Nothing, Just \"literal\" or Just <value name> (qualified or not)"

-- | The rendered derivation of a derived instance starts with @deriving@, which no written binding does.
metaFieldFix :: String -> String -> String -> String
metaFieldFix t field rhs
  | "deriving " `isPrefixOf` rhs = "Write the ModelMeta instance of '" <> escapeField t <> "' instead of deriving it."
  | otherwise                    = "Define " <> escapeField field <> " in one of those shapes in the ModelMeta instance of '" <> escapeField t <> "'."
