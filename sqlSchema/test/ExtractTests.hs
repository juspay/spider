{-# LANGUAGE LambdaCase #-}

-- | Extractor tests: each parses a fixture module @Fix.<Name>@ in-process
-- ('parseFixture') and checks what 'planFragment' makes of it.
module ExtractTests (run) where

import           Control.Exception    (ErrorCall (..), try)
import           Control.Monad        (unless)
import           Data.List            (isPrefixOf)
import           GHC.Types.SourceFile (HscSource (..))

import           ParseFixture         (parseFixture)
import           SqlSchema.Extract    (FragmentAction (..), planFragment)
import           SqlSchema.Plugin     (parseCliOptions)
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
  testParseFixtureRefuses
  testControlTable
  testNullability
  testFieldNamedOverride
  testPrimaryKeyShapes
  testModifierShapes
  testUnknownWrapper
  testTableNameRules
  testAppDbOverrides
  testBlacklistStagesDelete
  testBootAndSigUntouched
  testNothingToRecord
  testAcceptedMetaShapes
  testRdpRewrittenModifierFails
  testErrorsDeduplicated
  testErrorMessagesCarryCode

-- ---------------------------------------------------------------------------
-- Test cases
-- ---------------------------------------------------------------------------

-- | The fixture parser fails rather than parse under rules the fixture did not ask for.
testParseFixtureRefuses :: IO ()
testParseFixtureRefuses = mapM_ refused
  [ ("Pragma", [], ["{-# LANGUAGE OverloadedRecordDot #-}", "module Fix.Pragma where"], inPragma)
  , ("LowerCasePragma", [], ["{-# language OverloadedRecordDot #-}", "module Fix.LowerCasePragma where"], inPragma)
  , ("OptionsPragma", [], ["{-# OPTIONS_GHC -XOverloadedRecordDot #-}", "module Fix.OptionsPragma where"], inPragma)
  , ("UnknownExtension", ["OverloadedRecordDots"], ["module Fix.UnknownExtension where"], "unknown extension")
  , ("SyntaxError", [], ["module Fix.SyntaxError where", "x = = 1"], "parse failed at 2:")
  , ("NonFatalError", [], ["module Fix.NonFatalError where", "import Data.List qualified as L"], "reported errors")
  ]
  where
    inPragma = "must not set extensions in a pragma"
    refused (name, exts, source, reason) = do
      r <- try (parseFixture name exts (unlines source))
      case r of
        Left (ErrorCall msg) | (name <> ": ") `isInfixOf` msg, reason `isInfixOf` msg -> pure ()
        Left other -> failWith (name <> ": expected a refusal naming " <> show reason <> ", got " <> show other)
        Right _    -> failWith (name <> ": parseFixture accepted it")

testControlTable :: IO ()
testControlTable = do
  r <- planFix defaultCliOptions "Control" [] controlTable
  assertEq "control table"
    (Right (Stage (Just Fragment
      { fragmentModule = "Fix.Control"
      , fragmentTables =
          [ TableSchema "FooT" "Fix.Control" "foo" (Just "TRACKER")
              (Just (SchemaConfig "Config.getAppDbSchema")) (PrimaryKeyInfo "FooId" ["fooId"])
              [ ColumnInfo "fooId" "id" "Int" False True
              , ColumnInfo "fooName" "the_name" "Maybe Text" True False ] ]
      , fragmentDbEntityOverrides = [] })))
    r

-- | A column is nullable when its type's head is @Maybe@, qualified or not.
testNullability :: IO ()
testNullability = do
  tables <- tablesOf "nullability" =<< planFix defaultCliOptions "Nullability" []
    [ "data FooT f = Foo"
    , "  { fooId :: C f Int"
    , "  , fooNote :: Columnar f (Maybe Text)"
    , "  , fooCount :: C f (P.Maybe Int)"
    , "  , fooKind :: C f MaybeLike"
    , "  } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    ]
  assertEq "nullable"
    [("fooId", False), ("fooNote", True), ("fooCount", True), ("fooKind", False)]
    [(hsField c, nullable c) | t <- tables, c <- columns t]

testFieldNamedOverride :: IO ()
testFieldNamedOverride = do
  tables <- tablesOf "fieldNamed" =<< planFix defaultCliOptions "FieldNamed" []
    [ "data WidgetT f = Widget"
    , "  { widgetId :: C f Int"
    , "  , widgetName :: C f Text"
    , "  , widgetEnabled :: C f Bool"
    , "  } deriving (Generic, Beamable)"
    , "instance Table WidgetT where"
    , "  data PrimaryKey WidgetT f = WidgetId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = WidgetId . widgetId"
    , "instance ModelMeta WidgetT where"
    , "  modelTableName = \"widget\""
    , "  modelFieldModification = widgetTMod"
    , "widgetTMod = tableModification { widgetName = fieldNamed \"the_name\" }"
    ]
  assertEq "columns"
    [("widgetId", "id"), ("widgetName", "the_name"), ("widgetEnabled", "enabled")]
    [(hsField c, column c) | t <- tables, c <- columns t]

-- | The README's primaryKey shapes.
testPrimaryKeyShapes :: IO ()
testPrimaryKeyShapes = do
  let key2 = "data PrimaryKey FooT f = FooKey (C f Int) (C f Text) deriving (Generic, Beamable)"
      key1 = "data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
      key0 = "data PrimaryKey FooT f = FooNullary deriving (Generic, Beamable)"
      both = Right ("FooKey", ["fooId", "fooName"])
      one  = Right ("FooId", ["fooId"])
      shape name exts pkLines expected = assertEq name expected =<< primaryKeyOf name exts pkLines
  shape "PkEquation" [] [key2, "primaryKey x = FooKey (fooId x) (fooName x)"] both
  shape "PkLambda" [] [key1, "primaryKey = \\x -> FooId (fooId x)"] one
  shape "PkCompose" [] [key1, "primaryKey = FooId . fooId"] one
  shape "PkComposeSection" [] [key1, "primaryKey = FooId . (.fooId)"] one
  shape "PkComposeProjection" ["OverloadedRecordDot"] [key1, "primaryKey = FooId . (.fooId)"] one
  shape "PkRecordDot" ["OverloadedRecordDot"] [key2, "primaryKey x = FooKey x.fooId x.fooName"] both
  shape "PkDotChain" [] [key2, "primaryKey x = FooKey x.fooId x.fooName"] both
  shape "PkApplicative" [] [key2, "primaryKey = FooKey <$> fooId <*> fooName"] both
  shape "PkNullary" [] [key0, "primaryKey _ = FooNullary"] (Right ("FooNullary", []))
  shape "PkUnresolvable" [] [key1, "primaryKey = someFunction"]
    (Left [UNRESOLVABLE_PRIMARY_KEY "FooT"])

testModifierShapes :: IO ()
testModifierShapes = do
  let lenient = defaultCliOptions { allowNonStandardModifier = True }
      overridden = [("fooId", "id"), ("fooName", "the_name")]
      defaults = [("fooId", "id"), ("fooName", "name")]
      accepted name meta rest = do
        cols <- columnNames name =<< modifierFix defaultCliOptions name meta rest
        assertEq name overridden cols
      refused name meta rest err = do
        assertEq name (Left [err]) =<< modifierFix defaultCliOptions name meta rest
        cols <- columnNames name =<< modifierFix lenient name meta rest
        assertEq (name <> " with allowNonStandardModifier") defaults cols
  accepted "ModInline"
    ["modelFieldModification = tableModification { fooName = fieldNamed \"the_name\" }"] []
  accepted "ModNamed"
    ["modelFieldModification = fooTMod"]
    ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"]
  refused "ModQualified"
    ["modelFieldModification = M.fooTMod"] []
    (UNRESOLVABLE_MODIFIER_BINDING "FooT" "M.fooTMod")
  refused "ModGuarded"
    ["modelFieldModification", "  | True = fooTMod"]
    ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"]
    (UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification")
  refused "ModGuardedBinding"
    ["modelFieldModification = fooTMod"]
    ["fooTMod", "  | True = tableModification { fooName = fieldNamed \"the_name\" }"]
    (UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod")
  refused "ModMappend"
    ["modelFieldModification = fooTMod"]
    ["fooTMod = idTMod <> nameTMod"]
    (UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod")

testUnknownWrapper :: IO ()
testUnknownWrapper = do
  let fixture cli = planFix cli "UnknownWrapper" []
        [ "data FooT f = Foo { fooId :: C f Int, foo :: Int } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , "instance ModelMeta FooT where"
        , "  modelTableName = \"foo\""
        ]
  assertEq "unknown wrapper" (Left [UNRECOGNIZED_FIELD_WRAPPER "FooT" "foo" "Int"])
    =<< fixture defaultCliOptions
  tables <- tablesOf "allowUnknownColumnWrapper"
    =<< fixture defaultCliOptions { allowUnknownColumnWrapper = True }
  assertEq "field left out" [["fooId"]] (map (map hsField . columns) tables)

testTableNameRules :: IO ()
testTableNameRules = do
  overrides <- expectOk (parseCliOptions ["{\"tableNameOverrides\":{\"FooT\":\"foo_override\"}}"])
  let fixture name tableName cli = planFix cli name []
        [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , "instance ModelMeta FooT where"
        , "  modelTableName = " <> tableName
        ]
  assertEq "non-literal name" (Left [UNRESOLVABLE_TABLE_NAME "FooT"])
    =<< fixture "NameConst" "SomeConst.fooName" defaultCliOptions
  supplied <- tablesOf "tableNameOverrides" =<< fixture "NameConst" "SomeConst.fooName" overrides
  assertEq "override supplies it" ["foo_override"] (map modelTableName supplied)
  literal <- tablesOf "literal name" =<< fixture "NameLiteral" "\"foo\"" overrides
  assertEq "a literal wins over the override" ["foo"] (map modelTableName literal)

-- | Shaped like a real DB-settings module: qualified Beam names, a
-- @Database be@ deriving and two 'withDbModification' blocks.
testAppDbOverrides :: IO ()
testAppDbOverrides = do
  r <- planFix defaultCliOptions "AppDb" []
    [ "data AppDb f = AppDb"
    , "  { gadgets :: f (B.TableEntity GadgetsT)"
    , "  , widgets :: f (B.TableEntity WidgetT)"
    , "  , jobs :: f (B.TableEntity JobT)"
    , "  } deriving (Generic, B.Database be)"
    , "appDb :: Text -> B.DatabaseSettings be AppDb"
    , "appDb schema = B.defaultDbSettings `B.withDbModification` B.dbModification"
    , "    { gadgets = B.setEntityName \"Gadgets\""
    , "    , widgets = B.setEntityName \"Widgets\""
    , "    , jobs = B.setEntityName \"Jobs\""
    , "    }"
    , "  `B.withDbModification` B.dbModification { widgets = SWidget.widgetEMod schema }"
    ]
  let override field tableType sql = DbEntityOverride
        { dbField = field, dbTableType = tableType, sqlName = sql
        , overrideSourceModule = "Fix.AppDb", overrideSourceFile = "src/Fix/AppDb.hs" }
  assertEq "overrides"
    (Right (Stage (Just Fragment
      { fragmentModule = "Fix.AppDb"
      , fragmentTables = []
      , fragmentDbEntityOverrides =
          [ override "gadgets" "GadgetsT" "Gadgets"
          , override "widgets" "WidgetT" "Widgets"
          , override "jobs" "JobT" "Jobs" ] })))
    r

-- | A blacklisted module's earlier fragment is deleted, even when its tables
-- could not be read.  Entries match as module-name prefixes.
testBlacklistStagesDelete :: IO ()
testBlacklistStagesDelete = do
  let cli = defaultCliOptions { blacklistModules = ["Test.", "Fix.Black"] }
  assertEq "blacklisted" (Right (Stage Nothing)) =<< planFix cli "Blacklisted" [] controlTable
  assertEq "blacklisted, unreadable" (Right (Stage Nothing))
    =<< planFix cli "BlackUnreadable" [] (modifierTable ["modelFieldModification = M.fooTMod"] [])
  tables <- tablesOf "not a prefix"
    =<< planFix defaultCliOptions { blacklistModules = ["Black"] } "Blacklisted" [] controlTable
  assertEq "not a prefix" ["FooT"] (map codeName tables)

-- | A boot or signature file shares its module's fragment path; it must not touch it.
testBootAndSigUntouched :: IO ()
testBootAndSigUntouched = do
  let blacklisted = defaultCliOptions { blacklistModules = ["Fix."] }
  assertEq "hs-boot" (Right LeaveUntouched) =<< planFixAs HsBootFile defaultCliOptions "Boot" controlTable
  assertEq "hsig" (Right LeaveUntouched) =<< planFixAs HsigFile defaultCliOptions "Sig" controlTable
  assertEq "blacklisted hs-boot" (Right LeaveUntouched)
    =<< planFixAs HsBootFile blacklisted "BootBlacklisted" controlTable

testNothingToRecord :: IO ()
testNothingToRecord = do
  r <- planFix defaultCliOptions "Plain" []
    [ "data Colour = Red | Green deriving (Show, Eq)"
    , "data Pair a = Pair { pairFirst :: a, pairSecond :: a } deriving (Show)"
    , "instance Semigroup Colour where"
    , "  c <> _ = c"
    , "double :: Int -> Int"
    , "double x = x * 2"
    ]
  assertEq "nothing to record" (Right (Stage Nothing)) r

testAcceptedMetaShapes :: IO ()
testAcceptedMetaShapes = do
  tables <- tablesOf "meta shapes" =<< planFix defaultCliOptions "MetaShapes" []
    [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    , "  modelTableType = Just Se.CONFIG"
    , "  modelSchemaName = Just \"public\""
    , "data BarT f = Bar { barId :: C f Int } deriving (Generic, Beamable)"
    , "instance Table BarT where"
    , "  data PrimaryKey BarT f = BarId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = BarId . barId"
    , "instance ModelMeta BarT where"
    , "  modelTableName = \"bar\""
    , "  modelTableType = P.Just DOMAIN"
    , "  modelSchemaName = Just Config.getAppDbSchema"
    ]
  assertEq "meta"
    [ ("FooT", Just "CONFIG", Just (SchemaLiteral "public"))
    , ("BarT", Just "DOMAIN", Just (SchemaConfig "Config.getAppDbSchema")) ]
    [(codeName t, modelTableType t, modelSchemaName t) | t <- tables]

-- | record-dot-preprocessor's rewrite of a modifier, seen when it runs before
-- this plugin, is refused.
testRdpRewrittenModifierFails :: IO ()
testRdpRewrittenModifierFails = do
  r <- planFix defaultCliOptions "RdpModifier" ["TypeApplications", "DataKinds"]
    (modifierTable
      ["modelFieldModification = fooTMod"]
      ["fooTMod = setField @\"fooName\" tableModification (fieldNamed \"x\")"])
  assertEq "rewritten modifier" (Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod"]) r

-- | Each error is reported once, in first-occurrence order.
testErrorsDeduplicated :: IO ()
testErrorsDeduplicated = do
  r <- planFix defaultCliOptions "DuplicateErrors" []
    (modifierTable
      ["modelFieldModification = fooTMod"]
      [ "fooTMod = tableModification"
      , "  { fooName = fieldNamed a, fooId = fieldNamed b, fooName = fieldNamed c }" ])
  assertEq "deduplicated"
    (Left [UNRESOLVABLE_COLUMN_NAME "FooT" "fooName", UNRESOLVABLE_COLUMN_NAME "FooT" "fooId"]) r

testErrorMessagesCarryCode :: IO ()
testErrorMessagesCarryCode = mapM_ check
  [ UNRESOLVABLE_TABLE_NAME "FooT"
  , UNRESOLVABLE_COLUMN_NAME "FooT" "fooName"
  , UNRESOLVABLE_PRIMARY_KEY "FooT"
  , UNRECOGNIZED_FIELD_WRAPPER "FooT" "foo" "Int"
  , UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod"
  , UNRESOLVABLE_MODIFIER_BINDING "FooT" "M.fooTMod"
  , INVALID_PLUGIN_OPTIONS "not json"
  ]
  where
    check e = do
      assertEq "errorCode" (constructorName e) (errorCode e)
      let msg = generateErrorMessage "d" e
      unless (("sql-schema: " <> errorCode e <> ": ") `isPrefixOf` msg) $
        failWith ("message without its code prefix: " <> show msg)
    -- No wildcard: a new constructor breaks the build here, a reminder to sample it above.
    constructorName = \case
      UNRESOLVABLE_TABLE_NAME{}       -> "UNRESOLVABLE_TABLE_NAME"
      UNRESOLVABLE_COLUMN_NAME{}      -> "UNRESOLVABLE_COLUMN_NAME"
      UNRESOLVABLE_PRIMARY_KEY{}      -> "UNRESOLVABLE_PRIMARY_KEY"
      UNRECOGNIZED_FIELD_WRAPPER{}    -> "UNRECOGNIZED_FIELD_WRAPPER"
      UNRECOGNIZED_MODIFIER_SHAPE{}   -> "UNRECOGNIZED_MODIFIER_SHAPE"
      UNRESOLVABLE_MODIFIER_BINDING{} -> "UNRESOLVABLE_MODIFIER_BINDING"
      INVALID_PLUGIN_OPTIONS{}        -> "INVALID_PLUGIN_OPTIONS"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

planFix :: CliOptions -> String -> [String] -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
planFix cli name exts body = planFixWith HsSrcFile cli name exts body

planFixAs :: HscSource -> CliOptions -> String -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
planFixAs src cli name body = planFixWith src cli name [] body

planFixWith
  :: HscSource -> CliOptions -> String -> [String] -> [String]
  -> IO (Either [SqlSchemaError] FragmentAction)
planFixWith src cli name exts body = do
  let modName = "Fix." <> name
  decls <- parseFixture modName exts (unlines (("module " <> modName <> " where") : body))
  pure (planFragment cli src modName ("src/Fix/" <> name <> ".hs") decls)

tablesOf :: String -> Either [SqlSchemaError] FragmentAction -> IO [TableSchema]
tablesOf label = \case
  Right (Stage (Just fragment)) -> pure (fragmentTables fragment)
  other -> failWith (label <> ": expected a fragment, got " <> show other)

columnNames :: String -> Either [SqlSchemaError] FragmentAction -> IO [(String, String)]
columnNames label r = do
  tables <- tablesOf label r
  pure [(hsField c, column c) | t <- tables, c <- columns t]

-- | The primary key of the one table of a fixture whose Table instance has @pkLines@.
primaryKeyOf :: String -> [String] -> [String] -> IO (Either [SqlSchemaError] (String, [String]))
primaryKeyOf name exts pkLines = do
  r <- planFix defaultCliOptions name exts
    ([ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic, Beamable)"
     , "instance Table FooT where" ]
     <> map ("  " <>) pkLines
     <> [ "instance ModelMeta FooT where"
        , "  modelTableName = \"foo\"" ])
  case r of
    Left errs -> pure (Left errs)
    Right (Stage (Just Fragment { fragmentTables = [t] })) ->
      pure (Right (pkConstructor (primaryKey t), pkColumns (primaryKey t)))
    other -> failWith (name <> ": expected one table, got " <> show other)

modifierFix :: CliOptions -> String -> [String] -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
modifierFix cli name meta rest = planFix cli name [] (modifierTable meta rest)

-- | Table FooT whose ModelMeta instance also holds @meta@, followed by the
-- top-level declarations @rest@.
modifierTable :: [String] -> [String] -> [String]
modifierTable meta rest =
  [ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic, Beamable)"
  , "instance Table FooT where"
  , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
  , "  primaryKey = FooId . fooId"
  , "instance ModelMeta FooT where"
  , "  modelTableName = \"foo\"" ]
  <> map ("  " <>) meta
  <> rest

-- | A table that sets every ModelMeta field the extractor reads.
controlTable :: [String]
controlTable =
  [ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f (Maybe Text) }"
  , "  deriving (Generic, Beamable)"
  , "instance Table FooT where"
  , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
  , "  primaryKey = FooId . fooId"
  , "instance ModelMeta FooT where"
  , "  modelTableName = \"foo\""
  , "  modelTableType = Just TRACKER"
  , "  modelSchemaName = Just Config.getAppDbSchema"
  , "  modelFieldModification = fooTMod"
  , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
  ]
