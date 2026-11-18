{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Extractor tests: each parses a fixture module @Fix.<Name>@ in-process
-- ('parseFixture') and checks what 'planFragment' makes of it.
module ExtractTests (run) where

import           Control.Exception    (ErrorCall (..), try)
import           Control.Monad        (forM_, unless, when)
import           Data.Char            (GeneralCategory (Control, Format, LineSeparator,
                                                        ParagraphSeparator), generalCategory)
import           Data.Data            (Data, cast, gmapQ, gmapT)
import           Data.List            (isPrefixOf, isSuffixOf)
import           Data.Maybe           (fromMaybe)
import           Data.Proxy           (Proxy (..))
import           GHC.Generics         (C, C1, Constructor, D1, Generic (..), K1 (..), M1 (..), conName,
                                       (:*:) (..), (:+:) (..))
import           GHC.Hs               (GhcPs, HsType (..))
import           GHC.Hs               (HsDecl (..))
import           GHC.Hs               (LIdP)
import           GHC.Parser.Annotation (SrcSpanAnn' (..))
import           GHC.Types.Name.Occurrence (occNameString)
import           GHC.Types.Name.Reader (rdrNameOcc)
import           GHC.Types.SrcLoc     (GenLocated (..), noSrcSpan)
import           GHC.Types.SourceFile (HscSource (..))

import           ParseFixture         (parseFixture, parseFixtureHaddock)
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
  testEntityNameMappendCaptured
  testEntityNameInEModFails
  testEntityNameInWhereFails
  testEntityNameInLetFails
  testEntityNameInInstanceMethodFails
  testEntityNameInOtherDeclarationsFail
  testEntityNameUnknownFieldFails
  testEntityNameAmbiguousFieldFails
  testEntityNameTwiceInChainFails
  testEntityNameNonLiteralFails
  testEntityNameViaSetFieldFails
  testEntityNameInSpliceFails
  testEntityNameInCommentIgnored
  testEntityNameBlacklistedModuleSkipped
  testUncapturedEntityNameMessage
  testEntityNameDocumentedDatabaseDeriving
  testDocumentedBeamableDeriving
  testEntityNameQualifiedLabelFails
  testEntityNameAnnotatedModificationFails
  testEntityNameFieldOfAnyRecord
  testEntityNameChainOperandsFail
  testEntityNameShadowedFails
  testEntityNameNotAppliedFails
  testEntityNameUnreadValueFails
  testEntityNameRecordDotUpdateFails
  testEntityNameNamePeels
  testWatchedNamePunsCounted
  testEntityNameSameSpanCountedTwice
  testEntityNameForeignLabelFails
  testEntityNameUnderRecordWildcardFails
  testEntityNameUnplaceableSpansFail
  testEntityNameLaterOperandFails
  testModifyTableFieldsTrustedNamesBoundFails
  testModifyTableFieldsTypeAppliedFails
  testOverloadedRecordUpdatePunsLocated
  testModifyTableFieldsSameBindingOk
  testModifyTableFieldsMultilineOk
  testModifyTableFieldsWhereOk
  testModifyTableFieldsInImportIgnored
  testModifyTableFieldsOtherBindingFails
  testModifyTableFieldsInlineFails
  testModifyTableFieldsBareModifierFails
  testModifyTableFieldsQualifiedFails
  testModifyTableFieldsUnappliedFails
  testModifyTableFieldsOtherModuleTableFails
  testModifyTableFieldsShadowedBindingFails
  testEntityRenamersFail
  testEntityInternalsFail
  testInternalsInTypesIgnored
  testTableModificationsUnderHaddock
  testTableModificationsSeenEverywhere
  testModifyTableFieldsBlacklistedModuleSkipped
  testTableModificationMessages
  testBlacklistStagesDelete
  testPluginOptions
  testBootAndSigUntouched
  testNothingToRecord
  testAcceptedMetaShapes
  testRdpRewrittenModifierFails
  testErrorsDeduplicated
  testErrorMessagesCarryCode
  testErrorFieldsEscaped
  testErrorMessagesSanitized
  testMessageBackstop

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

-- | A rename in a @<>@ chain is read, @P.<>@ included (the real EMods qualify it).
testEntityNameMappendCaptured :: IO ()
testEntityNameMappendCaptured = do
  let captured name rest = do
        fragment <- fragmentOf name =<< planFix defaultCliOptions name [] (entityNameModule rest)
        assertEq (name <> " tables") ["FooT"] (map codeName (fragmentTables fragment))
        assertEq (name <> " overrides")
          [ DbEntityOverride
              { dbField = "foos", dbTableType = "FooT", sqlName = "foo_v2"
              , overrideSourceModule = "Fix." <> name, overrideSourceFile = "src/Fix/" <> name <> ".hs" } ]
          (fragmentDbEntityOverrides fragment)
  captured "EntityNameMappend"
    [ "db = defaultDbSettings `withDbModification`"
    , "  dbModification { foos = setEntityName \"foo_v2\" <> modifyTableFields fooTMod }" ]
  captured "EntityNameMappendQualified"
    [ "db schema = B.defaultDbSettings `B.withDbModification` B.dbModification"
    , "  { foos = B.modifyTableFields fooTMod P.<> B.setEntityName \"foo_v2\""
    , "      P.<> B.setEntitySchema (P.Just schema) }" ]
  captured "EntityNameMappendParens"
    [ "db schema = defaultDbSettings `withDbModification` dbModification"
    , "  { foos = (modifyTableFields fooTMod <> setEntityName \"foo_v2\") <> setEntitySchema (Just schema) }" ]

-- | P05: a rename in an EMod binding is not read.
testEntityNameInEModFails :: IO ()
testEntityNameInEModFails =
  expectUncaptured "EntityNameInEMod" [("11:11", notInModification)] =<< planFix defaultCliOptions "EntityNameInEMod" []
    (entityNameModule
      [ "fooEMod = setEntityName \"foo_v2\""
      , "db = defaultDbSettings `withDbModification` dbModification { foos = fooEMod }" ])

-- | P06.
testEntityNameInWhereFails :: IO ()
testEntityNameInWhereFails =
  expectUncaptured "EntityNameInWhere" [("12:40", notAppliedModification)] =<< planFix defaultCliOptions "EntityNameInWhere" []
    (entityNameModule
      [ "db = defaultDbSettings `withDbModification` mods"
      , "  where mods = dbModification { foos = setEntityName \"foo_v2\" }" ])

testEntityNameInLetFails :: IO ()
testEntityNameInLetFails =
  expectUncaptured "EntityNameInLet" [("11:41", notAppliedModification)] =<< planFix defaultCliOptions "EntityNameInLet" []
    (entityNameModule
      [ "db = let mods = dbModification { foos = setEntityName \"foo_v2\" }"
      , "     in defaultDbSettings `withDbModification` mods" ])

testEntityNameInInstanceMethodFails :: IO ()
testEntityNameInInstanceMethodFails =
  expectUncaptured "EntityNameInInstance" [("12:79", notAppliedModification)]
    =<< planFix defaultCliOptions "EntityNameInInstance" []
      (entityNameModule
        [ "instance HasDb Db where"
        , "  dbSettings = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ])

-- | A class default, a pattern synonym's builder and a rewrite rule.
testEntityNameInOtherDeclarationsFail :: IO ()
testEntityNameInOtherDeclarationsFail =
  expectUncaptured "EntityNameInOtherDecls"
    [("13:79", notAppliedModification), ("16:13", notInModification), ("17:39", notInModification)]
    =<< planFix defaultCliOptions "EntityNameInOtherDecls" ["PatternSynonyms"]
      (entityNameModule
        [ "class HasDb db where"
        , "  dbSettings :: DatabaseSettings be db"
        , "  dbSettings = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"a\" }"
        , "pattern Renamed :: EntityModification e be t"
        , "pattern Renamed <- _ where"
        , "  Renamed = setEntityName \"b\""
        , "{-# RULES \"rename\" forall m. keep m = setEntityName \"c\" #-}" ])

testEntityNameUnknownFieldFails :: IO ()
testEntityNameUnknownFieldFails =
  expectUncaptured "EntityNameUnknownField" [("11:69", "no single record of this module declares all of its dbModification's labels ('bars')")]
    =<< planFix defaultCliOptions "EntityNameUnknownField" []
      (entityNameModule
        [ "db = defaultDbSettings `withDbModification` dbModification { bars = setEntityName \"foo_v2\" }" ])

-- | A field two DB records give different table types is refused; the same type twice is not ambiguous.
testEntityNameAmbiguousFieldFails :: IO ()
testEntityNameAmbiguousFieldFails = do
  let fixture name otherType = planFix defaultCliOptions name []
        (entityNameModule
          [ "data OtherDb f = OtherDb { foos :: f (TableEntity " <> otherType <> ") } deriving (Generic, Database)"
          , "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ])
  expectUncaptured "EntityNameAmbiguousField" [("12:69", "has more than one type, or one other than f (TableEntity T), across this module's records (BarT, FooT)")]
    =<< fixture "EntityNameAmbiguousField" "BarT"
  fragment <- fragmentOf "same type twice" =<< fixture "EntityNameSharedField" "FooT"
  assertEq "same type twice" [("foos", "FooT", "foo_v2")]
    [(dbField o, dbTableType o, sqlName o) | o <- fragmentDbEntityOverrides fragment]

testEntityNameTwiceInChainFails :: IO ()
testEntityNameTwiceInChainFails =
  expectUncaptured "EntityNameTwiceInChain" [("12:12", twice), ("12:33", twice)]
    =<< planFix defaultCliOptions "EntityNameTwiceInChain" []
      (entityNameModule
        [ "db = defaultDbSettings `withDbModification` dbModification"
        , "  { foos = setEntityName \"a\" <> setEntityName \"b\" }" ])
  where twice = "more than one setEntityName operand"

testEntityNameNonLiteralFails :: IO ()
testEntityNameNonLiteralFails =
  expectUncaptured "EntityNameNonLiteral" [("11:74", "its argument is not a string literal")]
    =<< planFix defaultCliOptions "EntityNameNonLiteral" []
      (entityNameModule
        [ "db name = defaultDbSettings `withDbModification` dbModification { foos = setEntityName name }" ])

-- | record-dot-preprocessor's rewrite of a dbModification update, seen when it runs before this plugin.
testEntityNameViaSetFieldFails :: IO ()
testEntityNameViaSetFieldFails =
  expectUncaptured "EntityNameViaSetField" [("11:78", notInModification)]
    =<< planFix defaultCliOptions "EntityNameViaSetField" ["TypeApplications", "DataKinds"]
      (entityNameModule
        [ "db = defaultDbSettings `withDbModification` setField @\"foos\" dbModification (setEntityName \"x\")" ])

-- | Template Haskell: a quoted use, and a name quote such as @varE 'setEntityName@ takes.
testEntityNameInSpliceFails :: IO ()
testEntityNameInSpliceFails =
  expectUncaptured "EntityNameInSplice" [("11:10", notInModification), ("12:16", notInModification)]
    =<< planFix defaultCliOptions "EntityNameInSplice" ["TemplateHaskell"]
      (entityNameModule
        [ "$(foo [| setEntityName \"x\" |])"
        , "db = $(rename 'setEntityName \"y\")" ])

-- | Comments are not in the AST, and a string is not a use; with -haddock, a
-- doc comment that names 'setEntityName' is not one either.
testEntityNameInCommentIgnored :: IO ()
testEntityNameInCommentIgnored = mapM_ check [("plain", parseFixture), ("haddock", parseFixtureHaddock)]
  where
    check (label, parse) = do
      let modName = "Fix.EntityNameInComment"
      decls <- parse modName [] $ unlines $ ("module " <> modName <> " where") : entityNameModule
        [ "-- db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"x\" }"
        , "{- foos = setEntityName \"y\" -}"
        , "-- | Renamed with 'setEntityName' \"z\" elsewhere."
        , "db = defaultDbSettings"
        , "note = \"setEntityName\"" ]
      fragment <- fragmentOf label (planFragment defaultCliOptions HsSrcFile modName "f.hs" decls)
      assertEq (label <> ": tables") ["FooT"] (map codeName (fragmentTables fragment))
      assertEq (label <> ": overrides") [] (fragmentDbEntityOverrides fragment)

testEntityNameBlacklistedModuleSkipped :: IO ()
testEntityNameBlacklistedModuleSkipped =
  assertEq "blacklisted" (Right (Stage Nothing))
    =<< planFix defaultCliOptions { blacklistModules = ["Fix.EntityNameBlacklisted"] } "EntityNameBlacklisted" []
      (entityNameModule
        [ "fooEMod = setEntityName \"foo_v2\""
        , "db = defaultDbSettings `withDbModification` dbModification { foos = fooEMod }" ])

-- | The message names the location, the reason and the shape that is read.
testUncapturedEntityNameMessage :: IO ()
testUncapturedEntityNameMessage =
  mapM_ has ["11:11: the reason.", "'field = setEntityName \"sql_name\"'", "dbModification { … }"]
  where
    msg = generateErrorMessage "d" (UNCAPTURED_SET_ENTITY_NAME "11:11" "the reason")
    has part = unless (part `isInfixOf` msg) $
      failWith ("UNCAPTURED_SET_ENTITY_NAME message lacks " <> show part <> ": " <> show msg)

-- | Under @-haddock@ a documented deriving type is an 'HsDocTy'; the DB record must still be read.
testEntityNameDocumentedDatabaseDeriving :: IO ()
testEntityNameDocumentedDatabaseDeriving = mapM_ captured
  [ ("DocumentedDatabase", ["  deriving (Generic, Database -- ^ the settings record", "    )"])
  , ("DocumentedQualifiedDatabase", ["  deriving (Generic, B.Database be -- ^ the settings record", "    )"])
  , ("DocumentedSoleDatabase", ["  deriving Database -- ^ the settings record"]) ]
  where
    captured (name, derivingLines) = do
      let modName = "Fix." <> name
      decls <- parseFixtureHaddock modName [] $ unlines $ ("module " <> modName <> " where")
        : modifierTable ["modelFieldModification = fooTMod"]
            ( "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
            : "data Db f = Db { foos :: f (TableEntity FooT) }"
            : derivingLines
            <> ["db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"] )
      unless (documentedTypes decls > 0) $ failWith (name <> ": the fixture parsed without an HsDocTy")
      fragment <- fragmentOf name (planFragment defaultCliOptions HsSrcFile modName "f.hs" decls)
      assertEq name [("foos", "FooT", "foo_v2")]
        [(dbField o, dbTableType o, sqlName o) | o <- fragmentDbEntityOverrides fragment]

-- | Under @-haddock@ a documented Beamable deriving is still read: the table is extracted.
testDocumentedBeamableDeriving :: IO ()
testDocumentedBeamableDeriving = do
  let modName = "Fix.DocumentedBeamable"
  decls <- parseFixtureHaddock modName [] $ unlines
    [ "module " <> modName <> " where"
    , "data FooT f = Foo { fooId :: C f Int }"
    , "  deriving (Generic, Beamable -- ^ the table"
    , "    )"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\"" ]
  unless (documentedTypes decls > 0) $ failWith "DocumentedBeamable: the fixture parsed without an HsDocTy"
  tables <- tablesOf "DocumentedBeamable" (planFragment defaultCliOptions HsSrcFile modName "f.hs" decls)
  assertEq "DocumentedBeamable" ["FooT"] (map codeName tables)

documentedTypes :: Data a => a -> Int
documentedTypes d = case cast d :: Maybe (HsType GhcPs) of
  Just HsDocTy{} -> 1 + sum (gmapQ documentedTypes d)
  _              -> sum (gmapQ documentedTypes d)

-- | A label qualified with another module may name that module's record;
-- this module's own qualifier is read.
testEntityNameQualifiedLabelFails :: IO ()
testEntityNameQualifiedLabelFails = do
  expectUncaptured "QualifiedLabel" [("11:71", "its dbModification's label 'O.foos' is qualified with another module")]
    =<< planFix defaultCliOptions "EntityNameQualifiedLabel" []
      (entityNameModule
        ["db = defaultDbSettings `withDbModification` dbModification { O.foos = setEntityName \"foo_v2\" }"])
  capturedFooV2 "EntityNameOwnQualifier"
    ["db = defaultDbSettings `withDbModification` dbModification { Fix.EntityNameOwnQualifier.foos = setEntityName \"foo_v2\" }"]

-- | An annotation or a type application can select another module's record.
testEntityNameAnnotatedModificationFails :: IO ()
testEntityNameAnnotatedModificationFails = mapM_ refused
  [ ("AnnotatedBase", [], "db = defaultDbSettings `withDbModification` (dbModification :: DatabaseModification f be OtherDb) { foos = setEntityName \"foo_v2\" }", "11:108", baseReason)
  , ("TypeAppliedBase", ["TypeApplications"], "db = defaultDbSettings `withDbModification` (dbModification @OtherDb) { foos = setEntityName \"foo_v2\" }", "11:80", baseReason)
  , ("AnnotatedSettings", [], "db = (defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" } :: DatabaseSettings be OtherDb)", "11:70", "the settings expression carries a type annotation")
  , ("AnnotatedUpdate", [], "db = defaultDbSettings `withDbModification` (dbModification { foos = setEntityName \"foo_v2\" } :: DatabaseModification (DatabaseEntity be OtherDb) be OtherDb)", "11:70", "its dbModification { … } carries a type annotation") ]
  where
    baseReason = "its dbModification carries a type annotation or type application"
    refused (name, exts, line, at, why) =
      expectUncaptured name [(at, why)] =<< planFix defaultCliOptions ("EntityName" <> name) exts (entityNameModule [line])

-- | Every record of the module counts, whatever makes it a Database, and a
-- field that is not a TableEntity elsewhere is not resolved.
testEntityNameFieldOfAnyRecord :: IO ()
testEntityNameFieldOfAnyRecord = mapM_ refused
  [ ("StandaloneInstance", [], ["data Db2 f = Db2 { foos :: f (TableEntity BarT) } deriving Generic", "instance Database be Db2"], "13:69", "(BarT, FooT)")
  , ("StandaloneDeriving", ["StandaloneDeriving", "DerivingStrategies", "DeriveAnyClass"],
      ["data Db2 f = Db2 { foos :: f (TableEntity BarT) } deriving Generic", "deriving anyclass instance Database be Db2"], "13:69", "(BarT, FooT)")
  , ("ViewField", [], ["data Db2 f = Db2 { foos :: f (ViewEntity FooV) } deriving (Generic, Database)"], "12:69", "(FooT, a type other than TableEntity)")
  , ("PlainRecord", [], ["data Config = Config { foos :: Int }"], "12:69", "(FooT, a type other than TableEntity)")
  , ("PatSynField", ["PatternSynonyms"], ["pattern P{foos} <- Just foos"], "12:69", "(FooT, a type other than TableEntity)")
  , ("GadtRecord", ["GADTs"], ["data G where G :: { foos :: f (TableEntity BarT) } -> G"], "12:69", "(BarT, FooT)")
  , ("DataInstance", ["TypeFamilies"], ["data instance Fam Int = FamI { foos :: Int }"], "12:69", "(FooT, a type other than TableEntity)") ]
  where
    refused (name, exts, records, at, types) =
      expectUncaptured name [(at, "its field 'foos' has more than one type, or one other than f (TableEntity T), across this module's records " <> types)]
        =<< planFix defaultCliOptions ("EntityName" <> name) exts
          (entityNameModule (records <> ["db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"]))

-- | The parser leaves fixities unresolved, so a @<>@ chain is read only when its
-- other operands are known not to rename: GHC applies @$@ last, and an opaque
-- operand left of setEntityName runs after it.
testEntityNameChainOperandsFail :: IO ()
testEntityNameChainOperandsFail = mapM_ refused
  [ ("DollarChain", [], "const mempty $ modifyTableFields fooTMod <> setEntityName \"foo_v2\"", "11:113")
  , ("ApplyIfChain", ["applyIf c m = if c then m else mempty"], "applyIf False $ modifyTableFields fooTMod <> setEntityName \"foo_v2\"", "12:114")
  , ("OpaqueOperand", [], "otherEMod <> setEntityName \"foo_v2\"", "11:82") ]
  where
    refused (name, before, value, at) =
      expectUncaptured name [(at, "its '<>' chain has an operand other than 'modifyTableFields x' or 'setEntitySchema x'")]
        =<< planFix defaultCliOptions ("EntityName" <> name) []
          (entityNameModule (before <> ["db = defaultDbSettings `withDbModification` dbModification { foos = " <> value <> " }"]))

-- | A module that binds a name a rename's meaning rests on, in any binder
-- form, may have given it another meaning.
testEntityNameShadowedFails :: IO ()
testEntityNameShadowedFails = do
  -- A local (<>) refuses the chain's modifyTableFields capture too.
  expectErrors "ShadowMappend"
    [ ("UNCAPTURED_SET_ENTITY_NAME", "12:69", "this module binds '<>' itself")
    , ("UNCAPTURED_MODIFY_TABLE_FIELDS", "12:95", "this module binds '<>' itself") ]
    =<< planFix defaultCliOptions "EntityNameShadowMappend" ["PatternSynonyms", "RecordWildCards"]
      (entityNameModule ["_ <> b = b", dbLine "setEntityName \"foo_v2\" <> modifyTableFields fooTMod"])
  mapM_ shadowed
    [ ("ShadowWhere", [dbLine "setEntityName \"foo_v2\"", "  where setEntityName _ = mempty"], "11:69", "'setEntityName'")
    , ("ShadowLet", ["db = let setEntityName _ = mempty in defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"], "11:101", "'setEntityName'")
    , ("ShadowTop", ["setEntityName _ = mempty", dbLine "setEntityName \"foo_v2\""], "12:69", "'setEntityName'")
    , ("ShadowParameter", ["db setEntityName = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"], "11:83", "'setEntityName'")
    , ("ShadowField", ["data Cfg = Cfg { dbModification :: Int }", dbLine "setEntityName \"foo_v2\""], "12:69", "'dbModification'")
    , ("ShadowClassMethod", ["class Renames a where", "  withDbModification :: a", dbLine "setEntityName \"foo_v2\""], "13:69", "'withDbModification'")
    , ("ShadowForeign", ["foreign import ccall \"f\" setEntitySchema :: Int", dbLine "setEntityName \"foo_v2\""], "12:69", "'setEntitySchema'")
    , ("ShadowPattern", ["(modifyTableFields, unused) = (id, ())", dbLine "setEntityName \"foo_v2\""], "12:69", "'modifyTableFields'")
    , ("ShadowPatSynField", ["pattern P{setEntityName} <- Just setEntityName", dbLine "setEntityName \"foo_v2\""], "12:69", "'setEntityName'")
    , ("ShadowPatSynWildcard", ["pattern P{setEntityName} <- Renames{..}", dbLine "setEntityName \"foo_v2\""], "12:69", "'setEntityName'")
    , ("ShadowAssociatedField", ["class K a where", "  data D a", "instance K Int where", "  data D Int = D { setEntityName :: Int }", dbLine "setEntityName \"foo_v2\""], "15:69", "'setEntityName'") ]
  -- An instance method defines its class's method and binds no name.
  capturedFooV2 "EntityNameSemigroupInstance"
    ["instance Semigroup X where", "  _ <> b = b", dbLine "setEntityName \"foo_v2\" <> modifyTableFields fooTMod"]
  where
    dbLine value = "db = defaultDbSettings `withDbModification` dbModification { foos = " <> value <> " }"
    shadowed (name, rest, at, bound) =
      expectUncaptured name [(at, "this module binds " <> bound <> " itself")]
        =<< planFix defaultCliOptions ("EntityName" <> name) ["PatternSynonyms", "RecordWildCards", "TypeFamilies"] (entityNameModule rest)

-- | Only a dbModification applied by withDbModification to a binding's result
-- is read: one handed to another function may never be applied.
testEntityNameNotAppliedFails :: IO ()
testEntityNameNotAppliedFails = do
  expectUncaptured "IgnoredModification" [("11:77", notAppliedModification)]
    =<< planFix defaultCliOptions "EntityNameIgnoredModification" []
      (entityNameModule
        [ "db = defaultDbSettings `withDbModification` ignore (dbModification { foos = setEntityName \"foo_v2\" })"
        , "ignore _ = dbModification" ])
  expectUncaptured "DiscardedModification" [("11:55", notAppliedModification)]
    =<< planFix defaultCliOptions "EntityNameDiscardedModification" []
      (entityNameModule ["db = const defaultDbSettings (dbModification { foos = setEntityName \"foo_v2\" })"])
  capturedFooV2 "EntityNamePrefixApplied"
    ["db = withDbModification defaultDbSettings dbModification { foos = setEntityName \"foo_v2\" }"]
  capturedFooV2 "EntityNamePrefixQualified"
    ["db = B.withDbModification B.defaultDbSettings (B.dbModification { foos = B.setEntityName \"foo_v2\" })"]
  capturedFooV2 "EntityNameParenPartial"
    ["db = (withDbModification defaultDbSettings) dbModification { foos = setEntityName \"foo_v2\" }"]
  capturedFooV2 "EntityNameUnderLambda"
    ["db = \\schema -> defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"]

-- | A field value with a setEntityName in it that is not one of the read shapes.
testEntityNameUnreadValueFails :: IO ()
testEntityNameUnreadValueFails = mapM_ refused
  [ ("UnreadDollar", "setEntityName $ \"foo_v2\"", "11:69")
  , ("UnreadMconcat", "mconcat [setEntityName \"foo_v2\"]", "11:78")
  , ("UnreadId", "id (setEntityName \"foo_v2\")", "11:73") ]
  where
    refused (name, value, at) =
      expectUncaptured name [(at, "the value of field 'foos' is not setEntityName applied to a string literal")]
        =<< planFix defaultCliOptions ("EntityName" <> name) []
          (entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { foos = " <> value <> " }"])

-- | With OverloadedRecordUpdate the dbModification's fields are record-dot
-- updates, which carry no plain label.
testEntityNameRecordDotUpdateFails :: IO ()
testEntityNameRecordDotUpdateFails =
  expectUncaptured "RecordDotUpdate" [("11:69", "is not a plain 'field = value' update")]
    =<< planFix defaultCliOptions "EntityNameRecordDotUpdate" ["OverloadedRecordUpdate", "RebindableSyntax"]
      (entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"])

-- | The name is read under parentheses and a type annotation.
testEntityNameNamePeels :: IO ()
testEntityNameNamePeels = do
  capturedFooV2 "EntityNameParenthesised"
    ["db = defaultDbSettings `withDbModification` dbModification { foos = (setEntityName) \"foo_v2\" }"]
  capturedFooV2 "EntityNameAnnotatedName"
    ["db = defaultDbSettings `withDbModification` dbModification { foos = (setEntityName :: Text -> EntityModification f be t) \"foo_v2\" }"]

-- | A punned record field of a watched name is a use of that name: its value
-- is a parser placeholder, not a variable.
testWatchedNamePunsCounted :: IO ()
testWatchedNamePunsCounted = do
  expectUncaptured "PunnedSetEntityName" [("11:21", notInModification), ("12:17", notInModification)]
    =<< planFix defaultCliOptions "EntityNamePunned" ["NamedFieldPuns"]
      (entityNameModule ["renamer = Renames { setEntityName }", "renamed r = r { setEntityName }"])
  expectTableModErrors "PunnedTableModifications"
    [Uncaptured "11:15" notApplied, Renamer "12:11" "modifyEntityName"]
    =<< planFix defaultCliOptions "TableModificationsPunned" ["NamedFieldPuns"]
      (entityNameModule ["mods = Mods { modifyTableFields }", "rn = Rn { modifyEntityName }"])

-- | Uses that share a position are counted one by one: two placed at one line
-- by line directives, and a declaration a rewriting plugin could duplicate
-- (identical spans; the subtraction of captured uses is a multiset one).
testEntityNameSameSpanCountedTwice :: IO ()
testEntityNameSameSpanCountedTwice = do
  expectUncaptured "LineDirectives" [("40:69", "the value of field 'foos' is not setEntityName applied to a string literal")]
    =<< planFix defaultCliOptions "EntityNameLineDirectives" []
      (entityNameModule
        [ "# 40 \"Fix/EntityNameLineDirectives.hs\""
        , "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"
        , "# 40 \"Fix/EntityNameLineDirectives.hs\""
        , "dX = defaultDbSettings `withDbModification` dbModification { foos = setEntityName $ \"foo_v2\" }" ])
  let modName = "Fix.EntityNameDuplicated"
  decls <- parseFixture modName [] $ unlines $ ("module " <> modName <> " where")
    : entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"]
  expectUncaptured "DuplicatedDeclaration" [("11:69", "")]
    (planFragment defaultCliOptions HsSrcFile modName "f.hs" (decls <> [last decls]))

-- | GHC resolves an unannotated update to the record that declares all its
-- labels, which may be an imported record that shares the renamed field unless
-- one record of this module declares them all.
testEntityNameForeignLabelFails :: IO ()
testEntityNameForeignLabelFails = do
  mapM_ refused
    [ ("ForeignLabel", [], [update "extra"], "11:69", noRecord)
    , ("SplitLabels", [], ["data Cfg = Cfg { extra :: Int }", update "extra"], "12:69", noRecord)
    , ("QuotedRecord", ["TemplateHaskell"], ["quoted = [d| " <> db2 <> " |]", update "extra"], "12:69", noRecord)
    , ("QualifiedOtherLabel", [], [db2, update "O.extra"], "12:69", "its dbModification's label 'O.extra' is qualified with another module")
    , ("PatSynLabels", ["PatternSynonyms"], ["pattern P{foos, extra} <- (foos, extra)", update "extra"], "12:69", "its field 'foos' has more than one type") ]
  capturedFooV2 "EntityNameLocalLabels" [db2, update "extra"]
  where
    db2 = "data Db2 f = Db2 { foos :: f (TableEntity FooT), extra :: f (TableEntity BarT) } deriving (Generic, Database)"
    update l = "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\", " <> l <> " = mempty }"
    noRecord = "no single record of this module declares all of its dbModification's labels ('foos', 'extra')"
    refused (name, exts, rest, at, why) =
      expectUncaptured name [(at, why)]
        =<< planFix defaultCliOptions ("EntityName" <> name) ("DuplicateRecordFields" : exts) (entityNameModule rest)

-- | A record wildcard puts every field of its record in scope, so it may
-- rebind a name a capture relies on: in the use's declaration, or module-wide
-- from a top-level pattern binding or a top-level splice.
testEntityNameUnderRecordWildcardFails :: IO ()
testEntityNameUnderRecordWildcardFails = do
  mapM_ refused
    [ ("WildcardParam", ["db Renames{..} = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"], "11:81")
    , ("WildcardQualified", ["dbW2 M.Renames{..} = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"], "11:85")
    , ("WildcardTopPattern", ["Renames{..} = renames", dbLine], "12:69") ]
  expectUncaptured "SpliceQuoteWildcard" [("12:69", wildcard)]
    =<< planFix defaultCliOptions "EntityNameSpliceQuoteWildcard" ["RecordWildCards", "TemplateHaskell"]
      (entityNameModule ["$( [d| R.Renames{..} = renames |] )", dbLine])
  expectErrors "WildcardOps"
    [("UNCAPTURED_SET_ENTITY_NAME", "11:106", wildcard), ("UNCAPTURED_MODIFY_TABLE_FIELDS", "11:77", wildcard)]
    =<< planFix defaultCliOptions "EntityNameWildcardOps" ["RecordWildCards"]
      (entityNameModule
        ["db Ops{..} = defaultDbSettings `withDbModification` dbModification { foos = modifyTableFields fooTMod <> setEntityName \"foo_v2\" }"])
  expectTableModErrors "WildcardModifier" [Uncaptured "11:22" wildcard]
    =<< planFix defaultCliOptions "ModifyTableFieldsWildcard" ["RecordWildCards"]
      (entityNameModule ["fooEMod M.Mods{..} = modifyTableFields fooTMod"])
  capturedFooV2With ["RecordWildCards"] "EntityNameWildcardElsewhere" ["unrelated Renames{..} = ()", dbLine]
  where
    wildcard = "a record wildcard '{..}'"
    dbLine = "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"
    refused (name, rest, at) =
      expectUncaptured name [(at, wildcard)] =<< planFix defaultCliOptions ("EntityName" <> name) ["RecordWildCards"] (entityNameModule rest)

-- | A record wildcard covers the uses its declaration holds, wherever their
-- spans point: a line directive can split a declaration across files, and a
-- plugin that rewrote the module first may leave a use without a span.
testEntityNameUnplaceableSpansFail :: IO ()
testEntityNameUnplaceableSpansFail = do
  expectUncaptured "SplitDeclaration" [("1:27", wildcard)]
    =<< planFix defaultCliOptions "EntityNameSplitDeclaration" ["RecordWildCards"]
      (entityNameModule
        [ "db Renames{..} = defaultDbSettings `withDbModification`"
        , "# 1 \"Other.hs\""
        , "  dbModification { foos = setEntityName \"foo_v2\" }" ])
  expectTableModErrors "SplitModifierDeclaration" [Uncaptured "1:3" wildcard]
    =<< planFix defaultCliOptions "ModifyTableFieldsSplitDeclaration" ["RecordWildCards"]
      (entityNameModule ["fooEMod M.Mods{..} =", "# 1 \"Other.hs\"", "  modifyTableFields fooTMod"])
  let modName = "Fix.EntityNameUnplacedUse"
  decls <- parseFixture modName ["RecordWildCards"] $ unlines $ ("module " <> modName <> " where")
    : entityNameModule ["db Renames{..} = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"]
  expectUncaptured "UnplacedUse" [("<no location info>", wildcard)]
    (planFragment defaultCliOptions HsSrcFile modName "f.hs" (unplaced "setEntityName" decls))
  where
    wildcard = "a record wildcard '{..}'"

unplaced :: Data a => String -> a -> a
unplaced name = go
  where
    go :: forall d. Data d => d -> d
    go d = case cast d :: Maybe (LIdP GhcPs) of
      Just (L (SrcSpanAnn a _) n) | occNameString (rdrNameOcc n) == name ->
        fromMaybe d (cast (L (SrcSpanAnn a noSrcSpan) n))
      _ -> gmapT go d

-- | A later operand of withDbModification that isn't a dbModification update
-- runs after the rename and could change it.
testEntityNameLaterOperandFails :: IO ()
testEntityNameLaterOperandFails = mapM_ refused
  [ ("LaterOperand", "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" } `withDbModification` O.laterMods", "11:69")
  , ("LaterOperandPrefix", "db = withDbModification (withDbModification defaultDbSettings dbModification { foos = setEntityName \"foo_v2\" }) O.laterMods", "11:87") ]
  where
    refused (name, line, at) =
      expectUncaptured name [(at, "a later withDbModification operand is not a dbModification { … } update")]
        =<< planFix defaultCliOptions ("EntityName" <> name) [] (entityNameModule [line])

-- | A module that binds a name the DB-settings capture relies on may have given it another meaning.
testModifyTableFieldsTrustedNamesBoundFails :: IO ()
testModifyTableFieldsTrustedNamesBoundFails = do
  mapM_ refused
    [ ("LocalModifyTableFields", ["modifyTableFields _ = mempty", "fooEMod = modifyTableFields fooTMod"], "12:11", "'modifyTableFields'")
    , ("LocalFieldNamed", ["fieldNamed _ = B.fieldNamed \"y\"", "fooEMod = modifyTableFields fooTMod"], "12:11", "'fieldNamed'")
    , ("LocalTableModification", ["fooEMod = modifyTableFields fooTMod", "  where tableModification = B.tableModification"], "11:11", "'tableModification'")
    , ("LocalMappend", ["_ <> b = b", "fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)"], "12:18", "'<>'") ]
  sameAsWithoutEMod "ModifyTableFieldsSemigroupInstance" []
    ["instance Semigroup X where", "  _ <> b = b", "fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)"]
  where
    refused (name, rest, at, bound) =
      expectTableModErrors name [Uncaptured at ("this module binds " <> bound <> " itself")]
        =<< planFix defaultCliOptions ("ModifyTableFields" <> name) [] (entityNameModule rest)

testModifyTableFieldsTypeAppliedFails :: IO ()
testModifyTableFieldsTypeAppliedFails =
  expectTableModErrors "TypeApplied" [Uncaptured "11:11" "it has a type application"]
    =<< planFix defaultCliOptions "ModifyTableFieldsTypeApplied" ["TypeApplications"]
      (entityNameModule ["fooEMod = modifyTableFields @FooT fooTMod"])

-- | Under OverloadedRecordUpdate a pun's value has no location: each use is
-- reported at its field, so none is merged with another.
testOverloadedRecordUpdatePunsLocated :: IO ()
testOverloadedRecordUpdatePunsLocated = do
  expectErrors "ProjectionPuns"
    [ ("UNCAPTURED_SET_ENTITY_NAME", "11:17", notInModification), ("UNCAPTURED_SET_ENTITY_NAME", "12:22", notInModification)
    , ("UNCAPTURED_MODIFY_TABLE_FIELDS", "13:14", notApplied), ("UNSUPPORTED_ENTITY_RENAME", "14:12", "modifyEntityName") ]
    =<< planFix defaultCliOptions "EntityNameProjectionPuns" exts
      (entityNameModule
        [ "renamed r = r { setEntityName }", "renamedTwice r = r { setEntityName }"
        , "mods r = r { modifyTableFields }", "rn r = r { modifyEntityName }" ])
  expectUncaptured "ProjectionPunInModification" [("11:62", "is not a plain 'field = value' update")]
    =<< planFix defaultCliOptions "EntityNameProjectionPunInModification" exts
      (entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { setEntityName }"])
  where exts = ["OverloadedRecordUpdate", "RebindableSyntax", "NamedFieldPuns"]

-- | Fails unless the result is exactly these errors, in order: code, location,
-- and a part of the reason (the function's name for UNSUPPORTED_ENTITY_RENAME).
expectErrors :: String -> [(String, String, String)] -> Either [SqlSchemaError] FragmentAction -> IO ()
expectErrors label expected r = case r of
  Left errs | length errs == length expected, and (zipWith matches errs expected) -> pure ()
  _ -> failWith (label <> ": expected " <> show expected <> ", got " <> show r)
  where
    matches e (code, at, part) = case located e of
      Just (at', why) -> errorCode e == code && at' == at && part `isInfixOf` why
      Nothing         -> False
    located = \case
      UNCAPTURED_SET_ENTITY_NAME at why     -> Just (at, why)
      UNCAPTURED_MODIFY_TABLE_FIELDS at why -> Just (at, why)
      UNSUPPORTED_ENTITY_RENAME at fn       -> Just (at, fn)
      _                                     -> Nothing

capturedFooV2 :: String -> [String] -> IO ()
capturedFooV2 = capturedFooV2With []

capturedFooV2With :: [String] -> String -> [String] -> IO ()
capturedFooV2With exts name rest = do
  fragment <- fragmentOf name =<< planFix defaultCliOptions name exts (entityNameModule rest)
  assertEq name [("foos", "FooT", "foo_v2")]
    [(dbField o, dbTableType o, sqlName o) | o <- fragmentDbEntityOverrides fragment]

-- | Table FooT (modifier @fooTMod@) and DB record @Db@ with field @foos@ of
-- FooT, on lines 2–10; @rest@ starts on line 11.
entityNameModule :: [String] -> [String]
entityNameModule rest = modifierTable ["modelFieldModification = fooTMod"]
  ( "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
  : "data Db f = Db { foos :: f (TableEntity FooT) } deriving (Generic, Database)"
  : rest )

notInModification, notAppliedModification :: String
notInModification      = "it is not in a field of a dbModification { … }"
notAppliedModification = "its dbModification { … } is not the right operand of withDbModification in the result of a top-level binding"

-- | Fails unless the result is exactly these UNCAPTURED_SET_ENTITY_NAME errors: location and a part of the reason.
expectUncaptured :: String -> [(String, String)] -> Either [SqlSchemaError] FragmentAction -> IO ()
expectUncaptured label expected r = case r of
  Left errs | length errs == length expected, and (zipWith matches errs expected) -> pure ()
  _ -> failWith (label <> ": expected UNCAPTURED_SET_ENTITY_NAME at " <> show expected <> ", got " <> show r)
  where
    matches (UNCAPTURED_SET_ENTITY_NAME at why) (at', part) = at == at' && part `isInfixOf` why
    matches _ _ = False

fragmentOf :: String -> Either [SqlSchemaError] FragmentAction -> IO Fragment
fragmentOf label = \case
  Right (Stage (Just fragment)) -> pure fragment
  other -> failWith (label <> ": expected a fragment, got " <> show other)

-- | The real EMods' shape, and the argument under parentheses and a signature.
testModifyTableFieldsSameBindingOk :: IO ()
testModifyTableFieldsSameBindingOk = sameAsWithoutEMod "ModifyTableFieldsSameBinding" []
  [ "fooTMod :: FooT (B.FieldModification (B.TableField FooT))"
  , "fooEMod :: Text -> B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FooT)"
  , "fooEMod schema = B.modifyTableFields fooTMod <> setEntitySchema (Just schema)"
  , "fooEModNoSchema = B.modifyTableFields (fooTMod :: FooT (B.FieldModification (B.TableField FooT)))" ]

testModifyTableFieldsMultilineOk :: IO ()
testModifyTableFieldsMultilineOk = do
  sameAsWithoutEMod "ModifyTableFieldsMultiline" []
    [ "fooEMod schema = B.modifyTableFields fooTMod"
    , "  <> setEntitySchema (Just schema)" ]
  sameAsWithoutEMod "ModifyTableFieldsMultilineBody" []
    [ "fooEMod schema ="
    , "  modifyTableFields fooTMod"
    , "    P.<> STables.setEntitySchema (P.Just schema)" ]

testModifyTableFieldsWhereOk :: IO ()
testModifyTableFieldsWhereOk = sameAsWithoutEMod "ModifyTableFieldsWhere" ["ScopedTypeVariables"]
  [ "fooEMod :: forall be db. Text -> B.EntityModification (DatabaseEntity be db) be (B.TableEntity FooT)"
  , "fooEMod schema = B.modifyTableFields fooTMod <> go"
  , "  where"
  , "    go :: EntityModification (DatabaseEntity be db) be (B.TableEntity FooT)"
  , "    go = setEntitySchema (Just schema)" ]

-- | An import list is not in the declarations, so naming a function there is no use of it.
testModifyTableFieldsInImportIgnored :: IO ()
testModifyTableFieldsInImportIgnored = do
  let name = "ModifyTableFieldsInImport"
  without <- planFix defaultCliOptions name [] (entityNameModule [])
  _ <- fragmentOf name without
  assertEq name without =<< planFix defaultCliOptions name []
    ( "import Database.Beam (modifyTableFields, modifyTable, modifyEntityName, withTableModification, renamingFields)"
    : "import Database.Beam.Schema.Tables (EntityModification (..), DatabaseEntity (..), IsDatabaseEntity (dbEntityName), dbTableCurrentName)"
    : entityNameModule [] )

testModifyTableFieldsOtherBindingFails :: IO ()
testModifyTableFieldsOtherBindingFails =
  expectTableModErrors "OtherBinding" [Uncaptured "12:18" ("'otherTMod' " <> notTheModifier)]
    =<< planFix defaultCliOptions "ModifyTableFieldsOtherBinding" []
      (entityNameModule
        [ "otherTMod = tableModification { fooName = fieldNamed \"x\" }"
        , "fooEMod schema = modifyTableFields otherTMod <> setEntitySchema (Just schema)" ])

testModifyTableFieldsInlineFails :: IO ()
testModifyTableFieldsInlineFails =
  expectTableModErrors "Inline" [Uncaptured "11:18" notUnqualified]
    =<< planFix defaultCliOptions "ModifyTableFieldsInline" []
      (entityNameModule
        [ "fooEMod schema = modifyTableFields tableModification { fooName = fieldNamed \"x\" } <> setEntitySchema (Just schema)" ])

-- | A bare tableModification renames nothing, so there is nothing to apply.
testModifyTableFieldsBareModifierFails :: IO ()
testModifyTableFieldsBareModifierFails = do
  let name = "ModifyTableFieldsBareModifier"
      fixture rest = planFix defaultCliOptions name []
        (modifierTable ["modelFieldModification = fooTMod"] ("fooTMod = tableModification" : rest))
  _ <- fragmentOf (name <> " without the EMod") =<< fixture []
  expectTableModErrors "BareModifier" [Uncaptured "10:18" ("'fooTMod' " <> noFields)]
    =<< fixture ["fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)"]

testModifyTableFieldsQualifiedFails :: IO ()
testModifyTableFieldsQualifiedFails =
  expectTableModErrors "Qualified" [Uncaptured "11:18" notUnqualified]
    =<< planFix defaultCliOptions "ModifyTableFieldsQualified" []
      (entityNameModule
        [ "fooEMod schema = modifyTableFields M.fooTMod <> setEntitySchema (Just schema)" ])

testModifyTableFieldsUnappliedFails :: IO ()
testModifyTableFieldsUnappliedFails =
  expectTableModErrors "Unapplied" [Uncaptured "11:18" notApplied, Uncaptured "12:19" notApplied]
    =<< planFix defaultCliOptions "ModifyTableFieldsUnapplied" []
      (entityNameModule
        [ "fooEMod schema = modifyTableFields $ fooTMod"
        , "allEMods ms = map modifyTableFields ms" ])

-- | No table of the module has the binding as its modifier.
testModifyTableFieldsOtherModuleTableFails :: IO ()
testModifyTableFieldsOtherModuleTableFails =
  expectTableModErrors "OtherModuleTable" [Uncaptured "3:18" ("'fooTMod' " <> notTheModifier)]
    =<< planFix defaultCliOptions "ModifyTableFieldsOtherModuleTable" []
      [ "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
      , "fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)" ]

-- | A use may name a local binder of the modifier's name instead of the modifier.
testModifyTableFieldsShadowedBindingFails :: IO ()
testModifyTableFieldsShadowedBindingFails = mapM_ shadowed
  [ ("ShadowedByParameter", [], ["fooEMod fooTMod = modifyTableFields fooTMod"], "11:19")
  , ("ShadowedByWhere", [],
      [ "fooEMod = modifyTableFields fooTMod"
      , "  where fooTMod = tableModification { fooName = fieldNamed \"x\" }" ], "11:11")
  , ("ShadowedByLet", [],
      ["fooEMod = let fooTMod = tableModification { fooName = fieldNamed \"x\" } in modifyTableFields fooTMod"], "11:75")
  , ("ShadowedByAsPattern", [], ["fooEMod fooTMod@_ = modifyTableFields fooTMod"], "11:21")
  , ("ShadowedByPun", ["NamedFieldPuns"], ["fooEMod Mods{fooTMod} = modifyTableFields fooTMod"], "11:25")
  , ("ShadowedByRule", [],
      ["{-# RULES \"keep\" forall fooTMod. keep fooTMod = modifyTableFields fooTMod #-}"], "11:49") ]
  where
    shadowed (name, exts, rest, at) =
      expectTableModErrors name [Uncaptured at ("'fooTMod' " <> boundAgain)]
        =<< planFix defaultCliOptions ("ModifyTableFields" <> name) exts (entityNameModule rest)

testEntityRenamersFail :: IO ()
testEntityRenamersFail =
  expectTableModErrors "Renamers"
    [ Renamer "12:10" "modifyTable", Renamer "11:11" "modifyEntityName"
    , Renamer "13:13" "withTableModification", Renamer "14:10" "renamingFields" ]
    =<< planFix defaultCliOptions "EntityRenamers" []
      (entityNameModule
        [ "renamed = modifyEntityName (<> \"_v2\")"
        , "legacy = modifyTable id fooTMod"
        , "applied s = withTableModification fooTMod s"
        , "snaked = renamingFields snakeCase" ])

-- | A hand-written modification renames a table with no function the plugin
-- reads; each use of an entity internal, in an expression or a pattern, is refused.
testEntityInternalsFail :: IO ()
testEntityInternalsFail = mapM_ refused
  [ ("EntityModificationUse", ["custom = EntityModification (Endo id)", "unwrap (EntityModification e) = e"],
      [Renamer "11:10" "EntityModification", Renamer "12:9" "EntityModification"])
  , ("DatabaseEntityUse", ["rebuilt d = DatabaseEntity d", "described (DatabaseEntity d) = d", "blank = DatabaseEntity {}"],
      [Renamer "11:13" "DatabaseEntity", Renamer "12:12" "DatabaseEntity", Renamer "13:9" "DatabaseEntity"])
  , ("DbEntityNameUse", ["renamed d = d & B.dbEntityName .~ \"x\""], [Renamer "11:17" "dbEntityName"])
  , ("DbTableCurrentNameUse", ["current = dbTableCurrentName"], [Renamer "11:11" "dbTableCurrentName"])
  , ("EntityRenamedByHand",
      ["byHand = EntityModification (Endo (\\(DatabaseEntity d) -> DatabaseEntity (d & dbEntityName .~ \"x\")))"],
      [ Renamer "11:10" "EntityModification", Renamer "11:59" "DatabaseEntity", Renamer "11:79" "dbEntityName"
      , Renamer "11:38" "DatabaseEntity" ]) ]
  where
    refused (name, rest, expected) =
      expectTableModErrors name expected =<< planFix defaultCliOptions name [] (entityNameModule rest)

-- | Type positions are not uses: the real EMods' signatures name both types.
testInternalsInTypesIgnored :: IO ()
testInternalsInTypesIgnored = sameAsWithoutEMod "InternalsInTypes" ["TypeApplications"]
  [ "type FooEntity be db = B.DatabaseEntity be db (B.TableEntity FooT)"
  , "fooEMod :: Text -> B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity FooT)"
  , "fooEMod schema = (B.modifyTableFields fooTMod :: EntityModification (DatabaseEntity be db) be (TableEntity FooT))"
  , "  <> setEntitySchema @(DatabaseEntity be db) (Just schema)"
  , "settings :: Db (B.DatabaseEntity be Db)"
  , "settings = defaultDbSettings" ]

-- | Under -haddock, doc comments that name the functions change nothing.
testTableModificationsUnderHaddock :: IO ()
testTableModificationsUnderHaddock = mapM_ same
  [ ("HaddockSameBinding", True,
      ["-- | Applies 'modifyTableFields' to 'fooTMod'.", "fooEMod schema = B.modifyTableFields fooTMod <> setEntitySchema (Just schema)"])
  , ("HaddockRefused", False,
      ["-- | Uses 'modifyEntityName' and 'DatabaseEntity'.", "fooEMod = modifyTableFields otherTMod", "renamed = modifyEntityName id"]) ]
  where
    same (name, accepted, rest) = do
      let modName = "Fix." <> name
          src     = unlines (("module " <> modName <> " where") : entityNameModule rest)
          plan    = planFragment defaultCliOptions HsSrcFile modName "f.hs"
      plain <- parseFixture modName [] src
      documented <- parseFixtureHaddock modName [] src
      unless (docDecls plain == 0 && docDecls documented > 0) $
        failWith (name <> ": expected doc declarations with -haddock only")
      assertEq name (plan plain) (plan documented)
      case plan documented of
        Right (Stage (Just _)) | accepted -> pure ()
        Left [_, _] | not accepted        -> pure ()
        other -> failWith (name <> ": unexpected " <> show other)

docDecls :: Data a => a -> Int
docDecls d = case cast d :: Maybe (HsDecl GhcPs) of
  Just DocD{} -> 1 + sum (gmapQ docDecls d)
  _           -> sum (gmapQ docDecls d)

-- | An instance method, a where or let binding, a Template Haskell quote and a name quote.
testTableModificationsSeenEverywhere :: IO ()
testTableModificationsSeenEverywhere =
  expectTableModErrors "Everywhere"
    [ Uncaptured "12:12" ("'otherTMod' " <> notTheModifier), Uncaptured "15:10" ("'otherTMod' " <> notTheModifier)
    , Renamer "16:22" "modifyTable", Renamer "13:24" "modifyEntityName", Renamer "14:18" "withTableModification" ]
    =<< planFix defaultCliOptions "TableModificationsEverywhere" ["TemplateHaskell"]
      (entityNameModule
        [ "instance HasDb Db where"
        , "  dbEMod = modifyTableFields otherTMod"
        , "viaWhere = m where m = modifyEntityName id"
        , "viaLet = let m = withTableModification fooTMod in m"
        , "$(gen [| modifyTableFields otherTMod |])"
        , "viaQuote = $(rename 'modifyTable)" ])

-- | Blacklisted modules and boot files are skipped whatever they hold.
testModifyTableFieldsBlacklistedModuleSkipped :: IO ()
testModifyTableFieldsBlacklistedModuleSkipped = do
  let name = "ModifyTableFieldsBlacklisted"
      refused = entityNameModule ["fooEMod = modifyTableFields otherTMod", "renamed = modifyEntityName id"]
  expectTableModErrors "not blacklisted"
    [Uncaptured "11:11" ("'otherTMod' " <> notTheModifier), Renamer "12:11" "modifyEntityName"]
    =<< planFix defaultCliOptions name [] refused
  assertEq "blacklisted" (Right (Stage Nothing))
    =<< planFix defaultCliOptions { blacklistModules = ["Fix." <> name] } name [] refused
  assertEq "hs-boot" (Right LeaveUntouched) =<< planFixAs HsBootFile defaultCliOptions name refused

testTableModificationMessages :: IO ()
testTableModificationMessages = do
  has (UNCAPTURED_MODIFY_TABLE_FIELDS "11:18" "the reason")
    ["modifyTableFields at 11:18: the reason.", "'modifyTableFields xxxTMod'", fix]
  has (UNSUPPORTED_ENTITY_RENAME "12:10" "modifyTable")
    ["modifyTable at 12:10: ", "setEntityName \"sql_name\" as the value of its dbModification { … } field", "'modifyTableFields xxxTMod'", fix]
  where
    fix = "pass the binding named in modelFieldModification; if no column is renamed, drop modifyTableFields"
    has e = mapM_ $ \part -> let msg = generateErrorMessage "d" e in
      unless (part `isInfixOf` msg) $ failWith (errorCode e <> " message lacks " <> show part <> ": " <> show msg)

-- | The fixture with @rest@ plans exactly as without it, to a fragment.
sameAsWithoutEMod :: String -> [String] -> [String] -> IO ()
sameAsWithoutEMod name exts rest = do
  without <- planFix defaultCliOptions name exts (entityNameModule [])
  _ <- fragmentOf (name <> " without the EMod") without
  assertEq name without =<< planFix defaultCliOptions name exts (entityNameModule rest)

notApplied, notUnqualified, notTheModifier, boundAgain, noFields :: String
notApplied     = "it is not applied directly to an argument"
notUnqualified = "its argument is not an unqualified variable"
notTheModifier = "is not the modelFieldModification binding of a table extracted from this module"
boundAgain     = "is also bound elsewhere in this module"
noFields       = "is not a 'tableModification { … }' record update with at least one field"

data ExpectedError = Uncaptured String String | Renamer String String
  deriving Show

-- | Fails unless the result is exactly these errors, in order.
expectTableModErrors :: String -> [ExpectedError] -> Either [SqlSchemaError] FragmentAction -> IO ()
expectTableModErrors label expected r = case r of
  Left errs | length errs == length expected, and (zipWith matches errs expected) -> pure ()
  _ -> failWith (label <> ": expected " <> show expected <> ", got " <> show r)
  where
    matches (UNCAPTURED_MODIFY_TABLE_FIELDS at why) (Uncaptured at' part) = at == at' && part `isInfixOf` why
    matches (UNSUPPORTED_ENTITY_RENAME at fn) (Renamer at' fn')           = at == at' && fn == fn'
    matches _ _ = False

-- | A blacklisted module's earlier fragment is deleted, even when its tables
-- could not be read.  An entry ending in "." matches every module it prefixes;
-- any other entry matches only the module of exactly that name.
testBlacklistStagesDelete :: IO ()
testBlacklistStagesDelete = do
  let plan entries name = planFix defaultCliOptions { blacklistModules = entries } name []
      skipped entries name =
        assertEq (show entries <> " skips Fix." <> name) (Right (Stage Nothing))
          =<< plan entries name controlTable
      kept entries name = do
        tables <- tablesOf (show entries <> " keeps Fix." <> name) =<< plan entries name controlTable
        assertEq (show entries <> " keeps Fix." <> name) ["FooT"] (map codeName tables)
  skipped ["Test.", "Fix.Blacklisted"] "Blacklisted"
  skipped ["Fix."] "Blacklisted"
  skipped ["Fix.Blacklisted."] "Blacklisted.Sub"
  kept ["Fix.Blacklisted"] "Blacklisted.Sub"
  kept ["Fix.Black"] "Blacklisted"
  kept ["Black"] "Blacklisted"
  kept ["Fix.Black."] "Blacklisted"
  kept ["Blacklisted"] "Blacklisted"
  kept ["Fix.Blacklisted."] "Blacklisted"
  assertEq "blacklisted, unreadable" (Right (Stage Nothing))
    =<< plan ["Fix.BlackUnreadable"] "BlackUnreadable" (modifierTable ["modelFieldModification = M.fooTMod"] [])
  forM_ ["", ".Foo"] $ \entry ->
    case parseCliOptions ["{\"blacklistModules\":[" <> show entry <> "]}"] of
      Left msg | show entry `isInfixOf` msg -> pure ()
      other -> failWith ("blacklist entry " <> show entry <> ": expected an error naming it, got " <> show other)

testPluginOptions :: IO ()
testPluginOptions = do
  cli <- expectOk (parseCliOptions
    [ "{\"fragmentsDir\":\"./dist/build/sql-schema-fragments\""
      <> ",\"blacklistModules\":[\"Test.\",\"Spec.\",\"App.Db.Types.LocalTest\"]"
      <> ",\"tableNameOverrides\":{\"FooT\":\"foo\"}}" ])
  assertEq "fragmentsDir" "./dist/build/sql-schema-fragments" (fragmentsDir cli)
  assertEq "blacklistModules" ["Test.", "Spec.", "App.Db.Types.LocalTest"] (blacklistModules cli)
  assertEq "allow flags" (False, False) (allowUnknownColumnWrapper cli, allowNonStandardModifier cli)
  tables <- tablesOf "tableNameOverrides" =<< planFix cli "ConsumerOptions" []
    [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = SomeConst.fooName"
    ]
  assertEq "tableNameOverrides" ["foo"] (map modelTableName tables)
  case parseCliOptions ["{\"x\\u001b[2J\\nsql-schema: UNRESOLVABLE_TABLE_NAME: forged\":1}"] of
    Left msg | all (`notElem` msg) "\n\ESC" -> pure ()
    other -> failWith ("unknown key with a newline: expected a one-line error, got " <> show other)
  forM_ ["", ".", "./", "./."] $ \dir ->
    case parseCliOptions ["{\"fragmentsDir\":" <> show dir <> "}"] of
      Left msg | show dir `isInfixOf` msg -> pure ()
      other -> failWith ("fragmentsDir " <> show dir <> ": expected an error naming it, got " <> show other)

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
testErrorMessagesCarryCode = checkAll errorSamples
  where
    -- Every constructor of SqlSchemaError, read from its Generic Rep, needs a
    -- sample in errorSamples; the test fails naming any that has none.
    checkAll samples = do
      let names   = constructors (Proxy :: Proxy (Rep SqlSchemaError))
          sampled = map (constructorOf . from . fst) samples
          missing = [c | c <- names, c `notElem` sampled]
      unless (null missing) $
        failWith ("SqlSchemaError constructors without a sample: " <> unwords missing)
      unless (all (`elem` names) sampled) $
        failWith ("samples outside the enumerated constructors " <> show names)
      mapM_ check samples
    -- The literal pins each code; forcing the whole message also runs its
    -- errorText case, which plain -Wall only warns about when missing.
    check (e, code) = do
      assertEq "errorCode" code (errorCode e)
      unless (not (null code) && all (`elem` ['A' .. 'Z'] <> "_") code) $
        failWith ("code is not [A-Z_]+: " <> show code)
      let prefix = "sql-schema: " <> code <> ": "
          msg    = generateErrorMessage "d" e
      unless (prefix `isPrefixOf` msg && length msg > length prefix) $
        failWith ("message without its code prefix or text: " <> show msg)
      -- The backstop escapes a template newline not followed by two spaces.
      when ("\\n" `isInfixOf` msg) $
        failWith ("a template line is not a two-space continuation: " <> show msg)

errorSamples :: [(SqlSchemaError, String)]
errorSamples =
  [ (UNRESOLVABLE_TABLE_NAME "FooT", "UNRESOLVABLE_TABLE_NAME")
  , (UNRESOLVABLE_COLUMN_NAME "FooT" "fooName", "UNRESOLVABLE_COLUMN_NAME")
  , (UNRESOLVABLE_PRIMARY_KEY "FooT", "UNRESOLVABLE_PRIMARY_KEY")
  , (UNRECOGNIZED_FIELD_WRAPPER "FooT" "foo" "Int", "UNRECOGNIZED_FIELD_WRAPPER")
  , (UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod", "UNRECOGNIZED_MODIFIER_SHAPE")
  , (UNRESOLVABLE_MODIFIER_BINDING "FooT" "M.fooTMod", "UNRESOLVABLE_MODIFIER_BINDING")
  , (UNCAPTURED_SET_ENTITY_NAME "11:11" "its argument is not a string literal", "UNCAPTURED_SET_ENTITY_NAME")
  , (UNCAPTURED_MODIFY_TABLE_FIELDS "11:18" "its argument is not an unqualified variable", "UNCAPTURED_MODIFY_TABLE_FIELDS")
  , (UNSUPPORTED_ENTITY_RENAME "12:10" "modifyTable", "UNSUPPORTED_ENTITY_RENAME")
  , (INVALID_PLUGIN_OPTIONS "not json", "INVALID_PLUGIN_OPTIONS")
  , (STAGED_FRAGMENT_MISSING "d/Fix.Foo.yaml.pending", "STAGED_FRAGMENT_MISSING")
  ]

-- | For every constructor: values holding a newline, terminal escapes and
-- format or separator characters stay on their lines, escaped as written, and
-- values past the cap are cut without cutting the template.
testErrorFieldsEscaped :: IO ()
testErrorFieldsEscaped = forM_ errorSamples $ \(e, code) -> do
  let evil    = "x\n  sql-schema: FORGED: \ESC[31m\r\x202e\x2028\x2029"
      shown   = "x\\n  sql-schema: FORGED: \\ESC[31m\\r\\8238\\8232\\8233"
      long    = replicate 10000 'a'
      benign  = generateErrorMessage "d" e
      cut     = generateErrorMessage long (withFields (const long) e)
  -- beamDefaultColumnName keeps a value with an underscore whole, newline included.
  forM_ [(evil, shown), ("a_" <> evil, "a_" <> shown)] $ \(value, escaped) -> do
    let hostile = generateErrorMessage value (withFields (const value) e)
        forged  = [l | l <- drop 1 (lines hostile), "sql-schema:" `isPrefixOf` dropWhile (== ' ') l]
    unless (length (lines hostile) == length (lines benign) && null forged && escaped `isInfixOf` hostile
            && all (\c -> c == '\n' || not (unsafe c)) hostile) $
      failWith (code <> ": a hostile value is not one escaped line: " <> show hostile)
  unless (length (lines cut) == length (lines benign) && not ("[truncated]" `isSuffixOf` cut)
          && (replicate 500 'a' <> " … [truncated]") `isInfixOf` cut && not (replicate 501 'a' `isInfixOf` cut)) $
    failWith (code <> ": a long value is not cut to 500 characters, template kept: " <> show (take 300 cut))

-- | The template's own lines are unchanged, a value of 500 characters is kept
-- whole, and values full of escapes still meet the 4000-character message cap.
testErrorMessagesSanitized :: IO ()
testErrorMessagesSanitized = do
  assertEq "UNRESOLVABLE_PRIMARY_KEY message"
    (  "sql-schema: UNRESOLVABLE_PRIMARY_KEY: primaryKey for 'FooT' is not in a recognised shape.\n"
    <> "  Accepted forms:\n"
    <> "    primaryKey x = Con (f1 x) (f2 x) …      (function equation, any arity)\n"
    <> "    primaryKey   = \\x -> Con (f1 x) …       (explicit lambda)\n"
    <> "    primaryKey   = Con . field               (single-column composition)\n"
    <> "    primaryKey   = Con <$> f1 <*> f2 <*> …   (applicative composite)\n"
    <> "    primaryKey _ = ConNullary                (no-column PK)" )
    (generateErrorMessage "d" (UNRESOLVABLE_PRIMARY_KEY "FooT"))
  let atCap = generateErrorMessage "d" (UNRESOLVABLE_PRIMARY_KEY (replicate 500 'b'))
  unless (replicate 500 'b' `isInfixOf` atCap && not ("[truncated]" `isInfixOf` atCap)) $
    failWith ("a value of 500 characters is cut: " <> show (take 300 atCap))
  let escapes = replicate 500 '\x202e'
      huge    = generateErrorMessage "d" (UNRECOGNIZED_FIELD_WRAPPER escapes escapes escapes)
  unless (length huge <= 4000 && "sql-schema: UNRECOGNIZED_FIELD_WRAPPER: " `isPrefixOf` huge
          && "[truncated]" `isSuffixOf` huge) $
    failWith ("a message full of escapes is not cut: message length " <> show (length huge))

-- | The backstop for template text, which no message reaches with a raw
-- character: a newline survives only as a two-space continuation, and the cap
-- keeps 4000 characters and trims the cut before its marker.
testMessageBackstop :: IO ()
testMessageBackstop = do
  assertEq "continuation" "a\n  b" (escapeUnsafe "a\n  b")
  assertEq "one-space continuation" "a\\n b" (escapeUnsafe "a\n b")
  assertEq "bare newline" "a\\nb" (escapeUnsafe "a\nb")
  assertEq "escapes" "\\ESC[31m\\r\\8238\\8232\\8233x" (escapeUnsafe "\ESC[31m\r\x202e\x2028\x2029x")
  let marker = " … [truncated]"
  assertEq "4000 kept" (replicate 4000 'a') (capMessage (replicate 4000 'a'))
  assertEq "4001 cut" (replicate (4000 - length marker) 'a' <> marker) (capMessage (replicate 4001 'a'))
  assertEq "cut trimmed" (replicate 3980 'a' <> marker) (capMessage (replicate 3980 'a' <> replicate 30 ' '))

unsafe :: Char -> Bool
unsafe c = generalCategory c `elem` [Control, Format, LineSeparator, ParagraphSeparator]

-- | Through 'Generic', so every field of every constructor, a new one's too, is replaced.
withFields :: (String -> String) -> SqlSchemaError -> SqlSchemaError
withFields g = to . mapFields g . from

class Fields f where
  mapFields :: (String -> String) -> f p -> f p

instance Fields f => Fields (M1 i c f) where
  mapFields g (M1 x) = M1 (mapFields g x)

instance (Fields f, Fields g) => Fields (f :+: g) where
  mapFields g (L1 x) = L1 (mapFields g x)
  mapFields g (R1 x) = R1 (mapFields g x)

instance (Fields f, Fields g) => Fields (f :*: g) where
  mapFields g (a :*: b) = mapFields g a :*: mapFields g b

instance Fields (K1 i String) where
  mapFields g (K1 x) = K1 (g x)

-- | Types.hs keeps errorCode's Generic helper unexported, so the test reads
-- constructor names from 'Generic' metadata itself.
class Constructors f where
  constructors  :: Proxy f -> [String]
  constructorOf :: f p -> String

instance Constructors f => Constructors (D1 c f) where
  constructors _ = constructors (Proxy :: Proxy f)
  constructorOf (M1 x) = constructorOf x

instance (Constructors f, Constructors g) => Constructors (f :+: g) where
  constructors _ = constructors (Proxy :: Proxy f) <> constructors (Proxy :: Proxy g)
  constructorOf (L1 x) = constructorOf x
  constructorOf (R1 x) = constructorOf x

instance Constructor c => Constructors (C1 c f) where
  constructors _ = [conName (M1 Proxy :: M1 C c Proxy ())]
  constructorOf = conName

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
