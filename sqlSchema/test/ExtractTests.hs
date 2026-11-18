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
                                                        ParagraphSeparator), generalCategory, isAlphaNum)
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
import           SqlSchema.Extract    (FragmentAction (..), extensionErrors, planFragment, refusedExtensions)
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
  testStandaloneDerivingBeamable
  testPositionalTableFails
  testGadtTableFails
  testPositionalMixinFails
  testOrphanInstanceFails
  testNoBeamableMarkingFails
  testDatabaseRecordMarkedFails
  testPrimaryKeyBeamableNotATable
  testContextHeadsVisible
  testKindSignatureHeadVisible
  testQualifiedClassHeads
  testMissingModelMeta
  testDerivedMetaUnreadable
  testUnreadableInstanceHead
  testSchemaNameAccepted
  testSchemaNameRejected
  testTableTypeAccepted
  testTableTypeRejected
  testAbsentMetaFieldsAreNothing
  testDerivedMetaFieldsUnreadable
  testMetaFieldMessageListsOwnShapes
  testMetaFieldFixFitsInstance
  testDocumentedDerivingsRead
  testTableNamedPrimaryKey
  testKeyTypeInstancesFail
  testDuplicateInstancesFail
  testPromotedHeadUnreadable
  testUnextractedFixFitsReason
  testPunnedModifierFieldFails
  testOverloadedRecordUpdateModifierFails
  testPrimaryKeyFieldMustExist
  testPrimaryKeyFieldDroppedByOptOut
  testPrimaryKeyMessageListsShapes
  testWhereBindingsUnreadable
  testPatternAndRepeatedBindingsUnreadable
  testBindingRuleInMessages
  testRedefinedNamesFail
  testRedefinedNamesNotRead
  testNestedContextHeadsRead
  testDocumentedInstanceHeadRead
  testRefusedExtensions
  testRulesRefused
  testPatternSynonymNamesRefused
  testTypeNamesRefused
  testGenericMustBeStockDerived
  testTopLevelRecordWildcardRefused
  testUnsupportedMessagesNameTheirCause
  testPunnedInstanceBindingsUnreadable
  testImplicitParamBindingsUnreadable
  testColumnNameMessageCoversPun
  testTableNameMessageFix
  testNewRefusalsUnderHaddock
  testNewRefusalsHaveNoOptOut
  testLocalTableEntityRefused
  testModifyTableFieldsWhereModifierFails
  testInstanceAtBeamTypeBindsMethods
  testDerivedInstanceAtBeamTypeBindsMethods
  testWrittenBeamableMethodsRefused
  testReasonFixesEscaped

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

-- | A class default, a pattern synonym's builder and a rewrite rule, which also refuses the module.
testEntityNameInOtherDeclarationsFail :: IO ()
testEntityNameInOtherDeclarationsFail =
  assertEq "EntityNameInOtherDecls"
    (Left [ UNSUPPORTED_RULES "17:1", UNCAPTURED_SET_ENTITY_NAME "13:79" notAppliedModification
          , UNCAPTURED_SET_ENTITY_NAME "16:13" notInModification, UNCAPTURED_SET_ENTITY_NAME "17:39" notInModification ])
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
-- updates, which carry no plain label; the table's modifier is written so too.
testEntityNameRecordDotUpdateFails :: IO ()
testEntityNameRecordDotUpdateFails =
  assertEq "RecordDotUpdate"
    (Left [ UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod"
          , UNCAPTURED_SET_ENTITY_NAME "11:69" "its dbModification field is not a plain 'field = value' update (record-dot syntax or a pun), which the plugin does not read" ])
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
    , ("WildcardQualified", ["dbW2 M.Renames{..} = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"], "11:85") ]
  -- A top-level pattern binding with a wildcard also refuses the module.
  assertEq "WildcardTopPattern"
    (Left [ UNSUPPORTED_RECORD_WILDCARD "11:1"
          , UNCAPTURED_SET_ENTITY_NAME "12:69" "a record wildcard '{..}' in its declaration, a top-level pattern binding or a top-level splice may rebind setEntityName or a name the rename relies on" ])
    =<< planFix defaultCliOptions "EntityNameWildcardTopPattern" ["RecordWildCards"] (entityNameModule ["Renames{..} = renames", dbLine])
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
    , ("LocalTableModification", ["fooEMod = modifyTableFields fooTMod", "  where tableModification = B.tableModification"], "11:11", "'tableModification'")
    , ("LocalMappend", ["_ <> b = b", "fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)"], "12:18", "'<>'") ]
  -- The table's modifier reads fieldNamed too, so the table is refused as well.
  assertEq "LocalFieldNamed"
    (Left [REDEFINED_NAME "FooT" "fieldNamed", UNCAPTURED_MODIFY_TABLE_FIELDS "12:11" "this module binds 'fieldNamed' itself, so the use may not be Beam's"])
    =<< planFix defaultCliOptions "ModifyTableFieldsLocalFieldNamed" []
          (entityNameModule ["fieldNamed _ = B.fieldNamed \"y\"", "fooEMod = modifyTableFields fooTMod"])
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
-- reported at its field, so none is merged with another.  The table's modifier,
-- a record update too, is not read.
testOverloadedRecordUpdatePunsLocated :: IO ()
testOverloadedRecordUpdatePunsLocated = do
  expectErrors "ProjectionPuns"
    [ ("UNRECOGNIZED_MODIFIER_SHAPE", "", "fooTMod")
    , ("UNCAPTURED_SET_ENTITY_NAME", "11:17", notInModification), ("UNCAPTURED_SET_ENTITY_NAME", "12:22", notInModification)
    , ("UNCAPTURED_MODIFY_TABLE_FIELDS", "13:14", notApplied), ("UNSUPPORTED_ENTITY_RENAME", "14:12", "modifyEntityName") ]
    =<< planFix defaultCliOptions "EntityNameProjectionPuns" exts
      (entityNameModule
        [ "renamed r = r { setEntityName }", "renamedTwice r = r { setEntityName }"
        , "mods r = r { modifyTableFields }", "rn r = r { modifyEntityName }" ])
  expectErrors "ProjectionPunInModification"
    [("UNRECOGNIZED_MODIFIER_SHAPE", "", "fooTMod"), ("UNCAPTURED_SET_ENTITY_NAME", "11:62", "is not a plain 'field = value' update")]
    =<< planFix defaultCliOptions "EntityNameProjectionPunInModification" exts
      (entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { setEntityName }"])
  where exts = ["OverloadedRecordUpdate", "RebindableSyntax", "NamedFieldPuns"]

-- | Fails unless the result is exactly these errors, in order: code, location,
-- and a part of the reason (the function's name for UNSUPPORTED_ENTITY_RENAME;
-- for UNRECOGNIZED_MODIFIER_SHAPE, which has no location, the binding's).
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
      UNRECOGNIZED_MODIFIER_SHAPE _ binding -> Just ("", binding)
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
testModifyTableFieldsShadowedBindingFails = do
  mapM_ shadowed
    [ ("ShadowedByParameter", [], ["fooEMod fooTMod = modifyTableFields fooTMod"], "11:19")
    , ("ShadowedByWhere", [],
        [ "fooEMod = modifyTableFields fooTMod"
        , "  where fooTMod = tableModification { fooName = fieldNamed \"x\" }" ], "11:11")
    , ("ShadowedByLet", [],
        ["fooEMod = let fooTMod = tableModification { fooName = fieldNamed \"x\" } in modifyTableFields fooTMod"], "11:75")
    , ("ShadowedByAsPattern", [], ["fooEMod fooTMod@_ = modifyTableFields fooTMod"], "11:21")
    , ("ShadowedByPun", ["NamedFieldPuns"], ["fooEMod Mods{fooTMod} = modifyTableFields fooTMod"], "11:25") ]
  -- A rule also refuses the module.
  assertEq "ShadowedByRule"
    (Left [UNSUPPORTED_RULES "11:1", UNCAPTURED_MODIFY_TABLE_FIELDS "11:49" "'fooTMod' is also bound elsewhere in this module, so this use may not name the modifier binding"])
    =<< planFix defaultCliOptions "ModifyTableFieldsShadowedByRule" []
          (entityNameModule ["{-# RULES \"keep\" forall fooTMod. keep fooTMod = modifyTableFields fooTMod #-}"])
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
  forM_ ["", ".", "./", "./.", "a/..", "./a/..", "../x", "x/../y", "dist/../.."] $ \dir ->
    case parseCliOptions ["{\"fragmentsDir\":" <> show dir <> "}"] of
      Left msg | show dir `isInfixOf` msg -> pure ()
      other -> failWith ("fragmentsDir " <> show dir <> ": expected an error naming it, got " <> show other)
  -- The corpus's own spelling, and names with dots that are no '..' component.
  forM_ ["./dist/build/sql-schema-fragments", "dist/build/sql-schema-fragments", "a..b", "..x/y"] $ \dir ->
    case parseCliOptions ["{\"fragmentsDir\":" <> show dir <> "}"] of
      Right opts | fragmentsDir opts == dir -> pure ()
      other -> failWith ("fragmentsDir " <> show dir <> ": expected it accepted, got " <> show other)

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
  , (FRAGMENT_IO_ERROR "stage" "d/Fix.Foo.yaml.pending" "Too many levels of symbolic links", "FRAGMENT_IO_ERROR")
  , (UNEXTRACTED_TABLE "FooT" noDeclaration, "UNEXTRACTED_TABLE")
  , (MISSING_MODEL_META "FooT", "MISSING_MODEL_META")
  , (UNREADABLE_INSTANCE_HEAD "ModelMeta" "t", "UNREADABLE_INSTANCE_HEAD")
  , (UNRESOLVABLE_MODEL_META_FIELD "FooT" "modelSchemaName" "pure \"x\"", "UNRESOLVABLE_MODEL_META_FIELD")
  , (DUPLICATE_INSTANCE "ModelMeta" "FooT", "DUPLICATE_INSTANCE")
  , (UNKNOWN_PRIMARY_KEY_FIELD "FooT" "someHelper", "UNKNOWN_PRIMARY_KEY_FIELD")
  , (REDEFINED_NAME "FooT" "fieldNamed", "REDEFINED_NAME")
  , (UNSUPPORTED_EXTENSION "RebindableSyntax", "UNSUPPORTED_EXTENSION")
  , (UNSUPPORTED_RULES "14:1", "UNSUPPORTED_RULES")
  , (UNSUPPORTED_RECORD_WILDCARD "14:1", "UNSUPPORTED_RECORD_WILDCARD")
  ]

-- | For every constructor: values holding a newline, terminal escapes and
-- format or separator characters stay on their lines, escaped as written, and
-- values past the cap are cut without cutting the template.
testErrorFieldsEscaped :: IO ()
testErrorFieldsEscaped = forM_ errorSamples $ \(e, code) -> do
  let evil    = "x\n  sql-schema: FORGED: \ESC[31m\r\x202e\x2028\x2029"
      shown   = "x\\n  sql-schema: FORGED: \\ESC[31m\\r\\8238\\8232\\8233"
      -- M-2: a value's own backslash, before an "n" here, is escaped apart from an escaped newline.
      slashed      = "x\\n  sql-schema: FORGED"
      slashedShown = "x\\\\n  sql-schema: FORGED"
      long    = replicate 10000 'a'
      benign  = generateErrorMessage "d" e
      cut     = generateErrorMessage long (withFields (const long) e)
  -- beamDefaultColumnName keeps a value with an underscore whole, newline included.
  forM_ [(evil, shown), ("a_" <> evil, "a_" <> shown), (slashed, slashedShown), ("a_" <> slashed, "a_" <> slashedShown)] $ \(value, escaped) -> do
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
    (  "sql-schema: UNRESOLVABLE_PRIMARY_KEY: primaryKey for 'FooT' is missing or not in a shape the plugin reads.\n"
    <> "  It reads the primaryKey of the Table instance of 'FooT' in this module, written once as one equation, with no guards or where bindings, in one of these forms:\n"
    <> "    primaryKey x = Con (f1 x) (f2 x) …      (function equation, any arity)\n"
    <> "    primaryKey   = \\x -> Con (f1 x) …       (explicit lambda)\n"
    <> "    primaryKey   = Con . field               (single-column composition)\n"
    <> "    primaryKey   = Con . (.field)            (composition with a record-dot section)\n"
    <> "    primaryKey x = Con x.f1 x.f2             (record-dot fields, with or without OverloadedRecordDot)\n"
    <> "    primaryKey   = Con <$> f1 <*> f2 <*> …   (applicative composite)\n"
    <> "    primaryKey _ = ConNullary                (no-column key)\n"
    <> "  A guessed key could mark the wrong columns as the key; refusing to extract instead." )
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

-- | Standalone deriving marks a table as an inline deriving clause does (P01).
testStandaloneDerivingBeamable :: IO ()
testStandaloneDerivingBeamable = mapM_ check
  [ ("StandaloneAnyclass", "deriving anyclass instance Beamable FooT")
  , ("StandaloneDefault", "deriving instance Beamable FooT") ]
  where
    check (name, derivation) = do
      cols <- columnNames name =<< planFix defaultCliOptions name derivingExts
        [ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f (Maybe Text) }"
        , "  deriving (Generic)"
        , derivation
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , "instance ModelMeta FooT where"
        , "  modelTableName = \"foo\""
        , "  modelFieldModification = fooTMod"
        , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
        ]
      assertEq name [("fooId", "id"), ("fooName", "the_name")] cols

-- | P02.
testPositionalTableFails :: IO ()
testPositionalTableFails = do
  r <- planFix defaultCliOptions "Positional" []
    [ "data FooT f = Foo (Columnar f Int) (Columnar f (Maybe Text))"
    , "  deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
    , "  primaryKey (Foo i _) = FooId i"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    ]
  assertEq "positional" (Left [UNEXTRACTED_TABLE "FooT" notARecord]) r

-- | P03: the extractor reads only Haskell 98 record syntax.
testGadtTableFails :: IO ()
testGadtTableFails = do
  r <- planFix defaultCliOptions "Gadt" ["GADTs"]
    [ "data FooT f where"
    , "  Foo :: { fooId :: Columnar f Int, fooName :: Columnar f (Maybe Text) } -> FooT f"
    , "  deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    ]
  assertEq "GADT syntax" (Left [UNEXTRACTED_TABLE "FooT" notARecord]) r

-- | A Beamable type with no instances in the module still counts: the plugin
-- cannot tell a mixin from a table whose instances are elsewhere.
testPositionalMixinFails :: IO ()
testPositionalMixinFails = do
  r <- planFix defaultCliOptions "PositionalMixin" [] ["data MixT f = Mix (C f Int) deriving Beamable"]
  assertEq "positional mixin" (Left [UNEXTRACTED_TABLE "MixT" notARecord]) r

-- | An instance, written or derived, for a type declared elsewhere: each class
-- on its own marks the type.
testOrphanInstanceFails :: IO ()
testOrphanInstanceFails = mapM_ check
  [ ("OrphanTable",
      [ "instance Table OtherT where"
      , "  data PrimaryKey OtherT f = OtherId (C f Int) deriving (Generic, Beamable)"
      , "  primaryKey = OtherId . otherId" ])
  , ("OrphanDerivedTable", ["deriving anyclass instance Table OtherT"])
  , ("OrphanMeta", ["instance ModelMeta OtherT where", "  modelTableName = \"other\""])
  , ("OrphanBeamableInstance", ["instance Beamable OtherT"]) ]
  where
    check (name, body) =
      assertEq name (Left [UNEXTRACTED_TABLE "OtherT" noDeclaration])
        =<< planFix defaultCliOptions name derivingExts body

-- | P16 (Beamable in another module) and P17 (Beamable from a splice the
-- parser cannot see).
testNoBeamableMarkingFails :: IO ()
testNoBeamableMarkingFails = mapM_ check
  [ ("OrphanBeamable", [], [])
  , ("SpliceBeamable", ["TemplateHaskell"], ["$(deriveBeamable ''FooT)"]) ]
  where
    check (name, exts, extra) =
      assertEq name (Left [UNEXTRACTED_TABLE "FooT" noBeamable]) =<< planFix defaultCliOptions name exts
        ([ "data FooT f = Foo { fooId :: Columnar f Int } deriving (Generic)" ]
         <> extra <>
         [ "instance Table FooT where"
         , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
         , "  primaryKey = FooId . fooId"
         , "instance ModelMeta FooT where"
         , "  modelTableName = \"foo\"" ])

-- | A Database record is not read as a table, so a Beamable marking on it is refused.
testDatabaseRecordMarkedFails :: IO ()
testDatabaseRecordMarkedFails = do
  r <- planFix defaultCliOptions "DatabaseBeamable" []
    ["data AppDb f = AppDb { foos :: f (TableEntity FooT) } deriving (Generic, Database be, Beamable)"]
  assertEq "Database record" (Left [UNEXTRACTED_TABLE "AppDb" databaseRecord]) r

-- | A key's Beamable instance, derived or written, does not make it a table.
testPrimaryKeyBeamableNotATable :: IO ()
testPrimaryKeyBeamableNotATable = mapM_ check
  [ ("KeyDerived", "deriving anyclass instance Beamable (PrimaryKey FooT)")
  , ("KeyInstance", "instance Beamable (PrimaryKey FooT)")
  , ("KeyQualified", "instance B.Beamable (B.PrimaryKey FooT)") ]
  where
    check (name, keyBeamable) = do
      tables <- tablesOf name =<< planFix defaultCliOptions name derivingExts
        [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic)"
        , "  primaryKey = FooId . fooId"
        , keyBeamable
        , "instance ModelMeta FooT where"
        , "  modelTableName = \"foo\""
        ]
      assertEq name ["FooT"] (map codeName tables)

-- | P12: a ModelMeta instance with a context is read; its modifier is not skipped.
testContextHeadsVisible :: IO ()
testContextHeadsVisible = do
  overrides <- expectOk (parseCliOptions ["{\"tableNameOverrides\":{\"FooT\":\"foo\"}}"])
  let fixture name cli = planFix cli name []
        [ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f Text } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , "instance Show Int => ModelMeta FooT where"
        , "  modelTableName = SomeConst.fooName"
        , "  modelSchemaName = Just Config.getAppDbSchema"
        , "  modelFieldModification = fooTMod"
        , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
        ]
  assertEq "context head" (Left [UNRESOLVABLE_TABLE_NAME "FooT"]) =<< fixture "ContextHead" defaultCliOptions
  tables <- tablesOf "context head, override" =<< fixture "ContextHeadOverride" overrides
  assertEq "context head, override"
    [ TableSchema "FooT" "Fix.ContextHeadOverride" "foo" Nothing
        (Just (SchemaConfig "Config.getAppDbSchema")) (PrimaryKeyInfo "FooId" ["fooId"])
        [ ColumnInfo "fooId" "id" "Int" False True
        , ColumnInfo "fooName" "the_name" "Text" False False ] ]
    tables

-- | GHC accepts a kind signature around the whole head, and around its argument.
testKindSignatureHeadVisible :: IO ()
testKindSignatureHeadVisible = mapM_ check
  [ ("KindSignatureHead", "instance Table FooT where", "instance (ModelMeta FooT :: Constraint) where")
  , ("KindSignatureArgument", "instance Table (FooT :: (Type -> Type) -> Type) where", "instance ModelMeta (FooT :: (Type -> Type) -> Type) where") ]
  where
    check (name, tableHead, metaHead) = do
      cols <- columnNames name =<< planFix defaultCliOptions name ["KindSignatures"]
        [ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic, Beamable)"
        , tableHead
        , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , metaHead
        , "  modelTableName = \"foo\""
        , "  modelFieldModification = fooTMod"
        , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
        ]
      assertEq name [("fooId", "id"), ("fooName", "the_name")] cols

testQualifiedClassHeads :: IO ()
testQualifiedClassHeads = do
  tables <- tablesOf "qualified classes" =<< planFix defaultCliOptions "QualifiedClasses" []
    [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic)"
    , "instance B.Beamable FooT"
    , "instance B.Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, B.Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance Se.ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    ]
  assertEq "qualified classes" ["FooT"] (map codeName tables)

-- | P13: tableNameOverrides does not stand in for a missing ModelMeta.
testMissingModelMeta :: IO ()
testMissingModelMeta = do
  overrides <- expectOk (parseCliOptions ["{\"tableNameOverrides\":{\"FooT\":\"foo\"}}"])
  let fixture name cli = planFix cli name []
        [ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f Text } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        ]
  assertEq "no ModelMeta" (Left [MISSING_MODEL_META "FooT"]) =<< fixture "NoMeta" defaultCliOptions
  assertEq "no ModelMeta, override" (Left [MISSING_MODEL_META "FooT"]) =<< fixture "NoMetaOverride" overrides

-- | A derived ModelMeta instance, standalone or inline, is not missing, but
-- nothing in it can be read: deriving via could take its modifier from another type.
testDerivedMetaUnreadable :: IO ()
testDerivedMetaUnreadable = do
  overrides <- expectOk (parseCliOptions ["{\"tableNameOverrides\":{\"FooT\":\"foo\"}}"])
  let fixture name derivings standalone cli = planFix cli name derivingExts
        ([ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f Text } " <> derivings
         , "instance Table FooT where"
         , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
         , "  primaryKey = FooId . fooId"
         ] <> standalone)
      standaloneMeta name = fixture name "deriving (Generic, Beamable)" ["deriving anyclass instance ModelMeta FooT"]
      inlineMeta name = fixture name "deriving (Generic, Beamable) deriving anyclass (ModelMeta)" []
  assertEq "derived ModelMeta" (Left [UNRESOLVABLE_TABLE_NAME "FooT"]) =<< standaloneMeta "DerivedMeta" defaultCliOptions
  assertEq "derived ModelMeta, override"
    (Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"])
    =<< standaloneMeta "DerivedMetaOverride" overrides
  assertEq "inline-derived ModelMeta" (Left [UNRESOLVABLE_TABLE_NAME "FooT"])
    =<< inlineMeta "InlineDerivedMeta" defaultCliOptions
  assertEq "inline-derived ModelMeta, override"
    (Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"])
    =<< inlineMeta "InlineDerivedMetaOverride" overrides

-- | Heads naming no type of this module: a type variable, and a qualified type
-- whose instance a same-named local type would otherwise take.
testUnreadableInstanceHead :: IO ()
testUnreadableInstanceHead = do
  assertEq "type variable" (Left [UNREADABLE_INSTANCE_HEAD "ModelMeta" "t"])
    =<< planFix defaultCliOptions "TyVarHead" [] ["instance Table t => ModelMeta t"]
  assertEq "qualified type" (Left [UNREADABLE_INSTANCE_HEAD "ModelMeta" "M.FooT"])
    =<< planFix defaultCliOptions "QualifiedHead" []
          (controlTable <> ["instance ModelMeta M.FooT where", "  modelTableName = \"other\""])

testSchemaNameAccepted :: IO ()
testSchemaNameAccepted = mapM_ check
  [ ("SchemaNothing", "Nothing", Nothing)
  , ("SchemaNothingSig", "(P.Nothing :: Maybe Text)", Nothing)
  , ("SchemaLiteral", "Just \"public\"", public)
  , ("SchemaLiteralParens", "Just (\"public\")", public)
  , ("SchemaParens", "(Just \"public\")", public)
  , ("SchemaSig", "Just \"public\" :: Maybe Text", public)
  , ("SchemaInnerSig", "Just (\"public\" :: Text)", public)
  , ("SchemaQualifiedName", "Just Config.getAppDbSchema", Just (SchemaConfig "Config.getAppDbSchema"))
  , ("SchemaQualifiedJust", "P.Just x", Just (SchemaConfig "x")) ]
  where
    public = Just (SchemaLiteral "public")
    check (name, rhs, expected) = do
      tables <- tablesOf name =<< modifierFix defaultCliOptions name ["modelSchemaName = " <> rhs] []
      assertEq name [expected] (map modelSchemaName tables)

-- | P08, P09 and the other modelSchemaName bindings the plugin cannot read.
testSchemaNameRejected :: IO ()
testSchemaNameRejected = do
  mapM_ check
    [ ("SchemaDollar", ["modelSchemaName = Just $ \"x\""], "Just $ \"x\"")
    , ("SchemaPure", ["modelSchemaName = pure \"x\""], "pure \"x\"")
    , ("SchemaApplied", ["modelSchemaName = Just (f x)"], "Just (f x)")
    , ("SchemaConstructor", ["modelSchemaName = Just Public"], "Just Public")
    , ("SchemaBareName", ["modelSchemaName = mySchema"], "mySchema")
    , ("SchemaGuarded", ["modelSchemaName", "  | True = Just \"x\""], "modelSchemaName | True = Just \"x\"")
    , ("SchemaWhere", ["modelSchemaName = Just s where s = \"x\""],
        "modelSchemaName\n  = Just s\n  where\n      s = \"x\"")
    , ("SchemaArgument", ["modelSchemaName _ = Just \"public\""], "modelSchemaName _ = Just \"public\"")
    , ("SchemaPatternBinding", ["(modelSchemaName) = Just \"public\""], "(modelSchemaName) = Just \"public\"")
    , ("SchemaTuplePattern", ["(modelSchemaName, other) = (Nothing, ())"], "(modelSchemaName, other) = (Nothing, ())")
    , ("SchemaDuplicate", ["modelSchemaName = Nothing", "modelSchemaName = Just \"public\""],
        "modelSchemaName = Nothing\nmodelSchemaName = Just \"public\"")
    , ("SchemaDuplicateBadFirst", ["modelSchemaName = pure \"x\"", "modelSchemaName = Nothing"],
        "modelSchemaName = pure \"x\"\nmodelSchemaName = Nothing")
    , ("SchemaPatternAndEquation", ["(modelSchemaName) = Nothing", "modelSchemaName = Nothing"],
        "(modelSchemaName) = Nothing\nmodelSchemaName = Nothing") ]
  assertEq "P08" (Left [field "modelTableType" "Just $ TRACKER", field "modelSchemaName" "Just $ Config.getAppDbSchema"])
    =<< modifierFix defaultCliOptions "SchemaP08"
          ["modelTableType = Just $ TRACKER", "modelSchemaName = Just $ Config.getAppDbSchema"] []
  assertEq "P09" (Left [field "modelTableType" "pure TRACKER", field "modelSchemaName" "pure \"public\""])
    =<< modifierFix defaultCliOptions "SchemaP09" ["modelTableType = pure TRACKER", "modelSchemaName = pure \"public\""] []
  let both = "(modelTableType, modelSchemaName) = (Nothing, Nothing)"
  assertEq "one pattern binds both" (Left [field "modelTableType" both, field "modelSchemaName" both])
    =<< modifierFix defaultCliOptions "SchemaBothPattern" [both] []
  where
    field = UNRESOLVABLE_MODEL_META_FIELD "FooT"
    check (name, meta, rhs) =
      assertEq name (Left [field "modelSchemaName" rhs]) =<< modifierFix defaultCliOptions name meta []

testTableTypeAccepted :: IO ()
testTableTypeAccepted = mapM_ check
  [ ("TypeNothing", "Nothing", Nothing)
  , ("TypeNothingSig", "Nothing :: Maybe TableType", Nothing)
  , ("TypeTracker", "Just TRACKER", tracker)
  , ("TypeQualified", "Just Se.CONFIG", Just "CONFIG")
  , ("TypeQualifiedJust", "P.Just DOMAIN", Just "DOMAIN")
  , ("TypeCommonConfig", "Just COMMON_CONFIG", Just "COMMON_CONFIG")
  , ("TypeCommonTracker", "Just COMMON_TRACKER", Just "COMMON_TRACKER")
  , ("TypeParens", "(Just (TRACKER))", tracker)
  , ("TypeSig", "Just TRACKER :: Maybe TableType", tracker)
  , ("TypeInnerSig", "Just (Se.TRACKER :: Se.TableType)", tracker) ]
  where
    tracker = Just "TRACKER"
    check (name, rhs, expected) = do
      tables <- tablesOf name =<< modifierFix defaultCliOptions name ["modelTableType = " <> rhs] []
      assertEq name [expected] (map modelTableType tables)

testTableTypeRejected :: IO ()
testTableTypeRejected = mapM_ check
  [ ("TypeDollar", ["modelTableType = Just $ TRACKER"], "Just $ TRACKER")
  , ("TypePure", ["modelTableType = pure TRACKER"], "pure TRACKER")
  , ("TypeVariable", ["modelTableType = Just tableTypeVar"], "Just tableTypeVar")
  , ("TypeString", ["modelTableType = Just \"TRACKER\""], "Just \"TRACKER\"")
  , ("TypeBareName", ["modelTableType = myType"], "myType")
  , ("TypePatternSynonym", ["modelTableType = Just Tracker"], "Just Tracker")
  , ("TypeDollarSig", ["modelTableType = Just $ TRACKER :: Maybe TableType"], "Just $ TRACKER :: Maybe TableType")
  , ("TypeGuarded", ["modelTableType", "  | True = Just TRACKER", "  | otherwise = Nothing"],
      "modelTableType\n  | True = Just TRACKER\n  | otherwise = Nothing")
  , ("TypeEquations", ["modelTableType True = Just TRACKER", "modelTableType False = Nothing"],
      "modelTableType True = Just TRACKER\nmodelTableType False = Nothing")
  , ("TypePatternBinding", ["(modelTableType) = Just $ TRACKER"], "(modelTableType) = Just $ TRACKER")
  , ("TypeTuplePattern", ["(modelTableType, other) = (Nothing, ())"], "(modelTableType, other) = (Nothing, ())")
  , ("TypeDuplicate", ["modelTableType = Nothing", "modelTableType = Just TRACKER"],
      "modelTableType = Nothing\nmodelTableType = Just TRACKER")
  , ("TypeDuplicateBadFirst", ["modelTableType = pure TRACKER", "modelTableType = Nothing"],
      "modelTableType = pure TRACKER\nmodelTableType = Nothing")
  , ("TypePatternAndEquation", ["(modelTableType) = Nothing", "modelTableType = Nothing"],
      "(modelTableType) = Nothing\nmodelTableType = Nothing") ]
  where
    check (name, meta, rhs) =
      assertEq name (Left [UNRESOLVABLE_MODEL_META_FIELD "FooT" "modelTableType" rhs])
        =<< modifierFix defaultCliOptions name meta []

-- | Left out, a field takes the class default, Nothing.
testAbsentMetaFieldsAreNothing :: IO ()
testAbsentMetaFieldsAreNothing = do
  tables <- tablesOf "absent fields" =<< modifierFix defaultCliOptions "AbsentMetaFields" [] []
  assertEq "absent fields" [(Nothing, Nothing)] [(modelTableType t, modelSchemaName t) | t <- tables]

-- | Deriving via can take a derived instance's fields from another type, so
-- they are refused even when both opt-outs let the instance through; the error
-- shows the derivation with its strategy.
testDerivedMetaFieldsUnreadable :: IO ()
testDerivedMetaFieldsUnreadable = do
  cli <- expectOk (parseCliOptions
    ["{\"tableNameOverrides\":{\"FooT\":\"foo\"},\"allowNonStandardModifier\":true}"])
  mapM_ (standalone cli)
    [ ("DerivedFieldsPlain", "deriving instance ModelMeta FooT")
    , ("DerivedFieldsAnyclass", "deriving anyclass instance ModelMeta FooT")
    , ("DerivedFieldsVia", "deriving via BarT instance ModelMeta FooT") ]
  mapM_ (inline cli)
    [ ("DerivedFieldsInlineAnyclass", "deriving anyclass (ModelMeta)")
    , ("DerivedFieldsInlineVia", "deriving (ModelMeta) via BarT") ]
  where
    standalone cli (name, derivation) = derived cli name "" [derivation] derivation
    inline cli (name, clause) = derived cli name (" " <> clause) [] clause
    derived cli name clause standaloneLines shown =
      assertEq name (Left [field "modelTableType", field "modelSchemaName"])
        =<< planFix cli name (derivingExts <> ["DerivingVia"])
              ([ "data FooT f = Foo { fooId :: Columnar f Int } deriving (Generic, Beamable)" <> clause
               , "instance Table FooT where"
               , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
               , "  primaryKey = FooId . fooId" ] <> standaloneLines)
      where field f = UNRESOLVABLE_MODEL_META_FIELD "FooT" f shown

-- | Each field's message lists its own shapes, the table type's all five constructors.
testMetaFieldMessageListsOwnShapes :: IO ()
testMetaFieldMessageListsOwnShapes = do
  let tableType  = message "modelTableType"
      schemaName = message "modelSchemaName"
  mapM_ (\n -> unless (n `elem` identifiers tableType) $
          failWith ("modelTableType message omits " <> n <> ": " <> show tableType))
    tableTypeNames
  mapM_ (\n -> unless (n `notElem` identifiers schemaName) $
          failWith ("modelSchemaName message lists " <> n <> ": " <> show schemaName))
    tableTypeNames
  unless ("Just \"literal\"" `isInfixOf` schemaName && not ("Just \"literal\"" `isInfixOf` tableType)) $
    failWith ("the schema name's own shape is misplaced: " <> show (tableType, schemaName))
  mapM_ (\m -> unless ("written once as a plain equation" `isInfixOf` m) $
          failWith ("message does not say the binding is written once: " <> show m))
    [tableType, schemaName]
  where
    message field = generateErrorMessage "d" (UNRESOLVABLE_MODEL_META_FIELD "FooT" field "pure x")
    -- Whole words: CONFIG is a substring of COMMON_CONFIG.
    identifiers = words . map (\c -> if isAlphaNum c || c == '_' then c else ' ')
    tableTypeNames = ["DOMAIN", "CONFIG", "COMMON_CONFIG", "TRACKER", "COMMON_TRACKER"]

-- | A derived instance is fixed by writing it, a written binding by changing its shape.
testMetaFieldFixFitsInstance :: IO ()
testMetaFieldFixFitsInstance = do
  cli <- expectOk (parseCliOptions
    ["{\"tableNameOverrides\":{\"FooT\":\"foo\"},\"allowNonStandardModifier\":true}"])
  derived <- messages =<< planFix cli "FixDerived" derivingExts
    [ "data FooT f = Foo { fooId :: Columnar f Int } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "deriving anyclass instance ModelMeta FooT" ]
  written <- messages =<< modifierFix defaultCliOptions "FixWritten"
    ["modelTableType = pure TRACKER", "modelSchemaName = pure \"x\""] []
  assertEq "derived messages" 2 (length derived)
  assertEq "written messages" 2 (length written)
  mapM_ (assertMentions "message" "Write the ModelMeta instance of 'FooT' instead of deriving it.") derived
  mapM_ (assertLacks "message" "Define") derived
  mapM_ (assertLacks "message" "instead of deriving") written
  mapM_ (uncurry (assertMentions "message")) (zip ["Define modelTableType in one of those shapes", "Define modelSchemaName in one of those shapes"] written)
  where
    messages = \case
      Left errs -> pure (map (generateErrorMessage "d") errs)
      Right action -> failWith ("expected errors, got " <> show action)

-- | nixpkgs builds libraries with -haddock, which wraps a documented deriving
-- type in 'HsDocTy': each is read as without the comment.
testDocumentedDerivingsRead :: IO ()
testDocumentedDerivingsRead = do
  mapM_ documentedTable
    [ ("DocBeamable", ["  deriving ( Generic", "           , Beamable -- ^ the Beam class", "           )"])
    , ("DocBeforeBeamable", ["  deriving (Generic)", "  deriving -- | the Beam class", "    Beamable"])
    , ("DocGeneric", ["  deriving -- | the representation", "    Generic", "  deriving (Beamable)"]) ]
  assertEq "documented positional mixin" (Left [UNEXTRACTED_TABLE "MixT" notARecord])
    =<< planFixHaddock "DocMixin"
          ["data MixT f = Mix (C f Int)", "  deriving (Generic, Beamable -- ^ the Beam class", "    )"]
  r <- planFixHaddock "DocDatabase" (controlTable <>
    [ "data AppDb f = AppDb { foos :: f (TableEntity FooT) }"
    , "  deriving (Generic, Database be -- ^ the settings record"
    , "    )"
    , "appDb = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ])
  case r of
    Right (Stage (Just fragment)) ->
      assertEq "documented Database" [("foos", "FooT", "foo_v2")]
        [(dbField o, dbTableType o, sqlName o) | o <- fragmentDbEntityOverrides fragment]
    other -> failWith ("documented Database: expected a fragment, got " <> show other)
  where
    documentedTable (name, derivingLines) = do
      cols <- columnNames name =<< planFixHaddock name
        ([ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f Text }" ]
         <> derivingLines <>
         [ "instance Table FooT where"
         , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
         , "  primaryKey = FooId . fooId"
         , "instance ModelMeta FooT where"
         , "  modelTableName = \"foo\"" ])
      assertEq name [("fooId", "id"), ("fooName", "name")] cols

-- | A type named PrimaryKey is a table like any other; only a key,
-- @PrimaryKey t@, is not.
testTableNamedPrimaryKey :: IO ()
testTableNamedPrimaryKey = mapM_ check
  [ ("PkNameStandalone", "deriving (Generic)", ["instance B.Beamable PrimaryKey"])
  , ("PkNameInline", "deriving (Generic, B.Beamable)", []) ]
  where
    check (name, derivings, beamable) = do
      tables <- tablesOf name =<< planFix defaultCliOptions name []
        ([ "data PrimaryKey f = PK { pkId :: B.C f Int, pkName :: B.C f Int } " <> derivings ]
         <> beamable <>
         [ "instance B.Table PrimaryKey where"
         , "  data PrimaryKey PrimaryKey f = PKId (B.C f Int) deriving (Generic, B.Beamable)"
         , "  primaryKey = PKId . pkId"
         , "instance Se.ModelMeta PrimaryKey where"
         , "  modelTableName = \"pk\""
         , "  modelFieldModification = B.tableModification" ])
      assertEq name [("PrimaryKey", "pk")] [(codeName t, modelTableName t) | t <- tables]

-- | A table's key is not a table: its Table or ModelMeta instance is refused.
testKeyTypeInstancesFail :: IO ()
testKeyTypeInstancesFail =
  assertEq "key type as a table"
    (Left [ UNREADABLE_INSTANCE_HEAD "Table" "(PrimaryKey FooT)"
          , UNREADABLE_INSTANCE_HEAD "ModelMeta" "(PrimaryKey FooT)" ])
    =<< planFix defaultCliOptions "KeyAsTable" [] (controlTable <>
          [ "instance Table (PrimaryKey FooT) where"
          , "  data PrimaryKey (PrimaryKey FooT) f = KeyKey (C f Int) deriving (Generic, Beamable)"
          , "  primaryKey (FooId i) = KeyKey i"
          , "instance ModelMeta (PrimaryKey FooT) where"
          , "  modelTableName = \"foo_keys\"" ])

-- | Instances keyed alike, by class and type name: same-named classes of two
-- modules, and one parameterised type at two arguments.  A class applied to
-- two types is not taken for the one-parameter class.
testDuplicateInstancesFail :: IO ()
testDuplicateInstancesFail = do
  assertEq "Collide"
    (Left [UNREADABLE_INSTANCE_HEAD "Table" "Int FooT", DUPLICATE_INSTANCE "ModelMeta" "FooT"])
    =<< planFix defaultCliOptions "Collide" [] (controlTable <>
          [ "instance Other.ModelMeta FooT where"
          , "  modelTableName = \"evil\""
          , "instance Other.Table Int FooT where"
          , "  primaryKey = FooId . fooAlt" ])
  assertEq "Wrap2"
    (Left [DUPLICATE_INSTANCE "ModelMeta" "Wrap", DUPLICATE_INSTANCE "Table" "Wrap"])
    =<< planFix defaultCliOptions "Wrap2" ["KindSignatures"]
          [ "data FooTag (f :: Type -> Type)"
          , "data BarTag (f :: Type -> Type)"
          , "data Wrap (t :: (Type -> Type) -> Type) f = Wrap { wId :: C f Int, wNote :: C f Text }"
          , "  deriving (Generic, Beamable)"
          , "instance Table (Wrap FooTag) where"
          , "  data PrimaryKey (Wrap FooTag) f = WFooId (C f Int) deriving (Generic, Beamable)"
          , "  primaryKey = WFooId . wId"
          , "instance Table (Wrap BarTag) where"
          , "  data PrimaryKey (Wrap BarTag) f = WBarId (C f Int) deriving (Generic, Beamable)"
          , "  primaryKey = WBarId . wId"
          , "instance Se.ModelMeta (Wrap FooTag) where"
          , "  modelTableName = \"wrap_foo\""
          , "instance Se.ModelMeta (Wrap BarTag) where"
          , "  modelTableName = \"wrap_bar\"" ]

-- | DataKinds' promoted constructor 'FooT is not the type FooT.
testPromotedHeadUnreadable :: IO ()
testPromotedHeadUnreadable =
  assertEq "promoted head" (Left [UNREADABLE_INSTANCE_HEAD "ModelMeta" "'FooT"])
    =<< planFix defaultCliOptions "PromotedHead" ["DataKinds"]
          (controlTable <> ["instance ModelMeta 'FooT where", "  modelTableName = \"evil\""])

-- | Each UNEXTRACTED_TABLE reason comes with its own fix, and any other
-- reason with a general one.
testUnextractedFixFitsReason :: IO ()
testUnextractedFixFitsReason = mapM_ check
  [ (noDeclaration, "the module that declares")
  , (notARecord, "Columnar f T")
  , (noBeamable, "Derive Beamable")
  , (databaseRecord, "A database record is not a table")
  , (genericNotStock, "with the stock strategy")
  , (beamableMethods, "Drop the methods")
  , ("some other reason", "all written out") ]
  where
    fixes = ["the module that declares", "Columnar f T", "Derive Beamable", "A database record is not a table", "with the stock strategy", "Drop the methods", "all written out"]
    check (reason, fix) = do
      let msg = generateErrorMessage "d" (UNEXTRACTED_TABLE "FooT" reason)
      unless (fix `isInfixOf` msg && not (any (`isInfixOf` msg) (filter (/= fix) fixes))) $
        failWith ("UNEXTRACTED_TABLE " <> show reason <> ": message does not give its own fix: " <> show msg)

-- | P10: a punned field's value is a variable, not a literal.
testPunnedModifierFieldFails :: IO ()
testPunnedModifierFieldFails = do
  assertEq "punned field" (Left [UNRESOLVABLE_COLUMN_NAME "FooT" "fooName"])
    =<< planFix defaultCliOptions "PunnedField" ["NamedFieldPuns"]
          (modifierTable ["modelFieldModification = fooTMod"] ["fooTMod = tableModification { fooName }"])
  assertEq "punned field beside a literal" (Left [UNRESOLVABLE_COLUMN_NAME "FooT" "fooName"])
    =<< planFix defaultCliOptions "PunnedFieldInline" ["NamedFieldPuns"]
          (modifierTable ["modelFieldModification = tableModification { fooId = fieldNamed \"id\", fooName }"] [])

-- | P11: under OverloadedRecordUpdate a record update calls the setField in
-- scope, so neither a modifier nor a dbModification written so is read.
testOverloadedRecordUpdateModifierFails :: IO ()
testOverloadedRecordUpdateModifierFails = do
  let exts     = ["OverloadedRecordUpdate", "RebindableSyntax"]
      modifier = ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"]
      settings =
        [ "data Db f = Db { foos :: f (TableEntity FooT) } deriving (Generic, Database)"
        , "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ]
  assertEq "P11" (Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "fooTMod", UNCAPTURED_SET_ENTITY_NAME "11:69" notPlainUpdate])
    =<< planFix defaultCliOptions "OverloadedUpdate" exts
          (modifierTable ["modelFieldModification = fooTMod"] (modifier <> settings))
  assertEq "inline" (Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"])
    =<< planFix defaultCliOptions "OverloadedUpdateInline" exts
          (modifierTable ["modelFieldModification = tableModification { fooName = fieldNamed \"the_name\" }"] [])
  cols <- columnNames "allowNonStandardModifier"
    =<< planFix defaultCliOptions { allowNonStandardModifier = True } "OverloadedUpdateLenient" exts
          (modifierTable ["modelFieldModification = fooTMod"] modifier)
  assertEq "allowNonStandardModifier" [("fooId", "id"), ("fooName", "name")] cols
  assertEq "settings: no rename captured" (Left [UNCAPTURED_SET_ENTITY_NAME "3:69" notPlainUpdate])
    =<< planFix defaultCliOptions "OverloadedUpdateSettings" exts settings
  where
    notPlainUpdate = "its dbModification field is not a plain 'field = value' update (record-dot syntax or a pun), which the plugin does not read"

-- | P14: every field the key applies is a column the plugin extracts; a
-- qualified name is another module's, not the table's field.
testPrimaryKeyFieldMustExist :: IO ()
testPrimaryKeyFieldMustExist = do
  let key0 = "data PrimaryKey FooT f = FooNullary deriving (Generic, Beamable)"
      key1 = "data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
      key2 = "data PrimaryKey FooT f = FooKey (C f Int) (C f Text) deriving (Generic, Beamable)"
      unknown fields = Left [UNKNOWN_PRIMARY_KEY_FIELD "FooT" f | f <- fields]
      shape name exts pkLines expected = assertEq name expected =<< primaryKeyOf name exts pkLines
  shape "UnknownCompose" [] [key1, "primaryKey = FooId . someHelper"] (unknown ["someHelper"])
  shape "UnknownLambda" [] [key1, "primaryKey = \\x -> FooId (someHelper x)"] (unknown ["someHelper"])
  shape "UnknownApplicative" [] [key2, "primaryKey = FooKey <$> fooId <*> someHelper"] (unknown ["someHelper"])
  shape "UnknownEquation" [] [key2, "primaryKey x = FooKey (someHelper x) (otherHelper x)"]
    (unknown ["someHelper", "otherHelper"])
  shape "UnknownSection" [] [key1, "primaryKey = FooId . (.someHelper)"] (unknown ["someHelper"])
  shape "UnknownProjection" ["OverloadedRecordDot"] [key1, "primaryKey = FooId . (.someHelper)"] (unknown ["someHelper"])
  shape "UnknownRecordDot" ["OverloadedRecordDot"] [key1, "primaryKey x = FooId x.someHelper"] (unknown ["someHelper"])
  shape "UnknownDotChain" [] [key2, "primaryKey x = FooKey x.fooId x.someHelper"] (unknown ["someHelper"])
  shape "UnknownGetField" ["TypeApplications", "DataKinds"] [key1, "primaryKey x = FooId (getField @\"someHelper\" x)"]
    (unknown ["someHelper"])
  shape "QualifiedCompose" [] [key1, "primaryKey = FooId . M.fooId"] (unknown ["M.fooId"])
  shape "QualifiedApplied" [] [key1, "primaryKey x = FooId (M.fooId x)"] (unknown ["M.fooId"])
  shape "QualifiedApplicative" [] [key2, "primaryKey = FooKey <$> fooId <*> M.fooName"] (unknown ["M.fooName"])
  shape "QualifiedDotChainLast" [] [key2, "primaryKey x = FooKey x . fooId x . M.fooName"] (unknown ["M.fooName"])
  shape "QualifiedDotChainMiddle" [] [key2, "primaryKey x = FooKey x . M.fooId x . fooName"] (unknown ["M.fooId"])
  shape "QualifiedSection" [] [key1, "primaryKey = FooId . (. M.fooId)"] (unknown ["M.fooId"])
  shape "NullaryKey" [] [key0, "primaryKey _ = FooNullary"] (Right ("FooNullary", []))
  assertEq "HelperDefinedHere" (unknown ["someHelper"])
    =<< primaryKeyWith "HelperDefinedHere" [] [key1, "primaryKey = FooId . someHelper"] ["someHelper = fooId"]

-- | A field allowUnknownColumnWrapper leaves out is no column, so it cannot be the key.
testPrimaryKeyFieldDroppedByOptOut :: IO ()
testPrimaryKeyFieldDroppedByOptOut =
  assertEq "dropped key field" (Left [UNKNOWN_PRIMARY_KEY_FIELD "FooT" "fooId"])
    =<< planFix defaultCliOptions { allowUnknownColumnWrapper = True } "KeyFieldDropped" []
          [ "data FooT f = Foo { fooId :: Int, fooName :: C f Text } deriving (Generic, Beamable)"
          , "instance Table FooT where"
          , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
          , "  primaryKey = FooId . fooId"
          , "instance ModelMeta FooT where"
          , "  modelTableName = \"foo\"" ]

-- | L6: the message lists every shape the README lists.
testPrimaryKeyMessageListsShapes :: IO ()
testPrimaryKeyMessageListsShapes = mapM_ listed
  [ "primaryKey x = Con (f1 x) (f2 x)", "primaryKey   = \\x -> Con (f1 x)", "primaryKey   = Con . field"
  , "primaryKey   = Con . (.field)", "primaryKey x = Con x.f1 x.f2", "primaryKey   = Con <$> f1 <*> f2"
  , "primaryKey _ = ConNullary" ]
  where
    msg = generateErrorMessage "d" (UNRESOLVABLE_PRIMARY_KEY "FooT")
    listed s = unless (s `isInfixOf` msg) $ failWith ("UNRESOLVABLE_PRIMARY_KEY message omits " <> show s <> ": " <> show msg)

-- | A where binding on a definition the plugin reads could shadow a name read
-- there: the definition is unreadable, and allowNonStandardModifier gives
-- Beam's default names, not the shadowed binding's.
testWhereBindingsUnreadable :: IO ()
testWhereBindingsUnreadable = do
  overrides <- expectOk (parseCliOptions ["{\"tableNameOverrides\":{\"FooT\":\"foo_override\"}}"])
  let lenient = defaultCliOptions { allowNonStandardModifier = True }
      refused name meta rest err = do
        assertEq name (Left [err]) =<< modifierFix defaultCliOptions name meta rest
        cols <- columnNames name =<< modifierFix lenient name meta rest
        assertEq (name <> " with allowNonStandardModifier") [("fooId", "id"), ("fooName", "name")] cols
  refused "ModifierWhere"
    ["modelFieldModification = fooTMod", "  where fooTMod = tableModification { fooName = fieldNamed \"real_name\" }"]
    ["fooTMod = tableModification { fooName = fieldNamed \"decoy_name\" }"]
    (UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification")
  refused "ModifierWhereTableModification"
    [ "modelFieldModification = tableModification { fooName = fieldNamed \"the_name\" }"
    , "  where tableModification = B.tableModification { fooId = B.fieldNamed \"real_id\" }" ]
    []
    (UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification")
  refused "BindingWhereFieldNamed"
    ["modelFieldModification = fooTMod"]
    ["fooTMod = tableModification { fooName = fieldNamed \"decoy_name\" }", "  where fieldNamed _ = B.fieldNamed \"real_name\""]
    (UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod")
  refused "BindingWhereTableModification"
    ["modelFieldModification = fooTMod"]
    [ "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"
    , "  where tableModification = B.tableModification { fooId = B.fieldNamed \"real_id\" }" ]
    (UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod")
  assertEq "KeyWhere" (Left [UNRESOLVABLE_PRIMARY_KEY "FooT"])
    =<< primaryKeyOf "KeyWhere" []
          ["data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)", "primaryKey = FooId . fooId", "  where fooId = fooName"]
  let nameWhere = ["modelTableName = \"foo\"", "  where fromString _ = \"bar\""]
  assertEq "TableNameWhere" (Left [UNRESOLVABLE_TABLE_NAME "FooT"])
    =<< tableNameFix defaultCliOptions "TableNameWhere" nameWhere
  supplied <- tablesOf "TableNameWhere, override" =<< tableNameFix overrides "TableNameWhereOverride" nameWhere
  assertEq "TableNameWhere, override" ["foo_override"] (map modelTableName supplied)
  -- Kept: an empty where binds nothing, and a where elsewhere is not read.
  cols <- columnNames "BindingEmptyWhere" =<< modifierFix defaultCliOptions "BindingEmptyWhere"
    ["modelFieldModification = fooTMod"] ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }", "  where"]
  assertEq "BindingEmptyWhere" [("fooId", "id"), ("fooName", "the_name")] cols
  meta <- tablesOf "MetaEmptyWhere" =<< modifierFix defaultCliOptions "MetaEmptyWhere"
    ["modelSchemaName = Just \"public\"", "  where"] []
  assertEq "MetaEmptyWhere" [Just (SchemaLiteral "public")] (map modelSchemaName meta)
  emod <- columnNames "EModWhere" =<< planFix defaultCliOptions "EModWhere" []
    (controlTable <> ["fooEMod schema = modifyTableFields fooTMod <> setEntitySchema (Just schema)", "  where go = id"])
  assertEq "EModWhere" [("fooId", "id"), ("fooName", "the_name")] emod
  r <- planFix defaultCliOptions "SettingsWhere" []
    [ "data AppDb f = AppDb { foos :: f (TableEntity FooT) } deriving (Generic, Database be)"
    , "appDb = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }"
    , "  where go = id" ]
  case r of
    Right (Stage (Just fragment)) ->
      assertEq "SettingsWhere" [("foos", "FooT", "foo_v2")]
        [(dbField o, dbTableType o, sqlName o) | o <- fragmentDbEntityOverrides fragment]
    other -> failWith ("SettingsWhere: expected a fragment, got " <> show other)

-- | modelFieldModification, modelTableName, primaryKey and the modifier binding,
-- bound by a pattern or more than once, are unreadable: GHC rejects both, but
-- only after this plugin has run.
testPatternAndRepeatedBindingsUnreadable :: IO ()
testPatternAndRepeatedBindingsUnreadable = do
  let modifier  = Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"]
      binding   = Left [UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod"]
      tableName = Left [UNRESOLVABLE_TABLE_NAME "FooT"]
      key       = Left [UNRESOLVABLE_PRIMARY_KEY "FooT"]
      tmod      = ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"]
      key1      = "data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
  assertEq "ModifierPattern" modifier
    =<< modifierFix defaultCliOptions "ModifierPattern" ["(modelFieldModification) = fooTMod"] tmod
  assertEq "ModifierTwice" modifier
    =<< modifierFix defaultCliOptions "ModifierTwice" ["modelFieldModification = fooTMod", "modelFieldModification = tableModification"] tmod
  assertEq "BindingTwice" binding
    =<< modifierFix defaultCliOptions "BindingTwice" ["modelFieldModification = fooTMod"] (tmod <> ["fooTMod = tableModification"])
  assertEq "BindingAlsoPattern" binding
    =<< modifierFix defaultCliOptions "BindingAlsoPattern" ["modelFieldModification = fooTMod"]
          (tmod <> ["(fooTMod, other) = (tableModification, ())"])
  assertEq "BindingAlsoMethod" binding
    =<< modifierFix defaultCliOptions "BindingAlsoMethod" ["modelFieldModification = fooTMod"]
          (tmod <> ["class Mods a where", "  fooTMod :: a"])
  assertEq "BindingByPattern" binding
    =<< modifierFix defaultCliOptions "BindingByPattern" ["modelFieldModification = fooTMod"]
          ["(fooTMod) = tableModification { fooName = fieldNamed \"the_name\" }"]
  assertEq "TableNamePattern" tableName =<< tableNameFix defaultCliOptions "TableNamePattern" ["(modelTableName) = \"foo\""]
  assertEq "TableNameTwice" tableName
    =<< tableNameFix defaultCliOptions "TableNameTwice" ["modelTableName = \"foo\"", "modelTableName = \"bar\""]
  assertEq "KeyPattern" key =<< primaryKeyOf "KeyPattern" [] [key1, "(primaryKey) = FooId . fooId"]
  assertEq "KeyTwice" key =<< primaryKeyOf "KeyTwice" [] [key1, "primaryKey = FooId . fooId", "primaryKey = FooId . fooName"]

-- | Each message for a definition the plugin reads states the whole rule, so
-- it is accurate when the definition broke only the where or once rule.
testBindingRuleInMessages :: IO ()
testBindingRuleInMessages = mapM_ check
  [ UNRESOLVABLE_TABLE_NAME "FooT"
  , UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"
  , UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod"
  , UNRESOLVABLE_PRIMARY_KEY "FooT" ]
  where
    check e = do
      let msg = generateErrorMessage "d" e
      mapM_ (\rule -> unless (rule `isInfixOf` msg) $
              failWith (errorCode e <> ": message does not say " <> show rule <> ": " <> show msg))
        ["once", "no guards or where bindings"]

-- | A name the plugin reads a modifier or a key through, defined in the module
-- itself: it may then not be Beam's, base's or the table field's.
testRedefinedNamesFail :: IO ()
testRedefinedNamesFail = do
  mapM_ redefined
    [ ("RedefFieldNamed", [], ["fieldNamed _ = B.fieldNamed \"real_name\""], ["fieldNamed"])
    , ("RedefTableModification", [], ["tableModification = B.tableModification"], ["tableModification"])
    , ("RedefBoth", [], ["fieldNamed = B.fieldNamed", "tableModification = B.tableModification"], ["tableModification", "fieldNamed"])
    , ("RedefByPattern", [], ["(fieldNamed, other) = (B.fieldNamed, ())"], ["fieldNamed"])
    , ("RedefByPun", ["NamedFieldPuns"], ["Names{fieldNamed} = names"], ["fieldNamed"])
    , ("RedefPatternSynonymField", ["PatternSynonyms"], ["pattern P{fieldNamed} <- (fieldNamed, _)"], ["fieldNamed"])
    , ("RedefClassMethod", [], ["class Named a where", "  fieldNamed :: Text -> a"], ["fieldNamed"])
    , ("RedefRecordField", [], ["data Names = Names { fieldNamed :: Text }"], ["fieldNamed"])
    , ("RedefDataInstanceField", ["TypeFamilies"], ["data family Fam a", "data instance Fam Int = FamInt { fieldNamed :: Int }"], ["fieldNamed"])
    , ("RedefAssociatedField", [], ["instance Keyed FooT where", "  data Key FooT = FooKeyRec { fieldNamed :: Int }"], ["fieldNamed"])
    , ("RedefForeignImport", [], ["foreign import ccall \"f\" fieldNamed :: Int -> Int"], ["fieldNamed"])
    , ("RedefGadtField", ["GADTs"], ["data Names where", "  Names :: { fieldNamed :: Text } -> Names"], ["fieldNamed"])
    , ("RedefComposition", [], ["(.) f g x = f (g x)"], ["."])
    , ("RedefKeyField", [], ["fooId = fooName"], ["fooId"]) ]
  assertEq "RedefTableModificationBare" (Left [REDEFINED_NAME "FooT" "tableModification"])
    =<< modifierFix defaultCliOptions "RedefTableModificationBare"
          ["modelFieldModification = tableModification"] ["tableModification = B.tableModification"]
  assertEq "RedefFieldNamedUnread" (Left [UNRESOLVABLE_COLUMN_NAME "FooT" "fooName"])
    =<< modifierFix defaultCliOptions "RedefFieldNamedUnread"
          ["modelFieldModification = tableModification { fooName = fieldNamed someName }"] ["fieldNamed = B.fieldNamed"]
  assertEq "RedefFieldNamedInline" (Left [REDEFINED_NAME "FooT" "fieldNamed"])
    =<< modifierFix defaultCliOptions "RedefFieldNamedInline"
          ["modelFieldModification = tableModification { fooName = fieldNamed \"the_name\" }"] ["fieldNamed = B.fieldNamed"]
  assertEq "RedefApplicative" (Left [REDEFINED_NAME "FooT" "<$>", REDEFINED_NAME "FooT" "<*>"])
    =<< primaryKeyWith "RedefApplicative" []
          ["data PrimaryKey FooT f = FooKey (C f Int) (C f Text) deriving (Generic, Beamable)", "primaryKey = FooKey <$> fooId <*> fooName"]
          ["(<$>) = fmap", "(<*>) = ap"]
  assertEq "RedefGetField" (Left [REDEFINED_NAME "FooT" "getField"])
    =<< primaryKeyWith "RedefGetField" ["TypeApplications", "DataKinds"]
          ["data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)", "primaryKey x = FooId (getField @\"fooId\" x)"]
          ["getField = undefined"]
  where
    redefined (name, exts, extra, names) =
      assertEq name (Left [REDEFINED_NAME "FooT" n | n <- names]) =<< planFix defaultCliOptions name exts (controlTable <> extra)

-- | No error where nothing is read through the module's own definition.
testRedefinedNamesNotRead :: IO ()
testRedefinedNamesNotRead = do
  noModifier <- tablesOf "NoModifier" =<< planFix defaultCliOptions "NoModifier" []
    [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    , "fieldNamed = B.fieldNamed"
    , "tableModification = B.tableModification" ]
  assertEq "NoModifier" ["FooT"] (map codeName noModifier)
  bare <- columnNames "BareModifier" =<< modifierFix defaultCliOptions "BareModifier"
    ["modelFieldModification = tableModification"] ["fieldNamed = B.fieldNamed"]
  assertEq "BareModifier" [("fooId", "id"), ("fooName", "name")] bare
  assertEq "NoTable" (Right (Stage Nothing))
    =<< planFix defaultCliOptions "NoTable" [] ["fieldNamed = B.fieldNamed . T.toLower", "tableModification = B.tableModification"]
  assertEq "KeyParameter" (Right ("FooKey", ["fooId", "fooName"]))
    =<< primaryKeyWith "KeyParameter" []
          ["data PrimaryKey FooT f = FooKey (C f Int) (C f Text) deriving (Generic, Beamable)", "primaryKey x = FooKey (fooId x) (fooName x)"]
          ["x = ()"]
  shared <- tablesOf "PreludeNamedField" =<< planFix defaultCliOptions "PreludeNamedField" []
    [ "data FooT f = Foo { id :: C f Int } deriving (Generic, Beamable)"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . id"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\"" ]
  assertEq "PreludeNamedField" [["id"]] (map (pkColumns . primaryKey) shared)

-- | GHC rejects a context or forall nested in an instance head (GHC-71492), but
-- only after this plugin has run: such an instance is read, not skipped.
testNestedContextHeadsRead :: IO ()
testNestedContextHeadsRead = do
  let exts = ["StandaloneDeriving", "RankNTypes", "KindSignatures"]
  plain <- planFix defaultCliOptions "NestedHeads" exts
    [ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic)"
    , "deriving instance Beamable FooT"
    , "instance Table FooT where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta FooT where"
    , "  modelTableName = \"foo\""
    , "  modelFieldModification = fooTMod"
    , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }" ]
  _ <- columnNames "plain heads" plain
  nested <- planFixHaddockSame "NestedHeads" exts
    [ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic)"
    , "deriving instance Show Int => (Show Bool => Beamable FooT)"
    , "instance Show Int => (forall a. Table FooT) where"
    , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance (Show Int => ModelMeta -- ^ the class"
    , "  FooT :: Constraint) where"
    , "  modelTableName = \"foo\""
    , "  modelFieldModification = fooTMod"
    , "fooTMod = tableModification { fooName = fieldNamed \"the_name\" }" ]
  assertEq "nested heads read as plain ones" plain nested
  assertEq "only a nested Beamable marks the table" (Left [MISSING_MODEL_META "FooT"])
    =<< planFix defaultCliOptions "NestedBeamableOnly" exts
          [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic)"
          , "deriving instance Show Int => (Show Bool => Beamable FooT)" ]
  assertEq "nested orphan Table" (Left [UNEXTRACTED_TABLE "OtherT" noDeclaration])
    =<< planFix defaultCliOptions "NestedOrphanTable" exts
          ["instance (Show Int => Table OtherT) where", "  primaryKey = OtherId . otherId"]

-- | F04: a Haddock comment on an instance head's argument changes nothing.
testDocumentedInstanceHeadRead :: IO ()
testDocumentedInstanceHeadRead = do
  r <- planFixHaddockSame "DocumentedHeads" derivingExts
    [ "data FooT f = Foo { fooId :: Columnar f Int, fooName :: Columnar f Text }"
    , "  deriving (Generic)"
    , "-- | Beamable for FooT."
    , "deriving anyclass instance Beamable"
    , "  FooT -- ^ the table"
    , "instance Table"
    , "  FooT -- ^ the table"
    , "  where"
    , "  data PrimaryKey FooT f = FooId (Columnar f Int) deriving (Generic, Beamable)"
    , "  primaryKey = FooId . fooId"
    , "instance ModelMeta"
    , "  FooT -- ^ the table"
    , "  where"
    , "  modelTableName = \"foo\"" ]
  assertEq "documented heads" [("fooId", "id"), ("fooName", "name")] =<< columnNames "documented heads" r

-- | G1, G2 and F1: under RebindableSyntax the module's own getField and fromString decide what the
-- code reads; M-5: under OverlappingInstances or IncoherentInstances an instance overlaps a library's
-- without a pragma, also through a type synonym.  A module that stages tables or settings may enable none.
testRefusedExtensions :: IO ()
testRefusedExtensions = do
  let refused = ["RebindableSyntax", "OverlappingInstances", "IncoherentInstances"]
  assertEq "refused extensions" refused refusedExtensions
  forM_ refused $ \ext -> do
    assertEq (ext <> ", staged") [UNSUPPORTED_EXTENSION ext] (extensionErrors (== ext) True)
    assertEq (ext <> ", nothing staged") [] (extensionErrors (== ext) False)
  assertEq "all enabled" (map UNSUPPORTED_EXTENSION refused) (extensionErrors (const True) True)
  assertEq "none enabled" [] (extensionErrors (const False) True)

-- | R1: a rewrite rule can replace a name the plugin reads, and GHC applies it when it optimises.
testRulesRefused :: IO ()
testRulesRefused = do
  let rule i = "{-# RULES \"r" <> i <> "\" forall s. fieldNamed s = fieldNamed \"real_name\" #-}"
      at k   = show (length controlTable + 2 + k) <> ":1"
      settings =
        [ "data AppDb f = AppDb { foos :: f (TableEntity FooT) } deriving (Generic, Database be)"
        , "appDb = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ]
      unnamed =
        [ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
        , "instance Table FooT where"
        , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
        , "  primaryKey = FooId . fooId"
        , "instance ModelMeta FooT where"
        , "  modelTableName = SomeConst.fooName" ]
  assertEq "RulesTable" (Left [UNSUPPORTED_RULES (at 0)])
    =<< planFix defaultCliOptions "RulesTable" [] (controlTable <> [rule "1"])
  assertEq "RulesTwice" (Left [UNSUPPORTED_RULES (at 0), UNSUPPORTED_RULES (at 2)])
    =<< planFix defaultCliOptions "RulesTwice" [] (controlTable <> [rule "1", "x = 1", rule "2"])
  assertEq "RulesOnePragma" (Left [UNSUPPORTED_RULES (at 0)])
    =<< planFix defaultCliOptions "RulesOnePragma" []
          (controlTable <> ["{-# RULES \"a\" forall s. f s = g s ; \"b\" forall s. h s = k s #-}"])
  -- The rule's own setEntityName uses are not captured either.
  assertEq "RulesSettings"
    (Left [UNSUPPORTED_RULES "4:1", UNCAPTURED_SET_ENTITY_NAME "4:25" notInModification, UNCAPTURED_SET_ENTITY_NAME "4:43" notInModification])
    =<< planFix defaultCliOptions "RulesSettings" []
          (settings <> ["{-# RULES \"r\" forall s. setEntityName s = setEntityName \"other\" #-}"])
  assertEq "RulesBesideError" (Left [UNRESOLVABLE_TABLE_NAME "FooT", UNSUPPORTED_RULES (show (length unnamed + 2) <> ":1")])
    =<< planFix defaultCliOptions "RulesBesideError" [] (unnamed <> ["{-# RULES \"r\" forall s. f s = g s #-}"])
  -- Kept: a module that stages nothing, and a blacklisted one.
  assertEq "RulesNoTable" (Right (Stage Nothing)) =<< planFix defaultCliOptions "RulesNoTable" [] [rule "1"]
  assertEq "RulesSettingsNoOverride" (Right (Stage Nothing))
    =<< planFix defaultCliOptions "RulesSettingsNoOverride" []
          [ "data AppDb f = AppDb { foos :: f (TableEntity FooT) } deriving (Generic, Database be)"
          , "appDb = defaultDbSettings", rule "1" ]
  assertEq "RulesBlacklisted" (Right (Stage Nothing))
    =<< planFix defaultCliOptions { blacklistModules = ["Fix.RulesBlacklisted"] } "RulesBlacklisted" []
          (controlTable <> [rule "1"])

-- | A pattern synonym of this module that reuses a name the plugin reads the meta fields through.
testPatternSynonymNamesRefused :: IO ()
testPatternSynonymNamesRefused = do
  mapM_ refused ("Just" : "Nothing" : tableTypeConstructors)
  assertEq "PatSynBoth" (Left [REDEFINED_NAME "FooT" "Just", REDEFINED_NAME "FooT" "Nothing"])
    =<< planFix defaultCliOptions "PatSynBoth" ["PatternSynonyms"]
          (controlTable <> ["pattern Just = ()", "pattern Nothing = ()"])
  -- Kept: another name, and a module with no table.
  _ <- columnNames "PatSynOther" =<< planFix defaultCliOptions "PatSynOther" ["PatternSynonyms"]
    (controlTable <> ["pattern Tracker = ()"])
  assertEq "PatSynNoTable" (Right (Stage Nothing))
    =<< planFix defaultCliOptions "PatSynNoTable" ["PatternSynonyms"] ["pattern Just = ()"]
  where
    refused n = assertEq ("PatSyn" <> n) (Left [REDEFINED_NAME "FooT" n])
      =<< planFix defaultCliOptions ("PatSyn" <> n) ["PatternSynonyms"] (controlTable <> ["pattern " <> n <> " = ()"])

-- | TF1: a type of this module named like a Beam type the plugin reads a field's wrapper and
-- nullability through.
testTypeNamesRefused :: IO ()
testTypeNamesRefused = do
  mapM_ refused
    [ ("TypeSynonymC", [], ["type C f a = Columnar f (Maybe a)"], "C")
    , ("DataC", [], ["data C f a = C (f a)"], "C")
    , ("NewtypeColumnar", [], ["newtype Columnar f a = Columnar (f a)"], "Columnar")
    , ("ClosedFamilyC", ["TypeFamilies"], ["type family C f a where", "  C f a = Columnar f (Maybe a)"], "C")
    , ("OpenFamilyC", ["TypeFamilies"], ["type family C f a"], "C")
    , ("DataFamilyMaybe", ["TypeFamilies"], ["data family Maybe a"], "Maybe")
    , ("AssociatedTypeC", ["TypeFamilies"], ["class Wrap a where", "  type C a"], "C")
    , ("SynonymMaybe", [], ["type Maybe a = a"], "Maybe") ]
  assertEq "TypeNamesTwo" (Left [REDEFINED_NAME "FooT" "C", REDEFINED_NAME "FooT" "Maybe"])
    =<< planFix defaultCliOptions "TypeNamesTwo" [] (controlTable <> ["type C f a = f a", "type Maybe a = a"])
  -- Kept: other names, and a module with no table.
  _ <- columnNames "TypeOther" =<< planFix defaultCliOptions "TypeOther" []
    (controlTable <> ["type Cx f a = Columnar f a", "data Columnars = Columnars"])
  assertEq "TypeNoTable" (Right (Stage Nothing)) =<< planFix defaultCliOptions "TypeNoTable" [] ["type C f a = f a"]
  where
    refused (name, exts, extra, n) =
      assertEq name (Left [REDEFINED_NAME "FooT" n]) =<< planFix defaultCliOptions name exts (controlTable <> extra)

-- | H1, H2: Beam names a table's columns from its Generic representation, which only a stock
-- derivation, or the large-records plugin, takes from the record's fields; a written or otherwise
-- derived instance can set them to anything, also through a type synonym.
testGenericMustBeStockDerived :: IO ()
testGenericMustBeStockDerived = do
  mapM_ refused
    [ ("NoGeneric", [], tableWithoutGeneric)
    , ("HandGeneric", ["TypeFamilies"], tableWithoutGeneric <> writtenGeneric "Generic (FooT f)")
    , ("HandGenericArgument", ["TypeFamilies"], tableWithoutGeneric <> writtenGeneric "Generic (FooT Identity)")
    , ("HandGenericContext", ["TypeFamilies"], tableWithoutGeneric <> writtenGeneric "Show f => Generic (FooT f)")
    , ("HandGenericQualified", ["TypeFamilies"], tableWithoutGeneric <> writtenGeneric "G.Generic (FooT f)")
    , ("AliasGeneric", ["TypeFamilies"], tableWithoutGeneric <> ["type AliasT = FooT"] <> writtenGeneric "Generic (AliasT f)")
    , ("AliasGenericBesideStock", ["TypeFamilies"], tableWith "deriving (Generic, Beamable)" <> ["type AliasT = FooT"] <> writtenGeneric "Generic (AliasT f)")
    , ("HandGenericUnreadableHead", ["TypeFamilies"], tableWith "deriving (Generic, Beamable)" <> writtenGeneric "Generic (M.FooT f)")
    , ("ViaGeneric", ["DerivingVia"], tableWith "deriving (Beamable) deriving (Generic) via (Other f)")
    , ("NewtypeGeneric", ["DerivingStrategies"], tableWith "deriving (Beamable) deriving newtype (Generic)")
    , ("AnyclassGeneric", ["DerivingStrategies"], tableWith "deriving (Beamable) deriving anyclass (Generic)")
    , ("StandaloneViaGeneric", "DerivingVia" : derivingExts, tableWithoutGeneric <> ["deriving via (Other f) instance Generic (FooT f)"])
    , ("StandaloneAnyclassGeneric", derivingExts, tableWithoutGeneric <> ["deriving anyclass instance Generic (FooT f)"])
    , ("StandaloneAnyclassBesideStock", derivingExts, tableWith "deriving (Generic, Beamable)" <> ["deriving anyclass instance Generic (FooT f)"])
    , ("StandaloneViaAlias", "DerivingVia" : derivingExts, tableWithoutGeneric <> ["type AliasT = FooT", "deriving via (Other f) instance Generic (AliasT f)"])
    , ("StandaloneNarrowGeneric", derivingExts, tableWithoutGeneric <> ["deriving instance Generic (FooT Identity)"])
    , ("StandaloneAliasGeneric", derivingExts, tableWithoutGeneric <> ["type AliasT = FooT", "deriving instance Generic (AliasT f)"])
    , ("LargeRecordOtherType", [], "{-# ANN type OtherT largeRecord #-}" : tableWithoutGeneric)
    , ("LargeRecordValueAnnotation", [], "{-# ANN fooId largeRecord #-}" : tableWithoutGeneric)
    , ("LargeRecordModuleAnnotation", [], "{-# ANN module largeRecord #-}" : tableWithoutGeneric)
    , ("OtherTypeAnnotation", [], "{-# ANN type FooT otherOptions #-}" : tableWithoutGeneric)
    , ("LargeRecordApplied", [], "{-# ANN type FooT (largeRecord opts) #-}" : tableWithoutGeneric)
    , ("LargeRecordBesideWritten", ["TypeFamilies"], ("{-# ANN type FooT largeRecord #-}" : tableWithoutGeneric) <> writtenGeneric "Generic (FooT f)") ]
  assertEq "StandaloneRepeatedVariable" (Left [UNEXTRACTED_TABLE "PairT" genericNotStock])
    =<< planFix defaultCliOptions "StandaloneRepeatedVariable" derivingExts (pairTable <> ["deriving instance Generic (PairT f f)"])
  mapM_ kept
    [ ("DerivedGeneric", derivingExts, tableWithoutGeneric <> ["deriving instance Generic (FooT f)"])
    , ("StockStandaloneGeneric", derivingExts, tableWithoutGeneric <> ["deriving stock instance Generic (FooT f)"])
    , ("KindedStandaloneGeneric", "KindSignatures" : derivingExts, tableWithoutGeneric <> ["deriving instance Generic (FooT (f :: Type -> Type))"])
    , ("StockInlineGeneric", ["DerivingStrategies"], tableWith "deriving stock (Generic) deriving anyclass (Beamable)")
    , ("QualifiedGeneric", [], tableWith "deriving (G.Generic, Beamable)")
    , ("OtherGeneric", ["TypeFamilies"], tableWith "deriving (Generic, Beamable)" <> writtenGeneric "Generic (OtherT f)")
    , ("OtherTypeViaGeneric", ["DerivingVia"], tableWith "deriving (Generic, Beamable)" <> ["data OtherT = OtherT deriving (Generic) via Other"])
    , ("PairGeneric", derivingExts, pairTable <> ["deriving instance Generic (PairT x f)"])
    , ("LargeRecord", [], "{-# ANN type FooT largeRecord #-}" : tableWithoutGeneric)
    , ("LargeRecordQualified", [], "{-# ANN type FooT Nau.largeRecord #-}" : tableWithoutGeneric)
    , ("LargeRecordParenthesised", [], "{-# ANN type FooT (largeRecord) #-}" : tableWithoutGeneric) ]
  assertEq "HandGenericNoTable" (Right (Stage Nothing))
    =<< planFix defaultCliOptions "HandGenericNoTable" ["TypeFamilies"] (writtenGeneric "Generic (FooT f)")
  where
    refused (name, exts, body) =
      assertEq name (Left [UNEXTRACTED_TABLE "FooT" genericNotStock]) =<< planFix defaultCliOptions name exts body
    kept (name, exts, body) = tablesOf name =<< planFix defaultCliOptions name exts body
    tableWith clause = ("data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } " <> clause) : drop 1 tableWithoutGeneric
    pairTable =
      [ "data PairT x f = Pair { pairId :: C f Int } deriving (Beamable)"
      , "instance Table (PairT x) where"
      , "  data PrimaryKey (PairT x) f = PairId (C f Int) deriving (Generic, Beamable)"
      , "  primaryKey = PairId . pairId"
      , "instance ModelMeta (PairT x) where"
      , "  modelTableName = \"pair\"" ]

-- | A record wildcard in a top-level pattern binding binds every field in scope, and the parser
-- does not show which names those are.
testTopLevelRecordWildcardRefused :: IO ()
testTopLevelRecordWildcardRefused = do
  let at k = show (length controlTable + 2 + k) <> ":1"
      exts = ["RecordWildCards", "NamedFieldPuns"]
  assertEq "TopWildcard" (Left [UNSUPPORTED_RECORD_WILDCARD (at 0)])
    =<< planFix defaultCliOptions "TopWildcard" exts (controlTable <> ["Names{..} = names"])
  assertEq "TopWildcardNested" (Left [UNSUPPORTED_RECORD_WILDCARD (at 0)])
    =<< planFix defaultCliOptions "TopWildcardNested" exts (controlTable <> ["(count, Names{..}) = pair"])
  assertEq "TopWildcardTwice" (Left [UNSUPPORTED_RECORD_WILDCARD (at 0), UNSUPPORTED_RECORD_WILDCARD (at 1)])
    =<< planFix defaultCliOptions "TopWildcardTwice" exts (controlTable <> ["Names{..} = names", "Other{..} = other"])
  assertEq "TopWildcardNoTable" (Right (Stage Nothing))
    =<< planFix defaultCliOptions "TopWildcardNoTable" exts ["Names{..} = names"]
  -- Kept: a wildcard in a function's arguments, one in an expression, and a pun.
  cols <- columnNames "WildcardKept" =<< planFix defaultCliOptions "WildcardKept" exts
    (controlTable <> ["describe Names{..} = Names{..}", "Other{label} = other"])
  assertEq "WildcardKept" [("fooId", "id"), ("fooName", "the_name")] cols

-- | Each message names what it refuses: the extension, or the position.
testUnsupportedMessagesNameTheirCause :: IO ()
testUnsupportedMessagesNameTheirCause = mapM_ (uncurry (assertMentions "message"))
  [ ("enables ExampleExtension", generateErrorMessage "d" (UNSUPPORTED_EXTENSION "ExampleExtension"))
  , ("{-# LANGUAGE NoExampleExtension #-}", generateErrorMessage "d" (UNSUPPORTED_EXTENSION "ExampleExtension"))
  , ("with 'import Prelude'", generateErrorMessage "d" (UNSUPPORTED_EXTENSION "RebindableSyntax"))
  , ("can overlap a library's without an overlap pragma", generateErrorMessage "d" (UNSUPPORTED_EXTENSION "OverlappingInstances"))
  , ("{-# OVERLAPPING #-}", generateErrorMessage "d" (UNSUPPORTED_EXTENSION "IncoherentInstances"))
  , ("a module this module does not import, directly or not", generateErrorMessage "d" (UNSUPPORTED_RULES "12:3"))
  , ("12:3", generateErrorMessage "d" (UNSUPPORTED_RULES "12:3"))
  , ("12:3", generateErrorMessage "d" (UNSUPPORTED_RECORD_WILDCARD "12:3")) ]

-- | P1: a punned field of a pattern binding in the instance binds its label, so the method
-- is bound by a pattern, or twice; GHC rejects both, but only after this plugin has run.
testPunnedInstanceBindingsUnreadable :: IO ()
testPunnedInstanceBindingsUnreadable = do
  let exts     = ["NamedFieldPuns"]
      modifier = Left [UNRECOGNIZED_MODIFIER_SHAPE "FooT" "modelFieldModification"]
      tmod     = ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }"]
      meta name meta' rest = planFix defaultCliOptions name exts (modifierTable meta' rest)
      fieldOf = \case
        Left [UNRESOLVABLE_MODEL_META_FIELD "FooT" f _] -> Just f
        _                                               -> Nothing
  assertEq "PunnedModifier" modifier =<< meta "PunnedModifier" ["Mods{modelFieldModification} = mods"] tmod
  assertEq "PunnedModifierBesideEquation" modifier
    =<< meta "PunnedModifierBesideEquation" ["modelFieldModification = fooTMod", "Mods{modelFieldModification} = mods"] tmod
  assertEq "PunnedTableName" (Left [UNRESOLVABLE_TABLE_NAME "FooT"])
    =<< meta "PunnedTableName" ["Names{modelTableName} = names"] []
  assertEq "PunnedTableType" (Just "modelTableType") . fieldOf
    =<< meta "PunnedTableType" ["modelTableType = Just TRACKER", "Mods{modelTableType} = mods"] []
  assertEq "PunnedSchemaName" (Just "modelSchemaName") . fieldOf
    =<< meta "PunnedSchemaName" ["modelSchemaName = Nothing", "Mods{modelSchemaName} = mods"] []
  assertEq "PunnedKey" (Left [UNRESOLVABLE_PRIMARY_KEY "FooT"])
    =<< primaryKeyOf "PunnedKey" exts
          ["data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)", "primaryKey = FooId . fooId", "Keys{primaryKey} = keys"]

-- | A where binding of implicit parameters binds names too (@where ?x = …@), so it is unreadable
-- like any other where binding.
testImplicitParamBindingsUnreadable :: IO ()
testImplicitParamBindingsUnreadable = do
  assertEq "KeyImplicitParam" (Left [UNRESOLVABLE_PRIMARY_KEY "FooT"])
    =<< primaryKeyOf "KeyImplicitParam" ["ImplicitParams"]
          ["data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)", "primaryKey = FooId . fooId", "  where ?x = ()"]
  assertEq "ModifierImplicitParam" (Left [UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod"])
    =<< planFix defaultCliOptions "ModifierImplicitParam" ["ImplicitParams"]
          (modifierTable ["modelFieldModification = fooTMod"]
            ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }", "  where ?x = ()"])

-- | A punned field has no right-hand side, so the message names the pun apart from a computed value.
testColumnNameMessageCoversPun :: IO ()
testColumnNameMessageCoversPun = do
  let msg = generateErrorMessage "d" (UNRESOLVABLE_COLUMN_NAME "FooT" "fooName")
  mapM_ (\needle -> assertMentions "message" needle msg)
    ["punned", "{ fooName }", "fooName = fieldNamed"]
  when ("RHS" `isInfixOf` msg) $ failWith ("message speaks of a right-hand side a pun lacks: " <> show msg)

-- | The fix for a table name that is no literal also fits a literal that is where-bound or written
-- twice, and names the plugin option that supplies one.
testTableNameMessageFix :: IO ()
testTableNameMessageFix = do
  let msg = generateErrorMessage "d" (UNRESOLVABLE_TABLE_NAME "FooT")
  mapM_ (\needle -> assertMentions "message" needle msg)
    ["tableNameOverrides", "modelTableName = \"sql_name\""]
  when ("Replace the computed value" `isInfixOf` msg) $ failWith ("the fix still assumes a computed value: " <> show msg)

-- | The refusals hold with Haddock comments kept, as nixpkgs builds libraries.
testNewRefusalsUnderHaddock :: IO ()
testNewRefusalsUnderHaddock = mapM_ same
  [ ("DocSynonymC", [], controlTable <> ["-- | A synonym.", "type C f a = Columnar f (Maybe a)"], Left [REDEFINED_NAME "FooT" "C"])
  , ("DocClosedFamilyC", ["TypeFamilies"], controlTable <> ["-- | A family.", "type family C f a where", "  C f a = Columnar f (Maybe a)"], Left [REDEFINED_NAME "FooT" "C"])
  , ("DocAssociatedC", ["TypeFamilies"], controlTable <> ["-- | A class.", "class Wrap a where", "  -- | The type.", "  type C a"], Left [REDEFINED_NAME "FooT" "C"])
  , ("DocPatSyn", ["PatternSynonyms"], controlTable <> ["-- | A synonym.", "pattern TRACKER = ()"], Left [REDEFINED_NAME "FooT" "TRACKER"])
  , ("DocRules", [], controlTable <> ["-- | The rules.", "{-# RULES \"r\" forall s. f s = g s #-}"], Left [UNSUPPORTED_RULES (at 1)])
  , ("DocWildcard", ["RecordWildCards"], controlTable <> ["-- | Names.", "Names{..} = names"], Left [UNSUPPORTED_RECORD_WILDCARD (at 1)])
  , ("DocHandGeneric", ["TypeFamilies"], tableWithoutGeneric <> ["-- | By hand."] <> writtenGeneric "Generic (FooT f)", Left [UNEXTRACTED_TABLE "FooT" genericNotStock])
  , ("DocAnyclassGeneric", ["DerivingStrategies"]
    , ["data FooT f = Foo { fooId :: C f Int, fooName :: C f Text }", "  deriving (Beamable)", "  deriving anyclass (Generic -- ^ the representation", "    )"] <> drop 1 tableWithoutGeneric
    , Left [UNEXTRACTED_TABLE "FooT" genericNotStock]) ]
  where
    at k = show (length controlTable + 2 + k) <> ":1"
    same (name, exts, body, expected) = assertEq name expected =<< planFixHaddockSame name exts body

testNewRefusalsHaveNoOptOut :: IO ()
testNewRefusalsHaveNoOptOut = mapM_ refused
  [ ("NoOptOutPatSyn", ["PatternSynonyms"], controlTable <> ["pattern Just = ()"], Left [REDEFINED_NAME "FooT" "Just"])
  , ("NoOptOutType", [], controlTable <> ["type C f a = f a"], Left [REDEFINED_NAME "FooT" "C"])
  , ("NoOptOutGeneric", ["TypeFamilies"], tableWithoutGeneric <> writtenGeneric "Generic (FooT f)", Left [UNEXTRACTED_TABLE "FooT" genericNotStock])
  , ("NoOptOutRules", [], controlTable <> ["{-# RULES \"r\" forall s. f s = g s #-}"], Left [UNSUPPORTED_RULES at])
  , ("NoOptOutWildcard", ["RecordWildCards"], controlTable <> ["Names{..} = names"], Left [UNSUPPORTED_RECORD_WILDCARD at]) ]
  where
    at = show (length controlTable + 2) <> ":1"
    lenient = defaultCliOptions { allowNonStandardModifier = True, allowUnknownColumnWrapper = True }
    refused (name, exts, body, expected) = assertEq name expected =<< planFix lenient name exts body

-- | Both readers of TableEntity match it as written: a type of that name in the module refuses
-- its tables and its renames.
testLocalTableEntityRefused :: IO ()
testLocalTableEntityRefused = do
  assertEq "TableEntitySynonym" (Left [REDEFINED_NAME "FooT" "TableEntity"])
    =<< planFix defaultCliOptions "TableEntitySynonym" [] (controlTable <> ["type TableEntity = ViewEntity"])
  assertEq "TableEntityData" (Left [REDEFINED_NAME "FooT" "TableEntity"])
    =<< planFix defaultCliOptions "TableEntityData" [] (controlTable <> ["data TableEntity t = TableEntity"])
  expectUncaptured "TableEntitySettings" [("3:69", localTableEntity)]
    =<< planFix defaultCliOptions "TableEntitySettings" [] (settings <> ["type TableEntity = ViewEntity"])
  expectUncaptured "TableEntitySettingsData" [("3:69", localTableEntity)]
    =<< planFix defaultCliOptions "TableEntitySettingsData" [] (settings <> ["data TableEntity t = TableEntity"])
  expectUncaptured "TableEntityFamily" [("3:69", localTableEntity)]
    =<< planFix defaultCliOptions "TableEntityFamily" ["TypeFamilies"] (settings <> ["type family TableEntity t"])
  assertEq "TableEntityBoth"
    (Left [REDEFINED_NAME "FooT" "TableEntity", UNCAPTURED_SET_ENTITY_NAME "11:69" (localTableEntity <> ", so a field's 'f (TableEntity T)' may not be Beam's table entity")])
    =<< planFix defaultCliOptions "TableEntityBoth" []
          (entityNameModule ["db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }", "type TableEntity = ViewEntity"])
  where
    localTableEntity = "this module declares a type named 'TableEntity'"
    settings =
      [ "data Db f = Db { foos :: f (TableEntity FooT) } deriving (Generic, Database)"
      , "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" }" ]

-- | modifyTableFields reads the modifier binding as the columns do: under allowNonStandardModifier
-- a where-bound modifier, which the columns ignore, is not one the settings may apply.
testModifyTableFieldsWhereModifierFails :: IO ()
testModifyTableFieldsWhereModifierFails = do
  let lenient = defaultCliOptions { allowNonStandardModifier = True }
      body modifier = modifierTable ["modelFieldModification = fooTMod"] (modifier <> ["fooEMod = modifyTableFields fooTMod"])
      unread = "the modifier binding 'fooTMod' is not one top-level equation with no arguments, guards or where bindings, so the plugin does not read it"
  assertEq "OptOutWhereModifier" (Left [UNCAPTURED_MODIFY_TABLE_FIELDS "11:11" unread])
    =<< planFix lenient "OptOutWhereModifier" []
          (body ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }", "  where go = id"])
  assertEq "OptOutGuardedModifier" (Left [UNCAPTURED_MODIFY_TABLE_FIELDS "11:11" unread])
    =<< planFix lenient "OptOutGuardedModifier" []
          (body ["fooTMod", "  | True = tableModification { fooName = fieldNamed \"the_name\" }"])
  assertEq "WhereModifier"
    (Left [UNRESOLVABLE_MODIFIER_BINDING "FooT" "fooTMod", UNCAPTURED_MODIFY_TABLE_FIELDS "11:11" unread])
    =<< planFix defaultCliOptions "WhereModifier" []
          (body ["fooTMod = tableModification { fooName = fieldNamed \"the_name\" }", "  where go = id"])

-- | An instance whose type names EntityModification or DatabaseEntity, or one with an overlap
-- pragma, can change what '<>' does to the settings' modifications, so its method names count
-- as names the module binds.
testInstanceAtBeamTypeBindsMethods :: IO ()
testInstanceAtBeamTypeBindsMethods = mapM_ refused
  [ ("EntityModificationInstance", "instance Semigroup (EntityModification (DatabaseEntity be db) be (TableEntity FooT)) where")
  , ("DatabaseEntityInstance", "instance Semigroup (DatabaseEntity be db (TableEntity FooT)) where")
  , ("EntityModificationContext", "instance (m ~ EntityModification e be t) => Semigroup (Mod m) where")
  , ("OverlappingInstance", "instance {-# OVERLAPPING #-} Semigroup X where")
  , ("IncoherentInstance", "instance {-# INCOHERENT #-} Semigroup X where") ]
  where
    refused (name, instanceHead) =
      expectErrors name
        [ ("UNCAPTURED_SET_ENTITY_NAME", "13:69", "this module binds '<>' itself")
        , ("UNCAPTURED_MODIFY_TABLE_FIELDS", "13:95", "this module binds '<>' itself") ]
        =<< planFix defaultCliOptions ("EntityName" <> name) ["TypeFamilies"]
              (entityNameModule
                 [ instanceHead, "  _ <> b = b"
                 , "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" <> modifyTableFields fooTMod }" ])

-- | I-2: a derived instance at those types, or with an overlap pragma, binds its class's methods
-- as a written one does: a derived OVERLAPPING Semigroup can change how a '<>' chain combines.
testDerivedInstanceAtBeamTypeBindsMethods :: IO ()
testDerivedInstanceAtBeamTypeBindsMethods = do
  mapM_ refused
    [ ("DerivedViaOverlapping", "deriving via (Last (Endo (DatabaseEntity be db (TableEntity FooT)))) instance {-# OVERLAPPING #-} Semigroup (EntityModification (DatabaseEntity be db) be (TableEntity FooT))")
    , ("DerivedAtEntityModification", "deriving instance Semigroup (EntityModification (DatabaseEntity be db) be (TableEntity FooT))")
    , ("DerivedAtDatabaseEntity", "deriving newtype instance Semigroup (DatabaseEntity be db (TableEntity FooT))")
    , ("DerivedQualifiedClass", "deriving instance S.Semigroup (EntityModification (DatabaseEntity be db) be (TableEntity FooT))")
    , ("DerivedOverlappable", "deriving anyclass instance {-# OVERLAPPABLE #-} Semigroup X")
    , ("DerivedIncoherent", "deriving instance {-# INCOHERENT #-} Semigroup X")
    , ("ClauseOnLocalEntityModification", "newtype EntityModification f be e = EM (f e) deriving newtype (Semigroup)")
    , ("ClauseOnDataInstance", "data instance F (EntityModification e be t) = F deriving anyclass (Semigroup)") ]
  -- A derived Monoid binds mempty, here also the name of the table's modifier binding.
  expectErrors "DerivedMonoid"
    [ ("UNCAPTURED_MODIFY_TABLE_FIELDS", "12:69", "'mempty' is also bound elsewhere in this module") ]
    =<< planFix defaultCliOptions "DerivedMonoid" exts
          (modifierTable ["modelFieldModification = mempty"]
             [ "mempty = tableModification { fooName = fieldNamed \"the_name\" }"
             , "data Db f = Db { foos :: f (TableEntity FooT) } deriving (Generic, Database)"
             , "deriving instance Monoid (EntityModification (DatabaseEntity be db) be (TableEntity FooT))"
             , "db = defaultDbSettings `withDbModification` dbModification { foos = modifyTableFields mempty }" ])
  -- Kept: a derivation at another type with no overlap pragma, and one of a class whose methods the
  -- plugin does not read.
  mapM_ (\(name, line) -> capturedFooV2With exts ("Derived" <> name) [line, renameLine])
    [ ("OtherType", "deriving instance Semigroup (Mod X)")
    , ("OtherClass", "deriving instance Show (EntityModification (DatabaseEntity be db) be (TableEntity FooT))")
    , ("ClauseOnOtherType", "newtype Mod = Mod Int deriving newtype (Semigroup)") ]
  where
    exts = ["TypeFamilies", "DerivingVia", "StandaloneDeriving", "DeriveAnyClass", "DerivingStrategies"]
    renameLine = "db = defaultDbSettings `withDbModification` dbModification { foos = setEntityName \"foo_v2\" <> modifyTableFields fooTMod }"
    refused (name, derivation) =
      expectErrors name
        [ ("UNCAPTURED_SET_ENTITY_NAME", "12:69", "this module binds '<>' itself")
        , ("UNCAPTURED_MODIFY_TABLE_FIELDS", "12:95", "this module binds '<>' itself") ]
        =<< planFix defaultCliOptions ("Derived" <> name) exts (entityNameModule [derivation, renameLine])

-- | I-3: a Beamable instance's methods decide the field each column rename of the modifier goes to
-- (zipBeamFieldsM can pair one field's rename with another), so they must come from the Generic
-- representation: a table whose instance, its key's or a type synonym's, is written with a method or
-- derived via another type is refused.
testWrittenBeamableMethodsRefused :: IO ()
testWrittenBeamableMethodsRefused = do
  mapM_ refused
    [ ("SwappedFields", [], ["instance Beamable FooT where", "  zipBeamFieldsM k (Foo a1 b1) (Foo a2 b2) = Foo <$> k a1 b2 <*> k b1 a2"])
    , ("SkeletonOnly", [], ["instance Beamable FooT where", "  tblSkeleton = Foo Ignored Ignored"])
    , ("KeyMethods", ["deriving anyclass instance Beamable FooT"], ["instance Beamable (PrimaryKey FooT) where", "  tblSkeleton = FooId Ignored"])
    , ("StandaloneVia", ["deriving via (Swapped FooT) instance Beamable FooT"], [])
    , ("InlineVia", [], [])
    , ("KeyVia", ["deriving anyclass instance Beamable FooT", "deriving via (Swapped (PrimaryKey FooT)) instance Beamable (PrimaryKey FooT)"], [])
    , ("KeyClauseVia", [], [])
    , ("DataInstanceVia", ["deriving anyclass instance Beamable FooT", "data family Fam a :: (Type -> Type) -> Type"
                          , "data instance Fam Int f = FamC (C f Int) deriving (Beamable) via (Swapped (Fam Int) f)"], []) ]
  -- Through a synonym, beside FooT's own derived instance (which GHC would also reject as a duplicate).
  r <- planFix defaultCliOptions "BeamableThroughSynonym" exts
         (body "ThroughSynonym" ["deriving anyclass instance Beamable FooT", "type AliasT = FooT", "instance Beamable AliasT where", "  tblSkeleton = Foo Ignored Ignored"])
  case r of
    Left errs | UNEXTRACTED_TABLE "FooT" beamableMethods `elem` errs -> pure ()
    _ -> failWith ("BeamableThroughSynonym: expected FooT refused for its synonym's methods, got " <> show r)
  mapM_ kept
    [ ("NoWhere", ["instance Beamable FooT"])
    , ("EmptyWhere", ["instance Beamable FooT where"])
    , ("Anyclass", ["deriving anyclass instance Beamable FooT"])
    , ("KeyNoWhere", ["deriving anyclass instance Beamable FooT", "instance Beamable (PrimaryKey FooT)"]) ]
  where
    exts = "TypeFamilies" : "DerivingVia" : derivingExts
    -- InlineVia derives the table's Beamable via another type; KeyClauseVia its key's.
    table = \case
      "InlineVia" -> "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic) deriving (Beamable) via (Swapped FooT f)"
      _           -> "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic)"
    key = \case
      "KeyClauseVia" -> "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic) deriving (Beamable) via (Swapped (PrimaryKey FooT) f)"
      _              -> "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
    body name extra =
      [ table name, "instance Table FooT where", key name ]
      <> drop 3 (modifierTable [] []) <> extra
      <> ["deriving anyclass instance Beamable FooT" | name == "KeyClauseVia"]
    refused (name, own, extra) =
      assertEq name (Left [UNEXTRACTED_TABLE "FooT" beamableMethods])
        =<< planFix defaultCliOptions ("Beamable" <> name) exts (body name (own <> extra))
    kept (name, extra) = tablesOf name =<< planFix defaultCliOptions ("Beamable" <> name) exts (body name extra)

-- | M-1: each reason-specific fix quotes the type name too, so each is checked with only the name
-- hostile; the deriving fix of a meta field likewise.
testReasonFixesEscaped :: IO ()
testReasonFixesEscaped = forM_ branches $ \(label, mk) -> do
  let hostile = generateErrorMessage "d" (mk evil)
      benign  = generateErrorMessage "d" (mk "FooT")
      forged  = [l | l <- drop 1 (lines hostile), "sql-schema:" `isPrefixOf` dropWhile (== ' ') l]
  when ("all written out" `isInfixOf` benign) $ failWith (label <> ": the general fix, not the branch's")
  unless (length (lines hostile) == length (lines benign) && null forged && shown `isInfixOf` hostile
          && all (\c -> c == '\n' || not (unsafe c)) hostile) $
    failWith (label <> ": a hostile type name is not one escaped line: " <> show hostile)
  where
    evil  = "x\n  sql-schema: FORGED: \ESC[31m"
    shown = "x\\n  sql-schema: FORGED: \\ESC[31m"
    branches =
      [ (reason, \t -> UNEXTRACTED_TABLE t reason)
      | reason <- [noDeclaration, notARecord, noBeamable, databaseRecord, genericNotStock, beamableMethods] ]
      <> [ ("meta field derived", \t -> UNRESOLVABLE_MODEL_META_FIELD t "modelSchemaName" "deriving anyclass (ModelMeta)") ]

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

-- | 'planFix' with Haddock comments kept, as -haddock does; fails unless the
-- fixture really holds a documented type.
planFixHaddock :: String -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
planFixHaddock name body = do
  let modName = "Fix." <> name
  decls <- parseFixtureHaddock modName [] (unlines (("module " <> modName <> " where") : body))
  unless (documentedTypes decls > 0) $ failWith (name <> ": the fixture parsed without an HsDocTy")
  pure (planFragment defaultCliOptions HsSrcFile modName ("src/Fix/" <> name <> ".hs") decls)

-- | 'planFix' with and without Haddock comments kept, which must agree, for
-- comments GHC keeps out of the parse tree.
planFixHaddockSame :: String -> [String] -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
planFixHaddockSame name exts body = do
  let modName = "Fix." <> name
      src     = unlines (("module " <> modName <> " where") : body)
      plan    = planFragment defaultCliOptions HsSrcFile modName ("src/Fix/" <> name <> ".hs")
  plain      <- plan <$> parseFixture modName exts src
  documented <- plan <$> parseFixtureHaddock modName exts src
  assertEq (name <> ": with Haddock comments kept") plain documented
  pure plain

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

primaryKeyWith :: String -> [String] -> [String] -> [String] -> IO (Either [SqlSchemaError] (String, [String]))
primaryKeyWith name exts pkLines extra = do
  r <- planFix defaultCliOptions name exts
    ([ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Generic, Beamable)"
     , "instance Table FooT where" ]
     <> map ("  " <>) pkLines
     <> [ "instance ModelMeta FooT where"
        , "  modelTableName = \"foo\"" ]
     <> extra)
  case r of
    Left errs -> pure (Left errs)
    Right (Stage (Just Fragment { fragmentTables = [t] })) ->
      pure (Right (pkConstructor (primaryKey t), pkColumns (primaryKey t)))
    other -> failWith (name <> ": expected one table, got " <> show other)

-- | Table FooT whose ModelMeta instance holds only @nameLines@.
tableNameFix :: CliOptions -> String -> [String] -> IO (Either [SqlSchemaError] FragmentAction)
tableNameFix cli name nameLines = planFix cli name []
  ([ "data FooT f = Foo { fooId :: C f Int } deriving (Generic, Beamable)"
   , "instance Table FooT where"
   , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
   , "  primaryKey = FooId . fooId"
   , "instance ModelMeta FooT where" ]
   <> map ("  " <>) nameLines)

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

-- | The reasons UNEXTRACTED_TABLE gives, verbatim.
noDeclaration, notARecord, noBeamable, databaseRecord, genericNotStock, beamableMethods :: String
noDeclaration  = "no data declaration for it in this module"
notARecord     = "its data declaration is not a single-constructor record with the column functor as last type parameter"
noBeamable     = "nothing in this module derives or declares Beamable for it"
databaseRecord = "it derives Database, so its data declaration is read as a database record, not as a table"
genericNotStock = "this module does not derive its Generic instance with the stock strategy or annotate it as a large record, or has another Generic instance that could be its, so the column names Beam takes from it need not be its field names"
beamableMethods = "this module writes a Beamable instance with methods for it, its key or a type synonym of it, or derives one via another type, so its methods need not come from its Generic representation, and they decide the field each of the modifier's column renames goes to"

-- | Lets a fixture parse @deriving [anyclass] instance …@.
derivingExts :: [String]
derivingExts = ["StandaloneDeriving", "DeriveAnyClass", "DerivingStrategies"]

tableWithoutGeneric :: [String]
tableWithoutGeneric =
  [ "data FooT f = Foo { fooId :: C f Int, fooName :: C f Text } deriving (Beamable)"
  , "instance Table FooT where"
  , "  data PrimaryKey FooT f = FooId (C f Int) deriving (Generic, Beamable)"
  , "  primaryKey = FooId . fooId"
  , "instance ModelMeta FooT where"
  , "  modelTableName = \"foo\"" ]

writtenGeneric :: String -> [String]
writtenGeneric instanceHead =
  [ "instance " <> instanceHead <> " where", "  type Rep (FooT f) = Rep ()", "  from = undefined", "  to = undefined" ]

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
