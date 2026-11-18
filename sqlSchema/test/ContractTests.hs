{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of what the plugin and the merge read and write: contracts and fragments with keys they
-- do not know, lack or repeat, plugin options that repeat a key, strings a YAML 1.1 reader would
-- not read as strings, and the merge's refusal to prune a fragment whose source file still exists.
module ContractTests (run) where

import           Control.Exception     (IOException, SomeException, finally, try)
import           Control.Monad         (forM_)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List             (isPrefixOf)
import qualified Data.Yaml             as YAML
import           System.Directory      (createDirectoryIfMissing, emptyPermissions, getPermissions,
                                        removeFile, searchable, setPermissions, withCurrentDirectory)
import           System.Exit           (ExitCode (..))
import           System.FilePath       ((</>))

import           SqlSchema.Merge
import           SqlSchema.MergeCli    (mergeMain)
import           SqlSchema.Plugin      (parseCliOptions, pendingPath, stagePending, stagedTables)
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
  testIncludeUnknownKeysRefused
  testFragmentUnknownKeysRefused
  testStagedUnknownKeysRefused
  testOptionsRepeatedKeyRefused
  testContractQuotesYaml11Scalars
  testStagedFragmentQuotesYaml11Scalars
  testStagedMissingText
  testStagedUnreadableFix
  testUnlistedSourceRefused
  testUnlistedSourceCli
  testDeletedSourcePruned
  testUnrecordedFragmentPruned
  testRecordedSourceInertWithoutSourceDirs
  testUncheckableSourceFails
  testRequiredKeysRefused
  testRepeatedYamlKeyRefused
  testUnparseableStaleFragmentGuarded
  testUncheckableRecordRefused

-- ---------------------------------------------------------------------------
-- Keys a contract or fragment of this version does not have
-- ---------------------------------------------------------------------------

-- | One contract holding every object the merge writes, each with one marker line where a test
-- puts an extra key, or a key it misspells.
contractText :: (String -> String) -> String
contractText at = unlines
  [ "mergedTables:"
  , "- codeName: GadgetT"
  , "  sourceModule: Lib.Gadget"
  , "  modelTableName: gadget"
  , "  modelTableType: null"
  , "  modelSchemaName:"
  , "    tag: SchemaLiteral"
  , "    contents: public"
  , at "schema"
  , "  primaryKey:"
  , "    pkConstructor: GadgetId"
  , "    pkColumns: [gadgetId]"
  , at "key"
  , "  columns:"
  , "  - hsField: gadgetId"
  , "    column: id"
  , "    hsType: Int"
  , "    nullable: false"
  , "    isPrimaryKey: true"
  , at "column"
  , at "table"
  , "mergedDbEntityOverrides:"
  , "- dbField: gadgets"
  , "  dbTableType: GadgetT"
  , "  sqlName: gadget"
  , "  overrideSourceModule: Lib.Db"
  , "  overrideSourceFile: src/Lib/Db.hs"
  , at "override"
  , "generator:"
  , "  name: sql-schema"
  , "  version: 0.4.3"
  , "  rev: null"
  , at "generator"
  , at "top"
  ]

-- | The extra key for the marker @which@, indented to its object.
extraAt :: String -> String -> String -> String
extraAt which key at
  | at /= which = ""
  | otherwise   = indent <> key <> ": [1, 2, 3]"
  where
    indent = case which of
      "schema"    -> "    "
      "key"       -> "    "
      "column"    -> "    "
      "table"     -> "  "
      "override"  -> "  "
      "generator" -> "  "
      _           -> ""

-- | A key the merge does not know, or a misspelled one, in an included contract fails the merge:
-- ignored, a misspelled modelSchemaName would read as null and route the table to MySQL.
testIncludeUnknownKeysRefused :: IO ()
testIncludeUnknownKeysRefused = withTmpProject $ \tmp -> do
  let inc  = tmp </> "inc.yaml"
      opts = (baseOpts tmp) { moIncludeMerged = [inc], moNoOwnTables = True }
  forM_ ["schema", "key", "column", "table", "override", "generator", "top"] $ \which -> do
    BS.writeFile inc (BC.pack (contractText (extraAt which "futureKey")))
    r <- runMerge opts
    expectMergeError "--include-merged file" r
    expectMergeError "futureKey" r
  BS.writeFile inc (BC.pack (replaceOnce "  modelSchemaName:" "  modelSchemaNam:" (contractText (const ""))))
  r <- runMerge opts
  expectMergeError "--include-merged file" r
  expectMergeError "modelSchemaNam" r
  assertAbsent (tmp </> "merged.yaml")
  BS.writeFile inc (BC.pack (contractText (const "")))
  report <- expectOk =<< runMerge opts
  assertEq "the control contract's tables" 1 (mrIncludedTables report)

-- | The same in a fragment: the merge refuses it as unparseable while its module is live.
testFragmentUnknownKeysRefused :: IO ()
testFragmentUnknownKeysRefused = withTmpProject $ \tmp -> do
  let dir  = tmp </> "fragments"
      frag = dir </> "Lib.Gadget.yaml"
  createDirectoryIfMissing True dir
  forM_ [ ("fragment", fragmentYaml <> "fragmentFutureKey: 1\n")
        , ("table", replaceOnce "  modelTableName: gadget\n" "  modelTableName: gadget\n  futureKey: 1\n" fragmentYaml)
        , ("misspelled", replaceOnce "  modelSchemaName: null" "  modelSchemaNam: null" fragmentYaml) ] $ \(label, text) -> do
    BS.writeFile frag (BC.pack text)
    r <- runMerge (baseOpts tmp)
    expectMergeError "unparseable fragment" r
    expectMergeError (if label == "misspelled" then "modelSchemaNam" else "utureKey") r
  BS.writeFile frag (BC.pack fragmentYaml)
  _ <- expectOk =<< runMerge (baseOpts tmp)
  pure ()

fragmentYaml :: String
fragmentYaml = unlines
  [ "fragmentModule: Lib.Gadget"
  , "fragmentTables:"
  , "- codeName: GadgetT"
  , "  sourceModule: Lib.Gadget"
  , "  modelTableName: gadget"
  , "  modelTableType: null"
  , "  modelSchemaName: null"
  , "  primaryKey: {pkConstructor: GadgetId, pkColumns: [gadgetId]}"
  , "  columns:"
  , "  - {hsField: gadgetId, column: id, hsType: Int, nullable: false, isPrimaryKey: true}"
  , "fragmentDbEntityOverrides: []"
  ]

-- | And in the file the type-check step reads back: STAGED_FRAGMENT_UNREADABLE, naming the key.
testStagedUnknownKeysRefused :: IO ()
testStagedUnknownKeysRefused = withTmpProject $ \tmp -> do
  let frag = tmp </> "Lib.Gadget.yaml"
  BS.writeFile (pendingPath frag) (BC.pack fragmentYaml)
  assertEq "the control's tables" ["GadgetT"] . map codeName =<< stagedTables frag
  forM_ [ ("an extra key", fragmentYaml <> "fragmentFutureKey: 1\n", "fragmentFutureKey")
        , ("a misspelled key", replaceOnce "  modelSchemaName: null" "  modelSchemaNam: null" fragmentYaml, "modelSchemaNam") ] $
    \(label, text, key) -> do
      BS.writeFile (pendingPath frag) (BC.pack text)
      try (stagedTables frag) >>= \case
        Left (e :: SomeException)
          | "sql-schema: STAGED_FRAGMENT_UNREADABLE: " `isInfixOf` show e && key `isInfixOf` show e -> pure ()
        other -> failWith (label <> ": expected STAGED_FRAGMENT_UNREADABLE naming " <> key <> ", got " <> show (fmap (map codeName) other))

-- ---------------------------------------------------------------------------
-- Plugin options that give a key twice
-- ---------------------------------------------------------------------------

-- | aeson keeps one value of a repeated key without a word, so the option is refused instead,
-- at the top level and inside tableNameOverrides, also when an escape spells the key.
testOptionsRepeatedKeyRefused :: IO ()
testOptionsRepeatedKeyRefused = do
  forM_ [ ("{\"fragmentsDir\":\"a/frags\",\"fragmentsDir\":\"b/frags\"}", "fragmentsDir")
        , ("{\"allowUnknownColumnWrapper\":false,\"allowUnknownColumnWrapper\":true}", "allowUnknownColumnWrapper")
        , ("{\"tableNameOverrides\":{\"SettingsT\":\"settings\",\"SettingsT\":\"settings_v2\"}}", "SettingsT")
        , ("{\"fragmentsDir\":\"a/frags\",\"fragments\\u0044ir\":\"b/frags\"}", "fragmentsDir") ] $ \(raw, key) ->
    case parseCliOptions [raw] of
      Left msg | "twice" `isInfixOf` msg && key `isInfixOf` msg -> pure ()
      other -> failWith ("a repeated key in " <> raw <> ": expected a refusal naming " <> key <> ", got " <> show other)
  -- Whitespace, escapes and empty objects are no repeat, nor is one key in two different objects.
  forM_ [ "  {\"fragmentsDir\":\"a/frags\"}  ", "\n{ \"fragmentsDir\" : \"a/frags\" , \"blacklistModules\" : [ \"X.\" ] }\n"
        , "{\"fragmentsDir\":\"a\\\"b\",\"tableNameOverrides\":{}}" ] $ \raw ->
    either (\msg -> failWith ("refused " <> raw <> ": " <> msg)) (const (pure ())) (parseCliOptions [raw])
  case parseCliOptions ["{\"fragmentsDir\":\"a/frags\",\"tableNameOverrides\":{\"fragmentsDir\":\"x\"}}"] of
    Right cli -> assertEq "both values" ("a/frags", "fromList [(\"fragmentsDir\",\"x\")]")
                   (fragmentsDir cli, show (tableNameOverrides cli))
    Left msg  -> failWith ("one key in two objects refused: " <> msg)

-- ---------------------------------------------------------------------------
-- Strings a YAML 1.1 reader takes for another type
-- ---------------------------------------------------------------------------

-- | Each string PyYAML's resolvers would read as an int, float, timestamp, merge key or value, as
-- the validator loads the contract; Data.Yaml leaves them plain.
yaml11Strings :: [String]
yaml11Strings = ["1_000", "0b101", "1:20", "190:20:30", "2001-12-14", "2001-12-14t21:59:43.10-05:00", ".inf", "-.inf", ".NaN", "1_0.5", "<<", "="]

-- | They are single-quoted in the contract, and nothing else is.
testContractQuotesYaml11Scalars :: IO ()
testContractQuotesYaml11Scalars = withTmpProject $ \tmp -> do
  let dir = tmp </> "fragments"
      table = (sampleTable "Lib.Odd.OddT" "Lib.Odd" "1:20")
        { modelTableType = Just "2001-12-14", modelSchemaName = Just (SchemaLiteral ".inf")
        , columns = [ ColumnInfo { hsField = "f" <> show i, column = s, hsType = "Int", nullable = False, isPrimaryKey = False }
                    | (i, s) <- zip [(0 :: Int) ..] yaml11Strings ] }
  createDirectoryIfMissing True dir
  writeFragYaml (dir </> "Lib.Odd.yaml") (Fragment "Lib.Odd" [table] [])
  _ <- expectOk =<< runMerge (baseOpts tmp)
  out <- BC.unpack <$> BS.readFile (tmp </> "merged.yaml")
  forM_ yaml11Strings $ \s -> assertMentions ("column " <> s) ("column: '" <> s <> "'") out
  assertMentions "the table name" "modelTableName: '1:20'" out
  assertMentions "the table type" "modelTableType: '2001-12-14'" out
  assertMentions "the schema" "contents: '.inf'" out
  assertMentions "a plain string stays plain" "sourceModule: Lib.Odd" out
  assertMentions "a field name stays plain" "hsField: f0" out
  merged <- decodeFile (tmp </> "merged.yaml")
  assertEq "read back" [table] (mergedTables merged)

-- | The staged fragment is written the same way.
testStagedFragmentQuotesYaml11Scalars :: IO ()
testStagedFragmentQuotesYaml11Scalars = withTmpProject $ \tmp -> do
  let frag = tmp </> "Lib.Odd.yaml"
  stagePending frag (Just (sampleFragment "Lib.Odd" "Lib.Odd.OddT" "1:20"))
  staged <- BC.unpack <$> BS.readFile (pendingPath frag)
  assertMentions "the staged table name" "modelTableName: '1:20'" staged
  assertMentions "a plain string stays plain" "fragmentModule: Lib.Odd" staged

-- ---------------------------------------------------------------------------
-- STAGED_FRAGMENT_MISSING's text
-- ---------------------------------------------------------------------------

-- | Raised after type checking and at publish, so the text names both.
testStagedMissingText :: IO ()
testStagedMissingText = assertEq "STAGED_FRAGMENT_MISSING"
  (  "sql-schema: STAGED_FRAGMENT_MISSING: Nothing is staged at 'd/Fix.Foo.yaml.pending': the plugin stages a module's fragment there when it parses the module, reads it back after type checking to compare its tables with GHC's, and publishes it once the module has passed type checking, desugaring and -Werror, but something removed it in between.\n"
  <> "  Without it the tables cannot be compared, and publishing nothing would keep the module's previous fragment in the contract, so the compile fails instead.\n"
  <> "  Keep fragmentsDir for this plugin's files only, never compile two modules of the same name into it at the same time, then rebuild the module." )
  (generateErrorMessage "d" (STAGED_FRAGMENT_MISSING "d/Fix.Foo.yaml.pending"))

-- | Only another writer garbles a staged file, so the fix says so; e2e S10d pins it on a real compile.
testStagedUnreadableFix :: IO ()
testStagedUnreadableFix = assertMentions "STAGED_FRAGMENT_UNREADABLE's fix"
  "\n  Something other than this plugin wrote the file: keep fragmentsDir for this plugin's files only, then rebuild the module."
  (generateErrorMessage "d" (STAGED_FRAGMENT_UNREADABLE "d/Fix.Foo.yaml.pending" "YAML parse exception"))

-- ---------------------------------------------------------------------------
-- A fragment pruned while its source file exists
-- ---------------------------------------------------------------------------

-- | A table module in @src@, one in @gen@, each with its fragment and the source file the plugin
-- recorded, relative to @tmp@, where the compile ran; @record@ chooses which record one.
srcGenProject :: FilePath -> (String -> Bool) -> IO ()
srcGenProject tmp record = do
  touchFile (tmp </> "src" </> "App" </> "A.hs")
  touchFile (tmp </> "gen" </> "App" </> "B.hs")
  createDirectoryIfMissing True (tmp </> "frags")
  forM_ [("App.A", "src/App/A.hs"), ("App.B", "gen/App/B.hs")] $ \(m, path) ->
    BS.writeFile (tmp </> "frags" </> (m <> ".yaml")) $
      YAML.encode (sampleFragment m (m <> ".T") (m <> "_tab"))
      <> (if record m then BC.pack ("fragmentSourceFile: " <> path <> "\n") else BS.empty)

srcOnly :: MergeOptions
srcOnly = (defaultMergeOptions "frags" "out.yaml") { moSourceDirs = ["src"] }

-- | The reliability review's probe: a component whose sources are in @src@ and @gen@, merged with
-- @--source-dirs src@ only.  Pruning App.B would drop a live table with exit 0.
testUnlistedSourceRefused :: IO ()
testUnlistedSourceRefused = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  r <- runMerge srcOnly
  expectMergeError "sql-schema: UNLISTED_SOURCE_FILE: The fragment of module 'App.B' would be pruned as stale" r
  expectMergeError "the source file the plugin recorded for it, 'gen/App/B.hs', exists" r
  either (\errs -> assertLacks "App.A, which is listed" "'App.A'" (concat errs)) (const (pure ())) r
  assertAbsent "out.yaml"
  report <- expectOk =<< runMerge srcOnly { moSourceDirs = ["src", "gen"] }
  assertEq "every directory listed: nothing pruned" (2, 0) (mrTablesEmitted report, mrStalePruned report)

testUnlistedSourceCli :: IO ()
testUnlistedSourceCli = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  (code, err) <- withStderr tmp (mergeMain ["--fragments", "frags", "--out", "out.yaml", "--source-dirs", "src"])
  assertEq "exit code" (ExitFailure 1) code
  assertMentions "the code" "sql-schema: UNLISTED_SOURCE_FILE: " err
  assertLacks "no prune line" "pruned the fragment" err
  assertAbsent "out.yaml"

-- | A recorded source that is gone is a removed module: pruned, as before.
testDeletedSourcePruned :: IO ()
testDeletedSourcePruned = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  removeFile ("gen" </> "App" </> "B.hs")
  (r, err) <- withStderr tmp (runMerge srcOnly)
  report <- expectOk r
  assertEq "pruned" (1, 1) (mrTablesEmitted report, mrStalePruned report)
  assertMentions "the prune line" "pruned the fragment of \"App.B\"" err

-- | A fragment that records no source, from a plugin before this one, is pruned, as before.
testUnrecordedFragmentPruned :: IO ()
testUnrecordedFragmentPruned = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (== "App.A")
  report <- expectOk =<< runMerge srcOnly
  assertEq "pruned" (1, 1) (mrTablesEmitted report, mrStalePruned report)

-- | Without --source-dirs nothing is pruned, so nothing is checked.
testRecordedSourceInertWithoutSourceDirs :: IO ()
testRecordedSourceInertWithoutSourceDirs = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  report <- expectOk =<< runMerge srcOnly { moSourceDirs = [] }
  assertEq "kept" (2, 0) (mrTablesEmitted report, mrStalePruned report)

-- | A recorded source the merge can't check fails it rather than reading as gone, with a code that
-- names the module, the fragment and the system's error, from the CLI too.
testUncheckableSourceFails :: IO ()
testUncheckableSourceFails = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  before <- getPermissions "gen"
  setPermissions "gen" emptyPermissions
  withheld <- not . searchable <$> getPermissions "gen"
  flip finally (setPermissions "gen" before) $
    whenWithheld withheld "testUncheckableSourceFails" "a recorded source that can't be checked fails the merge" $ do
      r <- try (runMerge srcOnly)
      (code, err) <- withStderr tmp (mergeMain ["--fragments", "frags", "--out", "out.yaml", "--source-dirs", "src"])
      case r of
        Left (e :: IOException) -> failWith ("a recorded source that can't be checked: expected a coded refusal, got " <> show e)
        Right result -> forM_ [ "sql-schema: UNCHECKABLE_SOURCE_FILE: The fragment of module 'App.B' at 'frags/App.B.yaml' "
                              , "gen/App/B.hs", "ermission denied" ] $ \needle -> expectMergeError needle result
      assertEq "exit code" (ExitFailure 1) code
      assertMentions "the CLI's code" "sql-schema: UNCHECKABLE_SOURCE_FILE: The fragment of module 'App.B'" err
      assertAbsent "out.yaml"

-- ---------------------------------------------------------------------------
-- Keys a writer of another version leaves out, and keys given twice
-- ---------------------------------------------------------------------------

-- | Every key the merge writes must be present, null allowed for an optional value: aeson reads an
-- absent optional key as null, so an include without modelSchemaName would route its table to
-- MySQL.  Only the generator stamp may be absent, as in contracts written before 0.4.3.
testRequiredKeysRefused :: IO ()
testRequiredKeysRefused = withTmpProject $ \tmp -> do
  let inc  = tmp </> "inc.yaml"
      opts = (baseOpts tmp) { moIncludeMerged = [inc], moNoOwnTables = True }
      full = contractText (const "")
  forM_ [ ("modelSchemaName", "  modelSchemaName:\n    tag: SchemaLiteral\n    contents: public\n")
        , ("modelTableType", "  modelTableType: null\n")
        , ("rev", "  rev: null\n") ] $ \(key, gone) -> do
    BS.writeFile inc (BC.pack (replaceOnce gone "" full))
    r <- runMerge opts
    expectMergeError "--include-merged file" r
    expectMergeError ("key \\\"" <> key <> "\\\" not found") r
  -- aeson already requires the contents of a tagged constructor; its message differs by version.
  BS.writeFile inc (BC.pack (replaceOnce "    contents: public\n" "" full))
  r0 <- runMerge opts
  expectMergeError "--include-merged file" r0
  expectMergeError "contents" r0
  assertAbsent (tmp </> "merged.yaml")
  BS.writeFile inc (BC.pack (replaceOnce "generator:\n  name: sql-schema\n  version: 0.4.3\n  rev: null\n" "" full))
  report <- expectOk =<< runMerge opts
  assertEq "a contract without the stamp" 1 (mrIncludedTables report)
  let dir  = tmp </> "fragments"
      frag = dir </> "Lib.Gadget.yaml"
  createDirectoryIfMissing True dir
  BS.writeFile frag (BC.pack (replaceOnce "  modelSchemaName: null\n" "" fragmentYaml))
  r <- runMerge (baseOpts tmp)
  expectMergeError "unparseable fragment" r
  expectMergeError "modelSchemaName" r
  BS.writeFile (pendingPath frag) (BC.pack (replaceOnce "  modelTableType: null\n" "" fragmentYaml))
  expectStagedUnreadable "a staged fragment without modelTableType" "modelTableType" frag

-- | A key given twice in one mapping: Data.Yaml keeps the last value without a word, so an include
-- that repeats mergedDbEntityOverrides would lose the first list's renames.
testRepeatedYamlKeyRefused :: IO ()
testRepeatedYamlKeyRefused = withTmpProject $ \tmp -> do
  let inc  = tmp </> "inc.yaml"
      opts = (baseOpts tmp) { moIncludeMerged = [inc], moNoOwnTables = True }
      full = contractText (const "")
  forM_ [ ("mergedDbEntityOverrides", full <> "mergedDbEntityOverrides: []\n")
        , ("modelTableName", replaceOnce "  modelTableName: gadget\n" "  modelTableName: gadget\n  modelTableName: gadget_v2\n" full)
        , ("pkConstructor", replaceOnce "    pkConstructor: GadgetId\n" "    pkConstructor: GadgetId\n    pkConstructor: GadgetKey\n" full) ] $
    \(key, text) -> do
      BS.writeFile inc (BC.pack text)
      r <- runMerge opts
      expectMergeError "--include-merged file" r
      expectMergeError (key <> ": the key is given more than once") r
  assertAbsent (tmp </> "merged.yaml")
  let dir  = tmp </> "fragments"
      frag = dir </> "Lib.Gadget.yaml"
  createDirectoryIfMissing True dir
  BS.writeFile frag (BC.pack (fragmentYaml <> "fragmentTables: []\n"))
  r <- runMerge (baseOpts tmp)
  expectMergeError "unparseable fragment" r
  expectMergeError "fragmentTables: the key is given more than once" r
  BS.writeFile (pendingPath frag) (BC.pack (fragmentYaml <> "fragmentTables: []\n"))
  expectStagedUnreadable "a staged fragment that repeats fragmentTables" "fragmentTables: the key is given more than once" frag

expectStagedUnreadable :: String -> String -> FilePath -> IO ()
expectStagedUnreadable label needle frag = try (stagedTables frag) >>= \case
  Left (e :: SomeException)
    | "sql-schema: STAGED_FRAGMENT_UNREADABLE: " `isInfixOf` show e && needle `isInfixOf` show e -> pure ()
  other -> failWith (label <> ": expected STAGED_FRAGMENT_UNREADABLE naming " <> needle <> ", got " <> show (fmap (map codeName) other))

-- ---------------------------------------------------------------------------
-- The pruning check on fragments that do not decode, and on paths it cannot check
-- ---------------------------------------------------------------------------

-- | App.B's fragment, as 'srcGenProject' wrote it, changed by @f@; run in @tmp@.
editB :: (String -> String) -> IO ()
editB f = do
  let p = "frags" </> "App.B.yaml"
  text <- BC.unpack <$> BS.readFile p
  BS.writeFile p (BC.pack (f text))

-- | A stale fragment that no longer decodes is held to the same check, its recorded source read
-- from it as plain YAML: pruned, it would drop the tables of a module still compiled.
testUnparseableStaleFragmentGuarded :: IO ()
testUnparseableStaleFragmentGuarded = withTmpProject $ \tmp -> withCurrentDirectory tmp $ do
  srcGenProject tmp (const True)
  editB (<> "fragmentFutureKey: 1\n")
  (r, held) <- withStderr tmp (runMerge srcOnly)
  expectMergeError "sql-schema: UNLISTED_SOURCE_FILE: The fragment of module 'App.B' would be pruned as stale" r
  expectMergeError "'gen/App/B.hs', exists" r
  assertLacks "a held fragment's prune line" "pruned the fragment" held
  assertAbsent "out.yaml"
  -- Pruned, as a fragment without a record is.
  forM_ [ ("no record", unlines . filter (not . ("fragmentSourceFile:" `isPrefixOf`)) . lines)
        , ("a record that is no string", replaceOnce "fragmentSourceFile: gen/App/B.hs" "fragmentSourceFile: [gen/App/B.hs]")
        , ("no YAML", const "fragmentModule: [unclosed\n") ] $ \(label, f) -> do
    srcGenProject tmp (const True)
    editB (\t -> f t <> "fragmentFutureKey: 1\n")
    (r', err) <- withStderr tmp (runMerge srcOnly)
    case r' of
      Right report -> assertEq (label <> ": pruned") (1, 1) (mrTablesEmitted report, mrStalePruned report)
      Left errs    -> failWith (label <> ": expected the fragment pruned, got " <> show errs)
    assertMentions (label <> ": the prune line") "pruned the fragment of \"App.B\"" err
  srcGenProject tmp (const True)
  editB (<> "fragmentFutureKey: 1\n")
  removeFile ("gen" </> "App" </> "B.hs")
  report <- expectOk =<< runMerge srcOnly
  assertEq "a recorded source that is gone: pruned" (1, 1) (mrTablesEmitted report, mrStalePruned report)

-- | A recorded path that cannot name the file fails the merge, coded, naming the module and the
-- fragment: an empty one, or one holding U+FFFD, which the plugin writes for bytes of a file name
-- that are not UTF-8, so the file is not at the path.  Read as gone, it would be pruned.
testUncheckableRecordRefused :: IO ()
testUncheckableRecordRefused = withTmpProject $ \tmp -> withCurrentDirectory tmp $
  forM_ [("an empty path", "", "the recorded path is empty"), ("a path holding U+FFFD", "gen/App/\xFFFD.hs", "U+FFFD")] $
    \(label, path, why) -> do
      srcGenProject tmp (== "App.A")
      BS.writeFile ("frags" </> "App.B.yaml") (encodeFragmentFile (Just path) (sampleFragment "App.B" "App.B.T" "App.B_tab"))
      r <- runMerge srcOnly
      case r of
        Left errs | any (\e -> all (`isInfixOf` e) [ "sql-schema: UNCHECKABLE_SOURCE_FILE: The fragment of module 'App.B' at 'frags/App.B.yaml' "
                                                  , why ]) errs -> pure ()
        other -> failWith (label <> ": expected UNCHECKABLE_SOURCE_FILE naming App.B, its fragment and " <> show why <> ", got " <> show other)
      assertAbsent "out.yaml"

replaceOnce :: String -> String -> String -> String
replaceOnce old new s = case breakOn s of
  Just (a, b) -> a <> new <> drop (length old) b
  Nothing     -> error ("replaceOnce: no " <> show old)
  where
    breakOn x
      | take (length old) x == old = Just ("", x)
      | otherwise = case x of
          c : cs -> (\(a, b) -> (c : a, b)) <$> breakOn cs
          []     -> Nothing
