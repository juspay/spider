-- | In-process tests of the merge CLI: its exit codes, what it prints, and that
-- nothing reaches @--out@ unless the merge succeeds.
module CliTests (run) where

import           Control.Exception     (finally)
import           Control.Monad         (forM_, when)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.IORef            (newIORef, readIORef, writeIORef)
import           Data.List             (isPrefixOf, isSuffixOf, sort)
import           GHC.IO.Handle         (hDuplicate, hDuplicateTo)
import           System.Directory      (createDirectoryIfMissing, createDirectoryLink, createFileLink,
                                        emptyPermissions, getPermissions, listDirectory,
                                        pathIsSymbolicLink, removeFile, searchable, setOwnerReadable,
                                        setOwnerSearchable, setOwnerWritable, setPermissions,
                                        withCurrentDirectory, writable)
import           System.Exit           (ExitCode (..))
import           System.FilePath       ((</>))
import           System.IO             (Handle, IOMode (ReadMode), hClose, openBinaryFile, stderr,
                                        stdout)

import           SqlSchema.Merge
import           SqlSchema.MergeCli    (mergeMain, parseArgs, usage)
import           SqlSchema.Types
import           TestUtil

run :: IO ()
run = do
  testCliSuccessExit0
  testCliNoTablesExit1
  testCliRefusalKeepsOldOut
  testCliAllowEmpty
  testCliIncludeOnlyExit1
  testCliIncludeOnlyNoOwnTables
  testCliNoOwnTablesWithOwnTablesExit1
  testCliOptOutFlags
  testCliOptOutChecksExit1
  testCliValidationErrorExit1
  testCliIoErrorExit1
  testCliForeignTmpUntouched
  testCliReplacesReadOnlyOut
  testCliTempNextToOut
  testCliMistypedSourceDirExit1
  testCliSourceDirProblemsWarn
  testCliSourceDirLinkIntoUnsearchableExit1
  testCliSourceDirDanglingLinkIgnored
  testCliSourceDirLinkLoopWarns
  testCliSourceDirSelfLinkKeepsModules
  testCliSourceDirTwoSelfLinksTerminate
  testCliSourceDirAliasKeepsModules
  testCliSourceDirLongChainKeepsModules
  testCliWalkErrorKeepsEarlierWarnings
  testCliPrunedModuleNameEscaped
  testCliHostileNamesEscaped
  testCliFragmentsIsFileExit1
  testCliMissingFragmentsWarns
  testSkipNoticeIsVisible
  testCliClosedStdoutExit0
  testCliArgErrorsExit2
  testCliFlagMissingValueExit2
  testCliDuplicateFlagExit2
  testCliEmptyValuesExit2
  testCliFlagLikeValueExit2
  testParseArgs
  testUsageStatesRules

liveProject :: FilePath -> IO ()
liveProject tmp = do
  touchFile (srcDir tmp </> "Foo.hs")
  createDirectoryIfMissing True (fragDir tmp)
  writeFragYaml (fragDir tmp </> "Foo.yaml") (sampleFragment "Foo" "Foo.FooT" "foo")

srcDir, fragDir, outFile :: FilePath -> FilePath
srcDir  tmp = tmp </> "src"
fragDir tmp = tmp </> "fragments"
outFile tmp = tmp </> "merged.yaml"

argsFor :: FilePath -> [String]
argsFor tmp = ["--fragments", fragDir tmp, "--out", outFile tmp]

-- | The entries of @tmp@ that belong to a write of @--out@: the contract, and any @*.tmp@ file.
outEntries :: FilePath -> IO [FilePath]
outEntries tmp = filter owned . sort <$> listDirectory tmp
  where owned n = "merged.yaml" `isPrefixOf` n || ".tmp" `isSuffixOf` n

assertNothingWritten :: FilePath -> IO ()
assertNothingWritten tmp = assertEq "files written for --out" [] =<< outEntries tmp

runCli :: FilePath -> [String] -> ExitCode -> IO (String, String)
runCli tmp args expected = do
  (code, err) <- withStderr tmp (redirected stdout (tmp </> "cli.stdout") (mergeMain args))
  out <- BC.unpack <$> BS.readFile (tmp </> "cli.stdout")
  when (code /= expected) $
    failWith ("exit code: expected " <> show expected <> ", got " <> show code
              <> "\nstdout: " <> out <> "\nstderr: " <> err)
  pure (out, err)

testCliSuccessExit0 :: IO ()
testCliSuccessExit0 = withTmpProject $ \tmp -> do
  liveProject tmp
  let args = argsFor tmp ++ ["--source-dirs", srcDir tmp]
  (out, err) <- runCli tmp args ExitSuccess
  assertEq "stdout"
    ("sql-schema-merge: 1 fragments read, 0 stale pruned, 0 included contracts (0 tables), "
     <> "0 duplicates collapsed, 1 tables emitted, 0 overrides recorded -> " <> outFile tmp <> "\n")
    out
  assertEq "stderr" "" err
  merged <- decodeFile (outFile tmp)
  assertEq "tables" 1 (length (mergedTables merged))
  assertEq "generator rev" Nothing (generator merged >>= generatorRev)
  _ <- runCli tmp (args ++ ["--generator-rev", "abc123"]) ExitSuccess
  stamped <- decodeFile (outFile tmp)
  assertEq "generator rev" (Just "abc123") (generator stamped >>= generatorRev)

testCliNoTablesExit1 :: IO ()
testCliNoTablesExit1 = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  (out, err) <- runCli tmp (argsFor tmp) (ExitFailure 1)
  assertEq "stdout" "" out
  assertMentions "stderr" "refusing to publish: the contract has 0 tables" err
  assertNothingWritten tmp

-- | Each refusal leaves the contract of an earlier merge byte for byte as it was.
testCliRefusalKeepsOldOut :: IO ()
testCliRefusalKeepsOldOut = do
  keepsOut "0 tables" $ \tmp -> do
    removeFile (fragDir tmp </> "Foo.yaml")
    pure (argsFor tmp ++ ["--source-dirs", srcDir tmp])
  keepsOut "no own fragments" $ \tmp -> do
    removeFile (fragDir tmp </> "Foo.yaml")
    writeIncluded (tmp </> "upstream.yaml")
    pure (argsFor tmp ++ ["--source-dirs", srcDir tmp, "--include-merged", tmp </> "upstream.yaml"])
  keepsOut "--no-own-tables with own tables" $ \tmp ->
    pure (argsFor tmp ++ ["--source-dirs", srcDir tmp, "--no-own-tables"])
  where
    keepsOut name refusedArgs = withTmpProject $ \tmp -> do
      liveProject tmp
      _ <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
      before <- BS.readFile (outFile tmp)
      args <- refusedArgs tmp
      (out, err) <- runCli tmp args (ExitFailure 1)
      assertEq (name <> ": stdout") "" out
      assertMentions (name <> ": stderr") "(not written)" err
      after <- BS.readFile (outFile tmp)
      assertEq (name <> ": --out") before after
      assertEq (name <> ": files") ["merged.yaml"] =<< outEntries tmp

testCliAllowEmpty :: IO ()
testCliAllowEmpty = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  _ <- runCli tmp (argsFor tmp ++ ["--allow-empty"]) ExitSuccess
  merged <- decodeFile (outFile tmp)
  assertEq "tables" 0 (length (mergedTables merged))

testCliIncludeOnlyExit1 :: IO ()
testCliIncludeOnlyExit1 = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  writeIncluded (tmp </> "upstream.yaml")
  (_, err) <- runCli tmp (argsFor tmp ++ ["--include-merged", tmp </> "upstream.yaml"])
                (ExitFailure 1)
  assertMentions "stderr" "no fragment of this package's own modules was read" err
  assertNothingWritten tmp

testCliIncludeOnlyNoOwnTables :: IO ()
testCliIncludeOnlyNoOwnTables = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  writeIncluded (tmp </> "upstream.yaml")
  _ <- runCli tmp (argsFor tmp ++ ["--include-merged", tmp </> "upstream.yaml", "--no-own-tables"])
         ExitSuccess
  merged <- decodeFile (outFile tmp)
  assertEq "tables" 1 (length (mergedTables merged))

testCliNoOwnTablesWithOwnTablesExit1 :: IO ()
testCliNoOwnTablesWithOwnTablesExit1 = withTmpProject $ \tmp -> do
  liveProject tmp
  (_, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp, "--no-own-tables"])
                (ExitFailure 1)
  assertMentions "stderr" "--no-own-tables was given" err
  assertNothingWritten tmp

-- | Per opt-out flag: a project that needs it (setup returns extra arguments), and a phrase its messages share.
optOutCases :: [(String, FilePath -> IO [String], String)]
optOutCases =
  [ ( "--allow-empty-include"
    , \tmp -> do
        liveProject tmp
        writeEmptyIncluded (tmp </> "empty.yaml")
        pure ["--source-dirs", srcDir tmp, "--include-merged", tmp </> "empty.yaml"]
    , "holds no tables" )
  , ( "--allow-unmatched-override"
    , \tmp -> do
        liveProject tmp
        touchFile (srcDir tmp </> "Db.hs")
        writeFragYaml (fragDir tmp </> "Db.yaml") (overrideFragment "MissingT")
        pure ["--source-dirs", srcDir tmp]
    , "has no matching table" )
  , ( "--allow-columnless-table"
    , \tmp -> do
        liveProject tmp
        touchFile (srcDir tmp </> "LR.hs")
        writeFragYaml (fragDir tmp </> "LR.yaml") columnlessFragment
        pure ["--source-dirs", srcDir tmp]
    , "has no columns" )
  ]

testCliOptOutFlags :: IO ()
testCliOptOutFlags = do
  let base = ["--fragments", "f", "--out", "o"]
  forM_ [ ("--allow-empty-include", \o -> o { moAllowEmptyInclude = True })
        , ("--allow-unmatched-override", \o -> o { moAllowUnmatchedOverride = True })
        , ("--allow-columnless-table", \o -> o { moAllowColumnlessTable = True }) ] $ \(flag, set) ->
    assertEq flag (Right (set (defaultMergeOptions "f" "o"))) (parseArgs (base ++ [flag]))
  forM_ optOutCases $ \(flag, setup, needle) -> withTmpProject $ \tmp -> do
    extra <- setup tmp
    (out, err) <- runCli tmp (argsFor tmp ++ extra ++ [flag]) ExitSuccess
    assertMentions (flag <> ": stdout") "tables emitted" out
    assertMentions (flag <> ": stderr") "warning: " err
    assertMentions (flag <> ": stderr") needle err
    assertLacks (flag <> ": stderr") "ERROR" err
    assertEq (flag <> ": files") ["merged.yaml"] =<< outEntries tmp
  withTmpProject $ \tmp -> do
    liveProject tmp
    refusedAtParse tmp
      [ (v, argsFor tmp ++ [v], "unrecognised argument: " <> v)
      | v <- [ "--allow-empty-includes", "--allow-unmatched-overrides", "--allow-columnless-tables"
             , "--allow-columnless", "--allow-unmatched", "--allow-empty-include=1" ] ]

-- | Without its flag each check fails the merge (exit 1, nothing written, the flag named), and
-- another check's flag doesn't help.
testCliOptOutChecksExit1 :: IO ()
testCliOptOutChecksExit1 = forM_ optOutCases $ \(flag, setup, needle) -> withTmpProject $ \tmp -> do
  extra <- setup tmp
  let args = argsFor tmp ++ extra
  forM_ ([] : [[other] | (other, _, _) <- optOutCases, other /= flag]) $ \allowed -> do
    (out, err) <- runCli tmp (args ++ allowed) (ExitFailure 1)
    assertEq (flag <> ": stdout") "" out
    assertMentions (flag <> ": stderr") needle err
    assertMentions (flag <> ": stderr") flag err
    assertLacks (flag <> ": stderr") "warning:" err
    assertNothingWritten tmp

-- | The fragment and the included contract define the same type differently.
testCliValidationErrorExit1 :: IO ()
testCliValidationErrorExit1 = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  writeFragYaml (fragDir tmp </> "Up.yaml") (sampleFragment "Up" "Up.UpT" "not_up_tab")
  writeIncluded (tmp </> "upstream.yaml")
  (_, err) <- runCli tmp (argsFor tmp ++ ["--include-merged", tmp </> "upstream.yaml"])
                (ExitFailure 1)
  assertMentions "stderr" "conflicting definitions" err
  assertNothingWritten tmp

testCliIoErrorExit1 :: IO ()
testCliIoErrorExit1 = withTmpProject $ \tmp -> do
  liveProject tmp
  let argsWithOut out = ["--fragments", fragDir tmp, "--source-dirs", srcDir tmp, "--out", out]
      missing = tmp </> "missing" </> "merged.yaml"
      directory = tmp </> "taken"
  (out, err) <- runCli tmp (argsWithOut missing) (ExitFailure 1)
  assertEq "--out in a missing directory: stdout" "" out
  assertMentions "--out in a missing directory" (tmp </> "missing") err
  assertAbsent (tmp </> "missing")
  -- The rename fails after the temporary file was written.
  createDirectoryIfMissing True directory
  before <- sort <$> listDirectory tmp
  (out', err') <- runCli tmp (argsWithOut directory) (ExitFailure 1)
  assertEq "--out is a directory: stdout" "" out'
  assertMentions "--out is a directory" directory err'
  assertEq "--out is a directory: files" before . sort =<< listDirectory tmp

-- | A @<out>.tmp@ the merge did not create is never read, written through or removed, whether the
-- merge succeeds or fails: a file, a read-only file, and a symlink to a file elsewhere.
testCliForeignTmpUntouched :: IO ()
testCliForeignTmpUntouched = forM_ plants $ \(kind, plant) -> do
  foreignTmp kind plant "merged.yaml" ExitSuccess
  foreignTmp kind plant "taken" (ExitFailure 1)
  where
    plants =
      [ ("file", \_ p -> BS.writeFile p userData)
      , ("read-only file", \_ p -> BS.writeFile p userData
                                    >> setPermissions p (setOwnerReadable True emptyPermissions))
      , ("symlink to a file elsewhere", createFileLink)
      ]
    userData = BC.pack "user data\n"
    victimData = BC.pack "victim\n"
    foreignTmp :: String -> (FilePath -> FilePath -> IO ()) -> FilePath -> ExitCode -> IO ()
    foreignTmp kind plant outName expected = withTmpProject $ \tmp -> do
      liveProject tmp
      let dir = tmp </> "out"
          out = dir </> outName
          squatter = out <> ".tmp"
          victim = tmp </> "victim.txt"
          label = kind <> ", " <> (if expected == ExitSuccess then "merge succeeds" else "merge fails")
      createDirectoryIfMissing True dir
      -- A directory as --out fails the merge at the rename.
      when (outName == "taken") (createDirectoryIfMissing True out)
      BS.writeFile victim victimData
      plant victim squatter
      isLink <- pathIsSymbolicLink squatter
      content <- BS.readFile squatter
      listed <- sort <$> listDirectory dir
      _ <- runCli tmp ["--fragments", fragDir tmp, "--source-dirs", srcDir tmp, "--out", out] expected
      assertEq (label <> ": symlink") isLink =<< pathIsSymbolicLink squatter
      assertEq (label <> ": content") content =<< BS.readFile squatter
      assertEq (label <> ": victim") victimData =<< BS.readFile victim
      assertEq (label <> ": files") (if expected == ExitSuccess then sort (outName : listed) else listed)
        . sort =<< listDirectory dir

-- | An existing @--out@ is replaced by a rename, not rewritten in place: a read-only one is
-- replaced, and a reader that already has it open keeps the old contract.
testCliReplacesReadOnlyOut :: IO ()
testCliReplacesReadOnlyOut = withTmpProject $ \tmp -> do
  liveProject tmp
  let old = BC.pack "old contract\n"
  BS.writeFile (outFile tmp) old
  setPermissions (outFile tmp) (setOwnerReadable True emptyPermissions)
  reader <- openBinaryFile (outFile tmp) ReadMode
  (do _ <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
      assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)
      assertEq "open reader" old =<< BS.hGetContents reader)
    `finally` hClose reader

-- | The temporary file is made next to @--out@, not in the working directory, so the rename
-- stays on one filesystem.
testCliTempNextToOut :: IO ()
testCliTempNextToOut = withTmpProject $ \tmp -> do
  liveProject tmp
  let cwd = tmp </> "cwd"
      dir = tmp </> "elsewhere"
      rel = tmp </> "relative"
      argsWithOut out = ["--fragments", fragDir tmp, "--source-dirs", srcDir tmp, "--out", out]
      open = setOwnerWritable True (setOwnerSearchable True (setOwnerReadable True emptyPermissions))
  mapM_ (createDirectoryIfMissing True) [cwd, dir, rel]
  setPermissions cwd (setOwnerSearchable True (setOwnerReadable True emptyPermissions))
  (do cwdWritable <- writable <$> getPermissions cwd
      whenWithheld (not cwdWritable) "testCliTempNextToOut"
        "the temporary file is made next to --out, not in the working directory" $ do
        _ <- withCurrentDirectory cwd (runCli tmp (argsWithOut (dir </> "merged.yaml")) ExitSuccess)
        assertEq "--out elsewhere: files" ["merged.yaml"] =<< listDirectory dir)
    `finally` setPermissions cwd open
  _ <- withCurrentDirectory rel (runCli tmp (argsWithOut "relative.yaml") ExitSuccess)
  assertEq "relative --out: files" ["relative.yaml"] =<< listDirectory rel

-- | A mistyped second @--source-dirs@ entry (@gne@ for @gen@) would prune the fragment
-- of the module that lives there.
testCliMistypedSourceDirExit1 :: IO ()
testCliMistypedSourceDirExit1 = withTmpProject $ \tmp -> do
  let gen = tmp </> "gen"
      gne = tmp </> "gne"
      withDirs dirs = argsFor tmp ++ concat [["--source-dirs", d] | d <- dirs]
  touchFile (srcDir tmp </> "A" </> "Mod.hs")
  touchFile (gen </> "B" </> "Mod.hs")
  createDirectoryIfMissing True (fragDir tmp)
  writeFragYaml (fragDir tmp </> "A.Mod.yaml") (sampleFragment "A.Mod" "A.Mod.AT" "a_tab")
  writeFragYaml (fragDir tmp </> "B.Mod.yaml") (sampleFragment "B.Mod" "B.Mod.BT" "b_tab")
  (out, err) <- runCli tmp (withDirs [srcDir tmp, gne]) (ExitFailure 1)
  assertEq "stdout" "" out
  assertMentions "stderr" ("source dir " <> gne <> " does not exist") err
  assertMentions "stderr" "pruned fragment of \"B.Mod\"" err
  assertLacks "stderr" "warning:" err
  assertNothingWritten tmp
  _ <- runCli tmp (withDirs [srcDir tmp, gen]) ExitSuccess
  before <- BS.readFile (outFile tmp)
  assertEq "tables" 2 . length . mergedTables =<< decodeFile (outFile tmp)
  _ <- runCli tmp (withDirs [srcDir tmp, gne]) (ExitFailure 1)
  after <- BS.readFile (outFile tmp)
  assertEq "--out" before after

testCliSourceDirProblemsWarn :: IO ()
testCliSourceDirProblemsWarn = withTmpProject $ \tmp -> do
  let notDir = tmp </> "notadir.hs"
      gone   = tmp </> "gone"
  liveProject tmp
  touchFile notDir
  (_, err) <- runCli tmp
    (argsFor tmp ++ concat [["--source-dirs", d] | d <- [srcDir tmp, notDir, gone]]) ExitSuccess
  assertMentions "stderr" ("warning: source dir " <> notDir <> " is not a directory; treating as empty") err
  assertMentions "stderr" ("warning: source dir " <> gone <> " does not exist; treating as empty") err
  assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)

-- | A link into a directory that can't be searched fails the merge; read as a file, it would
-- prune the modules behind it.
testCliSourceDirLinkIntoUnsearchableExit1 :: IO ()
testCliSourceDirLinkIntoUnsearchableExit1 = withTmpProject $ \tmp -> do
  let locked = tmp </> "locked"
      link   = srcDir tmp </> "Link"
      open   = setOwnerSearchable True (setOwnerReadable True (setOwnerWritable True emptyPermissions))
  liveProject tmp
  touchFile (locked </> "target" </> "Sub" </> "B.hs")
  writeFragYaml (fragDir tmp </> "Link.Sub.B.yaml") (sampleFragment "Link.Sub.B" "Link.Sub.B.BT" "b_tab")
  createDirectoryLink (locked </> "target") link
  setPermissions locked emptyPermissions
  (do canSearch <- searchable <$> getPermissions locked
      whenWithheld (not canSearch) "testCliSourceDirLinkIntoUnsearchableExit1"
        "an entry that can't be inspected fails the merge, instead of reading as a file" $ do
        (out, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) (ExitFailure 1)
        assertEq "stdout" "" out
        assertMentions "stderr" link err
        assertNothingWritten tmp)
    `finally` setPermissions locked open

testCliSourceDirDanglingLinkIgnored :: IO ()
testCliSourceDirDanglingLinkIgnored = withTmpProject $ \tmp -> do
  liveProject tmp
  createFileLink (tmp </> "nowhere") (srcDir tmp </> "dangling")
  (_, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
  assertEq "stderr" "" err
  assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)

testCliSourceDirLinkLoopWarns :: IO ()
testCliSourceDirLinkLoopWarns = withTmpProject $ \tmp -> do
  let loopA = srcDir tmp </> "loopA"
      loopB = srcDir tmp </> "loopB"
  liveProject tmp
  createFileLink "loopB" loopA
  createFileLink "loopA" loopB
  (_, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
  forM_ [loopA, loopB] $ \l -> assertMentions "stderr" ("warning: skipped " <> show l) err
  assertMentions "stderr" "leads back to itself" err
  assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)

-- | A link to its own directory is skipped, with a warning naming it; the modules below it stay
-- live, through their real path.
testCliSourceDirSelfLinkKeepsModules :: IO ()
testCliSourceDirSelfLinkKeepsModules = withTmpProject $ \tmp -> do
  let self = srcDir tmp </> "self"
  liveProject tmp
  touchFile (srcDir tmp </> "Lib" </> "B.hs")
  writeFragYaml (fragDir tmp </> "Lib.B.yaml") (sampleFragment "Lib.B" "Lib.B.BT" "b_tab")
  createDirectoryLink "." self
  (_, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
  assertMentions "stderr" ("warning: skipped " <> show self) err
  assertEq "stderr lines" 1 (length (lines err))
  assertEq "tables" 2 . length . mergedTables =<< decodeFile (outFile tmp)

-- | Two links back to their directory: the walk ends, and no module is invented through a link.  A
-- fragment pruned while links were skipped fails the merge, naming both.
testCliSourceDirTwoSelfLinksTerminate :: IO ()
testCliSourceDirTwoSelfLinksTerminate = withTmpProject $ \tmp -> do
  let links = [srcDir tmp </> "Self1", srcDir tmp </> "Self2"]
  liveProject tmp
  touchFile (srcDir tmp </> "Lib" </> "B.hs")
  writeFragYaml (fragDir tmp </> "Lib.B.yaml") (sampleFragment "Lib.B" "Lib.B.BT" "b_tab")
  writeFragYaml (fragDir tmp </> "Self1.Lib.B.yaml") (sampleFragment "Self1.Lib.B" "Self1.Lib.B.BT" "ghost_tab")
  mapM_ (createDirectoryLink ".") links
  (out, err) <- withinSeconds 20 "the merge" $
    runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) (ExitFailure 1)
  assertEq "stdout" "" out
  forM_ links $ \l -> do
    assertMentions "stderr" ("warning: skipped " <> show l) err
    assertMentions "stderr" ("skipped link " <> show l) err
  assertMentions "stderr" "pruned the fragment of \"Self1.Lib.B\"" err
  assertMentions "stderr" "pruned fragment of \"Self1.Lib.B\"" err
  assertNothingWritten tmp

-- | An alias keeps its modules: @src/Compat -> Legacy/Compat@ is the only path that names @Compat.X@,
-- and its fragment must not be pruned.
testCliSourceDirAliasKeepsModules :: IO ()
testCliSourceDirAliasKeepsModules = withTmpProject $ \tmp -> do
  liveProject tmp
  touchFile (srcDir tmp </> "Legacy" </> "Compat" </> "X.hs")
  createDirectoryLink ("Legacy" </> "Compat") (srcDir tmp </> "Compat")
  writeFragYaml (fragDir tmp </> "Compat.X.yaml") (sampleFragment "Compat.X" "Compat.X.XT" "x_tab")
  (out, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
  assertMentions "stdout" "2 fragments read, 0 stale pruned" out
  assertEq "stderr" "" err
  assertEq "tables" ["Compat.X.XT", "Foo.FooT"] . map qualifiedType . mergedTables =<< decodeFile (outFile tmp)

-- | The same when a chain of links longer than the system follows leads to the directory.
testCliSourceDirLongChainKeepsModules :: IO ()
testCliSourceDirLongChainKeepsModules = withTmpProject $ \tmp -> do
  liveProject tmp
  touchFile (tmp </> "legacy" </> "Compat" </> "X.hs")
  linkChain False 64 (tmp </> "chain") (tmp </> "legacy" </> "Compat")
  createDirectoryLink (tmp </> "chain" </> "c1") (srcDir tmp </> "Compat")
  writeFragYaml (fragDir tmp </> "Compat.X.yaml") (sampleFragment "Compat.X" "Compat.X.XT" "x_tab")
  refused <- refusesToFollow (srcDir tmp </> "Compat")
  whenRefused refused "testCliSourceDirLongChainKeepsModules" "a chain of 64 links ends at its directory" $ do
    (out, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
    assertMentions "stdout" "2 fragments read, 0 stale pruned" out
    assertEq "stderr" "" err

-- | A walk that fails still prints the warnings it had collected, before the error; nothing is written.
testCliWalkErrorKeepsEarlierWarnings :: IO ()
testCliWalkErrorKeepsEarlierWarnings = withTmpProject $ \tmp -> do
  let cycleLink = srcDir tmp </> "a"
      bad       = srcDir tmp </> "z"
  liveProject tmp
  touchFile (tmp </> "file")
  createDirectoryLink "." cycleLink
  createFileLink (tmp </> "file" </> "x") bad
  (out, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) (ExitFailure 1)
  assertEq "stdout" "" out
  assertMentions "the warning" ("warning: skipped " <> show cycleLink) err
  assertMentions "the error" bad err
  assertNothingWritten tmp

testCliPrunedModuleNameEscaped :: IO ()
testCliPrunedModuleNameEscaped = withTmpProject $ \tmp -> do
  let evil = "Gone\nsql-schema-merge: 9 fragments read\ESC[31m"
  liveProject tmp
  writeFragYaml (fragDir tmp </> "Evil.yaml") (sampleFragment evil "Gone.GoneT" "gone")
  (_, err) <- runCli tmp (argsFor tmp ++ ["--source-dirs", srcDir tmp]) ExitSuccess
  assertMentions "stderr" ("pruned the fragment of " <> show evil) err
  assertLacks "a forged line" "\nsql-schema-merge: 9 fragments read" err
  assertLacks "an escape character" "\ESC" err

-- | Names of files and links in the trees read are as untrusted as fragment content: printed
-- escaped, I/O errors included.
testCliHostileNamesEscaped :: IO ()
testCliHostileNamesEscaped =
  forM_ [ ( "a fragment that doesn't parse", "unparseable fragment", ExitFailure 1, \_ -> []
          , \tmp -> BS.writeFile (hostileFragment tmp) (BC.pack ":::not::valid:::yaml:::") )
        , ( "a directory named like a fragment", "inappropriate type", ExitFailure 1, \_ -> []
          , \tmp -> createDirectoryIfMissing True (hostileFragment tmp) )
        , ( "a link back into the source directory", "warning: skipped", ExitSuccess
          , \tmp -> ["--source-dirs", srcDir tmp]
          , \tmp -> createDirectoryLink "." (srcDir tmp </> hostileField) )
        , ( "a link back into a directory with such a name", "warning: skipped", ExitSuccess
          , \tmp -> ["--source-dirs", srcDir tmp]
          , \tmp -> do
              createDirectoryIfMissing True (srcDir tmp </> hostileField)
              createDirectoryLink "." (srcDir tmp </> hostileField </> "self") )
        , ( "two links that lead to each other", "leads back to itself", ExitSuccess
          , \tmp -> ["--source-dirs", srcDir tmp]
          , \tmp -> do
              createFileLink (hostileField <> "2") (srcDir tmp </> hostileField)
              createFileLink hostileField (srcDir tmp </> (hostileField <> "2")) )
        , ( "a skipped link beside a pruned fragment", "skipped link", ExitFailure 1
          , \tmp -> ["--source-dirs", srcDir tmp]
          , \tmp -> do
              createDirectoryLink "." (srcDir tmp </> hostileField)
              writeFragYaml (fragDir tmp </> "Gone.yaml") (sampleFragment "Gone" "Gone.GoneT" "gone") ) ] $
    \(label, phrase, code, extraArgs, plant) -> withTmpProject $ \tmp -> do
      liveProject tmp
      plant tmp
      (_, err) <- runCli tmp (argsFor tmp ++ extraArgs tmp) code
      assertMentions label phrase err
      assertHostileEscaped label err
      when (code /= ExitSuccess) (assertNothingWritten tmp)
  where
    hostileFragment tmp = fragDir tmp </> (hostileField <> ".yaml")

testCliFragmentsIsFileExit1 :: IO ()
testCliFragmentsIsFileExit1 = withTmpProject $ \tmp -> do
  let file = tmp </> "frags"
  writeFile file "x\n"
  writeIncluded (tmp </> "upstream.yaml")
  (out, err) <- runCli tmp
    [ "--fragments", file, "--out", outFile tmp, "--include-merged", tmp </> "upstream.yaml"
    , "--allow-empty", "--no-own-tables" ]
    (ExitFailure 1)
  assertEq "stdout" "" out
  assertMentions "stderr" ("--fragments is not a directory: " <> file) err
  assertLacks "stderr" "warning:" err
  assertNothingWritten tmp

testCliMissingFragmentsWarns :: IO ()
testCliMissingFragmentsWarns = withTmpProject $ \tmp -> do
  let dangling = tmp </> "dangling"
  writeIncluded (tmp </> "upstream.yaml")
  createFileLink (tmp </> "nowhere") dangling
  forM_ [tmp </> "absent", dangling] $ \frags -> do
    (_, err) <- runCli tmp
      ["--fragments", frags, "--out", outFile tmp, "--include-merged", tmp </> "upstream.yaml", "--no-own-tables"]
      ExitSuccess
    assertMentions "stderr" ("warning: --fragments directory " <> frags <> " does not exist") err
    assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)
    removeFile (outFile tmp)

testSkipNoticeIsVisible :: IO ()
testSkipNoticeIsVisible = withTmpProject $ \tmp -> do
  ran <- newIORef False
  (_, skipped) <- withStderr tmp (whenWithheld False "testX" "the guard" (writeIORef ran True))
  assertMentions "stderr" "SKIPPED testX" skipped
  assertMentions "stderr" "the guard" skipped
  assertEq "ran although skipped" False =<< readIORef ran
  (_, quiet) <- withStderr tmp (whenWithheld True "testX" "the guard" (writeIORef ran True))
  assertEq "stderr when it runs" "" quiet
  assertEq "ran" True =<< readIORef ran

-- | A summary that can't be printed is noted on stderr, and fails nothing: not even with stderr
-- closed too.
testCliClosedStdoutExit0 :: IO ()
testCliClosedStdoutExit0 = withTmpProject $ \tmp -> do
  liveProject tmp
  let args = argsFor tmp ++ ["--source-dirs", srcDir tmp]
  (code, err) <- withStderr tmp (withClosed stdout (mergeMain args))
  assertEq "exit code" ExitSuccess code
  assertMentions "stderr" "its summary line could not be printed" err
  assertEq "tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)
  removeFile (outFile tmp)
  code' <- withClosed stdout (withClosed stderr (mergeMain args))
  assertEq "both closed: exit code" ExitSuccess code'
  assertEq "both closed: tables" 1 . length . mergedTables =<< decodeFile (outFile tmp)

withClosed :: Handle -> IO a -> IO a
withClosed h act = do
  saved <- hDuplicate h
  (hClose h >> act) `finally` (hDuplicateTo saved h >> hClose saved)

refusedAtParse :: FilePath -> [(String, [String], String)] -> IO ()
refusedAtParse tmp cases = forM_ cases $ \(name, args, reason) -> do
  (out, err) <- runCli tmp args (ExitFailure 2)
  assertEq (name <> ": stdout") "" out
  assertMentions (name <> ": reason") ("sql-schema-merge: " <> reason) err
  assertMentions (name <> ": usage") "Usage: sql-schema-merge" err
  assertNothingWritten tmp

testCliArgErrorsExit2 :: IO ()
testCliArgErrorsExit2 = withTmpProject $ \tmp -> do
  liveProject tmp
  refusedAtParse tmp
    [ ("unknown flag", argsFor tmp ++ ["--bogus"], "unrecognised argument: --bogus")
    , ("stray argument", argsFor tmp ++ ["stray"], "unrecognised argument: stray")
    , ("missing --fragments", ["--out", outFile tmp], "missing --fragments")
    , ("missing --out", ["--fragments", fragDir tmp], "missing --out")
    , ("no arguments", [], "missing --fragments")
    ]

testCliFlagMissingValueExit2 :: IO ()
testCliFlagMissingValueExit2 = withTmpProject $ \tmp -> do
  liveProject tmp
  refusedAtParse tmp $
    [ ("--out last", ["--fragments", fragDir tmp, "--out"], "--out needs a value")
    , ("--fragments last", ["--out", outFile tmp, "--fragments"], "--fragments needs a value")
    ]
    ++ [ (flag <> " last", argsFor tmp ++ [flag], flag <> " needs a value")
       | flag <- ["--source-dirs", "--include-merged", "--generator-rev"] ]

testCliDuplicateFlagExit2 :: IO ()
testCliDuplicateFlagExit2 = withTmpProject $ \tmp -> do
  liveProject tmp
  createDirectoryIfMissing True (tmp </> "fragments2")
  refusedAtParse tmp
    [ ("--fragments twice", argsFor tmp ++ ["--fragments", tmp </> "fragments2"]
      , "--fragments given more than once")
    , ("--out twice", argsFor tmp ++ ["--out", tmp </> "other.yaml"]
      , "--out given more than once")
    , ("--generator-rev twice", argsFor tmp ++ ["--generator-rev", "a", "--generator-rev", "b"]
      , "--generator-rev given more than once")
    ]
  assertAbsent (tmp </> "other.yaml")

-- | Where the flag allows, the command line carries an included contract and @--no-own-tables@,
-- so an empty path read as a missing one would still merge.
testCliEmptyValuesExit2 :: IO ()
testCliEmptyValuesExit2 = withTmpProject $ \tmp -> do
  createDirectoryIfMissing True (fragDir tmp)
  writeIncluded (tmp </> "upstream.yaml")
  let include = ["--include-merged", tmp </> "upstream.yaml", "--no-own-tables"]
  refusedAtParse tmp
    [ ("--fragments \"\"", ["--fragments", "", "--out", outFile tmp] ++ include
      , "--fragments has an empty value")
    , ("--out \"\"", ["--fragments", fragDir tmp, "--out", ""] ++ include
      , "--out has an empty value")
    , ("--source-dirs \"\"", argsFor tmp ++ ["--source-dirs", ""] ++ include
      , "--source-dirs has an empty value")
    , ("--include-merged \"\"", argsFor tmp ++ ["--include-merged", "", "--no-own-tables"]
      , "--include-merged has an empty value")
    , ("--generator-rev \"\"", argsFor tmp ++ ["--generator-rev", ""] ++ include
      , "--generator-rev has an empty value")
    ]

-- | An unquoted unset variable disappears instead, and the next option would
-- be taken as the value.
testCliFlagLikeValueExit2 :: IO ()
testCliFlagLikeValueExit2 = withTmpProject $ \tmp -> do
  liveProject tmp
  refusedAtParse tmp
    [ ("--generator-rev --no-own-tables", argsFor tmp ++ ["--generator-rev", "--no-own-tables"]
      , "--generator-rev needs a value, but the next argument is the option --no-own-tables")
    , ("--fragments --out", ["--fragments", "--out", outFile tmp]
      , "--fragments needs a value, but the next argument is the option --out")
    , ("--source-dirs --allow-empty", argsFor tmp ++ ["--source-dirs", "--allow-empty"]
      , "--source-dirs needs a value, but the next argument is the option --allow-empty")
    ]

testParseArgs :: IO ()
testParseArgs = do
  assertEq "required only" (Right (defaultMergeOptions "f" "o"))
    (parseArgs ["--fragments", "f", "--out", "o"])
  assertEq "every flag"
    (Right (defaultMergeOptions "f" "o")
      { moIncludeMerged = ["i1", "i2"], moSourceDirs = ["s1", "s2"], moGeneratorRev = Just "r"
      , moAllowEmpty = True, moNoOwnTables = True, moAllowEmptyInclude = True
      , moAllowUnmatchedOverride = True, moAllowColumnlessTable = True })
    (parseArgs [ "--no-own-tables", "--source-dirs", "s1", "--fragments", "f", "--include-merged", "i1"
               , "--allow-empty", "--generator-rev", "r", "--out", "o", "--source-dirs", "s2"
               , "--include-merged", "i2", "--allow-empty-include", "--allow-unmatched-override"
               , "--allow-columnless-table" ])
  let base = ["--fragments", "f", "--out", "o"]
  forM_ ([ (["--fragments", "f", "--fragments", "g", "--out", "o"], "--fragments")
         , (base ++ ["--out", "p"], "--out")
         , (base ++ ["--generator-rev", "a", "--generator-rev", "b"], "--generator-rev")
         , (["--fragments", "f"], "--out")
         , (["--out", "o"], "--fragments")
         , (base ++ ["--bogus"], "--bogus")
         , (["--fragments", "", "--out", "o"], "--fragments")
         , (["--fragments", "f", "--out", ""], "--out")
         , (["--fragments", "--out", "o"], "--fragments")
         ]
         ++ [ (base ++ [flag], flag) | flag <- valueFlags ]
         ++ [ (base ++ [flag, ""], flag) | flag <- valueFlags ]
         ++ [ (base ++ [flag, "--allow-empty"], flag) | flag <- valueFlags ]) $ \(args, needle) ->
    case parseArgs args of
      Left msg | needle `isInfixOf` msg -> pure ()
      other -> failWith (show args <> ": expected an error naming " <> needle <> ", got " <> show other)
  where
    valueFlags = ["--source-dirs", "--include-merged", "--generator-rev"]

testUsageStatesRules :: IO ()
testUsageStatesRules = forM_
  [ "may be given once", "empty or start with", "creates no --out and leaves an existing one"
  , "warns and reads as", "A path that is not a directory, or that", "can't be checked (below a file"
  , "unless a fragment was pruned", "that can't be checked (as above) is always an"
  , "directory on the walk's path from the entry is", "skipped, with a warning, and a fragment pruned in a"
  , "merge that skipped one is an error (exit 1); any"
  , "other link is followed under its own name", "More than 200000 entries listed and links"
  , "followed for one entry, or a chain of more than 1000", "errors stop the merge before the table checks"
  , "two different SQL names is an error whatever the flags"
  , "accept an --include-merged contract with no tables"
  , "accept a setEntityName override that matches no table"
  , "accept a table with no columns", "relaxes only its own check" ] $
  \needle -> assertMentions "usage" needle usage
