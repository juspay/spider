{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test runner: the merge tests ('MergeTests'), the extractor tests
-- ('ExtractTests'), and the helpers exported by 'SqlSchema.Plugin': the
-- column-name helper, 'parseCliOptions', 'optionsFingerprint' and
-- 'syncFragment'.
module Main (main) where

import           Control.Exception   (IOException, try)
import           Control.Monad       (when)
import qualified Data.ByteString     as BS
import qualified Data.Yaml           as YAML
import           System.Directory    (createDirectoryIfMissing, doesFileExist)
import           System.FilePath     ((</>))

import qualified ExtractTests
import qualified MergeTests
import           SqlSchema.Plugin    (beamDefaultColumnName, optionsFingerprint, parseCliOptions,
                                      syncFragment)
import           SqlSchema.Types     hiding (beamDefaultColumnName)
import           TestUtil

main :: IO ()
main = do
  MergeTests.run
  testBeamDefaultColumnName
  testParseCliOptions
  testOptionsFingerprint
  testSyncFragment
  ExtractTests.run
  putStrLn "sql-schema-test: all cases passed"

-- ---------------------------------------------------------------------------
-- Test cases
-- ---------------------------------------------------------------------------

-- | Expected values: beam-core's @defaultFieldName (sel :| [])@, juspay/beam 25f1bcd960.
testBeamDefaultColumnName :: IO ()
testBeamDefaultColumnName = mapM_ check
  [ ("id", "id"), ("_id", "id"), ("enabled", "enabled"), ("widgetEnabled", "enabled")
  , ("ownerTag", "tag"), ("shopAccountId", "account_id"), ("_userFirstName", "first_name")
  , ("item_kind", "item_kind"), ("_x_y", "x_y"), ("_", "_"), ("__", "__"), ("URLPath", "ath")
  , ("userURL", "u_r_l"), ("allow3DmodeHelpText", "dmode_help_text"), ("level2XY", "x_y")
  , ("bigWordwithoutcaps", "wordwithoutcaps"), ("a", "a"), ("A", "a"), ("aB", "b")
  , ("createdAt", "at")
  -- An underscore after the leading ones skips un-camel-casing, capitals included
  , ("user_Name", "user_Name"), ("_x_Y", "x_Y")
  -- U+0130 expands to "i" + U+0307 in a component (Text) but not as a head char (Char)
  , ("\304B", "i\775b"), ("a\304", "i")
  ]
  where
    check (sel, expected) = assertEq sel expected (beamDefaultColumnName sel)

testParseCliOptions :: IO ()
testParseCliOptions = do
  assertEq "no options" (Right defaultCliOptions) (parseCliOptions [])
  assertEq "fragmentsDir round-trips"
    (Right defaultCliOptions { fragmentsDir = "x" })
    (parseCliOptions ["{\"fragmentsDir\":\"x\"}"])
  case parseCliOptions ["{\"fragmentDir\":\"x\"}"] of
    Left msg | "fragmentDir" `isInfixOf` msg -> pure ()
    other -> failWith ("typo'd key: expected Left naming fragmentDir, got " <> show other)
  assertLeft "not json" (parseCliOptions ["not json"])
  assertLeft "two option strings"
    (parseCliOptions ["{\"fragmentsDir\":\"a\"}", "{\"blacklistModules\":[]}"])

testOptionsFingerprint :: IO ()
testOptionsFingerprint = do
  let a = ["{\"fragmentsDir\":\"a\"}"]
      b = ["{\"fragmentsDir\":\"b\"}"]
  assertEq "same options" (optionsFingerprint a) (optionsFingerprint a)
  when (optionsFingerprint a == optionsFingerprint b) $
    failWith "different options gave equal fingerprints"

-- | A recompiled module with nothing to record (or a blacklisted one) must
-- not leave its earlier fragment behind for the merge to pick up.
testSyncFragment :: IO ()
testSyncFragment = withTmpProject $ \tmp -> do
  let path = tmp </> "fragments" </> "Foo.yaml"
      frag = sampleFragment "Foo" "Foo.FooT" "foo"
  syncFragment path (Just frag)
  bs <- BS.readFile path
  assertEq "written" (Right frag) (either (Left . show) Right (YAML.decodeEither' bs))
  syncFragment path Nothing
  stale <- doesFileExist path
  when stale (failWith "stale fragment not removed")
  syncFragment path Nothing
  -- Only a missing file is ignored; any other failure must fail the compile.
  let dirPath = tmp </> "fragments" </> "Dir.yaml"
  createDirectoryIfMissing True dirPath
  r <- try (syncFragment dirPath Nothing)
  case r of
    Left (_ :: IOException) -> pure ()
    Right ()                -> failWith "deleting a directory fragment path did not fail"
