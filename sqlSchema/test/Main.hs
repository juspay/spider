{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test runner: the merge tests ('MergeTests'), the extractor tests
-- ('ExtractTests'), the fragment staging tests ('StageTests'), and the
-- helpers exported by 'SqlSchema.Plugin': the column-name helper,
-- 'parseCliOptions' and 'optionsFingerprint'.
module Main (main) where

import           Control.Monad       (when)

import qualified ExtractTests
import qualified MergeTests
import           SqlSchema.Plugin    (beamDefaultColumnName, optionsFingerprint, parseCliOptions)
import           SqlSchema.Types     hiding (beamDefaultColumnName)
import qualified StageTests
import           TestUtil

main :: IO ()
main = do
  MergeTests.run
  testBeamDefaultColumnName
  testParseCliOptions
  testOptionsFingerprint
  StageTests.run
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
