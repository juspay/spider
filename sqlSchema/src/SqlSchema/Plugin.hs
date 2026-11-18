{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | GHC plugin that extracts the SQL schema implied by Beam table
-- definitions and writes one YAML fragment per module.  A separate
-- @sql-schema-merge@ executable combines all fragments into the final
-- contract.
--
-- Per-module fragments solve the incremental-build problem: each module's
-- fragment is rewritten (or deleted, once the module has nothing to record)
-- only when GHC recompiles the module, and stale fragments are pruned at
-- merge time by checking that the source file still exists.  A compile
-- stages the fragment when it parses the module and publishes it only once
-- the module has passed type checking, desugaring and -Werror, so a compile
-- that fails in between leaves the published fragment as it was.
--
-- Safety policy: anything the extractor cannot recognise becomes a hard
-- compile error.  Silent omission of a table or wrong column name would
-- produce a downstream false-pass against the prod DB schema — i.e. prod
-- downtime — so the only safe response to an unknown shape is to refuse
-- to compile.  Two opt-out flags exist for migrations:
-- @allowUnknownColumnWrapper@ and @allowNonStandardModifier@.
module SqlSchema.Plugin
  ( plugin
  , beamDefaultColumnName
  , optionsFingerprint
  , parseCliOptions
  , pendingPath
  , stagePending
  , publishStaged
  ) where

import           Control.Exception          (Exception, IOException, catch, onException, throwIO,
                                             try)
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable              (toList)
import           Data.List.NonEmpty         (NonEmpty (..), nonEmpty)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Yaml                  as YAML
import           GHC.Fingerprint            (Fingerprint, fingerprintString)
import           System.Directory           (createDirectoryIfMissing, getFileSize,
                                             removeFile, renameFile)
import           System.FilePath            (takeDirectory, takeFileName, (</>))
import           System.IO                  (hClose, openBinaryTempFileWithDefaultPermissions)
import           System.IO.Error            (isDoesNotExistError)

-- GHC API (>= 9.0)
import qualified GHC
import           GHC.Driver.Env             (Hsc)
import           GHC.Data.Bag               (listToBag)
import           GHC.Data.FastString        (mkFastString)
import           GHC.Driver.Plugins         (CommandLineOption, Plugin (..),
                                             PluginRecompile (..), defaultPlugin)
#if __GLASGOW_HASKELL__ >= 904
import           GHC.Driver.Errors.Types    (ghcUnknownMessage)
import           GHC.Driver.Plugins         (ParsedResult (..))
import           GHC.Utils.Error            (mkErrorMsgEnvelope)
#endif
import           GHC.Hs
import           GHC.Plugins                (CoreM, CoreToDo (..), ModGuts (..))
import qualified GHC.Types.Error            as ParseError
import           GHC.Types.SourceFile       (isHsBootOrSig)
import           GHC.Types.SrcLoc           (SrcSpan, mkGeneralSrcSpan, unLoc)
import qualified GHC.Types.SourceError      as ParseError
import           GHC.Unit.Module            (isInteractiveModule)
import           GHC.Unit.Module.Location   (ModLocation (..))
import           GHC.Unit.Module.ModSummary (ModSummary (..))
import           GHC.Utils.Outputable       (docToSDoc, reallyAlwaysQualify)
import qualified GHC.Utils.Ppr              as Pretty

import           SqlSchema.Extract          (FragmentAction (..), planFragment)
import           SqlSchema.Types


-- ---------------------------------------------------------------------------
-- Plugin entry point
-- ---------------------------------------------------------------------------

-- | GHC already recompiles every module when the plugin package itself
-- changes (it records the plugin unit); fingerprinting the options makes an
-- option change do the same, so each fragment is rewritten under the new options.
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile    = \opts -> pure (MaybeRecompile (optionsFingerprint opts))
  , parsedResultAction = onParsed
  , installCoreToDos   = \opts todos -> pure $
      if any isPublishPass todos then todos
      else todos ++ [CoreDoPluginPass publishPassName (publishPass opts)]
  }

publishPassName :: String
publishPassName = "sql-schema: publish the staged fragment"

-- | A plugin given twice installs its passes twice; a second publish pass would
-- find the staged fragment already published.
isPublishPass :: CoreToDo -> Bool
isPublishPass = \case
  CoreDoPluginPass name _ -> name == publishPassName
  CoreDoPasses todos      -> any isPublishPass todos
  _                       -> False

-- | GHC 9.4 wrapped the parsed module in 'ParsedResult'; it is returned unchanged.
#if __GLASGOW_HASKELL__ >= 904
onParsed :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
onParsed opts modSummary pr = pr <$ stageFragment opts modSummary (parsedResultModule pr)
#else
onParsed :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
onParsed opts modSummary hpm = hpm <$ stageFragment opts modSummary hpm
#endif


optionsFingerprint :: [CommandLineOption] -> Fingerprint
optionsFingerprint = fingerprintString . show

-- | At most one JSON option string is supported; anything else is an error
-- rather than a silent fall back to defaults.
parseCliOptions :: [CommandLineOption] -> Either String CliOptions
parseCliOptions = \case
  []    -> Right defaultCliOptions
  [raw] -> case A.eitherDecode (BL.fromStrict (encodeUtf8 (T.pack raw))) of
    Right v  -> Right v
    Left err -> Left ("cannot parse the plugin option as JSON CliOptions: " <> err)
  raws  -> Left ("expected at most one -fplugin-opt JSON string, got "
                   <> show (length raws))


stageFragment
  :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc ()
stageFragment opts modSummary hpm = do
  let modSpan  = mkFileSrcSpan (ms_location modSummary)
  case parseCliOptions opts of
    Left err  -> throwSqlSchemaErrors (fragmentsDir defaultCliOptions) modSpan
                   (INVALID_PLUGIN_OPTIONS err :| [])
    Right cli -> stageFragmentWith cli modSummary hpm

stageFragmentWith :: CliOptions -> ModSummary -> HsParsedModule -> Hsc ()
stageFragmentWith cli modSummary hpm = do
  let modSpan  = mkFileSrcSpan (ms_location modSummary)
      modStr   = moduleNameToString modSummary
      srcPath  = modulePath modSummary
      decls    = hsmodDecls (unLoc (hpm_module hpm))

  case planFragment cli (ms_hsc_src modSummary) modStr srcPath decls of
    Left errs              -> case nonEmpty errs of
      Just es -> throwSqlSchemaErrors (fragmentsDir cli) modSpan es
      Nothing -> throwPluginMessages modSpan (internalError :| [])
    Right LeaveUntouched   -> pure ()
    Right (Stage fragment) -> liftIO (stagePending (fragmentPath cli modStr) fragment)
  where
    internalError =
      "sql-schema: INTERNAL_ERROR: the extractor rejected this module without saying why.\n"
      <> "  This is a bug in the sql-schema plugin, not in the module; report it with the module's source."

-- | Appended to the end of the Core pipeline, which GHC starts only after
-- type checking, desugaring and the -Werror check pass: for a module it
-- generates code for, and on 9.8 for any module under -fwrite-if-simplified-core.
-- GHC never starts it for an hs-boot or hsig file; the guard keeps such a file
-- from publishing its module's staged fragment if a later GHC does.  GHCi's
-- prompt declarations reach it without the parse step, so they have nothing staged.
publishPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
publishPass opts guts
  | isHsBootOrSig (mg_hsc_src guts)      = pure guts
  | isInteractiveModule (mg_module guts) = pure guts
  | otherwise = guts <$ case parseCliOptions opts of
      Left err  -> throwSqlSchemaErrors (fragmentsDir defaultCliOptions) (mg_loc guts)
                     (INVALID_PLUGIN_OPTIONS err :| [])
      Right cli -> do
        let frag = fragmentPath cli (GHC.moduleNameString (GHC.moduleName (mg_module guts)))
        published <- liftIO (try (publishStaged frag))
        case published of
          Left (StagedFragmentMissing pending) ->
            throwSqlSchemaErrors (fragmentsDir cli) (mg_loc guts) (STAGED_FRAGMENT_MISSING pending :| [])
          Right () -> pure ()

fragmentPath :: CliOptions -> String -> FilePath
fragmentPath cli modStr = fragmentsDir cli </> modStr <> ".yaml"

-- | Where a fragment is staged.  The merge reads only @*.yaml@ files, so a
-- staged fragment never reaches the contract.
pendingPath :: FilePath -> FilePath
pendingPath = (<> ".pending")

-- | Stages the fragment for 'publishStaged': its YAML, or with 'Nothing' an
-- empty file, which publishes as a deletion of the fragment an earlier compile
-- left (a restored build directory would otherwise keep a dropped table).
stagePending :: FilePath -> Maybe Fragment -> IO ()
stagePending frag fragment = do
  createDirectoryIfMissing True (takeDirectory frag)
  atomicWrite (pendingPath frag) (maybe BS.empty YAML.encode fragment)

-- | Replaces the fragment with what was staged, or throws 'StagedFragmentMissing'.
-- A deletion removes the staged file first: a kill between the two removals
-- then leaves the fragment as before this compile, which matches the interface
-- file GHC never wrote, so the next build recompiles and stages again.
publishStaged :: FilePath -> IO ()
publishStaged frag = publish `catch` \e ->
  -- The fragment's own removal ignores a missing file, so a missing file here is the staged one.
  if isDoesNotExistError e then throwIO (StagedFragmentMissing pending) else throwIO e
  where
    pending = pendingPath frag
    publish = do
      size <- getFileSize pending
      if size == 0
        then do
          removeFile pending
          removeFile frag `catch` \e -> unless (isDoesNotExistError e) (throwIO e)
        else renameFile pending frag

-- | Nothing is staged at this path.  'show' renders the compile error, code first,
-- since that is what an uncaught exception prints.
newtype StagedFragmentMissing = StagedFragmentMissing FilePath

instance Show StagedFragmentMissing where
  show (StagedFragmentMissing pending) =
    generateErrorMessage (takeDirectory pending) (STAGED_FRAGMENT_MISSING pending)

instance Exception StagedFragmentMissing


-- ---------------------------------------------------------------------------
-- Atomic file write
-- ---------------------------------------------------------------------------

-- | Each write gets a fresh temp name: same-named modules compiled at once into
-- one directory would otherwise race on one fixed temp file.
atomicWrite :: FilePath -> BS.ByteString -> IO ()
atomicWrite path bytes = do
  (tmp, h) <- openBinaryTempFileWithDefaultPermissions (takeDirectory path) (takeFileName path <> ".tmp")
  (BS.hPut h bytes >> hClose h >> renameFile tmp path)
    `onException` ((hClose h `catch` ignore) >> (removeFile tmp `catch` ignore))
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()


-- ---------------------------------------------------------------------------
-- Compile-error emission
-- ---------------------------------------------------------------------------

throwSqlSchemaErrors :: MonadIO m => FilePath -> SrcSpan -> NonEmpty SqlSchemaError -> m a
throwSqlSchemaErrors fragDir modSpan = throwPluginMessages modSpan . fmap (generateErrorMessage fragDir)

-- | Fails the compile with one error per message.  'NonEmpty', because GHC
-- treats an empty set of errors as a failure with nothing to report.
throwPluginMessages :: MonadIO m => SrcSpan -> NonEmpty String -> m a
throwPluginMessages modSpan msgs =
#if __GLASGOW_HASKELL__ >= 904
  ParseError.throwErrors $ ParseError.mkMessages $ listToBag
    [ mkErrorMsgEnvelope modSpan reallyAlwaysQualify
        (ghcUnknownMessage (ParseError.mkPlainError ParseError.noHints
           (docToSDoc (Pretty.text m))))
    | m <- toList msgs ]
#else
  ParseError.throwErrors $ listToBag
    [ ParseError.mkErr modSpan reallyAlwaysQualify
        (ParseError.mkDecorated
           [docToSDoc (Pretty.text m)])
    | m <- toList msgs ]
#endif


-- ---------------------------------------------------------------------------
-- Small AST helpers
-- ---------------------------------------------------------------------------

moduleNameToString :: ModSummary -> String
moduleNameToString = GHC.moduleNameString . GHC.moduleName . ms_mod

modulePath :: ModSummary -> FilePath
modulePath ms = case ml_hs_file (ms_location ms) of
  Just p  -> p
  Nothing -> "<unknown>"

mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan loc = case ml_hs_file loc of
  Just p  -> mkGeneralSrcSpan (mkFastString p)
  Nothing -> mkGeneralSrcSpan (mkFastString "<unknown>")
