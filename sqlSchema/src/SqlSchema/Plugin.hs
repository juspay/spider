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
-- merge time by checking that the source file still exists.
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
  , syncFragment
  ) where

import           Control.Exception          (catch, throwIO)
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Yaml                  as YAML
import           GHC.Fingerprint            (Fingerprint, fingerprintString)
import           System.Directory           (createDirectoryIfMissing,
                                             removeFile, renameFile)
import           System.FilePath            (takeDirectory, (</>))
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
import qualified GHC.Types.Error            as ParseError
import           GHC.Types.SrcLoc           (SrcSpan, mkGeneralSrcSpan, unLoc)
import qualified GHC.Types.SourceError      as ParseError
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
  }

-- | GHC 9.4 wrapped the parsed module in 'ParsedResult'; it is returned unchanged.
#if __GLASGOW_HASKELL__ >= 904
onParsed :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
onParsed opts modSummary pr = pr <$ writeFragment opts modSummary (parsedResultModule pr)
#else
onParsed :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
onParsed opts modSummary hpm = hpm <$ writeFragment opts modSummary hpm
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


writeFragment
  :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc ()
writeFragment opts modSummary hpm = do
  let modSpan  = mkFileSrcSpan (ms_location modSummary)
  case parseCliOptions opts of
    Left err  -> throwSqlSchemaErrors (fragmentsDir defaultCliOptions) modSpan
                   [INVALID_PLUGIN_OPTIONS err]
    Right cli -> writeFragmentWith cli modSummary hpm

writeFragmentWith :: CliOptions -> ModSummary -> HsParsedModule -> Hsc ()
writeFragmentWith cli modSummary hpm = do
  let fragDir  = fragmentsDir cli
      modSpan  = mkFileSrcSpan (ms_location modSummary)
      modStr   = moduleNameToString modSummary
      srcPath  = modulePath modSummary
      fragPath = fragDir </> modStr <> ".yaml"
      decls    = hsmodDecls (unLoc (hpm_module hpm))

  case planFragment cli (ms_hsc_src modSummary) modStr srcPath decls of
    Left errs              -> throwSqlSchemaErrors fragDir modSpan errs
    Right LeaveUntouched   -> pure ()
    Right (Stage fragment) -> liftIO (syncFragment fragPath fragment)

-- | Writes the module's fragment, or with 'Nothing' removes the one an earlier
-- compile left: a restored build directory would otherwise keep a dropped
-- table in the contract.
syncFragment :: FilePath -> Maybe Fragment -> IO ()
syncFragment path = \case
  Just fragment -> do
    createDirectoryIfMissing True (takeDirectory path)
    atomicYamlWrite path fragment
  Nothing -> removeFile path `catch` \e -> unless (isDoesNotExistError e) (throwIO e)


-- ---------------------------------------------------------------------------
-- Atomic file write
-- ---------------------------------------------------------------------------

atomicYamlWrite :: A.ToJSON a => FilePath -> a -> IO ()
atomicYamlWrite path value = do
  let tmp = path <> ".tmp"
  BS.writeFile tmp (YAML.encode value)
  renameFile tmp path


-- ---------------------------------------------------------------------------
-- Compile-error emission
-- ---------------------------------------------------------------------------

throwSqlSchemaErrors :: FilePath -> SrcSpan -> [SqlSchemaError] -> Hsc ()
throwSqlSchemaErrors fragDir modSpan errs =
#if __GLASGOW_HASKELL__ >= 904
  ParseError.throwErrors $ ParseError.mkMessages $ listToBag
    [ mkErrorMsgEnvelope modSpan reallyAlwaysQualify
        (ghcUnknownMessage (ParseError.mkPlainError ParseError.noHints
           (docToSDoc (Pretty.text (generateErrorMessage fragDir e)))))
    | e <- errs ]
#else
  ParseError.throwErrors $ listToBag
    [ ParseError.mkErr modSpan reallyAlwaysQualify
        (ParseError.mkDecorated
           [docToSDoc (Pretty.text (generateErrorMessage fragDir e))])
    | e <- errs ]
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
