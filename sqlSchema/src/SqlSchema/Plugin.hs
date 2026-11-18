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
-- merge time, by module, against @--source-dirs@.  A compile
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
  , loadCliOptions
  , pendingPath
  , stagePending
  , stagePendingFrom
  , publishStaged
  , stagedTables
  , stagedTableNames
  ) where

import           Control.Exception          (Exception, IOException, catch, onException, throwIO,
                                             try)
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Aeson                 as A
#if MIN_VERSION_aeson(2,1,2)
import           Data.Aeson.Decoding.ByteString (bsToTokens)
import           Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import           Data.Aeson.Key             (Key)
#else
import           Data.Aeson.Internal        (IResult (ISuccess))
import qualified Data.Aeson.Parser          as AP
#endif
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable              (toList)
import           Data.List                  (isPrefixOf)
import           Data.List.NonEmpty         (NonEmpty (..), nonEmpty)
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Yaml                  as YAML
import           GHC.Fingerprint            (Fingerprint, fingerprintString)
import           System.Directory           (canonicalizePath, createDirectoryIfMissing, getFileSize,
                                             removeFile, renameFile)
import           System.FilePath            (splitDirectories, takeDirectory, takeFileName, (</>))
import           System.IO                  (hClose, openBinaryTempFileWithDefaultPermissions)
import           System.IO.Error            (isDoesNotExistError, tryIOError)

-- GHC API (>= 9.0)
import qualified GHC
import           GHC.Driver.Env             (Hsc)
import           GHC.Driver.Session         (FlagSpec (..), xFlags, xopt)
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
import           GHC.Types.SrcLoc           (SrcSpan, isGoodSrcSpan, mkGeneralSrcSpan, unLoc)
import qualified GHC.Types.SourceError      as ParseError
import           GHC.Unit.Module            (isInteractiveModule)
import           GHC.Unit.Module.Location   (ModLocation (..))
import           GHC.Unit.Module.ModSummary (ModSummary (..))
import           GHC.Utils.Outputable       (docToSDoc, reallyAlwaysQualify)
import qualified GHC.Utils.Ppr              as Pretty

import           SqlSchema.Extract          (FragmentAction (..), extensionErrors, planFragment)
import           SqlSchema.Types

import           Data.Function              (on)
import           Data.List                  (nubBy)
import           Data.Maybe                 (isNothing)
import           GHC.Builtin.Names          (genClassName, m1TyConName, metaConsDataConName, metaDataDataConName,
                                             metaSelDataConName, monoidClassName, prodTyConName, repTyConName,
                                             semigroupClassName, u1TyConName)
import           GHC.Builtin.Types          (consDataConName, eqTyConName, heqTyConName, justDataConName, maybeTyCon,
                                             nilDataConName, nothingDataConName, tupleDataConName)
import           GHC.Core.ConLike           (ConLike (..))
import           GHC.Core.DataCon           (DataCon, dataConExTyCoVars, dataConFieldLabels, dataConName,
                                             dataConOrigArgTys, dataConTheta, dataConTyCon, dataConUnivTyVars)
import           GHC.Core.FamInstEnv        (normaliseType)
import           GHC.Core.InstEnv           (ClsInst (..), classInstances, lookupInstEnv)
#if __GLASGOW_HASKELL__ >= 904
import           GHC.Core.InstEnv           (getPotentialUnifiers)
import           GHC.Core.Reduction         (reductionReducedType)
#endif
#if __GLASGOW_HASKELL__ >= 906
import           GHC.Core.TyCo.Compare      (eqType)
#else
import           GHC.Core.Type              (eqType)
#endif
import           GHC.Core.Multiplicity      (scaledThing)
import           GHC.Core.TyCon             (Role (..), TyCon, isAlgTyCon, isPromotedDataCon_maybe, isTypeFamilyTyCon,
                                             tyConDataCons, tyConName, tyConTyVars)
import           GHC.Core.Type              (Type, dropForAlls, expandTypeSynonyms, getTyVar_maybe, isStrLitTy,
                                             mkTyConApp, mkTyVarTys, splitAppTy_maybe, splitTyConApp_maybe,
                                             tyConAppTyCon_maybe)
import           GHC.Data.Bag               (bagToList)
import           GHC.Data.FastString        (unpackFS)
import           GHC.Driver.Env             (hsc_units)
import           GHC.Driver.Plugins         (keepRenamedSource)
import           GHC.Iface.Env              (lookupOrig)
import           GHC.Tc.Instance.Family     (tcGetFamInstEnvs)
import           GHC.Tc.Types               (TcGblEnv (..), TcM)
import           GHC.Tc.Utils.Env           (tcGetInstEnvs, tcLookupClass, tcLookupGlobal, tcLookupTyCon)
import           GHC.Tc.Utils.Monad         (getTopEnv, setGblEnv, tryTc)
import           GHC.Types.Id               (idType)
import           GHC.Types.Basic            (Boxity (..))
import           GHC.Types.FieldLabel       (flSelector)
import           GHC.Types.Name             (Name, getName, getOccString, isDataConName, nameIsLocalOrFrom,
                                             nameModule_maybe, nameSrcSpan)
import           GHC.Types.Name.Occurrence  (mkTcOcc, occName, occNameString)
import           GHC.Types.Name.Set         (allUses, nameSetElemsStable)
import           GHC.Types.SrcLoc           (GenLocated (..))
import           GHC.Types.TyThing          (TyThing (..))
import           GHC.Types.TypeEnv          (typeEnvTyCons)
import           GHC.Types.Var              (TyVar)
import           GHC.Unit.Module            (mkModule, moduleUnit)
import           GHC.Unit.State             (UnitState, lookupUnit, unitPackageNameString)
import           GHC.Utils.Outputable       (Outputable, SDocContext (..), defaultSDocContext, defaultUserStyle, ppr,
                                             showSDocOneLine)

import           SqlSchema.Extract          (InstanceHead (..), Origin (..), ResolvedColumn (..), ResolvedField (..),
                                             ResolvedGeneric (..), ResolvedValue (..), columnMismatches, fromModule,
                                             renderOrigin, foreignLargeRecords, genericMismatches,
                                             routingMismatches, unexpectedClasses, unextractedInstances,
                                             untrustedInstances)


-- ---------------------------------------------------------------------------
-- Plugin entry point
-- ---------------------------------------------------------------------------

-- | Fingerprinting the options makes an option change recompile every module,
-- so each fragment is rewritten under the new options.  A plugin rebuilt under
-- the same unit id need not: GHC keeps modules while the old library is readable.
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile    = \opts -> pure (MaybeRecompile (optionsFingerprint opts))
  , parsedResultAction = onParsed
    -- GHC keeps the renamed source, which the cross-check reads, only when asked.
  , renamedResultAction   = keepRenamedSource
  , typeCheckResultAction = \opts ms env -> env <$ checkInstances opts ms env
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
  [raw] -> let bytes = encodeUtf8 (T.pack raw) in case A.eitherDecode (BL.fromStrict bytes) of
    Right v
      | Just dup <- repeatedKey bytes ->
          Left ("the plugin option gives a key twice in one object, and only one of the values would be read: " <> dup)
      | otherwise -> Right v
    Left err -> Left ("cannot parse the plugin option as JSON CliOptions: " <> err)
  raws  -> Left ("expected at most one -fplugin-opt JSON string, got "
                   <> show (length raws))

-- | Why the JSON is refused: an object that gives a key twice (aeson's decode keeps one value
-- silently), or JSON this scan cannot read, which the caller has already decoded.
repeatedKey :: BS.ByteString -> Maybe String
#if MIN_VERSION_aeson(2,1,2)
repeatedKey = either Just (const Nothing) . value . bsToTokens
  where
    value :: Tokens k String -> Either String k
    value = \case
      TkLit _ k      -> Right k
      TkText _ k     -> Right k
      TkNumber _ k   -> Right k
      TkArrayOpen a  -> array a
      TkRecordOpen r -> record [] r
      TkErr e        -> Left e
    array :: TkArray k String -> Either String k
    array = \case
      TkItem t     -> value t >>= array
      TkArrayEnd k -> Right k
      TkArrayErr e -> Left e
    record :: [Key] -> TkRecord k String -> Either String k
    record seen = \case
      TkPair key t
        | key `elem` seen -> Left ("found duplicate key: " <> show key)
        | otherwise       -> value t >>= record (key : seen)
      TkRecordEnd k -> Right k
      TkRecordErr e -> Left e
#else
repeatedKey = either (Just . snd) (const Nothing) . AP.eitherDecodeStrictWith AP.jsonNoDup' ISuccess
#endif


-- | 'parseCliOptions', then the fragments directory resolved: it may be neither the working
-- directory nor one above it, where deleting a module's stale <Module>.yaml can delete an unrelated
-- file.  An absolute path or a link can name either without the '..' the parse refuses.
loadCliOptions :: [CommandLineOption] -> IO (Either String CliOptions)
loadCliOptions opts = case parseCliOptions opts of
  Left err  -> pure (Left err)
  Right cli -> do
    let dir = fragmentsDir cli
    resolved <- tryIOError ((,) <$> canonicalizePath "." <*> canonicalizePath dir)
    pure $ case resolved of
      Left e -> Left ("cannot resolve fragmentsDir " <> show dir <> ": " <> show e)
      Right (cwd, real)
        | splitDirectories real `isPrefixOf` splitDirectories cwd ->
            Left ("invalid fragmentsDir " <> show dir <> ": it resolves to " <> show real <> ", the working"
                  <> " directory or one above it, where deleting a module's stale <Module>.yaml can delete"
                  <> " an unrelated file; use a directory of its own, e.g. dist/build/sql-schema-fragments")
        | otherwise -> Right cli

stageFragment
  :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc ()
stageFragment opts modSummary hpm = do
  let modSpan  = mkFileSrcSpan (ms_location modSummary)
  loaded <- liftIO (loadCliOptions opts)
  case loaded of
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
    Right (Stage fragment) ->
      case nonEmpty (extensionErrors (extensionOn modSummary) (isJust fragment)) of
        Just es -> throwSqlSchemaErrors (fragmentsDir cli) modSpan es
        Nothing -> liftIO (try (stagePendingFrom (ml_hs_file (ms_location modSummary)) (fragmentPath cli modStr) fragment)) >>= \case
          Left (StagingFailure err) -> throwSqlSchemaErrors (fragmentsDir cli) modSpan (err :| [])
          Right ()                  -> pure ()
  where
    internalError =
      "sql-schema: INTERNAL_ERROR: the extractor rejected this module without saying why.\n"
      <> "  This is a bug in the sql-schema plugin, not in the module; report it with the module's source."

-- | Looked up by name, since the extension's type is in a package this one does not depend on;
-- an unknown name counts as on.
extensionOn :: ModSummary -> String -> Bool
extensionOn ms name = case [flagSpecFlag f | f <- xFlags, flagSpecName f == name] of
  [ext] -> xopt ext (ms_hspp_opts ms)
  _     -> True

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
  | otherwise = (guts <$) $ liftIO (loadCliOptions opts) >>= \case
      Left err  -> throwSqlSchemaErrors (fragmentsDir defaultCliOptions) (mg_loc guts)
                     (INVALID_PLUGIN_OPTIONS err :| [])
      Right cli -> do
        let frag = fragmentPath cli (GHC.moduleNameString (GHC.moduleName (mg_module guts)))
        published <- liftIO (try (publishStaged frag))
        case published of
          Left (StagingFailure err) -> throwSqlSchemaErrors (fragmentsDir cli) (mg_loc guts) (err :| [])
          Right ()                  -> pure ()

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
stagePending = stagePendingFrom Nothing

-- | 'stagePending', with the module's source file recorded in the fragment ('encodeFragmentFile').
stagePendingFrom :: Maybe FilePath -> FilePath -> Maybe Fragment -> IO ()
stagePendingFrom source frag fragment = ioFailure "stage" (pendingPath frag) $ do
  createDirectoryIfMissing True (takeDirectory frag)
  atomicWrite (pendingPath frag) (maybe BS.empty (encodeFragmentFile source) fragment)

-- | Replaces the fragment with what was staged, or throws a 'StagingFailure'.
-- A deletion removes the staged file first: a kill between the two removals
-- then leaves the fragment as before this compile, which matches the interface
-- file GHC never wrote, so the next build recompiles and stages again.
publishStaged :: FilePath -> IO ()
publishStaged frag = ioFailure "publish" frag $ publish `catch` \e ->
  -- The fragment's own removal ignores a missing file, so a missing file here is the staged one.
  if isDoesNotExistError e then throwIO (StagingFailure (STAGED_FRAGMENT_MISSING pending)) else throwIO e
  where
    pending = pendingPath frag
    publish = do
      size <- getFileSize pending
      if size == 0
        then do
          removeFile pending
          removeFile frag `catch` \e -> unless (isDoesNotExistError e) (throwIO e)
        else renameFile pending frag

-- | A staging or publishing step failed.  'show' renders the compile error, code first,
-- since that is what an uncaught exception prints.
newtype StagingFailure = StagingFailure SqlSchemaError

instance Show StagingFailure where
  show (StagingFailure err) = generateErrorMessage "" err

instance Exception StagingFailure

-- | An I/O error in @act@ becomes a 'StagingFailure' naming @path@.
ioFailure :: String -> FilePath -> IO a -> IO a
ioFailure action path act = act `catch` \e ->
  throwIO (StagingFailure (FRAGMENT_IO_ERROR action path (show (e :: IOException))))


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
throwSqlSchemaErrors fragDir modSpan = throwSqlSchemaErrorsAt fragDir . fmap ((,) modSpan)

throwSqlSchemaErrorsAt :: MonadIO m => FilePath -> NonEmpty (SrcSpan, SqlSchemaError) -> m a
throwSqlSchemaErrorsAt fragDir = throwPluginMessagesAt . fmap (fmap (generateErrorMessage fragDir))

throwPluginMessages :: MonadIO m => SrcSpan -> NonEmpty String -> m a
throwPluginMessages modSpan = throwPluginMessagesAt . fmap ((,) modSpan)

-- | Fails the compile with one error per message, each at its span.  'NonEmpty',
-- because GHC treats an empty set of errors as a failure with nothing to report.
throwPluginMessagesAt :: MonadIO m => NonEmpty (SrcSpan, String) -> m a
throwPluginMessagesAt msgs =
#if __GLASGOW_HASKELL__ >= 904
  ParseError.throwErrors $ ParseError.mkMessages $ listToBag
    [ mkErrorMsgEnvelope sp reallyAlwaysQualify
        (ghcUnknownMessage (ParseError.mkPlainError ParseError.noHints
           (docToSDoc (Pretty.text m))))
    | (sp, m) <- toList msgs ]
#else
  ParseError.throwErrors $ listToBag
    [ ParseError.mkErr sp reallyAlwaysQualify
        (ParseError.mkDecorated
           [docToSDoc (Pretty.text m)])
    | (sp, m) <- toList msgs ]
#endif


-- ---------------------------------------------------------------------------
-- Type-check cross-check
-- ---------------------------------------------------------------------------

-- | The tables of the fragment staged at @frag@ ('stagePending'), none for a staged
-- deletion, or a 'StagingFailure' as 'publishStaged' throws one.
stagedTables :: FilePath -> IO [TableSchema]
stagedTables frag = do
  bytes <- ioFailure "read" pending $ BS.readFile pending `catch` \e ->
    if isDoesNotExistError e then throwIO (StagingFailure (STAGED_FRAGMENT_MISSING pending)) else throwIO e
  if BS.null bytes
    then pure []
    else case decodeFragmentFile bytes of
      Right (fragment, _) -> pure (fragmentTables fragment)
      Left err       -> throwIO (StagingFailure (STAGED_FRAGMENT_UNREADABLE pending (YAML.prettyPrintParseException err)))
  where
    pending = pendingPath frag

stagedTableNames :: FilePath -> IO [String]
stagedTableNames = fmap (map codeName) . stagedTables

-- | After type checking, compares what the parse step staged with what GHC resolved:
-- every instance (Template Haskell's included) and its class, and each staged table's
-- routing fields, column types, Generic instance and the Semigroup and Monoid instances
-- its renames combine with.  A boot or signature file stages nothing.
checkInstances :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM ()
checkInstances opts ms env
  | isHsBootOrSig (ms_hsc_src ms) = pure ()
  | otherwise = case parseCliOptions opts of
      Left err -> throwSqlSchemaErrors (fragmentsDir defaultCliOptions) modSpan (INVALID_PLUGIN_OPTIONS err :| [])
      Right cli
        | isBlacklistedModule cli modStr -> pure ()
        | otherwise -> do
            let frag = fragmentPath cli modStr
            staged <- liftIO (try (stagedTables frag))
            tables <- case staged of
              Right ts                  -> pure ts
              Left (StagingFailure err) -> throwSqlSchemaErrors (fragmentsDir cli) modSpan (err :| [])
            origin <- nameOrigin . hsc_units <$> getTopEnv
            let at n
                  | nameIsLocalOrFrom (tcg_semantic_mod env) n, isGoodSrcSpan (nameSrcSpan n) = nameSrcSpan n
                  | otherwise = modSpan
                tableSpan t = maybe modSpan (at . getName) (tableTyCon env t)
                instanceHeadOf = \case
                  Nothing -> HeadNone
                  Just n
                    | getOccString n == "PrimaryKey", fromModule (origin n) beamCoreModule -> HeadKey
                    | nameIsLocalOrFrom (tcg_semantic_mod env) n -> HeadLocal (getOccString n)
                    | otherwise -> HeadForeign (getOccString n)
                instanceErrs =
                  [ (at (is_dfun_name i), e)
                  | i <- tcg_insts env, let c = is_cls_nm i; h = instanceHead i
                  , e <- unexpectedClasses [(getOccString c, getOccString <$> h, origin c)]
                         <> unextractedInstances (map codeName tables) [(getOccString c, instanceHeadOf h)] ]
            routingErrs <- case tcg_rn_decls env of
              Just grp -> do
                routing <- concat <$> mapM (\t -> map ((,) (tableSpan t)) <$> resolveRouting origin (modelMetaBindings grp) t) tables
                anns <- mapM (\(t, n) -> (\o -> (getOccString t, origin n, o)) <$> valueTypeOrigin origin n)
                             (largeRecordAnnotations grp)
                pure (routing <> [(tableSpan t, e) | t <- tables, e <- foreignLargeRecords [codeName t] anns])
              Nothing
                | null tables -> pure []
                | otherwise   -> throwPluginMessages modSpan (renamedSourceMissing :| [])
            -- GHC runs each plugin under the environment it started with; an earlier plugin's
            -- typeCheckResultAction may have handed this one a changed environment.
            (generics, trustErrs) <- setGblEnv env $ (,)
              <$> mapM (\t -> (,) t <$> resolveGeneric origin env t) tables
              <*> instanceTrustErrors origin at env tables
            let genericErrs =
                  [ (tableSpan t, e)
                  | (t, (labels, _, g)) <- generics, e <- genericMismatches (allowUnknownColumnWrapper cli) (codeName t) labels (map hsField (columns t)) g ]
                columnErrs =
                  [ (maybe (tableSpan t) at field, e)
                  | (t, (_, large, _)) <- generics, (c, r, field) <- resolveColumns origin large env t
                  , e <- columnMismatches (codeName t) [(c, r)] ]
            case nonEmpty (nubBy ((==) `on` snd) (instanceErrs <> routingErrs <> columnErrs <> genericErrs <> trustErrs)) of
              Just es -> throwSqlSchemaErrorsAt (fragmentsDir cli) es
              Nothing -> pure ()
  where
    modSpan = mkFileSrcSpan (ms_location ms)
    modStr  = moduleNameToString ms
    renamedSourceMissing =
      "sql-schema: INTERNAL_ERROR: GHC kept no renamed source for this module, so its tables' routing fields cannot be checked.\n"
      <> "  This plugin asks GHC to keep it, so another plugin of this compile dropped it: compile the module without the other -fplugin flags to find which, and report it with the module's source and those flags."

nameOrigin :: UnitState -> Name -> Maybe Origin
nameOrigin units n = do
  m <- nameModule_maybe n
  pure (Origin (unitPackageNameString <$> lookupUnit units (moduleUnit m)) (GHC.moduleNameString (GHC.moduleName m)))

-- | The type constructor heading the instance's last type, type synonyms expanded.
instanceHead :: ClsInst -> Maybe Name
instanceHead inst = case reverse (is_tys inst) of
  ty : _ -> tyConName . fst <$> splitTyConApp_maybe ty
  []     -> Nothing

-- | The method bindings of each ModelMeta instance of the renamed source, by its head
-- type constructor: the class and the head matched by name, as the parse step reads them.
modelMetaBindings :: HsGroup GhcRn -> [(String, LHsBinds GhcRn)]
modelMetaBindings grp =
  [ (getOccString t, binds)
  | L _ (ClsInstD _ ClsInstDecl{ cid_poly_ty = L _ sig, cid_binds = binds }) <- hsGroupInstDecls grp
  , Just (cls, [L _ ty]) <- [rnTypeHead (unLoc (sig_body sig))]
  , getOccString cls == "ModelMeta"
  , Just (t, _) <- [rnTypeHead ty]
  ]

-- | The type and the @largeRecord@ of each @{-# ANN type T largeRecord #-}@ of the renamed
-- source, the name matched as the parse step matches it, under parentheses and type signatures.
largeRecordAnnotations :: HsGroup GhcRn -> [(Name, Name)]
largeRecordAnnotations grp =
  [ (t, n)
#if __GLASGOW_HASKELL__ >= 906
  | L _ (HsAnnotation _ (TypeAnnProvenance (L _ t)) (L _ e)) <- hs_annds grp
#else
  | L _ (HsAnnotation _ _ (TypeAnnProvenance (L _ t)) (L _ e)) <- hs_annds grp
#endif
  , HsVar _ (L _ n) <- [peelRn e]
  , getOccString n == "largeRecord"
  ]

-- | The name heading a type and the types it is applied to, under the contexts, foralls,
-- parentheses, kind signatures and doc comments the parse step peels.
rnTypeHead :: HsType GhcRn -> Maybe (Name, [LHsType GhcRn])
rnTypeHead = go []
  where
    go args = \case
      HsQualTy _ _ (L _ t)   -> go args t
      HsForAllTy _ _ (L _ t) -> go args t
      HsParTy _ (L _ t)      -> go args t
      HsKindSig _ (L _ t) _  -> go args t
      HsDocTy _ (L _ t) _    -> go args t
      HsAppTy _ (L _ f) x    -> go (x : args) f
      HsTyVar _ _ (L _ n)    -> Just (n, args)
      _                      -> Nothing

resolveRouting :: (Name -> Maybe Origin) -> [(String, LHsBinds GhcRn)] -> TableSchema -> TcM [SqlSchemaError]
resolveRouting origin metas t = case [binds | (h, binds) <- metas, h == codeName t] of
  [binds] -> routingMismatches t <$> resolveField origin "modelTableType" binds
                                 <*> resolveField origin "modelSchemaName" binds
  found   -> pure [ MODEL_META_FIELD_MISMATCH (codeName t) "modelTableType"
                      ("GHC's renamed source has " <> show (length found)
                       <> " ModelMeta instances for it, not one, so the plugin cannot tell which one it read") ]

-- | The instance's binding of @field@ as GHC resolved it, if it is the one equation with no
-- arguments, guards or where bindings that the parse step reads.
resolveField :: (Name -> Maybe Origin) -> String -> LHsBinds GhcRn -> TcM ResolvedField
resolveField origin field binds = case [b | L _ b@FunBind{ fun_id = L _ n } <- bagToList binds, getOccString n == field] of
  [] -> pure FieldAbsent
  [FunBind{ fun_matches = MG{ mg_alts = L _ [L _ (Match _ _ [] GRHSs{ grhssGRHSs = [L _ (GRHS _ [] (L _ rhs))]
                                                                     , grhssLocalBinds = local })] } }]
    | noBindings local -> resolveMaybe origin rhs
  bs -> pure (FieldOther (unwords (map rendered bs)))
  where
    noBindings = \case
      EmptyLocalBinds _  -> True
      HsValBinds _ vals  -> isEmptyValBinds vals
      _                  -> False

resolveMaybe :: (Name -> Maybe Origin) -> HsExpr GhcRn -> TcM ResolvedField
resolveMaybe origin expr = case peelRn expr of
  HsVar _ (L _ n)
    | n == nothingDataConName     -> pure FieldNothing
    | getOccString n == "Nothing" -> pure (FieldForeign "Nothing" (origin n))
  HsApp _ (L _ f) (L _ x)
    | HsVar _ (L _ j) <- peelRn f, j == justDataConName     -> FieldJust <$> resolveValue origin (peelRn x)
    | HsVar _ (L _ j) <- peelRn f, getOccString j == "Just" -> pure (FieldForeign "Just" (origin j))
  e -> pure (FieldOther (rendered e))

-- | A data constructor and a pattern synonym share a namespace, so only the type
-- environment tells them apart.
resolveValue :: (Name -> Maybe Origin) -> HsExpr GhcRn -> TcM ResolvedValue
resolveValue origin = \case
  HsVar _ (L _ n)
    | isDataConName n -> do
        thing <- tcLookupGlobal n
        pure $ case thing of
          AConLike (RealDataCon dc) ->
            let tc = dataConTyCon dc in ValueConstructor (getOccString n) (origin (tyConName tc)) (getOccString tc)
          AConLike (PatSynCon _) -> ValuePatternSynonym (getOccString n) (origin n)
          _                      -> ValueOther (getOccString n)
    | otherwise -> pure (ValueVariable (getOccString n))
  HsLit _ (HsString _ fs)                         -> pure (ValueLiteral (unpackFS fs))
  HsOverLit _ OverLit{ ol_val = HsIsString _ fs } -> pure (ValueLiteral (unpackFS fs))
  e -> pure (ValueOther (rendered e))

-- | 'stripPar' with the type signatures 'maybeShape' peels, on the renamed source.
peelRn :: HsExpr GhcRn -> HsExpr GhcRn
peelRn = \case
#if __GLASGOW_HASKELL__ >= 904
  HsPar _ _ (L _ e) _       -> peelRn e
#else
  HsPar _ (L _ e)           -> peelRn e
#endif
  ExprWithTySig _ (L _ e) _ -> peelRn e
  e                         -> e

-- | Each staged column of a table with the type GHC gives its record field, and the field's
-- selector; for a large-records table (@large@, 'resolveGeneric'), the type its equality
-- constraint fixes.
resolveColumns :: (Name -> Maybe Origin) -> Bool -> TcGblEnv -> TableSchema -> [(ColumnInfo, ResolvedColumn, Maybe Name)]
resolveColumns origin large env t =
  case tableTyCon env t of
    Just tc | [dc] <- tyConDataCons tc ->
      let fields  = [ (occNameString (occName fl), (flSelector fl, ty))
                    | (fl, ty) <- zip (dataConFieldLabels dc) (map scaledThing (dataConOrigArgTys dc)) ]
          functor = case reverse (dataConUnivTyVars dc) of
            v : _ -> Just v
            []    -> Nothing
          resolve ty = either ColumnOther (columnType origin functor) (if large then fixedType dc ty else Right ty)
      in [ case lookup (hsField c) fields of
             Just (sel, ty) -> (c, resolve ty, Just sel)
             Nothing        -> (c, ColumnMissing, Nothing)
         | c <- columns t ]
    _ -> [ (c, ColumnOther ("GHC finds no data type '" <> codeName t <> "' with one constructor in this module"), Nothing)
         | c <- columns t ]

-- | large-records 0.4.4 types each field as an existential type variable of the constructor,
-- which one equality constraint of the constructor fixes, written @forall f1. (f1 ~ τ) => …@.
fixedType :: DataCon -> Type -> Either String Type
fixedType dc ty = case equatedTypes dc ty of
  Nothing -> Right ty
  Just [x]
    | maybe True (`notElem` dataConExTyCoVars dc) (getTyVar_maybe x) -> Right x
    | otherwise -> Left ("GHC types it as the type variable '" <> rendered ty <> "', which an equality constraint of its record's constructor fixes only to the type variable '"
                         <> rendered x <> "', so its type is not known")
  Just xs -> Left ("GHC types it as the type variable '" <> rendered ty <> "', which " <> show (length xs)
                   <> " constraints '" <> rendered ty <> " ~ t' of its record's constructor fix, not one, so its type is not known")

-- | For a field typed by an existential type variable of its constructor, the types the
-- constructor's equality constraints (@~@ or @~~@, either way round) equate the variable with.
equatedTypes :: DataCon -> Type -> Maybe [Type]
equatedTypes dc ty = case getTyVar_maybe ty of
  Just v | v `elem` dataConExTyCoVars dc -> Just (nubBy eqType (concatMap (equated v) (dataConTheta dc)))
  _ -> Nothing
  where
    equated v p = case splitTyConApp_maybe p of
      Just (tc, args) | tyConName tc `elem` [eqTyConName, heqTyConName], Just (a, b) <- lastTwo args ->
        [b | getTyVar_maybe a == Just v] <> [a | getTyVar_maybe b == Just v]
      _ -> []

-- | Mirrors the parse step's reading of a column field: @Columnar f a@ or @C f a@, @f@ the
-- table's last type parameter, nullable when @a@ is headed by @Maybe@; here on the type GHC
-- gives the field, with type synonyms expanded and the names resolved.
columnType :: (Name -> Maybe Origin) -> Maybe TyVar -> Type -> ResolvedColumn
columnType origin functor ty = case splitTyConApp_maybe expanded of
  Just (tc, [f, a])
    | getOccString tc == "Columnar", fromModule (origin (tyConName tc)) beamCoreModule ->
        if isNothing functor || getTyVar_maybe f /= functor
          then ColumnOther ("GHC types it as '" <> shown <> "', which applies Columnar to something other than the table's last type parameter")
          else case splitTyConApp_maybe a of
            Just (atc, _)
              | atc == maybeTyCon     -> ColumnColumnar True shown
              | isTypeFamilyTyCon atc -> ColumnOther ("GHC types it as '" <> shown <> "', whose type inside Columnar is a type family application, so its nullability is not known")
              | otherwise             -> ColumnColumnar False shown
            Nothing -> ColumnOther ("GHC types it as '" <> shown <> "', whose type inside Columnar has no type constructor at its head, so its nullability is not known")
  Just (tc, _)
    | getOccString tc == "Columnar" ->
        ColumnOther ("GHC types it as '" <> shown <> "', with the Columnar of " <> renderOrigin (origin (tyConName tc)) <> ", not beam-core's")
  _ -> ColumnOther ("GHC types it as '" <> shown <> "', which is not beam-core's Columnar applied to the table's last type parameter")
  where
    expanded = expandTypeSynonyms ty
    shown    = rendered expanded

tableTyCon :: TcGblEnv -> TableSchema -> Maybe TyCon
tableTyCon env t = case [tc | tc <- typeEnvTyCons (tcg_type_env env), isAlgTyCon tc, getOccString tc == codeName t] of
  [tc] | [_] <- tyConDataCons tc -> Just tc
  _                              -> Nothing

valueTypeOrigin :: (Name -> Maybe Origin) -> Name -> TcM (Maybe Origin)
valueTypeOrigin origin n = do
  thing <- tcLookupGlobal n
  pure $ case thing of
    AnId i -> origin . tyConName =<< tyConAppTyCon_maybe (expandTypeSynonyms (dropForAlls (idType i)))
    _      -> Nothing

-- | The table's record fields, whether it is a large-records table, and its GHC.Generics instance
-- among every instance GHC sees here (imported and orphan ones included, by class Name): its
-- representation's selectors, type families reduced, or a large-records table's @MetadataOf@
-- names.  That plugin consumes the @largeRecord@ annotation before renaming, so a large-records
-- table is told by its representation: large-generics' @ThroughLRGenerics@ of it.
resolveGeneric :: (Name -> Maybe Origin) -> TcGblEnv -> TableSchema -> TcM ([String], Bool, ResolvedGeneric)
resolveGeneric origin env t = case tableTyCon env t of
  Nothing -> pure ([], False, GenericOther ("GHC finds no data type '" <> codeName t <> "' with one constructor in this module"))
  Just tc -> (\(large, g) -> ([occNameString (occName fl) | dc <- tyConDataCons tc, fl <- dataConFieldLabels dc], large, g)) <$> do
    cls  <- tcLookupClass genClassName
    envs <- tcGetInstEnvs
    fams <- tcGetFamInstEnvs
    let ty = mkTyConApp tc (mkTyVarTys (tyConTyVars tc))
#if __GLASGOW_HASKELL__ >= 904
        (matches, potential, _) = lookupInstEnv False envs cls [ty]
        unifiers = getPotentialUnifiers potential
        normalise x = reductionReducedType (normaliseType fams Nominal x)
#else
        (matches, unifiers, _) = lookupInstEnv False envs cls [ty]
        normalise x = snd (normaliseType fams Nominal x)
#endif
        throughLR rep = case splitTyConApp_maybe rep of
          Just (rtc, args)
            | getOccString rtc == "ThroughLRGenerics", fromModule (origin (tyConName rtc)) throughLRGenericsModule
            , x : _ <- reverse args, x `eqType` ty -> nameModule_maybe (tyConName rtc)
          _ -> Nothing
    case (matches, unifiers) of
      ([_], []) -> do
        repTc <- tcLookupTyCon repTyConName
        let rep = normalise (mkTyConApp repTc [ty])
        case throughLR rep of
          Just m -> do
            -- A package named large-generics need not define MetadataOf; GHC's own lookup error has no code.
            found <- fst <$> tryTc (tcLookupTyCon =<< lookupOrig (mkModule (moduleUnit m) (GHC.mkModuleName (snd largeGenericsModule)))
                                                                (mkTcOcc "MetadataOf"))
            pure . (,) True $ case found of
              Just metadataOf -> either GenericOther GenericMetadata (metadataNames (normalise (mkTyConApp metadataOf [ty])))
              Nothing -> GenericOther ("its representation is large-generics' ThroughLRGenerics, but GHC finds no MetadataOf in module "
                                       <> snd largeGenericsModule <> " of that package, so the field names Beam reads are not known")
          Nothing -> pure (False, either GenericOther GenericSelectors (repSelectors rep))
      _ -> pure (False, GenericInstances [origin (is_dfun_name i) | (i, _) <- matches] [origin (is_dfun_name i) | i <- unifiers])

-- | The field names, in order, of large-generics metadata: @'[ '("name", type), … ]@.
metadataNames :: Type -> Either String [String]
metadataNames md = go md
  where
    go x = case splitTyConApp_maybe x of
      Just (tc, args)
        | promoted tc == Just nilDataConName -> Right []
        | promoted tc == Just consDataConName, Just (field, rest) <- lastTwo args -> (:) <$> name field <*> go rest
      _ -> Left ("its large-records metadata is '" <> rendered md <> "', not a list of field names and types")
    name f = case splitTyConApp_maybe f of
      Just (tc, args)
        | promoted tc == Just (tupleDataConName Boxed 2), Just (s, _) <- lastTwo args, Just fs <- isStrLitTy s -> Right (unpackFS fs)
      _ -> Left ("its large-records metadata has '" <> rendered f <> "' where a field's name and type belong")

-- | The selectors, in order, of a GHC.Generics representation of one constructor.
repSelectors :: Type -> Either String [Maybe String]
repSelectors rep = case metaApp metaDataDataConName rep of
  Nothing -> Left ("its Generic representation is '" <> rendered rep <> "', not GHC.Generics' representation of a data type")
  Just con -> case metaApp metaConsDataConName con of
    Nothing     -> Left ("its Generic representation has '" <> rendered con <> "' where one constructor belongs")
    Just fields -> traverse selector (products fields)
  where
    products x = case splitTyConApp_maybe x of
      Just (tc, args)
        | tyConName tc == prodTyConName, Just (a, b) <- lastTwo args -> products a <> products b
        | tyConName tc == u1TyConName -> []
      _ -> [x]
    selector x = case splitTyConApp_maybe x of
      Just (tc, args)
        | tyConName tc == m1TyConName, Just (meta, _) <- lastTwo args
        , Just (mtc, name : _) <- splitTyConApp_maybe meta, promoted mtc == Just metaSelDataConName -> Right (symbol name)
      _ -> Left ("its Generic representation has '" <> rendered x <> "' where a field belongs")
    symbol name = case splitTyConApp_maybe name of
      Just (jtc, args) | promoted jtc == Just justDataConName, s : _ <- reverse args -> unpackFS <$> isStrLitTy s
      _ -> Nothing

-- | The type under @M1 i meta@ when @meta@ is the given promoted constructor.
metaApp :: Name -> Type -> Maybe Type
metaApp dc x = case splitTyConApp_maybe x of
  Just (tc, args)
    | tyConName tc == m1TyConName, Just (meta, inner) <- lastTwo args
    , Just (mtc, _) <- splitTyConApp_maybe meta, promoted mtc == Just dc -> Just inner
  _ -> Nothing

promoted :: TyCon -> Maybe Name
promoted tc = dataConName <$> isPromotedDataCon_maybe tc

-- | The last two of a type constructor's arguments, after its kind arguments.
lastTwo :: [a] -> Maybe (a, a)
lastTwo xs = case reverse xs of
  b : a : _ -> Just (a, b)
  _         -> Nothing

-- | Once the module uses beam-core's setEntityName or modifyTableFields: for each table it can
-- rename (staged here, or the table of a record field of this module), every Semigroup and
-- Monoid instance that could apply at its EntityModification, for any database and backend.
-- Scanning the class's instances, unlike lookupInstEnv, also finds incoherent ones.
instanceTrustErrors :: (Name -> Maybe Origin) -> (Name -> SrcSpan) -> TcGblEnv -> [TableSchema] -> TcM [(SrcSpan, SqlSchemaError)]
instanceTrustErrors origin at env tables =
  case [ (n, m) | n <- nameSetElemsStable (allUses (tcg_dus env))
                , getOccString n `elem` ["setEntityName", "modifyTableFields"]
                , fromModule (origin n) beamCoreModule, Just m <- [nameModule_maybe n] ] of
    []    -> pure []
    (use, m) : _ -> do
      -- A beam-core that defines them in another module would fail the compile with GHC's uncoded lookup error.
      found <- mapM (\o -> (,) o . fst <$> tryTc (tcLookupTyCon =<< lookupOrig m (mkTcOcc o))) ["EntityModification", "TableEntity"]
      case found of
        [(_, Just modification), (_, Just entity)] -> do
          envs    <- tcGetInstEnvs
          classes <- mapM tcLookupClass [semigroupClassName, monoidClassName]
          let (fieldTables, unknown) = recordTables entity (typeEnvTyCons (tcg_type_env env))
              renamed = nubBy ((==) `on` fst) ([(tc, getName tc) | t <- tables, Just tc <- [tableTyCon env t]] <> fieldTables)
          pure $ [ (at n, UNCHECKABLE_RENAMES what why) | (n, what, why) <- unknown ]
              <> [ (at n, e) | (t, n) <- renamed, cls <- classes
                   , e <- untrustedInstances (getOccString cls) (getOccString t)
                            [origin (is_dfun_name i) | i <- classInstances envs cls, mayApplyAt modification entity t i] ]
        _ -> pure [ (at use, UNCHECKABLE_RENAMES ("module " <> GHC.moduleNameString (GHC.moduleName m) <> " of the beam-core it uses")
                                                ("GHC finds no " <> o <> " in it, where beam-core 0.9 defines it beside setEntityName"))
                  | (o, Nothing) <- found ]

-- | The tables of this module's record fields typed @f (TableEntity T)@, each with the field's
-- selector, or its record's type for a field without one; a large record's field is read at the
-- type its equality constraint fixes ('fixedType').  Second, each field of an existential type
-- variable that equality constraints mention but do not fix to one type, which could hold a
-- table unseen; a variable no equality constraint mentions (@forall a. Show a => …@) holds none.
recordTables :: TyCon -> [TyCon] -> ([(TyCon, Name)], [(Name, String, String)])
recordTables entity tcs =
  ( [ (x, n)
    | (dc, ty, n, _) <- fields
    , Right fixed <- [fixedType dc ty]
    , Just (_, arg) <- [splitAppTy_maybe (expandTypeSynonyms fixed)]
    , Just (etc, [a]) <- [splitTyConApp_maybe arg], etc == entity
    , Just x <- [tyConAppTyCon_maybe a] ]
  , [ (n, what, why) | (dc, ty, n, what) <- fields, Just (_ : _) <- [equatedTypes dc ty], Left why <- [fixedType dc ty] ] )
  where
    fields =
      [ (dc, ty, n, what)
      | tc <- tcs, isAlgTyCon tc, dc <- tyConDataCons tc
      , let tys = map scaledThing (dataConOrigArgTys dc)
            fls = dataConFieldLabels dc
      , (ty, (n, what)) <- zip tys $ if length fls == length tys
          then [(flSelector fl, "the field '" <> occNameString (occName fl) <> "' of '" <> getOccString tc <> "'") | fl <- fls]
          else map (const (getName tc, "a field of '" <> getOccString tc <> "'")) tys ]

-- | Whether the instance's head could be @EntityModification f be (TableEntity t)@ for some
-- @f@ and @be@: a variable where a type constructor would be could be anything.
mayApplyAt :: TyCon -> TyCon -> TyCon -> ClsInst -> Bool
mayApplyAt modification entity t inst = case reverse (is_tys inst) of
  ty : _ -> case splitTyConApp_maybe ty of
    Nothing -> True
    Just (tc, args) -> tc == modification && case reverse args of
      e : _ -> case splitTyConApp_maybe e of
        Nothing -> True
        Just (etc, eargs) -> etc == entity && case reverse eargs of
          x : _ -> maybe True (== t) (tyConAppTyCon_maybe x)
          []    -> False
      [] -> False
  [] -> False

-- | On one line, without the uniques the default rendering gives type variables.
rendered :: Outputable a => a -> String
rendered = showSDocOneLine defaultSDocContext { sdocStyle = defaultUserStyle, sdocSuppressUniques = True } . ppr


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
