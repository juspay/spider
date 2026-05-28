{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | GHC plugin that extracts the SQL schema implied by Beam table
-- definitions and writes one YAML fragment per module.  A separate
-- @sql-schema-merge@ executable combines all fragments into the final
-- contract.
--
-- Per-module fragments solve the incremental-build problem: each module's
-- fragment is rewritten only when GHC recompiles the module, and stale
-- fragments are pruned at merge time by checking that the source file
-- still exists.
--
-- Safety policy: anything the extractor cannot recognise becomes a hard
-- compile error.  Silent omission of a table or wrong column name would
-- produce a downstream false-pass against the prod DB schema — i.e. prod
-- downtime — so the only safe response to an unknown shape is to refuse
-- to compile.  Two opt-out flags exist for migrations:
-- @allowUnknownColumnWrapper@ and @allowNonStandardModifier@.
module SqlSchema.Plugin (plugin) where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (foldl', isPrefixOf, nub)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Yaml                  as YAML
import           System.Directory           (createDirectoryIfMissing,
                                             renameFile)
import           System.FilePath            ((</>))

-- GHC API (>= 9.0)
import qualified GHC
import           GHC.Driver.Env             (Hsc)
import           GHC.Data.Bag               (bagToList, listToBag)
import           GHC.Data.FastString        (mkFastString, unpackFS)
import           GHC.Driver.Plugins         (CommandLineOption, Plugin (..),
                                             PluginRecompile (..), defaultPlugin)
import           GHC.Hs
import qualified GHC.Types.Error            as ParseError
import           GHC.Types.Name.Reader      (RdrName (..), rdrNameOcc)
import           GHC.Types.SrcLoc           (GenLocated (..), SrcSpan,
                                             mkGeneralSrcSpan, unLoc)
import qualified GHC.Types.SourceError      as ParseError
import           GHC.Unit.Module.Location   (ModLocation (..))
import           GHC.Unit.Module.ModSummary (ModSummary (..))
import           GHC.Utils.Outputable       (Outputable, docToSDoc, ppr,
                                             reallyAlwaysQualify, showSDocUnsafe)
import qualified GHC.Utils.Ppr              as Pretty

import           SqlSchema.Types


-- ---------------------------------------------------------------------------
-- Plugin entry point
-- ---------------------------------------------------------------------------

-- | 'NoForceRecompile' is deliberate: when a module's source hasn't
-- changed, its existing fragment on disk is still correct, and the
-- merge CLI prunes stale fragments by checking that the source file
-- exists.  Switching to 'ForceRecompile' would re-run every module's
-- plugin pass on every build for no extra safety.
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile    = const (pure NoForceRecompile)
  , parsedResultAction = writeFragment
  }


parseCliOptions :: [CommandLineOption] -> CliOptions
parseCliOptions = \case
  []        -> defaultCliOptions
  (raw : _) ->
    case A.decode (BL.fromStrict (encodeUtf8 (T.pack raw))) of
      Just v  -> v
      Nothing -> defaultCliOptions


writeFragment
  :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
writeFragment opts modSummary hpm = do
  let cli      = parseCliOptions opts
      fragDir  = fragmentsDir cli
      modSpan  = mkFileSrcSpan (ms_location modSummary)
      modStr   = moduleNameToString modSummary
      srcPath  = modulePath modSummary

  if any (`isPrefixOf` modStr) (blacklistModules cli)
    then pure hpm
    else do
      let decls        = hsmodDecls (unLoc (hpm_module hpm))
          rm           = foldl' visitDecl emptyRawModule decls
          rows         = extractFromModule cli modStr rm
          errs         = concat [es | (_, Left es) <- rows]
          okTables     = [ts | (_, Right ts) <- rows]
          dbOverrides  = extractDbOverrides modStr srcPath rm

      when (not (null errs))
        (throwSqlSchemaErrors fragDir modSpan errs)

      when (not (null okTables) || not (null dbOverrides)) $ liftIO $ do
        createDirectoryIfMissing True fragDir
        let fragment = Fragment
              { fragmentModule           = modStr
              , fragmentTables           = okTables
              , fragmentDbEntityOverrides = dbOverrides
              }
            fragPath = fragDir </> modStr <> ".yaml"
        atomicYamlWrite fragPath fragment
      pure hpm


-- ---------------------------------------------------------------------------
-- Atomic file write
-- ---------------------------------------------------------------------------

atomicYamlWrite :: A.ToJSON a => FilePath -> a -> IO ()
atomicYamlWrite path value = do
  let tmp = path <> ".tmp"
  BS.writeFile tmp (YAML.encode value)
  renameFile tmp path


-- ---------------------------------------------------------------------------
-- Module-level extraction
-- ---------------------------------------------------------------------------

data RawModule = RawModule
  { rmTables          :: [(SrcSpan, String, RawTable)]
    -- ^ Candidate table-shaped data decls (single-constructor records
    --   with at least one type variable).  Filtered to actual Beam
    --   tables at extraction time via 'rmBeamableTypes'.
  , rmBeamableTypes   :: Set String
    -- ^ Type names that are known to be Beam tables — populated by either
    --   inline @deriving Beamable@ on the data decl, OR by a standalone
    --   @instance Beamable XxxT@ declaration elsewhere in the module.
  , rmModelMetas      :: Map.Map String RawModelMeta
  , rmTableInsts      :: Map.Map String RawTableInst
  , rmBindings        :: Map.Map String (LHsExpr GhcPs)
    -- ^ top-level @name = expr@ value bindings (e.g. each table's
    --   @xxxTMod@, and any @appDb@-style DB-settings binding).
  , rmDbRecords       :: Map.Map String (Map.Map String String)
    -- ^ DB-record data declarations (those that derive @Database@) keyed
    --   by the record type name (e.g. @\"AppDb\"@), with an inner map
    --   from record-field name (e.g. @\"gadgets\"@) to the unqualified
    --   table-type constructor inside @TableEntity@ (e.g. @\"GadgetT\"@).
  }

emptyRawModule :: RawModule
emptyRawModule = RawModule [] Set.empty Map.empty Map.empty Map.empty Map.empty

data RawTable = RawTable
  { rtFields      :: [RawField]
    -- ^ Fields whose @Columnar f T@ wrapper was recognised, in source
    --   order.
  , rtFieldErrors :: [SqlSchemaError]
    -- ^ One entry per field whose wrapper was NOT recognised.  Surfaced
    --   from 'assemble' so the typeName can be filled in.
  } deriving Show

data RawField = RawField
  { rfName     :: String
  , rfInner    :: String        -- pretty-printed type inside Columnar f _
  , rfNullable :: Bool          -- inner type's head is "Maybe"
  } deriving Show

-- | Intermediate per-field result from the data-decl pass.
data RawFieldOrError
  = RFOk RawField
  | RFErr SqlSchemaError

data RawModelMeta = RawModelMeta
  { rmmTableNameLit :: Maybe String
  , rmmModifierName :: Maybe String
  , rmmTableType    :: Maybe String
  }

data RawTableInst = RawTableInst
  { rtiPkPats :: [Pat GhcPs]
  , rtiPkBody :: Maybe (HsExpr GhcPs)
  }

extractFromModule
  :: CliOptions
  -> String
  -> RawModule
  -> [(String, Either [SqlSchemaError] TableSchema)]
extractFromModule cli modName rm =
  [ assemble cli modName rm tyName rt
  | (_, tyName, rt) <- rmTables rm
  , Set.member tyName (rmBeamableTypes rm)
  ]


visitDecl :: RawModule -> LHsDecl GhcPs -> RawModule
visitDecl rm (L l decl) = case decl of
  -- Beam-table-shaped data decl: stash as a candidate regardless of
  -- deriving clause (the actual is-a-Beam-table filter happens at
  -- extraction time, gated by 'rmBeamableTypes').  If the decl HAS an
  -- inline 'deriving Beamable', mark the type as Beam-confirmed too.
  TyClD _ (DataDecl { tcdLName = L _ lname
                    , tcdTyVars = tyVars
                    , tcdDataDefn = defn })
    | Just tbl <- extractRawTable tyVars defn ->
        let tyName = occToString lname
            rm'   = rm { rmTables = rmTables rm <> [(GHC.locA l, tyName, tbl)] }
        in  if hasBeamableDeriving defn
              then rm' { rmBeamableTypes = Set.insert tyName (rmBeamableTypes rm') }
              else rm'
    | hasDatabaseDeriving defn
    , Just dbMap <- extractDbRecord tyVars defn ->
        rm { rmDbRecords = Map.insert (occToString lname) dbMap (rmDbRecords rm) }
  InstD _ (ClsInstD _ ClsInstDecl{ cid_poly_ty, cid_binds }) ->
    case headTypeOfInstance cid_poly_ty of
      -- Standalone 'instance Beamable XxxT' — marks XxxT as a Beam
      -- table even if its data decl lacks an inline 'deriving Beamable'.
      Just ("Beamable", typeName) ->
        rm { rmBeamableTypes = Set.insert typeName (rmBeamableTypes rm) }
      Just ("ModelMeta", typeName) ->
        rm { rmModelMetas = Map.insert typeName
                              (extractModelMeta cid_binds)
                              (rmModelMetas rm) }
      Just ("Table", typeName) ->
        rm { rmTableInsts = Map.insert typeName
                              (extractTableInst cid_binds)
                              (rmTableInsts rm) }
      _ -> rm
  ValD _ (FunBind { fun_id = L _ nm, fun_matches = mg })
    | Just rhs <- singleRhs mg ->
        rm { rmBindings = Map.insert (occToString nm) rhs (rmBindings rm) }
  ValD _ (PatBind { pat_lhs = L _ (VarPat _ (L _ nm)), pat_rhs = grhss })
    | Just rhs <- grhssExpr grhss ->
        rm { rmBindings = Map.insert (occToString nm) rhs (rmBindings rm) }
  _ -> rm


-- | Convert an HsType into the @(className, headTypeName)@ pair for instance
-- heads of the form @Cls Foo@.
headTypeOfInstance :: LHsSigType GhcPs -> Maybe (String, String)
headTypeOfInstance lsig =
  case unLoc (sig_body (unLoc lsig)) of
    HsAppTy _ (L _ cls) (L _ ty) -> do
      clsN <- tyConHead cls
      tyN  <- tyConHead ty
      Just (clsN, tyN)
    _ -> Nothing

tyConHead :: HsType GhcPs -> Maybe String
tyConHead = \case
  HsTyVar _ _ (L _ n)   -> Just (occToString n)
  HsAppTy _ (L _ a) _   -> tyConHead a
  HsParTy _ (L _ a)     -> tyConHead a
  HsKindSig _ (L _ a) _ -> tyConHead a
  _                     -> Nothing


-- ---------------------------------------------------------------------------
-- Beam table data declaration
-- ---------------------------------------------------------------------------

-- | Build a 'RawTable' from a Beamable-deriving data declaration.
-- Surfaces both the recognised fields AND a parallel list of unrecognised
-- wrappers so 'assemble' can emit errors with the type name attached.
extractRawTable
  :: LHsQTyVars GhcPs -> HsDataDefn GhcPs -> Maybe RawTable
extractRawTable tyVars defn = do
  fVarName <- lastTypeVarName tyVars
  case dd_cons defn of
    [L _ (ConDeclH98{ con_args = RecCon (L _ recs) })] -> do
      let raw = concatMap (extractField fVarName . unLoc) recs
          oks = [r | RFOk r <- raw]
          es  = [e | RFErr e <- raw]
      Just RawTable { rtFields = oks, rtFieldErrors = es }
    _ -> Nothing

lastTypeVarName :: LHsQTyVars GhcPs -> Maybe String
lastTypeVarName HsQTvs{ hsq_explicit = vars } =
  case reverse vars of
    []          -> Nothing
    (L _ v : _) -> Just $ case v of
      UserTyVar _ _ (L _ n)     -> occToString n
      KindedTyVar _ _ (L _ n) _ -> occToString n

-- | Extract a list of field entries (one per name on the LHS — fields with
-- multiple names share the type).  Each entry is either a recognised
-- 'RawField' or an 'RFErr' tagging the wrapper as unrecognised.
extractField :: String -> ConDeclField GhcPs -> [RawFieldOrError]
extractField fVarName ConDeclField{ cd_fld_names = lnames, cd_fld_type = lty } =
  case unwrapColumnar fVarName (unLoc lty) of
    Just innerTy ->
      let inner = sdocText innerTy
          isMb  = isMaybeTy innerTy
      in  [ RFOk RawField { rfName     = occNameFromFieldOcc (unLoc ln)
                          , rfInner    = stripParens inner
                          , rfNullable = isMb
                          }
          | ln <- lnames
          ]
    Nothing ->
      -- typeName is empty here; 'assemble' (which knows it from the data
      -- decl) patches it via 'patchTypeName' before reporting.
      [ RFErr (UNRECOGNIZED_FIELD_WRAPPER ""
                  (occNameFromFieldOcc (unLoc ln))
                  (stripParens (sdocText (unLoc lty))))
      | ln <- lnames
      ]

isMaybeTy :: HsType GhcPs -> Bool
isMaybeTy = \case
  HsAppTy _ (L _ h) _ -> tyConHead h == Just "Maybe"
  HsParTy _ (L _ a)   -> isMaybeTy a
  _                   -> False

occNameFromFieldOcc :: FieldOcc GhcPs -> String
occNameFromFieldOcc (FieldOcc _ (L _ n)) = occToString n

-- | Strip @Columnar f@ / @C f@ (with optional qualifier) from the front of
-- a field type.  Returns 'Nothing' if the wrapper isn't there.
unwrapColumnar :: String -> HsType GhcPs -> Maybe (HsType GhcPs)
unwrapColumnar fVarName outer = case outer of
  HsAppTy _ (L _ inner) (L _ rhs)
    | Just (ctor, lhs) <- splitApp inner
    , ctor `elem` ["Columnar", "C"]
    , isTyVarName fVarName lhs -> Just rhs
  HsParTy _ (L _ t) -> unwrapColumnar fVarName t
  _ -> Nothing
  where
    splitApp :: HsType GhcPs -> Maybe (String, HsType GhcPs)
    splitApp (HsAppTy _ (L _ a) (L _ b)) = do
      n <- tyConHead a
      Just (n, b)
    splitApp _ = Nothing

    isTyVarName name = \case
      HsTyVar _ _ (L _ rn) -> occToString rn == name
      HsParTy _ (L _ t)    -> isTyVarName name t
      _                    -> False

hasBeamableDeriving :: HsDataDefn GhcPs -> Bool
hasBeamableDeriving = derivesAny ["Beamable"]

hasDatabaseDeriving :: HsDataDefn GhcPs -> Bool
hasDatabaseDeriving = derivesAny ["Database"]

derivesAny :: [String] -> HsDataDefn GhcPs -> Bool
derivesAny names defn = any clauseHas (dd_derivs defn)
  where
    clauseHas (L _ HsDerivingClause{ deriv_clause_tys = L _ tys }) =
      any (mentioned . sig_body . unLoc) (derivingTypes tys)

    derivingTypes = \case
      DctSingle _ ty -> [ty]
      DctMulti _ tys -> tys
      XDerivClauseTys _ -> []

    mentioned (L _ ty) = case tyConHead ty of
      Just n -> n `elem` names
      _      -> False


-- ---------------------------------------------------------------------------
-- DB-record extraction (for setEntityName overrides)
-- ---------------------------------------------------------------------------

-- | Read a DB-record data decl of the form
--   @data Db f = Db { x :: f (TableEntity XT), y :: f (TableEntity YT) } deriving (.., Database)@
-- and return the @fieldName -> unqualified TableT name@ map.  Fields whose
-- type doesn't fit the @f (TableEntity SomeT)@ pattern are skipped silently
-- (they aren't tables — they might be auxiliary).
extractDbRecord :: LHsQTyVars GhcPs -> HsDataDefn GhcPs -> Maybe (Map.Map String String)
extractDbRecord _ defn =
  case dd_cons defn of
    [L _ (ConDeclH98{ con_args = RecCon (L _ recs) })] -> Just $
      Map.fromList
        [ (occNameFromFieldOcc (unLoc ln), tt)
        | L _ (ConDeclField{ cd_fld_names = lnames, cd_fld_type = L _ ty }) <- recs
        , ln <- lnames
        , Just tt <- [tableEntityOf ty]
        ]
    _ -> Nothing

-- | Strip @f (TableEntity SomeT)@ → @"SomeT"@.  The outer @f@ application
-- can be any single-arg type application (we don't check that it matches
-- the row type variable, because some DB records use different conventions).
tableEntityOf :: HsType GhcPs -> Maybe String
tableEntityOf = \case
  HsAppTy _ _ (L _ inner) -> tableEntityCore inner
  HsParTy _ (L _ t)       -> tableEntityOf t
  _                       -> Nothing
  where
    tableEntityCore = \case
      HsAppTy _ (L _ h) (L _ rhs)
        | tyConHead h == Just "TableEntity" -> tyConHead rhs
      HsParTy _ (L _ t) -> tableEntityCore t
      _                 -> Nothing


-- ---------------------------------------------------------------------------
-- DB-entity overrides extraction (setEntityName from withDbModification)
-- ---------------------------------------------------------------------------

-- | Walk every top-level binding's RHS, collect every
-- @dbModification { f = setEntityName "lit", … }@ found anywhere in the
-- expression, and pair each field with the unqualified table type drawn
-- from any DB-record decl in the same module.
extractDbOverrides :: String -> FilePath -> RawModule -> [DbEntityOverride]
extractDbOverrides modName srcPath RawModule{ rmBindings, rmDbRecords } =
  let fieldToType = Map.unions (Map.elems rmDbRecords)
        -- If two DB records share a field name (very unusual), the union
        -- prefers the first.  The merge CLI will catch any ambiguity by
        -- type lookup; this is a best-effort field→type map.
      raw = concatMap (collectDbModUpdates . unLoc) (Map.elems rmBindings)
  in  [ DbEntityOverride
          { dbField              = f
          , dbTableType          = tt
          , sqlName              = sql
          , overrideSourceModule = modName
          , overrideSourceFile   = srcPath
          }
      | (f, sql) <- raw
      , Just tt  <- [Map.lookup f fieldToType]
      ]

-- | Find every @dbModification { … }@ record-update anywhere in an
-- expression tree.  Returns the union of all
-- @(fieldName, setEntityName literal)@ pairs found in any such update.
collectDbModUpdates :: HsExpr GhcPs -> [(String, String)]
collectDbModUpdates = go
  where
    go expr = case stripPar expr of
      RecordUpd _ (L _ base) flds
        | varName base == Just "dbModification" -> extractUpdates flds
        | otherwise                              -> []
      OpApp _ (L _ a) _ (L _ b) -> go a <> go b
      HsApp _ (L _ f) (L _ x)   -> go f <> go x
      HsLam _ MG{ mg_alts = L _ matches }
                                -> concatMap (matchBody . unLoc) matches
      HsLet _ _ (L _ e)         -> go e
      ExprWithTySig _ (L _ e) _ -> go e
      HsPar _ (L _ e)           -> go e
      _                         -> []

    matchBody Match{ m_grhss = grhss } = case grhssExpr grhss of
      Just (L _ e) -> go e
      Nothing      -> []

    extractUpdates = \case
      Left rfs -> [ (occToString (rdrNameAmbiguousFieldOcc lbl), lit)
                  | L _ HsRecField{ hsRecFieldLbl = L _ lbl
                                  , hsRecFieldArg = L _ rhs
                                  , hsRecPun = False } <- rfs
                  , Just lit <- [setEntityNameLit rhs]
                  ]
      Right _  -> []  -- punned/rebindable updates; not used in this codebase

setEntityNameLit :: HsExpr GhcPs -> Maybe String
setEntityNameLit = \case
  HsApp _ (L _ f) (L _ arg)
    | varName f == Just "setEntityName"
    , Just s <- stringLiteral arg -> Just s
  HsPar _ (L _ e) -> setEntityNameLit e
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- ModelMeta / Table instance bodies
-- ---------------------------------------------------------------------------

extractModelMeta :: LHsBinds GhcPs -> RawModelMeta
extractModelMeta binds = foldl' step empty (bagToList binds)
  where
    empty = RawModelMeta Nothing Nothing Nothing
    step acc (L _ (FunBind{ fun_id = L _ nm, fun_matches = mg }))
      | Just rhs <- singleRhs mg =
          case occToString nm of
            "modelTableName"         ->
              acc { rmmTableNameLit = stringLiteral (unLoc rhs) }
            "modelFieldModification" ->
              acc { rmmModifierName = varName (unLoc rhs) }
            "modelTableType"         ->
              acc { rmmTableType = justConstructorOf (unLoc rhs) }
            _                        -> acc
    step acc _ = acc

extractTableInst :: LHsBinds GhcPs -> RawTableInst
extractTableInst binds = foldl' step (RawTableInst [] Nothing) (bagToList binds)
  where
    step acc (L _ (FunBind{ fun_id = L _ nm, fun_matches = mg }))
      | occToString nm == "primaryKey"
      , Just (pats, body) <- singleEquation mg =
          acc { rtiPkPats = pats, rtiPkBody = Just body }
    step acc _ = acc

singleEquation
  :: MatchGroup GhcPs (LHsExpr GhcPs)
  -> Maybe ([Pat GhcPs], HsExpr GhcPs)
singleEquation MG{ mg_alts = L _ [L _ (Match _ _ pats grhss)] } = do
  body <- grhssExpr grhss
  Just (map unLoc pats, unLoc body)
singleEquation _ = Nothing


-- ---------------------------------------------------------------------------
-- Per-table assembly
-- ---------------------------------------------------------------------------

assemble
  :: CliOptions
  -> String          -- module name
  -> RawModule
  -> String          -- type name e.g. "WidgetT"
  -> RawTable
  -> (String, Either [SqlSchemaError] TableSchema)
assemble cli modName rm typeName rt =
  let metaM      = Map.lookup typeName (rmModelMetas rm)
      tabM       = Map.lookup typeName (rmTableInsts rm)
      fieldErrs  = [ patchTypeName typeName e | e <- rtFieldErrors rt ]
      result = do
        when (not (null fieldErrs) && not (allowUnknownColumnWrapper cli))
             (Left fieldErrs)
        nameLit <- case metaM >>= rmmTableNameLit of
          Just s  -> Right s
          Nothing -> case Map.lookup typeName (tableNameOverrides cli) of
            Just s  -> Right s
            Nothing -> Left [UNRESOLVABLE_TABLE_NAME typeName]
        overrides <- extractOverrides cli typeName metaM (rmBindings rm)
        pkInfo <- case tabM of
          Just RawTableInst{ rtiPkPats = pats, rtiPkBody = Just body } ->
            extractPrimaryKey typeName pats body
          _ -> Left [UNRESOLVABLE_PRIMARY_KEY typeName]
        let cols = [ ColumnInfo
                       { hsField      = rfName f
                       , column       = Map.findWithDefault (rfName f)
                                          (rfName f) overrides
                       , hsType       = rfInner f
                       , nullable     = rfNullable f
                       , isPrimaryKey = rfName f `elem` pkColumns pkInfo
                       }
                   | f <- rtFields rt ]
        Right TableSchema
          { haskellType    = modName <> "." <> typeName
          , sourceModule   = modName
          , tableName      = nameLit
          , modelTableType = metaM >>= rmmTableType
          , primaryKey     = pkInfo
          , columns        = cols
          }
  in (typeName, result)

-- | Patch error variants that didn't have the type name at
-- field-extraction time.
patchTypeName :: String -> SqlSchemaError -> SqlSchemaError
patchTypeName t (UNRECOGNIZED_FIELD_WRAPPER "" f ty) =
  UNRECOGNIZED_FIELD_WRAPPER t f ty
patchTypeName _ e = e


extractOverrides
  :: CliOptions
  -> String
  -> Maybe RawModelMeta
  -> Map.Map String (LHsExpr GhcPs)
  -> Either [SqlSchemaError] (Map.Map String String)
extractOverrides cli typeName metaM bindings =
  case metaM >>= rmmModifierName of
    Nothing  -> Right Map.empty
    Just nm  -> case Map.lookup nm bindings of
      Nothing -> Right Map.empty
      Just e  -> extractColumnOverrides cli typeName nm (unLoc e)


-- | Parse @tableModification { f1 = fieldNamed "c1", … }@.  Any other
-- shape is rejected unless 'allowNonStandardModifier' is set, because
-- silently treating "I don't recognise this binding" as "no overrides"
-- would emit Haskell field names as DB column names — a wrong YAML with
-- no signal.
extractColumnOverrides
  :: CliOptions
  -> String
  -> String                          -- name of the *TMod binding
  -> HsExpr GhcPs
  -> Either [SqlSchemaError] (Map.Map String String)
extractColumnOverrides cli typeName bindingName expr = case stripPar expr of
  HsVar _ (L _ n)
    | occToString n == "tableModification" -> Right Map.empty
  RecordUpd _ (L _ base) flds
    | varName base == Just "tableModification" ->
        let upds = case flds of
              Left rs -> rs
              Right _ -> []
            (errs, ok) = foldl' classify ([], Map.empty) upds
        in if null errs then Right ok else Left errs
  _ ->
    if allowNonStandardModifier cli
      then Right Map.empty
      else Left [UNRECOGNIZED_MODIFIER_SHAPE typeName bindingName]
  where
    classify (es, acc) (L _ HsRecField{ hsRecFieldLbl = L _ lbl
                                      , hsRecFieldArg = L _ rhs
                                      , hsRecPun = False }) =
      let fieldName = occToString (rdrNameAmbiguousFieldOcc lbl)
      in case fieldNamedLiteral rhs of
           Just c  -> (es, Map.insert fieldName c acc)
           Nothing -> (es <> [UNRESOLVABLE_COLUMN_NAME typeName fieldName], acc)
    classify acc _ = acc


fieldNamedLiteral :: HsExpr GhcPs -> Maybe String
fieldNamedLiteral = \case
  HsApp _ (L _ f) (L _ arg)
    | varName f == Just "fieldNamed"
    , Just lit <- stringLiteral arg -> Just lit
  HsPar _ (L _ inner) -> fieldNamedLiteral inner
  _ -> Nothing


-- | Parse the @primaryKey@ definition.  Accepts:
--
--   * @primaryKey x = Con (f1 x) (f2 x) …@   (function equation, any arity)
--   * @primaryKey   = \\x -> Con (f1 x) …@   (explicit lambda)
--   * @primaryKey   = Con . f@               (operator composition)
--   * @primaryKey   = Con . (.field)@        (composition with a record-dot
--                                             section)
--   * @primaryKey x = Con x.f1 x.f2@         (native @OverloadedRecordDot@)
--   * @primaryKey   = Con \<$> f1 \<*> f2 …@   (applicative composite)
--   * @primaryKey _ = ConNullary@            (no-column PK)
extractPrimaryKey
  :: String -> [Pat GhcPs] -> HsExpr GhcPs
  -> Either [SqlSchemaError] PrimaryKeyInfo
extractPrimaryKey typeName pats body =
  case (pats, stripPar body) of
    ([p], rhs)
      | Just xName <- patVarName p ->
          case extractCall xName rhs of
            Just info -> Right info
            Nothing   -> case extractDotChainNoRdpPk xName rhs of
              Just info -> Right info
              Nothing   -> Left [UNRESOLVABLE_PRIMARY_KEY typeName]
    ([], HsLam _ MG{ mg_alts = L _ [L _ (Match _ _ lpats grhss)] })
      | [L _ p] <- lpats
      , Just xName <- patVarName p
      , Just lrhs  <- grhssExpr grhss ->
          case extractCall xName (unLoc lrhs) of
            Just info -> Right info
            Nothing   -> case extractDotChainNoRdpPk xName (unLoc lrhs) of
              Just info -> Right info
              Nothing   -> Left [UNRESOLVABLE_PRIMARY_KEY typeName]
    -- "Con . field" — single-column eta-reduced form.
    ([], OpApp _ (L _ (HsVar _ (L _ conN))) (L _ op) (L _ (HsVar _ (L _ fldN))))
      | isDot op ->
          Right PrimaryKeyInfo
            { pkConstructor = occToString conN
            , pkColumns     = [occToString fldN]
            }
    -- "Con . (.field)" — composition with a record-dot section.
    ([], OpApp _ (L _ (HsVar _ (L _ conN))) (L _ op) (L _ rhs))
      | isDot op
      , Just f <- projectionFieldName rhs ->
          Right PrimaryKeyInfo
            { pkConstructor = occToString conN
            , pkColumns     = [f]
            }
    ([], appBody)
      | Just info <- extractApplicative appBody -> Right info
    _ -> Left [UNRESOLVABLE_PRIMARY_KEY typeName]
  where

    extractCall xName expr = case unwindApp expr of
      (HsVar _ (L _ conN), args) -> do
        cols <- mapM (extractAccessor xName) args
        pure PrimaryKeyInfo
          { pkConstructor = occToString conN
          , pkColumns     = cols
          }
      _ -> Nothing

    extractAccessor xName arg = case stripPar arg of
      -- Plain "fieldName x".
      HsApp _ (L _ f) (L _ x)
        | Just fname <- varName f
        , varName x == Just xName -> Just fname
      -- RDP-style "getField @\"fieldName\" x".
      HsApp _ (L _ inner) (L _ x)
        | varName x == Just xName
        , Just fname <- getFieldTyApp inner -> Just fname
      -- Native OverloadedRecordDot "x.fieldName".
      HsGetField _ (L _ x) (L _ lbl)
        | varName x == Just xName -> Just (fieldLabelString lbl)
      _ -> Nothing

    getFieldTyApp = \case
      HsAppType _ (L _ f) HsWC{ hswc_body = L _ ty }
        | varName f == Just "getField"
        , HsTyLit _ (HsStrTy _ fs) <- ty -> Just (unpackFS fs)
      HsPar _ (L _ e) -> getFieldTyApp e
      _ -> Nothing

    patVarName p = case stripParPat p of
      VarPat _ (L _ n) -> Just (occToString n)
      WildPat _        -> Just "_"
      _                -> Nothing

    isDot e = case e of
      HsVar _ (L _ n) -> occToString n == "."
      _               -> False

    -- | Recognise @(.field)@.  Without @OverloadedRecordDot@ enabled
    -- (the common case in this codebase), this is parsed as
    -- @SectionR (.) field@ — a right section of the composition
    -- operator.  RDP rewrites it semantically later, but at the
    -- parsedResultAction stage we still see the section.  We also
    -- accept the native @HsProjection@ form for files that do enable
    -- the extension.
    projectionFieldName e = case stripPar e of
      HsProjection _ (L _ lbl :| []) -> Just (fieldLabelString lbl)
      SectionR _ (L _ op) (L _ (HsVar _ (L _ n)))
        | isDotOp op -> Just (occToString n)
      _ -> Nothing

    isDotOp e = case e of
      HsVar _ (L _ n) -> occToString n == "."
      _               -> False

fieldLabelString :: HsFieldLabel GhcPs -> String
fieldLabelString HsFieldLabel{ hflLabel = L _ fs } = unpackFS fs

-- | When the source uses @primaryKey x = Con x.f1 x.f2 … x.fN@ but the
-- module does NOT enable @OverloadedRecordDot@, GHC's parser interprets
-- the dots as composition operators rather than as record selection.
-- The resulting AST is an @OpApp@ chain
-- @Con x . (f1 x . (f2 x . … . fN))@ where each @fK x@ is an HsApp and
-- the final element is a bare @HsVar@.  RDP rewrites this later at the
-- typechecker stage, so the source still compiles — but at our
-- parsedResultAction the raw form is what we see.
--
-- This function recognises that exact shape and recovers the column
-- list.  Only fires when:
--   * the function arg is a single 'VarPat' bound to @xName@; and
--   * the head of the dot chain is exactly @Con xName@; and
--   * every middle element is @fieldK xName@; and
--   * the tail element is a bare @HsVar fieldN@.
-- Any deviation returns 'Nothing' — we'd rather false-fail (loud) than
-- guess a column list.
extractDotChainNoRdpPk :: String -> HsExpr GhcPs -> Maybe PrimaryKeyInfo
extractDotChainNoRdpPk xName body = case opAppDotChain body of
  Just (first : rest@(_ : _)) -> do
    (conN, vN) <- splitHeadConArg first
    if vN /= xName then Nothing else do
      cols <- traverseSplit rest
      Just PrimaryKeyInfo { pkConstructor = conN, pkColumns = cols }
  _ -> Nothing
  where
    -- Flatten an OpApp tree of @.@ operators into a left-to-right list of
    -- operands, regardless of whether the tree is left- or
    -- right-associative.  We don't trust GHC's `.` associativity in this
    -- context because the codebase parses @x.fieldA x.fieldB@ in ways
    -- that don't match Prelude's @infixr 9@.
    opAppDotChain :: HsExpr GhcPs -> Maybe [HsExpr GhcPs]
    opAppDotChain e = case stripPar e of
      OpApp _ (L _ lhs) (L _ op) (L _ rhs)
        | isDotV op -> Just (chainSide lhs <> chainSide rhs)
      _ -> Nothing

    chainSide e = case opAppDotChain e of
      Just xs -> xs
      Nothing -> [e]

    isDotV = \case
      HsVar _ (L _ n) -> occToString n == "."
      _               -> False

    splitHeadConArg :: HsExpr GhcPs -> Maybe (String, String)
    splitHeadConArg e = case stripPar e of
      HsApp _ (L _ con) (L _ x) -> do
        c <- varName con
        v <- varName x
        Just (c, v)
      _ -> Nothing

    -- Every middle element must be @field xName@; the final element must
    -- be a bare @HsVar field@.
    traverseSplit :: [HsExpr GhcPs] -> Maybe [String]
    traverseSplit [] = Just []
    traverseSplit [final] = do
      f <- varName (stripPar final)
      Just [f]
    traverseSplit (mid : rest) = do
      f <- case stripPar mid of
        HsApp _ (L _ fE) (L _ x) -> do
          n <- varName fE
          v <- varName x
          if v == xName then Just n else Nothing
        _ -> Nothing
      rs <- traverseSplit rest
      Just (f : rs)


-- | Peel @Con <$> a1 <*> a2 <*> … <*> an@ into @(Con, [a1, …, an])@,
-- where each @ai@ must be a plain field accessor (HsVar).  Used for the
-- applicative composite-PK form, e.g.
-- @primaryKey = Composite <$> partA <*> partB@.
extractApplicative :: HsExpr GhcPs -> Maybe PrimaryKeyInfo
extractApplicative expr = case stripPar expr of
  OpApp _ _ _ _ -> do
    (conE, accessors) <- peel expr
    conN  <- varName conE
    cols  <- mapM varName accessors
    Just PrimaryKeyInfo { pkConstructor = conN, pkColumns = cols }
  _ -> Nothing
  where
    peel :: HsExpr GhcPs -> Maybe (HsExpr GhcPs, [HsExpr GhcPs])
    peel e = case stripPar e of
      OpApp _ (L _ lhs) (L _ op) (L _ rhs)
        | isAp op -> do
            (con, acc) <- peel lhs
            Just (con, acc <> [rhs])
        | isFmap op -> Just (lhs, [rhs])
      _ -> Nothing

    isAp = \case
      HsVar _ (L _ n) -> occToString n == "<*>"
      _               -> False
    isFmap = \case
      HsVar _ (L _ n) -> occToString n == "<$>"
      _               -> False


unwindApp :: HsExpr GhcPs -> (HsExpr GhcPs, [HsExpr GhcPs])
unwindApp = go []
  where
    go acc (HsApp _ f x) = go (unLoc x : acc) (unLoc f)
    go acc e             = (e, acc)


-- ---------------------------------------------------------------------------
-- Compile-error emission
-- ---------------------------------------------------------------------------

throwSqlSchemaErrors :: FilePath -> SrcSpan -> [SqlSchemaError] -> Hsc ()
throwSqlSchemaErrors fragDir modSpan errs =
  ParseError.throwErrors $ listToBag
    [ ParseError.mkErr modSpan reallyAlwaysQualify
        (ParseError.mkDecorated
           [docToSDoc (Pretty.text (generateErrorMessage fragDir e))])
    | e <- nub errs ]


-- ---------------------------------------------------------------------------
-- Small AST helpers
-- ---------------------------------------------------------------------------

sdocText :: Outputable a => a -> String
sdocText = showSDocUnsafe . ppr

occToString :: RdrName -> String
occToString = sdocText . rdrNameOcc

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

stripParens :: String -> String
stripParens s = case T.strip (T.pack s) of
  t | T.length t >= 2
    , T.head t == '('
    , T.last t == ')' -> T.unpack (T.drop 1 (T.dropEnd 1 t))
    | otherwise       -> T.unpack t

singleRhs :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs)
singleRhs MG{ mg_alts = L _ [L _ (Match _ _ _ grhss)] } = grhssExpr grhss
singleRhs _ = Nothing

grhssExpr :: GRHSs GhcPs (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs)
grhssExpr GRHSs{ grhssGRHSs = [L _ (GRHS _ [] body)] } = Just body
grhssExpr _ = Nothing

stripPar :: HsExpr GhcPs -> HsExpr GhcPs
stripPar (HsPar _ (L _ e)) = stripPar e
stripPar e = e

stripParPat :: Pat GhcPs -> Pat GhcPs
stripParPat (ParPat _ (L _ p)) = stripParPat p
stripParPat p = p

stringLiteral :: HsExpr GhcPs -> Maybe String
stringLiteral = \case
  HsLit _ (HsString _ fs)     -> Just (unpackFS fs)
  HsLit _ (HsStringPrim _ bs) -> Just (T.unpack (T.pack (show bs)))
  HsPar _ (L _ e)             -> stringLiteral e
  ExprWithTySig _ (L _ e) _   -> stringLiteral e
  _                           -> Nothing

-- | Extract a syntactic identifier name, stripping parens and
-- expression-level type signatures.  The ExprWithTySig case is what
-- lets us recognise idioms like
-- @(B.tableModification :: T) { f = … }@ — without it the RecordUpd
-- base reads as Nothing and the modifier is rejected.
varName :: HsExpr GhcPs -> Maybe String
varName = \case
  HsVar _ (L _ n)           -> Just (occToString n)
  HsPar _ (L _ e)           -> varName e
  ExprWithTySig _ (L _ e) _ -> varName e
  _                         -> Nothing

conName :: HsExpr GhcPs -> Maybe String
conName = varName

justConstructorOf :: HsExpr GhcPs -> Maybe String
justConstructorOf = \case
  HsApp _ (L _ j) (L _ x)
    | varName j == Just "Just" -> conName x
  HsPar _ (L _ e) -> justConstructorOf e
  _ -> Nothing
