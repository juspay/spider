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

-- | Pure extraction: the fragment a parsed module contributes to the SQL
-- schema contract.  Internal; no API stability.  It is exposed so the
-- test-suite can check every extractor rule without running GHC.
module SqlSchema.Extract
  ( FragmentAction (..)
  , planFragment
  ) where

import           Control.Monad              (when)
import           Data.Foldable              (toList)
import           Data.List                  (foldl', isPrefixOf, nub)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T

-- GHC API (>= 9.0)
import qualified GHC
import           GHC.Data.Bag               (bagToList)
import           GHC.Data.FastString        (unpackFS)
#if __GLASGOW_HASKELL__ >= 906
import           Language.Haskell.Syntax.Basic (field_label)
#endif
import           GHC.Hs
import           GHC.Types.Name.Reader      (RdrName (..), rdrNameOcc)
import           GHC.Types.SrcLoc           (GenLocated (..), SrcSpan, unLoc)
import           GHC.Types.SourceFile       (HscSource, isHsBootOrSig)
import           GHC.Utils.Outputable       (Outputable, ppr, showSDocUnsafe)

import           SqlSchema.Types


-- ---------------------------------------------------------------------------
-- Fragment planning
-- ---------------------------------------------------------------------------

-- | What becomes of a module's fragment file.
data FragmentAction
  = LeaveUntouched
    -- ^ A boot or signature file: the fragment is the real module's.
  | Stage (Maybe Fragment)
    -- ^ The module's fragment; 'Nothing' deletes an earlier one.
  deriving (Eq, Show)

-- | The arguments are the plugin options, @ms_hsc_src@, the module name, the
-- source path and the parsed declarations.  Errors come de-duplicated, in
-- first-occurrence order.
planFragment
  :: CliOptions -> HscSource -> String -> FilePath -> [LHsDecl GhcPs]
  -> Either [SqlSchemaError] FragmentAction
planFragment cli src modStr srcPath decls =
  -- A boot or signature file shares its module's name and fragment path but
  -- defines no tables; it must not touch the real module's fragment.
  if isHsBootOrSig src then Right LeaveUntouched
  else if any (`isPrefixOf` modStr) (blacklistModules cli)
    then Right (Stage Nothing)
    else do
      let rm           = foldl' visitDecl emptyRawModule decls
          rows         = extractFromModule cli modStr rm
          errs         = concat [es | (_, Left es) <- rows]
          okTables     = [ts | (_, Right ts) <- rows]
          dbOverrides  = extractDbOverrides modStr srcPath rm

      when (not (null errs))
        (Left (nub errs))

      Right $ Stage $
        if null okTables && null dbOverrides
          then Nothing
          else Just Fragment
            { fragmentModule            = modStr
            , fragmentTables            = okTables
            , fragmentDbEntityOverrides = dbOverrides
            }


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
  { rmmTableNameLit  :: Maybe String
  , rmmModifier      :: ModifierDef
    -- ^ Whether the instance defines @modelFieldModification@, and how.
  , rmmTableType     :: Maybe String
  , rmmSchemaName    :: Maybe ModelSchemaName
  }

data ModifierDef
  = ModifierAbsent
  | ModifierRhs (LHsExpr GhcPs)
    -- ^ A single unguarded right-hand side.
  | ModifierUnreadable
    -- ^ Defined, but not by a single unguarded right-hand side (e.g. guarded).

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
  -- A data decl may be a Beam TABLE (single-ctor record + type var, deriving
  -- Beamable) OR a Beam DB-record (same shape, deriving Database, e.g.
  -- `data AppDb f = … deriving Database`).  Both fit the table-record shape
  -- that extractRawTable matches, so the DB-record case (deriving Database)
  -- MUST be checked FIRST — otherwise extractRawTable claims the DB-record as a
  -- (non-Beamable, ultimately dropped) table candidate, rmDbRecords stays empty,
  -- and the setEntityName overrides in its withDbModification are never extracted.
  TyClD _ (DataDecl { tcdLName = L _ lname
                    , tcdTyVars = tyVars
                    , tcdDataDefn = defn })
    | hasDatabaseDeriving defn
    , Just dbMap <- extractDbRecord tyVars defn ->
        rm { rmDbRecords = Map.insert (occToString lname) dbMap (rmDbRecords rm) }
    | Just tbl <- extractRawTable tyVars defn ->
        let tyName = occToString lname
            rm'   = rm { rmTables = rmTables rm <> [(GHC.locA l, tyName, tbl)] }
        in  if hasBeamableDeriving defn
              then rm' { rmBeamableTypes = Set.insert tyName (rmBeamableTypes rm') }
              else rm'
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
  -- toList: dd_cons is a list before GHC 9.6 and a Foldable DataDefnCons after.
  case toList (dd_cons defn) of
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
  case toList (dd_cons defn) of
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
      Let (L _ e)               -> go e
      ExprWithTySig _ (L _ e) _ -> go e
      Par (L _ e)               -> go e
      _                         -> []

    matchBody Match{ m_grhss = grhss } = case grhssExpr grhss of
      Just (L _ e) -> go e
      Nothing      -> []

    extractUpdates flds =
      [ (ambiguousFieldName lbl, lit)
      | L _ (RecField (L _ lbl) (L _ rhs) False) <- regularRecUpdFields flds
      , Just lit <- [setEntityNameLit rhs]
      ]

setEntityNameLit :: HsExpr GhcPs -> Maybe String
setEntityNameLit = \case
  HsApp _ (L _ f) (L _ arg)
    | varName f == Just "setEntityName"
    , Just s <- stringLiteral arg -> Just s
  Par (L _ e) -> setEntityNameLit e
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- ModelMeta / Table instance bodies
-- ---------------------------------------------------------------------------

extractModelMeta :: LHsBinds GhcPs -> RawModelMeta
extractModelMeta binds = foldl' step empty (bagToList binds)
  where
    empty = RawModelMeta Nothing ModifierAbsent Nothing Nothing
    step acc (L _ (FunBind{ fun_id = L _ nm, fun_matches = mg }))
      | Just rhs <- singleRhs mg =
          case occToString nm of
            "modelTableName"         ->
              acc { rmmTableNameLit = stringLiteral (unLoc rhs) }
            "modelFieldModification" ->
              acc { rmmModifier = ModifierRhs rhs }
            "modelTableType"         ->
              acc { rmmTableType = justConstructorOf (unLoc rhs) }
            "modelSchemaName"        ->
              acc { rmmSchemaName = extractModelSchemaName (unLoc rhs) }
            _                        -> acc
      | occToString nm == "modelFieldModification" =
          acc { rmmModifier = ModifierUnreadable }
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
                       , column       = Map.findWithDefault (beamDefaultColumnName (rfName f))
                                          (rfName f) overrides
                       , hsType       = rfInner f
                       , nullable     = rfNullable f
                       , isPrimaryKey = rfName f `elem` pkColumns pkInfo
                       }
                   | f <- rtFields rt ]
        Right TableSchema
          { codeName        = typeName
          , sourceModule    = modName
          , modelTableName  = nameLit
          , modelTableType  = metaM >>= rmmTableType
          , modelSchemaName = metaM >>= rmmSchemaName
          , primaryKey      = pkInfo
          , columns         = cols
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
  case maybe ModifierAbsent rmmModifier metaM of
    ModifierAbsent -> Right Map.empty
    ModifierUnreadable ->
      orNoOverrides (UNRECOGNIZED_MODIFIER_SHAPE typeName "modelFieldModification")
    ModifierRhs rhs -> case varRdrName (unLoc rhs) of
      Just rdr | occToString rdr /= "tableModification" -> case rdr of
        -- Qualified means non-local, even when it names this module's own binding.
        Unqual _ | Just e <- Map.lookup (occToString rdr) bindings ->
          extractColumnOverrides cli typeName (occToString rdr) (unLoc e)
        _ -> orNoOverrides (UNRESOLVABLE_MODIFIER_BINDING typeName (sdocText rdr))
      _ -> extractColumnOverrides cli typeName "modelFieldModification" (unLoc rhs)
  where
    orNoOverrides err
      | allowNonStandardModifier cli = Right Map.empty
      | otherwise                    = Left [err]


-- | Parse @tableModification { f1 = fieldNamed "c1", … }@.  Any other
-- shape is rejected unless 'allowNonStandardModifier' is set, because
-- silently treating "I don't recognise this binding" as "no overrides"
-- would emit Beam's default names for overridden columns — a wrong YAML with
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
        let upds = regularRecUpdFields flds
            (errs, ok) = foldl' classify ([], Map.empty) upds
        in if null errs then Right ok else Left errs
  _ ->
    if allowNonStandardModifier cli
      then Right Map.empty
      else Left [UNRECOGNIZED_MODIFIER_SHAPE typeName bindingName]
  where
    classify (es, acc) (L _ (RecField (L _ lbl) (L _ rhs) False)) =
      let fieldName = ambiguousFieldName lbl
      in case fieldNamedLiteral rhs of
           Just c  -> (es, Map.insert fieldName c acc)
           Nothing -> (es <> [UNRESOLVABLE_COLUMN_NAME typeName fieldName], acc)
    classify acc _ = acc


fieldNamedLiteral :: HsExpr GhcPs -> Maybe String
fieldNamedLiteral = \case
  HsApp _ (L _ f) (L _ arg)
    | varName f == Just "fieldNamed"
    , Just lit <- stringLiteral arg -> Just lit
  Par (L _ inner) -> fieldNamedLiteral inner
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
      AppType (L _ f) HsWC{ hswc_body = L _ ty }
        | varName f == Just "getField"
        , HsTyLit _ (HsStrTy _ fs) <- ty -> Just (unpackFS fs)
      Par (L _ e) -> getFieldTyApp e
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

#if __GLASGOW_HASKELL__ >= 906
fieldLabelString :: DotFieldOcc GhcPs -> String
fieldLabelString DotFieldOcc{ dfoLabel = L _ fl } = unpackFS (field_label fl)
#elif __GLASGOW_HASKELL__ >= 904
fieldLabelString :: DotFieldOcc GhcPs -> String
fieldLabelString DotFieldOcc{ dfoLabel = L _ fs } = unpackFS fs
#else
fieldLabelString :: HsFieldLabel GhcPs -> String
fieldLabelString HsFieldLabel{ hflLabel = L _ fs } = unpackFS fs
#endif

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
-- GHC version compatibility
-- ---------------------------------------------------------------------------

-- GHC 9.4 added token fields to HsPar/ParPat/HsLet/HsAppType and renamed
-- HsRecField to HsFieldBind; GHC 9.8 replaced the Either of record-update
-- fields with LHsRecUpdFields.
#if __GLASGOW_HASKELL__ >= 904
pattern Par :: LHsExpr GhcPs -> HsExpr GhcPs
pattern Par e <- HsPar _ _ e _

pattern ParP :: LPat GhcPs -> Pat GhcPs
pattern ParP p <- ParPat _ _ p _

pattern Let :: LHsExpr GhcPs -> HsExpr GhcPs
pattern Let e <- HsLet _ _ _ _ e

pattern AppType :: LHsExpr GhcPs -> LHsWcType GhcPs -> HsExpr GhcPs
pattern AppType f ty <- HsAppType _ f _ ty

pattern RecField :: lhs -> rhs -> Bool -> HsFieldBind lhs rhs
pattern RecField lbl rhs pun <- HsFieldBind{ hfbLHS = lbl, hfbRHS = rhs, hfbPun = pun }
#else
pattern Par :: LHsExpr GhcPs -> HsExpr GhcPs
pattern Par e <- HsPar _ e

pattern ParP :: LPat GhcPs -> Pat GhcPs
pattern ParP p <- ParPat _ p

pattern Let :: LHsExpr GhcPs -> HsExpr GhcPs
pattern Let e <- HsLet _ _ e

pattern AppType :: LHsExpr GhcPs -> LHsWcType GhcPs -> HsExpr GhcPs
pattern AppType f ty <- HsAppType _ f ty

pattern RecField :: GHC.Located id -> arg -> Bool -> HsRecField' id arg
pattern RecField lbl rhs pun <- HsRecField{ hsRecFieldLbl = lbl, hsRecFieldArg = rhs, hsRecPun = pun }
#endif

-- OverloadedRecordUpdate forms (r { a.b = x }) carry no plain field label and are skipped.
#if __GLASGOW_HASKELL__ >= 908
regularRecUpdFields :: LHsRecUpdFields GhcPs -> [LHsRecUpdField GhcPs GhcPs]
regularRecUpdFields = \case
  RegularRecUpdFields _ fs   -> fs
  OverloadedRecUpdFields _ _ -> []
#else
regularRecUpdFields
  :: Either [LHsRecUpdField GhcPs] [LHsRecUpdProj GhcPs] -> [LHsRecUpdField GhcPs]
regularRecUpdFields = either id (const [])
#endif


-- ---------------------------------------------------------------------------
-- Small AST helpers
-- ---------------------------------------------------------------------------

sdocText :: Outputable a => a -> String
sdocText = showSDocUnsafe . ppr

occToString :: RdrName -> String
occToString = sdocText . rdrNameOcc

-- | Matches the constructors directly: the RdrName accessor was renamed in GHC 9.8.
ambiguousFieldName :: AmbiguousFieldOcc GhcPs -> String
ambiguousFieldName = \case
  Unambiguous _ (L _ n) -> occToString n
  Ambiguous   _ (L _ n) -> occToString n

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
stripPar (Par (L _ e)) = stripPar e
stripPar e = e

stripParPat :: Pat GhcPs -> Pat GhcPs
stripParPat (ParP (L _ p)) = stripParPat p
stripParPat p = p

stringLiteral :: HsExpr GhcPs -> Maybe String
stringLiteral = \case
  HsLit _ (HsString _ fs)     -> Just (unpackFS fs)
  HsLit _ (HsStringPrim _ bs) -> Just (T.unpack (T.pack (show bs)))
  Par (L _ e)                 -> stringLiteral e
  ExprWithTySig _ (L _ e) _   -> stringLiteral e
  _                           -> Nothing

-- | Extract a syntactic identifier name, stripping parens and
-- expression-level type signatures.  The ExprWithTySig case is what
-- lets us recognise idioms like
-- @(B.tableModification :: T) { f = … }@ — without it the RecordUpd
-- base reads as Nothing and the modifier is rejected.
varName :: HsExpr GhcPs -> Maybe String
varName = fmap occToString . varRdrName

varRdrName :: HsExpr GhcPs -> Maybe RdrName
varRdrName = \case
  HsVar _ (L _ n)           -> Just n
  Par (L _ e)               -> varRdrName e
  ExprWithTySig _ (L _ e) _ -> varRdrName e
  _                         -> Nothing

conName :: HsExpr GhcPs -> Maybe String
conName = varName

justConstructorOf :: HsExpr GhcPs -> Maybe String
justConstructorOf = \case
  HsApp _ (L _ j) (L _ x)
    | varName j == Just "Just" -> conName x
  Par (L _ e) -> justConstructorOf e
  _ -> Nothing

-- | Classify the RHS of a @modelSchemaName = Just _@ binding.
--
-- Recognised inner forms:
--
--   * a string literal @"public"@           → 'SchemaLiteral'
--   * a (possibly qualified) value name
--     @Config.getAppDbSchema@               → 'SchemaConfig'
--
-- The qualifier on a value reference is preserved by pretty-printing the
-- 'RdrName' directly rather than going through 'rdrNameOcc' (which would
-- strip the module qualifier and lose the PG marker the validator looks
-- for).  Anything else (literals other than strings, function
-- applications, etc.) yields 'Nothing' — silently dropping unknown shapes
-- is fine here because the validator's fallback is "treat as MySQL",
-- which is the same behaviour as a missing @modelSchemaName@.
extractModelSchemaName :: HsExpr GhcPs -> Maybe ModelSchemaName
extractModelSchemaName = peel
  where
    peel = \case
      Par (L _ e)               -> peel e
      ExprWithTySig _ (L _ e) _ -> peel e
      HsApp _ (L _ j) (L _ x)
        | varName j == Just "Just" -> inner x
      _ -> Nothing

    inner expr = case expr of
      Par (L _ e)                 -> inner e
      ExprWithTySig _ (L _ e) _   -> inner e
      HsLit _ (HsString _ fs)     -> Just (SchemaLiteral (unpackFS fs))
      HsVar _ (L _ rn)            -> Just (SchemaConfig (sdocText rn))
      _                           -> Nothing
