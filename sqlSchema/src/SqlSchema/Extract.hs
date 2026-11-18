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
  , extensionErrors
  , refusedExtensions
    -- * Type-check cross-check
  , Origin (..)
  , fromModule
  , renderOrigin
  , ResolvedField (..)
  , ResolvedValue (..)
  , ResolvedColumn (..)
  , ResolvedGeneric (..)
  , InstanceHead (..)
  , unextractedInstances
  , unexpectedClasses
  , routingMismatches
  , columnMismatches
  , foreignLargeRecords
  , genericMismatches
  , untrustedInstances
  ) where

import           Control.Monad              (when)
import           Data.Data                  (Data, cast, gmapQ)
import           Data.Either                (partitionEithers)
import           Data.Foldable              (toList)
import           Data.List                  (foldl', nub)
import           Data.List                  (intercalate, isSubsequenceOf, (\\))
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust, isNothing, mapMaybe)
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
import           GHC.Types.Name.Occurrence  (isTcOcc, isTvOcc, isVarOcc, occNameString)
import           GHC.Types.Name.Reader      (RdrName (..), rdrNameOcc)
import           GHC.Types.SrcLoc           (GenLocated (..), SrcSpan, srcSpanStartCol, srcSpanStartLine,
                                             srcSpanToRealSrcSpan, unLoc)
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
  else if isBlacklistedModule cli modStr
    then Right (Stage Nothing)
    else do
      let seed         = emptyRawModule
            { rmTopBinders      = topBinders decls
            , rmDerivedGeneric  = derivedGeneric decls
            , rmLargeRecords    = largeRecords decls
            , rmBeamableNotGeneric = beamableNotGeneric decls
            , rmNonStockGeneric = nonStockGeneric decls }
          rm           = foldl' visitDecl seed decls
          rows         = extractFromModule cli modStr rm
          errs         = concat [es | (_, Left es) <- rows] <> moduleErrs <> entityNameErrs <> tableModErrs
          okTables     = [ts | (_, Right ts) <- rows]
          uses         = watchedUses decls
          (dbOverrides, entityNameErrs) = extractDbOverrides modStr srcPath decls uses rm
          tableModErrs = checkTableModifications decls uses rm
          moduleErrs   = if null rows && null dbOverrides then [] else moduleErrors decls

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

-- | The refusals for the module's extensions, which 'planFragment' does not see: given whether
-- an extension is on, and whether the module stages a fragment.
extensionErrors :: (String -> Bool) -> Bool -> [SqlSchemaError]
extensionErrors enabled staged = [UNSUPPORTED_EXTENSION ext | staged, ext <- refusedExtensions, enabled ext]

-- | Extensions that change what the names the plugin reads mean, in a way the parse does not show.
refusedExtensions :: [String]
refusedExtensions = ["RebindableSyntax", "OverlappingInstances", "IncoherentInstances"]

-- | A rewrite rule, or a record wildcard in a top-level pattern: the parse does not show their
-- effect on the names the plugin reads.
moduleErrors :: [LHsDecl GhcPs] -> [SqlSchemaError]
moduleErrors decls =
  [UNSUPPORTED_RULES (lineCol (GHC.locA l)) | L l RuleD{} <- decls]
  <> [ UNSUPPORTED_RECORD_WILDCARD (lineCol (GHC.locA l))
     | L l (ValD _ PatBind{ pat_lhs = lhs }) <- decls, hasRecordWildcard lhs ]

hasRecordWildcard :: Data a => a -> Bool
hasRecordWildcard d = case cast d :: Maybe (HsRecFields GhcPs (LPat GhcPs)) of
  Just HsRecFields{ rec_dotdot = Just _ } -> True
  _                                       -> or (gmapQ hasRecordWildcard d)


-- ---------------------------------------------------------------------------
-- Module-level extraction
-- ---------------------------------------------------------------------------

data RawModule = RawModule
  { rmTables          :: [(SrcSpan, String, RawTable)]
    -- ^ Candidate table-shaped data decls (single-constructor records
    --   with at least one type variable).  Filtered to actual Beam
    --   tables at extraction time via 'rmBeamableTypes'.
  , rmBeamableTypes   :: Set String
    -- ^ Types marked Beamable: by a deriving clause on their data declaration
    --   (any shape), by standalone deriving, or by an instance.
  , rmModelMetas      :: Map.Map String RawModelMeta
  , rmTableInsts      :: Map.Map String RawTableInst
  , rmBindings        :: Map.Map String (LHsExpr GhcPs)
    -- ^ top-level @name = expr@ value bindings (e.g. each table's
    --   @xxxTMod@, and any @appDb@-style DB-settings binding).
  , rmDbRecords       :: Map.Map String (Map.Map String (Set String))
    -- ^ DB-record data declarations (those that derive @Database@) keyed
    --   by the record type name (e.g. @\"AppDb\"@), with an inner map
    --   from record-field name (e.g. @\"gadgets\"@) to the unqualified
    --   table-type constructors inside @TableEntity@ (e.g. @\"GadgetT\"@).
  , rmDataDecls       :: Set String
    -- ^ The type name of every data declaration, whatever its shape.
  , rmUnreadableHeads :: [(String, String)]
    -- ^ @(class, rendered head type)@ of each Table, ModelMeta or Beamable
    --   instance whose type the plugin cannot attribute to a type of this
    --   module ('instanceType').
  , rmInstanceKeys    :: [(String, String)]
    -- ^ @(class, type name)@ of every Table, ModelMeta and Beamable instance
    --   recorded, written or derived; a key seen twice is 'DUPLICATE_INSTANCE'.
  , rmTopBinders      :: Map.Map String [TopBinder]
  , rmTypeDecls       :: Set String
    -- ^ The name of every type synonym and type or data family declared in this
    --   module, associated ones included; data types are in 'rmDataDecls'.
  , rmDerivedGeneric  :: Set String
    -- ^ Each type the module derives Generic for, at every argument.
  , rmLargeRecords    :: Set String
    -- ^ Each type annotated @{-# ANN type T largeRecord #-}@: the large-records plugin
    --   generates its Generic instance from the record.
  , rmBeamableNotGeneric :: [Maybe String]
    -- ^ The type of each Beamable instance whose methods need not come from Generic
    --   ('beamableNotGeneric'); 'Nothing' when it is not one unqualified name.
  , rmNonStockGeneric :: [Maybe String]
    -- ^ The head type constructor of each Generic instance not derived with the stock
    --   strategy: written, or derived anyclass, newtype or via; 'Nothing' when the head is
    --   not one unqualified name.
  }

emptyRawModule :: RawModule
emptyRawModule = RawModule [] Set.empty Map.empty Map.empty Map.empty Map.empty Set.empty [] [] Map.empty Set.empty Set.empty Set.empty [] []

data TopBinder
  = TopValue (HsBind GhcPs)
    -- ^ A value or pattern binding, or a pattern synonym.
  | TopField
    -- ^ A record field of a data declaration or a data instance.
  | TopOther
    -- ^ A class method, a foreign import or a record pattern synonym's field.

-- | Not seen: a name bound by a Template Haskell splice or by a record wildcard.
topBinders :: [LHsDecl GhcPs] -> Map.Map String [TopBinder]
topBinders decls = Map.fromListWith (flip (<>)) [(n, [b]) | L _ decl <- decls, (n, b) <- binders decl]
  where
    binders = \case
      ValD _ bind -> [(n, TopValue bind) | n <- boundNames bind]
                     <> [ (occNameFromFieldOcc (recordPatSynField f), TopOther)
                        | PatSynBind _ PSB{ psb_args = RecCon fs } <- [bind], f <- fs ]
      TyClD _ ClassDecl{ tcdSigs = sigs } -> [(occToString n, TopOther) | L _ (ClassOpSig _ False ns _) <- sigs, L _ n <- ns]
      TyClD _ DataDecl{ tcdDataDefn = defn } -> fields defn
      InstD _ (ClsInstD _ ClsInstDecl{ cid_datafam_insts = dfis }) -> fields dfis
      InstD _ (DataFamInstD _ dfi) -> fields dfi
      ForD _ ForeignImport{ fd_name = L _ n } -> [(occToString n, TopOther)]
      _ -> []
    fields :: Data a => a -> [(String, TopBinder)]
    fields = map (\n -> (n, TopField)) . recordFieldNames

recordFieldNames :: Data a => a -> [String]
recordFieldNames d = case cast d :: Maybe (ConDeclField GhcPs) of
  Just ConDeclField{ cd_fld_names = ns } -> [occNameFromFieldOcc (unLoc n) | n <- ns]
  Nothing                                -> concat (gmapQ recordFieldNames d)

-- | A punned record field binds its label, which 'collectHsBindBinders' misses:
-- the parser leaves a placeholder for the variable.
punnedPatternFields :: Data a => a -> [String]
punnedPatternFields d = case cast d :: Maybe PatRecField of
  Just (RecField (L _ lbl) _ True) -> occNameFromFieldOcc lbl : below
  _                                -> below
  where below = concat (gmapQ punnedPatternFields d)

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

-- | A 'Left' field is unreadable ('metaField'); 'assemble' reports it.
data RawModelMeta = RawModelMeta
  { rmmTableNameLit  :: Maybe String
  , rmmModifier      :: ModifierDef
    -- ^ Whether the instance defines @modelFieldModification@, and how.
  , rmmTableType     :: Either String (Maybe String)
  , rmmSchemaName    :: Either String (Maybe ModelSchemaName)
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
  <> [ (t, Left [UNEXTRACTED_TABLE t reason]) | (t, reason) <- unextractedTables rm ]
  <> [ (cls, Left [UNREADABLE_INSTANCE_HEAD cls ty]) | (cls, ty) <- rmUnreadableHeads rm ]
  <> [ (t, Left [DUPLICATE_INSTANCE cls t]) | (cls, t) <- duplicateInstances (rmInstanceKeys rm) ]

duplicateInstances :: [(String, String)] -> [(String, String)]
duplicateInstances keys =
  Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(k, 1) | k <- keys]))

-- | Each type marked as a table (Beamable, Table or ModelMeta) that the
-- comprehension above does not assemble, with the reason.
unextractedTables :: RawModule -> [(String, String)]
unextractedTables rm =
  [ (t, reason t)
  | t <- Set.toAscList marked
  , not (Set.member t tableShaped && Set.member t (rmBeamableTypes rm))
  ]
  where
    marked = Set.unions
      [rmBeamableTypes rm, Map.keysSet (rmTableInsts rm), Map.keysSet (rmModelMetas rm)]
    tableShaped = Set.fromList [t | (_, t, _) <- rmTables rm]
    reason t
      | Map.member t (rmDbRecords rm)       = reasonDatabaseRecord
      | not (Set.member t (rmDataDecls rm)) = reasonNoDeclaration
      | not (Set.member t tableShaped)      = reasonNotARecord
      | otherwise                           = reasonNoBeamable


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
        declared { rmDbRecords = Map.insert tyName dbMap (rmDbRecords rm) }
    | Just tbl <- extractRawTable tyVars defn ->
        declared { rmTables = rmTables rm <> [(GHC.locA l, tyName, tbl)] }
    | otherwise -> declared
    where
      tyName = occToString lname
      -- Any shape: a Beamable type the extractor cannot read is reported, not skipped.
      declared = foldl' derive rm { rmDataDecls = Set.insert tyName (rmDataDecls rm) } (derivedTypes defn)
      -- A deriving clause's @Cls a …@ is an instance @Cls a … T@.
      derive acc (clause, clsTy) = case typeHead clsTy of
        Just (cls, args) ->
          recordInstance (occToString cls)
            (if null args then Right (Just tyName) else Left (unwords (map sdocText args <> [tyName])))
            (Left clause) acc
        Nothing -> acc
  TyClD _ SynDecl{ tcdLName = L _ n } ->
    rm { rmTypeDecls = Set.insert (occToString n) (rmTypeDecls rm) }
  TyClD _ FamDecl{ tcdFam = fam } ->
    rm { rmTypeDecls = Set.insert (familyName fam) (rmTypeDecls rm) }
  TyClD _ ClassDecl{ tcdATs = ats } ->
    rm { rmTypeDecls = foldr (Set.insert . familyName . unLoc) (rmTypeDecls rm) ats }
  InstD _ (ClsInstD _ ClsInstDecl{ cid_poly_ty, cid_binds }) ->
    visitInstance cid_poly_ty (Right cid_binds) rm
  DerivD _ d@DerivDecl{ deriv_type = HsWC{ hswc_body = lsig } } ->
    visitInstance lsig (Left (sdocText d)) rm
  ValD _ (FunBind { fun_id = L _ nm, fun_matches = mg })
    | Just rhs <- singleRhs mg ->
        rm { rmBindings = Map.insert (occToString nm) rhs (rmBindings rm) }
  ValD _ (PatBind { pat_lhs = L _ (VarPat _ (L _ nm)), pat_rhs = grhss })
    | Just rhs <- grhssExpr grhss ->
        rm { rmBindings = Map.insert (occToString nm) rhs (rmBindings rm) }
  _ -> rm


familyName :: FamilyDecl GhcPs -> String
familyName FamilyDecl{ fdLName = L _ n } = occToString n

-- | Records a written ('Right' its bindings) or standalone-derived instance.
visitInstance :: LHsSigType GhcPs -> Either String (LHsBinds GhcPs) -> RawModule -> RawModule
visitInstance lsig methods rm = case headTypeOfInstance lsig of
  Just (cls, args) -> recordInstance cls (instanceType cls args) methods rm
  Nothing          -> rm

-- | The head of each Generic instance in the module that is not a stock derivation
-- ('rmNonStockGeneric'): Beam names a table's columns from its Generic representation, which
-- such an instance can set to anything.  With no strategy written, GHC derives Generic stock
-- or refuses the clause.
nonStockGeneric :: [LHsDecl GhcPs] -> [Maybe String]
nonStockGeneric = concatMap (instances . unLoc)
  where
    instances = \case
      TyClD _ DataDecl{ tcdLName = L _ n, tcdDataDefn = defn } ->
        [Just (occToString n) | (False, ty) <- derivingStrategies defn, tyConHead ty == Just "Generic"]
      DerivD _ DerivDecl{ deriv_type = HsWC{ hswc_body = lsig }, deriv_strategy = strategy }
        | Just ("Generic", args) <- headTypeOfInstance lsig, not (stockStrategy strategy) -> [headTyCon args]
      InstD _ (ClsInstD _ ClsInstDecl{ cid_poly_ty = lsig })
        | Just ("Generic", args) <- headTypeOfInstance lsig -> [headTyCon args]
      _ -> []

-- | The type of each Beamable instance whose methods need not come from the Generic
-- representation: written with a method, or derived via another type; a key's instance names its
-- table, and one in a data instance's deriving clause is 'Nothing'.  'zipBeamFieldsM' decides the
-- field each of the modifier's column renames goes to.
beamableNotGeneric :: [LHsDecl GhcPs] -> [Maybe String]
beamableNotGeneric = concatMap (instances . unLoc)
  where
    instances = \case
      TyClD _ DataDecl{ tcdLName = L _ n, tcdDataDefn = defn } -> [Just (occToString n) | viaBeamable defn]
      DerivD _ DerivDecl{ deriv_type = HsWC{ hswc_body = lsig }, deriv_strategy = Just (L _ ViaStrategy{}) }
        | Just ("Beamable", args) <- headTypeOfInstance lsig -> [beamableHead args]
      InstD _ (ClsInstD _ ClsInstDecl{ cid_poly_ty = lsig, cid_binds = binds, cid_datafam_insts = dfis }) ->
        [beamableHead args | not (null (bagToList binds)), Just ("Beamable", args) <- [headTypeOfInstance lsig]]
        <> [Nothing | L _ DataFamInstDecl{ dfid_eqn = FamEqn{ feqn_rhs = defn } } <- dfis, viaBeamable defn]
      InstD _ (DataFamInstD _ DataFamInstDecl{ dfid_eqn = FamEqn{ feqn_rhs = defn } }) -> [Nothing | viaBeamable defn]
      _ -> []
    viaBeamable defn = or [True | (Just (L _ ViaStrategy{}), ty) <- derivingClauses defn, tyConHead ty == Just "Beamable"]
    beamableHead = \case
      [ty] | Just (k, t : _) <- typeHead ty, occToString k == "PrimaryKey" -> headTyCon [t]
      args -> headTyCon args

-- | The types the module derives Generic for at every type argument: in their own declaration, or
-- standalone with the type itself in the head, applied to distinct type variables.  A narrower
-- head, or one through a synonym, leaves the rest to another instance.  'nonStockGeneric' refuses a
-- derivation by another strategy than stock.
derivedGeneric :: [LHsDecl GhcPs] -> Set String
derivedGeneric = Set.fromList . concatMap (derivations . unLoc)
  where
    derivations = \case
      TyClD _ DataDecl{ tcdLName = L _ n, tcdDataDefn = defn } ->
        [occToString n | (_, ty) <- derivingStrategies defn, tyConHead ty == Just "Generic"]
      DerivD _ DerivDecl{ deriv_type = HsWC{ hswc_body = lsig } }
        | Just ("Generic", args) <- headTypeOfInstance lsig -> toList (atEveryArgument args)
      _ -> []

atEveryArgument :: [HsType GhcPs] -> Maybe String
atEveryArgument = \case
  [ty] | Just (n@(Unqual occ), args) <- typeHead ty, isTcOcc occ
       , Just vs <- traverse tyVarName args, length (nub vs) == length vs -> Just (occToString n)
  _ -> Nothing
  where
    tyVarName = \case
      HsTyVar _ _ (L _ v@(Unqual o)) | isTvOcc o -> Just (occToString v)
      HsParTy _ (L _ t)                          -> tyVarName t
      HsKindSig _ (L _ t) _                      -> tyVarName t
      _                                          -> Nothing

-- | The types an annotation @{-# ANN type T largeRecord #-}@ marks, with the name in any qualifier.
largeRecords :: [LHsDecl GhcPs] -> Set String
largeRecords decls = Set.fromList
  [ occToString t | L _ (AnnD _ ann) <- decls, Just (t, e) <- [typeAnnotation ann], varName e == Just "largeRecord" ]

stockStrategy :: Maybe (LDerivStrategy GhcPs) -> Bool
stockStrategy = \case
  Nothing                    -> True
  Just (L _ StockStrategy{}) -> True
  Just _                     -> False

headTyCon :: [HsType GhcPs] -> Maybe String
headTyCon = \case
  [ty] | Just (n@(Unqual occ), _) <- typeHead ty, isTcOcc occ -> Just (occToString n)
  _ -> Nothing

-- | Records an instance of a Table, ModelMeta or Beamable class (other classes
-- are ignored) for @target@, read as 'instanceType' reads one.  A derived
-- instance has no methods to read: 'Left' holds its rendered derivation.
recordInstance
  :: String -> Either String (Maybe String) -> Either String (LHsBinds GhcPs) -> RawModule
  -> RawModule
recordInstance cls target methods rm = case (lookup cls recorders, target) of
  (Nothing, _)                  -> rm
  (Just _, Left ty)             -> rm { rmUnreadableHeads = rmUnreadableHeads rm <> [(cls, ty)] }
  (Just _, Right Nothing)       -> rm
  (Just record, Right (Just t)) -> (record t) { rmInstanceKeys = rmInstanceKeys rm <> [(cls, t)] }
  where
    recorders =
      [ ("Beamable",  \t -> rm { rmBeamableTypes = Set.insert t (rmBeamableTypes rm) })
      , ("ModelMeta", \t -> rm { rmModelMetas = Map.insert t (either derivedMeta extractModelMeta methods) (rmModelMetas rm) })
      , ("Table",     \t -> rm { rmTableInsts = Map.insert t (either (const (RawTableInst [] Nothing)) extractTableInst methods) (rmTableInsts rm) })
      ]
    -- Deriving via can take the methods from another type, so none is read as absent.
    derivedMeta derivation = RawModelMeta Nothing ModifierUnreadable (Left derivation) (Left derivation)

-- | The table type an instance of @cls@ applied to @args@ is for: 'Right'
-- 'Nothing' for a key's Beamable instance, which is for no table, and 'Left'
-- the rendered head unless it is one unqualified type constructor (maybe applied).
instanceType :: String -> [HsType GhcPs] -> Either String (Maybe String)
instanceType cls = \case
  [ty] -> case typeHead ty of
    Just (n, _ : _) | occToString n == "PrimaryKey" ->
      if cls == "Beamable" then Right Nothing else Left (sdocText ty)
    -- A qualified name may be another module's type, which, keyed by its
    -- unqualified name, would take the instance of a same-named type here.
    -- isTcOcc also refuses a type variable and a promoted constructor ('FooT).
    Just (n@(Unqual occ), _) | isTcOcc occ -> Right (Just (occToString n))
    _ -> Left (sdocText ty)
  args -> Left (unwords (map sdocText args))

-- | The class of an instance head and the types it is applied to, under any
-- context, parentheses or kind signature (@instance C => (Cls ty :: Constraint)@).
-- GHC rejects a context or forall nested in the head (GHC-71492), but only
-- after this plugin has run, so a nested one is read through too.
headTypeOfInstance :: LHsSigType GhcPs -> Maybe (String, [HsType GhcPs])
headTypeOfInstance lsig = go (unLoc (sig_body (unLoc lsig)))
  where
    go = \case
      HsQualTy _ _ (L _ t)   -> go t
      HsForAllTy _ _ (L _ t) -> go t
      HsParTy _ (L _ t)      -> go t
      HsKindSig _ (L _ t) _  -> go t
      HsDocTy _ (L _ t) _    -> go t
      t                      -> (\(cls, args) -> (occToString cls, args)) <$> typeHead t

tyConHead :: HsType GhcPs -> Maybe String
tyConHead = fmap (occToString . fst) . typeHead

-- | The name heading a type application, which may be a type variable, and the
-- types it is applied to, through parentheses, kind signatures and Haddock
-- comments: -haddock (nixpkgs passes it) wraps a documented type in 'HsDocTy'.
typeHead :: HsType GhcPs -> Maybe (RdrName, [HsType GhcPs])
typeHead = go []
  where
    go args = \case
      HsTyVar _ _ (L _ n)       -> Just (n, args)
      HsAppTy _ (L _ f) (L _ x) -> go (x : args) f
      HsParTy _ (L _ t)         -> go args t
      HsKindSig _ (L _ t) _     -> go args t
      HsDocTy _ (L _ t) _       -> go args t
      _                         -> Nothing


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

hasDatabaseDeriving :: HsDataDefn GhcPs -> Bool
hasDatabaseDeriving defn = any ((== Just "Database") . tyConHead . snd) (derivedTypes defn)

-- | The rendered clause keeps the deriving strategy for the error message.
derivedTypes :: HsDataDefn GhcPs -> [(String, HsType GhcPs)]
derivedTypes defn =
  [ (sdocText clause, unLoc (sig_body (unLoc ty)))
  | L _ clause@HsDerivingClause{ deriv_clause_tys = L _ tys } <- dd_derivs defn
  , ty <- case tys of
      DctSingle _ t -> [t]
      DctMulti _ ts -> ts
  ]

-- | Each class a deriving clause names, with whether the clause derives it stock.
derivingStrategies :: HsDataDefn GhcPs -> [(Bool, HsType GhcPs)]
derivingStrategies defn = [(stockStrategy strategy, ty) | (strategy, ty) <- derivingClauses defn]

-- | Each class a deriving clause names, with the clause's strategy.
derivingClauses :: HsDataDefn GhcPs -> [(Maybe (LDerivStrategy GhcPs), HsType GhcPs)]
derivingClauses defn =
  [ (strategy, unLoc (sig_body (unLoc ty)))
  | L _ HsDerivingClause{ deriv_clause_strategy = strategy, deriv_clause_tys = L _ tys } <- dd_derivs defn
  , ty <- case tys of
      DctSingle _ t -> [t]
      DctMulti _ ts -> ts
  ]


-- ---------------------------------------------------------------------------
-- DB-record extraction (for setEntityName overrides)
-- ---------------------------------------------------------------------------

-- | Read a DB-record data decl of the form
--   @data Db f = Db { x :: f (TableEntity XT), y :: f (TableEntity YT) } deriving (.., Database)@
-- and return the @fieldName -> unqualified TableT names@ map.  Fields whose
-- type doesn't fit the @f (TableEntity SomeT)@ pattern are skipped silently
-- (they aren't tables — they might be auxiliary).
extractDbRecord :: LHsQTyVars GhcPs -> HsDataDefn GhcPs -> Maybe (Map.Map String (Set String))
extractDbRecord _ defn =
  case toList (dd_cons defn) of
    [L _ (ConDeclH98{ con_args = RecCon (L _ recs) })] -> Just $
      Map.fromListWith Set.union
        [ (occNameFromFieldOcc (unLoc ln), Set.singleton tt)
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

-- | The renames the module's DB settings apply ('appliedRenames'), each
-- resolved to the one table type its field has across the module's records,
-- and an error for every other @setEntityName@ use: a rename the contract does
-- not record would never be checked.
extractDbOverrides
  :: String -> FilePath -> [LHsDecl GhcPs] -> Uses -> RawModule
  -> ([DbEntityOverride], [SqlSchemaError])
extractDbOverrides modName srcPath decls uses RawModule{ rmBindings, rmDataDecls, rmTypeDecls } =
  ( [o | (_, Right o) <- reached]
  , [ UNCAPTURED_SET_ENTITY_NAME (lineCol sp) (reason sp)
    | sp <- usesOf "setEntityName" uses \\ captured ] )
  where
    reached    = [ (sp, lit >>= resolve f sp)
                 | (f, sp, lit) <- concatMap (appliedRenames modName labelSets . unLoc) (Map.elems rmBindings) ]
    captured   = [sp | (sp, Right _) <- reached]
    refused    = [(sp, why) | (sp, Left why) <- reached]
    reason sp  = fromMaybe (unread sp) (lookup sp refused)
    unread sp
      | sp `elem` inModifications = "its dbModification { … } is not the right operand of withDbModification in the result of a top-level binding"
      | otherwise                 = "it is not in a field of a dbModification { … }"
    inModifications = usesInDbModifications decls
    fieldTypes = recordFieldTypes decls
    labelSets  = recordLabelSets decls
    shadowing  = [n | n <- trustedNames, Map.member n (binderCounts decls)]
    covered    = underRecordWildcard decls uses
    resolve f sp sql
      | not (null shadowing) =
          Left ("this module binds " <> intercalate ", " ["'" <> n <> "'" | n <- shadowing] <> " itself, so a use may not be Beam's")
      | covered sp =
          Left "a record wildcard '{..}' in its declaration, a top-level pattern binding or a top-level splice may rebind setEntityName or a name the rename relies on"
      | any (Set.member "TableEntity") [rmDataDecls, rmTypeDecls] =
          Left "this module declares a type named 'TableEntity', so a field's 'f (TableEntity T)' may not be Beam's table entity"
      | otherwise = case Set.toList (Map.findWithDefault Set.empty f fieldTypes) of
          [TableType tt] -> Right DbEntityOverride
                              { dbField              = f
                              , dbTableType          = tt
                              , sqlName              = sql
                              , overrideSourceModule = modName
                              , overrideSourceFile   = srcPath
                              }
          []  -> Left ("its field '" <> f <> "' is declared in no record of this module")
          tys -> Left ("its field '" <> f <> "' has more than one type, or one other than f (TableEntity T), across this module's records ("
                       <> intercalate ", " (map fieldTypeText tys) <> ")")

-- | The names a captured rename's meaning rests on: a module that binds one of
-- them anywhere could give it another meaning, so its renames are not read.
trustedNames :: [String]
trustedNames = ["setEntityName", "dbModification", "withDbModification", "<>", "modifyTableFields", "setEntitySchema"]

-- | Each setEntityName in a @dbModification { … }@ that a top-level binding's
-- result applies (a right operand of @withDbModification@), with its literal
-- or why it is not read.  A dbModification elsewhere may never be applied.
appliedRenames :: String -> [Set String] -> HsExpr GhcPs -> [(String, SrcSpan, Either String String)]
appliedRenames modName labelSets = settings Nothing
  where
    settings why = \case
      Par (L _ e) -> settings why e
      Let (L _ e) -> settings why e
      HsLam _ MG{ mg_alts = L _ ms } ->
        [ r | L _ Match{ m_grhss = g } <- ms, Just (L _ e) <- [grhssExpr g], r <- settings why e ]
      ExprWithTySig _ (L _ e) _ ->
        settings (Just "the settings expression carries a type annotation, which may select another module's Database record") e
      OpApp _ (L _ l) (L _ op) (L _ r)
        | isVar "withDbModification" op -> settings (maybe (later r) Just why) l <> modification why r
      HsApp _ (L _ inner) (L _ r)
        | HsApp _ (L _ f) (L _ l) <- stripPar inner
        , isVar "withDbModification" f -> settings (maybe (later r) Just why) l <> modification why r
      _ -> []

    later r
      | isModificationUpdate r = Nothing
      | otherwise = Just "a later withDbModification operand is not a dbModification { … } update, so it could change the rename"

    modification why e = case stripPar e of
      RecordUpd _ (L _ base) flds
        | isVar "dbModification" base -> fields why flds
        | annotatedDbModification base ->
            fields (Just "its dbModification carries a type annotation or type application, which may select another module's Database record") flds
      ExprWithTySig _ (L _ x) _ ->
        modification (Just "its dbModification { … } carries a type annotation, which may select another module's Database record") x
      _ -> []

    fields why flds =
      let regular = fromMaybe [] (regularRecUpdFields flds)
          why'    = maybe (updatedRecord [lbl | L _ (RecField (L _ lbl) _ _) <- regular]) Just why
          plain   = concat [ field why' lbl rhs | L _ (RecField (L _ lbl) (L _ rhs) False) <- regular ]
      in  plain <> [ ("", sp, Left "its dbModification field is not a plain 'field = value' update (record-dot syntax or a pun), which the plugin does not read")
                   | sp <- usesOf "setEntityName" (watchedUses flds) \\ [s | (_, s, _) <- plain] ]

    field why lbl rhs =
      let f    = ambiguousFieldName lbl
          uses = usesOf "setEntityName" (watchedUses rhs)
      in case why of
           Just reason -> [ (f, sp, Left reason) | sp <- uses ]
           Nothing     ->
             let readings = setEntityNameLit rhs
             in  [ (f, sp, r) | (sp, r) <- readings ]
              <> [ (f, sp, Left ("the value of field '" <> f <> "' is not setEntityName applied to a string literal, alone or in a '<>' chain"))
                 | sp <- uses \\ map fst readings ]

    -- GHC resolves an unannotated update to the record that declares all of its
    -- labels, which is this module's only if one of its records declares them.
    updatedRecord lbls
      | r : _ <- mapMaybe qualifiedLabel lbls = Just r
      | any (\s -> all (`Set.member` s) names) labelSets = Nothing
      | otherwise = Just ("no single record of this module declares all of its dbModification's labels ("
                          <> intercalate ", " ["'" <> n <> "'" | n <- names] <> "), so it may update another module's record")
      where names = map ambiguousFieldName lbls

    qualifiedLabel lbl = case ambiguousFieldRdr lbl of
      Unqual _ -> Nothing
      Qual m _ | GHC.moduleNameString m == modName -> Nothing
      rdr -> Just ("its dbModification's label '" <> sdocText rdr <> "' is qualified with another module, so it may name that module's record")

isModificationUpdate :: HsExpr GhcPs -> Bool
isModificationUpdate e = case stripPar e of
  RecordUpd _ (L _ base) _  -> isVar "dbModification" base || annotatedDbModification base
  ExprWithTySig _ (L _ x) _ -> isModificationUpdate x
  _                         -> False

annotatedDbModification :: HsExpr GhcPs -> Bool
annotatedDbModification e = case stripPar e of
  ExprWithTySig _ (L _ x) _ -> isVar "dbModification" x || annotatedDbModification x
  AppType (L _ x) _         -> isVar "dbModification" x || annotatedDbModification x
  _                         -> False

-- | The @setEntityName@ applications of a dbModification field value, each
-- with the span of its @setEntityName@: the literal when the value is
-- @setEntityName "lit"@, alone or the only such operand of a @<>@ chain
-- whose other operands are @modifyTableFields x@ or @setEntitySchema x@
-- (@<>@ is associative, so its nesting doesn't matter); else why not.
setEntityNameLit :: HsExpr GhcPs -> [(SrcSpan, Either String String)]
setEntityNameLit rhs = case partitionEithers (map operand (mappendOperands rhs)) of
  ([(sp, arg)], others)
    | all nonRenaming others -> [(sp, maybe (Left "its argument is not a string literal") Right (stringLiteral arg))]
    | otherwise -> [(sp, Left "its '<>' chain has an operand other than 'modifyTableFields x' or 'setEntitySchema x', which could change or drop the rename")]
  (apps, _) -> [(sp, Left "its '<>' chain has more than one setEntityName operand") | (sp, _) <- apps]
  where
    mappendOperands e = case stripPar e of
      OpApp _ (L _ a) (L _ op) (L _ b)
        | varName op == Just "<>" -> mappendOperands a <> mappendOperands b
      _ -> [e]

    operand e = case stripPar e of
      HsApp _ (L _ f) (L _ arg)
        | Just sp <- varSpanNamed "setEntityName" f -> Left (sp, arg)
      _ -> Right e

    nonRenaming e = case stripPar e of
      HsApp _ (L _ f) _ -> any (\n -> isJust (varSpanNamed n f)) ["modifyTableFields", "setEntitySchema"]
      _                 -> False

usesInDbModifications :: Data a => a -> [SrcSpan]
usesInDbModifications = go
  where
    go :: forall d. Data d => d -> [SrcSpan]
    go d = case cast d :: Maybe (HsExpr GhcPs) of
      Just (RecordUpd _ (L _ base) flds)
        | isVar "dbModification" base || annotatedDbModification base -> usesOf "setEntityName" (watchedUses flds)
      _ -> concat (gmapQ go d)

data FieldType = TableType String | NotTableEntity
  deriving (Eq, Ord)

fieldTypeText :: FieldType -> String
fieldTypeText = \case
  TableType t    -> t
  NotTableEntity -> "a type other than TableEntity"

-- | Each record field declared in the declarations (data types and instances,
-- GADT records, record pattern synonyms) with its type: 'TableType' when it
-- is @f (TableEntity T)@.
recordFieldTypes :: Data a => a -> Map.Map String (Set FieldType)
recordFieldTypes = Map.fromListWith Set.union . go
  where
    go :: forall d. Data d => d -> [(String, Set FieldType)]
    go d
      | Just ConDeclField{ cd_fld_names = ns, cd_fld_type = L _ ty } <- (cast d :: Maybe (ConDeclField GhcPs)) =
          [ (occNameFromFieldOcc (unLoc n), Set.singleton (maybe NotTableEntity TableType (tableEntityOf ty))) | n <- ns ]
      | Just RecordPatSynField{ recordPatSynField = fo } <- (cast d :: Maybe (RecordPatSynField GhcPs)) =
          [ (occNameFromFieldOcc fo, Set.singleton NotTableEntity) ]
      | otherwise = concat (gmapQ go d)

-- | The labels of each record constructor and record pattern synonym, outside
-- declaration quotes: a quoted record may never be spliced.
recordLabelSets :: Data a => a -> [Set String]
recordLabelSets = go
  where
    go :: forall d. Data d => d -> [Set String]
    go d
      | Just DecBrL{} <- (cast d :: Maybe UntypedQuote) = []
      | Just fs <- (cast d :: Maybe [LConDeclField GhcPs]) =
          [Set.fromList [occNameFromFieldOcc (unLoc n) | L _ ConDeclField{ cd_fld_names = ns } <- fs, n <- ns]]
      | Just fs <- (cast d :: Maybe [RecordPatSynField GhcPs]) =
          [Set.fromList [occNameFromFieldOcc fo | RecordPatSynField{ recordPatSynField = fo } <- fs]]
      | otherwise = concat (gmapQ go d)

watchedNames :: [String]
watchedNames = "setEntityName" : "modifyTableFields" : entityRenamers

-- | The sites the checks need, found in one traversal, in source order.  A
-- pun's value is a parser placeholder, so a punned field counts as a use of its
-- label.  Comments and import lists are not in the declarations.
data Uses = Uses
  { useSites              :: [(String, SrcSpan)]
  , conSites              :: [(String, SrcSpan)]
  , modifyTableFieldsArgs :: [(SrcSpan, HsExpr GhcPs)]
  , typeAppliedSites      :: [SrcSpan]
    -- ^ @modifyTableFields \@T@
  , wildcardSites         :: [SrcSpan]
    -- ^ record wildcard patterns @C{..}@
  }

data Site
  = UseSite (String, SrcSpan) | ConSite (String, SrcSpan) | AppSite (SrcSpan, HsExpr GhcPs)
  | TyAppSite SrcSpan | WildSite SrcSpan

watchedUses :: Data a => a -> Uses
watchedUses x = Uses [u | UseSite u <- found] [c | ConSite c <- found] [a | AppSite a <- found]
                     [t | TyAppSite t <- found] [w | WildSite w <- found]
  where
    found = go x

    go :: forall d. Data d => d -> [Site]
    go d
      | Just e <- (cast d :: Maybe (HsExpr GhcPs)) = case e of
          HsVar _ (L l rdr)               -> use l rdr
          RecordCon{ rcon_con = L l rdr } -> con l rdr <> below
          HsApp _ (L _ f) (L _ arg)
            | Just sp <- varSpanNamed "modifyTableFields" f -> AppSite (sp, arg) : below
          AppType (L _ f) _
            | Just sp <- varSpanNamed "modifyTableFields" f -> TyAppSite sp : below
          _                               -> below
      | Just (VarBr _ _ (L l rdr)) <- (cast d :: Maybe UntypedQuote) = use l rdr
      | Just ConPat{ pat_con = L l rdr, pat_args = args } <- (cast d :: Maybe (Pat GhcPs)) =
          con l rdr <> [WildSite (GHC.locA l) | RecCon HsRecFields{ rec_dotdot = Just _ } <- [args]] <> below
      -- An OverloadedRecordUpdate pun's variable has no location: report it at its field.
      | Just (L l (RecField _ (L _ (HsVar _ (L _ rdr))) True)) <- (cast d :: Maybe (LHsRecUpdProj GhcPs)) =
          useAt (GHC.locA l) rdr
      | Just (L l (RecField (L _ lbl) _ True)) <- (cast d :: Maybe (LHsRecField GhcPs (LHsExpr GhcPs))) =
          pun l (occNameFromFieldOcc lbl) <> below
      | Just (L l (RecField (L _ lbl) _ True)) <- (cast d :: Maybe UpdField) =
          pun l (ambiguousFieldName lbl) <> below
      | otherwise = below
      where below = concat (gmapQ go d)

    use l = useAt (GHC.locA l)
    useAt sp rdr = [UseSite (n, sp) | let n = occNameString (rdrNameOcc rdr), n `elem` watchedNames]
    con l rdr = [ConSite (n, GHC.locA l) | let n = occNameString (rdrNameOcc rdr), n `elem` watchedNames]
    pun l n   = [UseSite (n, GHC.locA l) | n `elem` watchedNames]

usesOf :: String -> Uses -> [SrcSpan]
usesOf name uses = [sp | (n, sp) <- useSites uses, n == name]

-- | Whether a record wildcard may bind names at a use: one in the use's
-- top-level declaration, or in a top-level pattern binding or splice, which can
-- bind them module-wide.  The declaration is found by its uses, not its span: a
-- line directive can split a declaration's span across files.
underRecordWildcard :: [LHsDecl GhcPs] -> Uses -> SrcSpan -> Bool
underRecordWildcard decls uses
  | null (wildcardSites uses) = const False
  | otherwise                 = \sp -> any (covers sp) wild
  where
    wild = [ (moduleWide d, map snd (useSites u)) | d <- decls, let u = watchedUses d, not (null (wildcardSites u)) ]
    covers sp (wide, held) = wide || sp `elem` held
    moduleWide = \case
      L _ (ValD _ PatBind{}) -> True
      L _ (SpliceD _ _)      -> True
      _                      -> False


-- ---------------------------------------------------------------------------
-- DB-settings column renames (modifyTableFields)
-- ---------------------------------------------------------------------------

-- | An error for every DB-settings rename that could differ from the contract:
-- each @modifyTableFields@ not applied to the modifier binding the contract's
-- column names come from, and each use of an 'entityRenamers' name.
checkTableModifications :: [LHsDecl GhcPs] -> Uses -> RawModule -> [SqlSchemaError]
checkTableModifications decls uses rm@RawModule{ rmTopBinders } =
  [ UNCAPTURED_MODIFY_TABLE_FIELDS (lineCol sp) (fromMaybe (unapplied sp) (lookup sp refused))
  | sp <- usesOf "modifyTableFields" uses \\ captured ]
  <> [ UNSUPPORTED_ENTITY_RENAME (lineCol sp) fn | fn <- entityRenamers, sp <- usesOf fn uses ]
  <> [ UNSUPPORTED_ENTITY_RENAME (lineCol sp) con | (con, sp) <- conSites uses, con `elem` entityRenamers ]
  where
    applied   = [ (sp, argumentRead sp arg) | (sp, arg) <- modifyTableFieldsArgs uses ]
    captured  = [ sp | (sp, Right ()) <- applied ]
    refused   = [ (sp, why) | (sp, Left why) <- applied ]
    modifiers = modifierBindings rm
    binders   = binderCounts decls
    covered   = underRecordWildcard decls uses
    unapplied sp
      | sp `elem` typeAppliedSites uses = "it has a type application, which the plugin does not read"
      | otherwise                       = "it is not applied directly to an argument"
    trusted = [n | n <- ["modifyTableFields", "tableModification", "fieldNamed", "<>"], Map.member n binders]
    argumentRead sp arg = case varRdrName arg of
      Just rdr@(Unqual _)
        | not (Set.member b modifiers) ->
            Left (quoted b <> " is not the modelFieldModification binding of a table extracted from this module")
        | Map.findWithDefault 0 b binders > 1 ->
            Left (quoted b <> " is also bound elsewhere in this module, so this use may not name the modifier binding")
        | not (null trusted) ->
            Left ("this module binds " <> intercalate ", " (map quoted trusted) <> " itself, so the use may not be Beam's")
        | covered sp ->
            Left ("a record wildcard '{..}' in its declaration, a top-level pattern binding or a top-level splice may rebind " <> quoted b <> " or modifyTableFields")
        | isNothing (modifierBinding rmTopBinders b) ->
            Left ("the modifier binding " <> quoted b <> " is not one top-level equation with no arguments, guards or where bindings, so the plugin does not read it")
        | not (setsAField b) ->
            Left ("the modifier binding " <> quoted b <> " is not a 'tableModification { … }' record update with at least one field")
        | otherwise -> Right ()
        where b = occToString rdr
      _ -> Left "its argument is not an unqualified variable"
    setsAField b = maybe False (> 0) (tableModificationFields . unLoc =<< modifierBinding rmTopBinders b)
    quoted b = "'" <> b <> "'"

-- | Beam names that can rename a table or its columns in a way the plugin
-- cannot read: functions that rename through a @Text -> Text@ or
-- @NonEmpty Text -> Text@ function or apply a table modification directly,
-- and the entity internals a hand-written modification is built from.
entityRenamers :: [String]
entityRenamers =
  [ "modifyTable", "modifyEntityName", "withTableModification", "renamingFields"
  , "EntityModification", "DatabaseEntity", "dbEntityName", "dbTableCurrentName" ]

-- | The binding each extracted table's @modelFieldModification@ names.
modifierBindings :: RawModule -> Set String
modifierBindings RawModule{ rmTables, rmBeamableTypes, rmModelMetas } = Set.fromList
  [ b
  | (_, tyName, _) <- rmTables
  , Set.member tyName rmBeamableTypes
  , Just RawModelMeta{ rmmModifier = ModifierRhs rhs } <- [Map.lookup tyName rmModelMetas]
  , Just rdr@(Unqual _) <- [varRdrName (unLoc rhs)]
  , let b = occToString rdr
  , b /= "tableModification"
  ]

-- | How many fields a modifier's @tableModification { … }@ sets, as
-- 'extractColumnOverrides' reads it: none for a bare @tableModification@.
tableModificationFields :: HsExpr GhcPs -> Maybe Int
tableModificationFields expr = case stripPar expr of
  HsVar _ (L _ n) | occToString n == "tableModification" -> Just 0
  RecordUpd _ (L _ base) flds
    | varName base == Just "tableModification" -> Just (maybe 0 length (regularRecUpdFields flds))
  _ -> Nothing

-- | How many times each name is bound in the declarations: by a binding, a
-- pattern, a punned record pattern field, a RULES binder, a record field, a
-- class method, a foreign import or a record pattern synonym's field, which
-- binds a selector even when its pattern binds the variable through a record
-- wildcard.  An instance method's own name is not counted: it defines its
-- class's method and cannot rebind the name; it can still change what the
-- method does at the instance's type, so it counts in an instance whose type
-- names EntityModification or DatabaseEntity, or that has an overlap pragma.  A derived instance
-- under the same rule, standalone or in a deriving clause, binds its class's methods, as far as
-- 'derivedMethods' knows them.  Names that a record wildcard or a splice's output binds are not seen.
binderCounts :: Data a => a -> Map.Map String Int
binderCounts = Map.fromListWith (+) . map (\n -> (n, 1)) . go
  where
    go :: forall d. Data d => d -> [String]
    go d
      | Just inst <- (cast d :: Maybe (ClsInstDecl GhcPs))
      , not (redefinesBeamMethods inst) = concat (gmapQ instancePart d)
      | otherwise                       = derived d <> bound d <> concat (gmapQ go d)

    redefinesBeamMethods :: ClsInstDecl GhcPs -> Bool
    redefinesBeamMethods ClsInstDecl{ cid_poly_ty = ty, cid_overlap_mode = overlap } =
      isJust overlap || namesBeamEntity (typeNames ty)

    namesBeamEntity = any (`elem` ["EntityModification", "DatabaseEntity"])

    -- A deriving clause has no overlap pragma (GHC rejects one), and the instances that bind names
    -- here, Semigroup's and Monoid's, take the declared type as their only argument.
    derived :: forall d. Data d => d -> [String]
    derived d
      | Just DerivDecl{ deriv_type = HsWC{ hswc_body = lsig }, deriv_overlap_mode = overlap }
          <- (cast d :: Maybe (DerivDecl GhcPs))
      , isJust overlap || namesBeamEntity (typeNames lsig)
      , Just (cls, _) <- headTypeOfInstance lsig = derivedMethods cls
      | Just DataDecl{ tcdLName = L _ n, tcdDataDefn = defn } <- (cast d :: Maybe (TyClDecl GhcPs)) =
          clauses [occToString n] defn
      | Just FamEqn{ feqn_tycon = L _ n, feqn_pats = pats, feqn_rhs = defn }
          <- (cast d :: Maybe (FamEqn GhcPs (HsDataDefn GhcPs))) = clauses (occToString n : typeNames pats) defn
      | otherwise = []

    clauses declared defn =
      [ m | namesBeamEntity declared, (_, ty) <- derivingStrategies defn, Just cls <- [tyConHead ty]
          , m <- derivedMethods cls ]

    -- The methods' bodies and the instance's other parts can still bind names.
    instancePart :: forall e. Data e => e -> [String]
    instancePart e = case cast e :: Maybe (LHsBinds GhcPs) of
      Just methods -> concat [concat (gmapQ go m) | L _ m <- bagToList methods]
      Nothing      -> go e

    bound :: forall d. Data d => d -> [String]
    bound d
      | Just FunBind{ fun_id = L _ n } <- (cast d :: Maybe (HsBind GhcPs)) = [occToString n]
      | Just (RecField (L _ lbl) _ True) <- (cast d :: Maybe PatRecField)  = [occNameFromFieldOcc lbl]
      | Just (_ :: Pat GhcPs) <- cast d                                     = namesBelow d
      | Just (_ :: RuleBndr GhcPs) <- cast d                                = namesBelow d
      | Just ConDeclField{ cd_fld_names = ns } <- (cast d :: Maybe (ConDeclField GhcPs)) =
          [occNameFromFieldOcc (unLoc n) | n <- ns]
      | Just (ClassOpSig _ False ns _) <- (cast d :: Maybe (Sig GhcPs))   = [occToString n | L _ n <- ns]
      | Just ForeignImport{ fd_name = L _ n } <- (cast d :: Maybe (ForeignDecl GhcPs)) = [occToString n]
      | Just RecordPatSynField{ recordPatSynField = fo } <- (cast d :: Maybe (RecordPatSynField GhcPs)) =
          [occNameFromFieldOcc fo]
      | otherwise                                                           = []

    -- The names a pattern or RULES binder holds directly are those it binds,
    -- plus a constructor pattern's constructor, which no value binding shares.
    namesBelow :: forall d. Data d => d -> [String]
    namesBelow d = [occToString n | Just (L _ n) <- gmapQ (\c -> cast c :: Maybe (LIdP GhcPs)) d]


typeNames :: Data a => a -> [String]
typeNames d = case cast d :: Maybe (HsType GhcPs) of
  Just (HsTyVar _ _ (L _ n)) -> [occToString n]
  _                          -> concat (gmapQ typeNames d)


-- ---------------------------------------------------------------------------
-- ModelMeta / Table instance bodies
-- ---------------------------------------------------------------------------

extractModelMeta :: LHsBinds GhcPs -> RawModelMeta
extractModelMeta binds = RawModelMeta tableName modifier tableType schemaName
  where
    tableType  = metaField tableTypeOf (bindingsOf "modelTableType")
    schemaName = metaField schemaNameOf (bindingsOf "modelSchemaName")
    tableName  = stringLiteral . unLoc =<< plainBinding (bindingsOf "modelTableName")
    modifier   = case bindingsOf "modelFieldModification" of
      [] -> ModifierAbsent
      bs -> maybe ModifierUnreadable ModifierRhs (plainBinding bs)
    bindingsOf name = bindingsNamed name binds

extractTableInst :: LHsBinds GhcPs -> RawTableInst
extractTableInst binds = case oneEquation (bindingsNamed "primaryKey" binds) of
  Just (pats, L _ body) -> RawTableInst (map unLoc pats) (Just body)
  Nothing               -> RawTableInst [] Nothing

bindingsNamed :: String -> LHsBinds GhcPs -> [HsBind GhcPs]
bindingsNamed name binds =
  [ bind
  | L _ bind <- bagToList binds
  , name `elem` boundNames bind
  ]

boundNames :: HsBind GhcPs -> [String]
boundNames bind =
  map occToString (collectHsBindBinders CollNoDictBinders bind)
  <> [n | PatBind{ pat_lhs = lhs } <- [bind], n <- punnedPatternFields lhs]

-- | The methods a derived instance of the class binds.  Of the names the plugin trusts only '<>' is a
-- class method; Monoid's count too, as a modifier binding may share a name with one.
derivedMethods :: String -> [String]
derivedMethods = \case
  "Semigroup" -> ["<>", "sconcat", "stimes"]
  "Monoid"    -> ["mempty", "mappend", "mconcat"]
  _           -> []

-- | A binding with guards is not read, nor one with where bindings, which could
-- shadow a name the plugin reads in its equation.
oneEquation :: [HsBind GhcPs] -> Maybe ([LPat GhcPs], LHsExpr GhcPs)
oneEquation = \case
  [FunBind{ fun_matches = MG{ mg_alts = L _ [L _ (Match _ _ pats GRHSs{ grhssGRHSs = [L _ (GRHS _ [] rhs)], grhssLocalBinds = local })] } }]
    | bindsNothing local -> Just (pats, rhs)
  _ -> Nothing

plainBinding :: [HsBind GhcPs] -> Maybe (LHsExpr GhcPs)
plainBinding binds = case oneEquation binds of
  Just ([], rhs) -> Just rhs
  _              -> Nothing

bindsNothing :: HsLocalBinds GhcPs -> Bool
bindsNothing = \case
  EmptyLocalBinds _  -> True
  HsValBinds _ binds -> isEmptyValBinds binds
  HsIPBinds _ _      -> False


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
      own        = ownDefinitions rm typeName
      result = do
        when (not (null own)) (Left own)
        when (not (null fieldErrs) && not (allowUnknownColumnWrapper cli))
             (Left fieldErrs)
        when (isNothing metaM) (Left [MISSING_MODEL_META typeName])
        nameLit <- case metaM >>= rmmTableNameLit of
          Just s  -> Right s
          Nothing -> case Map.lookup typeName (tableNameOverrides cli) of
            Just s  -> Right s
            Nothing -> Left [UNRESOLVABLE_TABLE_NAME typeName]
        overrides <- extractOverrides cli typeName metaM (rmTopBinders rm)
        (tableType, schemaName) <- metaFields typeName metaM
        (pkInfo, pkNames) <- case tabM of
          Just RawTableInst{ rtiPkPats = pats, rtiPkBody = Just body } ->
            (\info -> (info, namesReadIn pats body)) <$> extractPrimaryKey typeName pats body
          _ -> Left [UNRESOLVABLE_PRIMARY_KEY typeName]
        let unknown = [c | c <- pkColumns pkInfo, c `notElem` map rfName (rtFields rt)]
            keyErrs = [UNKNOWN_PRIMARY_KEY_FIELD typeName c | c <- unknown]
              <> [ REDEFINED_NAME typeName n
                 | n <- pkNames, n `notElem` unknown, any notField (Map.findWithDefault [] n (rmTopBinders rm)) ]
        when (not (null keyErrs)) (Left keyErrs)
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
          , modelTableType  = tableType
          , modelSchemaName = schemaName
          , primaryKey      = pkInfo
          , columns         = cols
          }
  in (typeName, result)

-- | Definitions in this module that change how the plugin must read the table: names it matches
-- as written, and Generic instances, from which Beam names the columns.  The table's own must be a
-- stock derivation, or the one the large-records plugin generates, and its Beamable methods must
-- come from that representation.
ownDefinitions :: RawModule -> String -> [SqlSchemaError]
ownDefinitions rm typeName =
  [ REDEFINED_NAME typeName n
  | n <- ["C", "Columnar", "Maybe", "TableEntity"], Set.member n (rmDataDecls rm) || Set.member n (rmTypeDecls rm) ]
  <> [ REDEFINED_NAME typeName n
     | n <- "Just" : "Nothing" : tableTypeConstructors, Map.member n (rmTopBinders rm) ]
  <> [ UNEXTRACTED_TABLE typeName reasonGenericNotStock
     | not (Set.member typeName (rmDerivedGeneric rm) || Set.member typeName (rmLargeRecords rm))
       || any (maybe True mayBeTable) (rmNonStockGeneric rm) ]
  <> [ UNEXTRACTED_TABLE typeName reasonBeamableMethods | any (maybe True mayBeTable) (rmBeamableNotGeneric rm) ]
  where
    -- A type synonym of the module may stand for the table.
    mayBeTable n = n == typeName || Set.member n (rmTypeDecls rm)

-- | Both optional ModelMeta fields, or an error for each unreadable one.
metaFields
  :: String -> Maybe RawModelMeta
  -> Either [SqlSchemaError] (Maybe String, Maybe ModelSchemaName)
metaFields typeName metaM = case (tableTypeE, schemaNameE) of
  (Right tt, Right sn) -> Right (tt, sn)
  _ -> Left ( [UNRESOLVABLE_MODEL_META_FIELD typeName "modelTableType" r | Left r <- [tableTypeE]]
           <> [UNRESOLVABLE_MODEL_META_FIELD typeName "modelSchemaName" r | Left r <- [schemaNameE]] )
  where
    -- No instance is MISSING_MODEL_META, reported before this.
    tableTypeE  = maybe (Right Nothing) rmmTableType metaM
    schemaNameE = maybe (Right Nothing) rmmSchemaName metaM

-- | The names a key's definition uses, its parameters left out: the plugin read
-- each as a field selector, an operator or the key's constructor, which a
-- definition of the name in this module would replace.
namesReadIn :: [Pat GhcPs] -> HsExpr GhcPs -> [String]
namesReadIn pats body = nub [n | n <- variables body, n `notElem` params]
  where
    params = patternVariables pats <> patternVariables body

notField :: TopBinder -> Bool
notField = \case
  TopField -> False
  _        -> True

variables :: Data a => a -> [String]
variables d = case cast d :: Maybe (HsExpr GhcPs) of
  Just (HsVar _ (L _ n)) -> [occToString n]
  _                      -> concat (gmapQ variables d)

patternVariables :: Data a => a -> [String]
patternVariables d = case cast d :: Maybe (Pat GhcPs) of
  Just (VarPat _ (L _ n)) -> [occToString n]
  _                       -> concat (gmapQ patternVariables d)

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
  -> Map.Map String [TopBinder]
  -> Either [SqlSchemaError] (Map.Map String String)
extractOverrides cli typeName metaM binders =
  case maybe ModifierAbsent rmmModifier metaM of
    ModifierAbsent -> Right Map.empty
    ModifierUnreadable ->
      orNoOverrides (UNRECOGNIZED_MODIFIER_SHAPE typeName "modelFieldModification")
    ModifierRhs rhs -> case varRdrName (unLoc rhs) of
      Just rdr | occToString rdr /= "tableModification" -> case rdr of
        -- Qualified means non-local, even when it names this module's own binding.
        Unqual _ | Just e <- modifierBinding binders (occToString rdr) ->
          extractColumnOverrides cli typeName (Map.keysSet binders) (occToString rdr) (unLoc e)
        _ -> orNoOverrides (UNRESOLVABLE_MODIFIER_BINDING typeName (sdocText rdr))
      _ -> extractColumnOverrides cli typeName (Map.keysSet binders) "modelFieldModification" (unLoc rhs)
  where
    orNoOverrides err
      | allowNonStandardModifier cli = Right Map.empty
      | otherwise                    = Left [err]

-- | The binding a modifier name refers to: its only top-level binder, one plain
-- equation.  'checkTableModifications' reads it the same way, so a column rename
-- the DB settings apply through it is one the contract records.
modifierBinding :: Map.Map String [TopBinder] -> String -> Maybe (LHsExpr GhcPs)
modifierBinding binders b = case Map.findWithDefault [] b binders of
  [TopValue bind] -> plainBinding [bind]
  _               -> Nothing


-- | Parse @tableModification { f1 = fieldNamed "c1", … }@.  Any other
-- shape is rejected unless 'allowNonStandardModifier' is set, because
-- silently treating "I don't recognise this binding" as "no overrides"
-- would emit Beam's default names for overridden columns — a wrong YAML with
-- no signal.
extractColumnOverrides
  :: CliOptions
  -> String
  -> Set String                      -- names the module binds at top level
  -> String                          -- name of the *TMod binding
  -> HsExpr GhcPs
  -> Either [SqlSchemaError] (Map.Map String String)
extractColumnOverrides cli typeName defined bindingName expr = case stripPar expr of
  HsVar _ (L _ n)
    | occToString n == "tableModification" -> readThrough ["tableModification"] (Right Map.empty)
  RecordUpd _ (L _ base) flds
    | varName base == Just "tableModification"
    , Just upds <- regularRecUpdFields flds ->
        let (errs, ok) = foldl' classify ([], Map.empty) upds
        in readThrough ("tableModification" : ["fieldNamed" | not (Map.null ok)])
             (if null errs then Right ok else Left errs)
  _ ->
    if allowNonStandardModifier cli
      then Right Map.empty
      else Left [UNRECOGNIZED_MODIFIER_SHAPE typeName bindingName]
  where
    -- The names this reading matched: a definition of one in this module could
    -- be what the modifier means.
    readThrough names result = case filter (`Set.member` defined) names of
      []        -> result
      redefined -> Left [REDEFINED_NAME typeName n | n <- redefined]
    -- A punned field's value is a variable, so it is no literal either.
    classify (es, acc) (L _ (RecField (L _ lbl) (L _ rhs) _)) =
      let fieldName = ambiguousFieldName lbl
      in case fieldNamedLiteral rhs of
           Just c  -> (es, Map.insert fieldName c acc)
           Nothing -> (es <> [UNRESOLVABLE_COLUMN_NAME typeName fieldName], acc)
    classify (es, acc) _ = (es <> [UNRECOGNIZED_MODIFIER_SHAPE typeName bindingName], acc)


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
            , pkColumns     = [sdocText fldN]
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
        | Just fname <- accessorName f
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
        | isDotOp op -> Just (sdocText n)
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
      f <- accessorName final
      Just [f]
    traverseSplit (mid : rest) = do
      f <- case stripPar mid of
        HsApp _ (L _ fE) (L _ x) -> do
          n <- accessorName fE
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
    cols  <- mapM accessorName accessors
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


-- | A key field as written: a qualified name stays qualified, so it is none
-- of the table's own fields.
accessorName :: HsExpr GhcPs -> Maybe String
accessorName = fmap sdocText . varRdrName


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

-- | The fields of a plain record update; 'Nothing' for OverloadedRecordUpdate,
-- under which an update calls the setField in scope.
#if __GLASGOW_HASKELL__ >= 908
regularRecUpdFields :: LHsRecUpdFields GhcPs -> Maybe [LHsRecUpdField GhcPs GhcPs]
regularRecUpdFields = \case
  RegularRecUpdFields _ fs   -> Just fs
  OverloadedRecUpdFields _ _ -> Nothing
#else
regularRecUpdFields
  :: Either [LHsRecUpdField GhcPs] [LHsRecUpdProj GhcPs] -> Maybe [LHsRecUpdField GhcPs]
regularRecUpdFields = either Just (const Nothing)
#endif

-- GHC 9.4 moved the untyped quotes, name quotes included, from HsBracket to HsQuote.
#if __GLASGOW_HASKELL__ >= 904
type UntypedQuote = HsQuote GhcPs
#else
type UntypedQuote = HsBracket GhcPs
#endif

-- A field of a record update expression; GHC 9.8 gave it a second index.
#if __GLASGOW_HASKELL__ >= 908
type UpdField = LHsRecUpdField GhcPs GhcPs
#else
type UpdField = LHsRecUpdField GhcPs
#endif

-- The type a @{-# ANN type T e #-}@ annotates, and @e@; GHC 9.6 moved the source text into the extension field.
typeAnnotation :: AnnDecl GhcPs -> Maybe (RdrName, HsExpr GhcPs)
#if __GLASGOW_HASKELL__ >= 906
typeAnnotation (HsAnnotation _ (TypeAnnProvenance (L _ t)) (L _ e)) = Just (t, e)
#else
typeAnnotation (HsAnnotation _ _ (TypeAnnProvenance (L _ t)) (L _ e)) = Just (t, e)
#endif
typeAnnotation _ = Nothing

-- A field of a record pattern; a punned one binds its label.
#if __GLASGOW_HASKELL__ >= 904
type PatRecField = HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs)
#else
type PatRecField = HsRecField' (FieldOcc GhcPs) (LPat GhcPs)
#endif


-- ---------------------------------------------------------------------------
-- Small AST helpers
-- ---------------------------------------------------------------------------

sdocText :: Outputable a => a -> String
sdocText = showSDocUnsafe . ppr

occToString :: RdrName -> String
occToString = sdocText . rdrNameOcc

lineCol :: SrcSpan -> String
lineCol sp = case srcSpanToRealSrcSpan sp of
  Just r  -> show (srcSpanStartLine r) <> ":" <> show (srcSpanStartCol r)
  Nothing -> sdocText sp

-- | Matches the constructors directly: the RdrName accessor was renamed in GHC 9.8.
ambiguousFieldName :: AmbiguousFieldOcc GhcPs -> String
ambiguousFieldName = \case
  Unambiguous _ (L _ n) -> occToString n
  Ambiguous   _ (L _ n) -> occToString n

ambiguousFieldRdr :: AmbiguousFieldOcc GhcPs -> RdrName
ambiguousFieldRdr = \case
  Unambiguous _ (L _ n) -> n
  Ambiguous   _ (L _ n) -> n

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

-- | The span of the name in what 'varRdrName' reads, when its OccName is @name@.
varSpanNamed :: String -> HsExpr GhcPs -> Maybe SrcSpan
varSpanNamed name = \case
  HsVar _ (L l n) | hasOcc name n -> Just (GHC.locA l)
  Par (L _ e)                     -> varSpanNamed name e
  ExprWithTySig _ (L _ e) _       -> varSpanNamed name e
  _                               -> Nothing

hasOcc :: String -> RdrName -> Bool
hasOcc name rdr = occNameString (rdrNameOcc rdr) == name

-- | A variable with this OccName, any qualifier, under parentheses only: a
-- type annotation could change what it means.
isVar :: String -> HsExpr GhcPs -> Bool
isVar name e = case stripPar e of
  HsVar _ (L _ n) -> hasOcc name n
  _               -> False

-- | A field from the instance's bindings of it.  None is the class default,
-- 'Nothing'; anything but one equation with no arguments, guards or @where@
-- bindings is 'Left' (rendered), since 'Nothing' would misroute the table.
metaField :: (HsExpr GhcPs -> Maybe (Maybe a)) -> [HsBind GhcPs] -> Either String (Maybe a)
metaField readRhs = \case
  [] -> Right Nothing
  binds | Just (L _ rhs) <- plainBinding binds -> maybe (Left (rendered rhs)) Right (readRhs rhs)
  binds -> Left (intercalate "\n" (map rendered binds))
  where
    -- GHC indents an expression with a type signature.
    rendered :: Outputable o => o -> String
    rendered = T.unpack . T.strip . T.pack . sdocText

-- | @Nothing@ or @Just CON@ for a @TableType@ constructor, recorded
-- unqualified.  Any other name, e.g. a pattern synonym for one, is not the
-- constructor the validator routes on.
tableTypeOf :: HsExpr GhcPs -> Maybe (Maybe String)
tableTypeOf = maybeShape $ \case
  HsVar _ (L _ rn) | occToString rn `elem` tableTypeConstructors -> Just (occToString rn)
  _                                                               -> Nothing

-- | @Nothing@, @Just "lit"@ or @Just name@ (a value name).  The validator reads
-- the name's qualifier, so it is rendered from the 'RdrName', not its 'OccName'.
schemaNameOf :: HsExpr GhcPs -> Maybe (Maybe ModelSchemaName)
schemaNameOf = maybeShape $ \case
  HsLit _ (HsString _ fs)                     -> Just (SchemaLiteral (unpackFS fs))
  HsVar _ (L _ rn) | isVarOcc (rdrNameOcc rn) -> Just (SchemaConfig (sdocText rn))
  _                                           -> Nothing

-- | Names are matched as written: nothing is resolved at parse time, so a pattern synonym called @Just@ reads as @Just@.
maybeShape :: (HsExpr GhcPs -> Maybe a) -> HsExpr GhcPs -> Maybe (Maybe a)
maybeShape inner expr = case peel expr of
  HsVar _ (L _ n) | occToString n == "Nothing" -> Just Nothing
  HsApp _ (L _ j) (L _ x)
    | varName j == Just "Just"                 -> Just <$> inner (peel x)
  _                                            -> Nothing
  where
    peel = \case
      Par (L _ e)               -> peel e
      ExprWithTySig _ (L _ e) _ -> peel e
      e                         -> e


-- ---------------------------------------------------------------------------
-- Type-check cross-check
-- ---------------------------------------------------------------------------

-- | Where GHC says a name is defined: the package of its unit, 'Nothing' when no
-- package of the package database provides the unit (the package being compiled,
-- say), and its module.
data Origin = Origin (Maybe String) String
  deriving (Eq, Show)

-- | Whether a name is defined in this (package, module), as 'beamCoreModule' gives one.
fromModule :: Maybe Origin -> (String, String) -> Bool
fromModule origin (pkg, m) = origin == Just (Origin (Just pkg) m)

renderOrigin :: Maybe Origin -> String
renderOrigin = \case
  Just (Origin (Just pkg) m) -> "module " <> m <> " of package " <> pkg
  Just (Origin Nothing m)    -> "module " <> m <> ", which no package of the package database provides"
  Nothing                    -> "no module"

-- | A table's modelTableType or modelSchemaName binding as GHC resolved it, under the
-- parentheses and type signatures the parse step peels.
data ResolvedField
  = FieldAbsent
    -- ^ The instance does not bind it, so it is the class's default.
  | FieldNothing
    -- ^ base's @Nothing@.
  | FieldJust ResolvedValue
    -- ^ base's @Just@ applied to this.
  | FieldForeign String (Maybe Origin)
    -- ^ A @Just@ or @Nothing@ that is not base's, and where it is defined.
  | FieldOther String
    -- ^ Any other shape, rendered.
  deriving (Eq, Show)

data ResolvedValue
  = ValueConstructor String (Maybe Origin) String
    -- ^ A data constructor, its type constructor's origin and its type constructor.
  | ValuePatternSynonym String (Maybe Origin)
  | ValueVariable String
  | ValueLiteral String
  | ValueOther String
    -- ^ Anything else, rendered.
  deriving (Eq, Show)

-- | A staged column's record field as GHC types it, type synonyms expanded.
data ResolvedColumn
  = ColumnMissing
    -- ^ The table's record has no field of that name.
  | ColumnColumnar Bool String
    -- ^ beam-core's @Columnar@ applied to the table's column functor and a type,
    --   whether that type is base's @Maybe t@, and the field's type rendered.
  | ColumnOther String
    -- ^ Why it is neither.
  deriving (Eq, Show)

-- | The type constructor heading an instance's last type, as GHC resolves it.
data InstanceHead
  = HeadLocal String
  | HeadForeign String
  | HeadKey
    -- ^ beam-core's @PrimaryKey@ family.
  | HeadNone
  deriving (Eq, Show)

-- | The errors for the module's instances ('tcg_insts': class name, head) of a table the
-- parse step did not stage, such as one Template Haskell generated, or of a type this
-- module does not declare.  A key's (@PrimaryKey t@) instance is for no table.
unextractedInstances :: [String] -> [(String, InstanceHead)] -> [SqlSchemaError]
unextractedInstances staged insts = nub
  [ err
  | (cls, h) <- insts
  , isJust (classModule cls)
  , err <- case h of
      HeadNone      -> [UNREADABLE_INSTANCE_HEAD cls noTyConHead]
      HeadKey       -> []
      HeadForeign t -> [UNEXTRACTED_TABLE t reasonForeignHead]
      HeadLocal t   -> [UNEXTRACTED_TABLE t reasonUnseenInstance | t `notElem` staged]
  ]

-- | The errors for the instances of a class the parse step reads by name that is not
-- defined in that class's package and module ('classModule').
unexpectedClasses :: [(String, Maybe String, Maybe Origin)] -> [SqlSchemaError]
unexpectedClasses insts = nub
  [ UNEXPECTED_CLASS cls (fromMaybe noTyConHead mhead) (renderOrigin origin)
  | (cls, mhead, origin) <- insts
  , Just expected <- [classModule cls]
  , not (fromModule origin expected)
  ]

noTyConHead :: String
noTyConHead = "a type with no type constructor at its head"

-- | The errors for a staged table whose modelTableType and modelSchemaName, as GHC
-- resolved them, are not the values the parse step read and staged.
routingMismatches :: TableSchema -> ResolvedField -> ResolvedField -> [SqlSchemaError]
routingMismatches t tableType schemaName =
  [ MODEL_META_FIELD_MISMATCH (codeName t) "modelTableType" (mismatch staged tableType)
  | let staged = maybe "Nothing" ("Just " <>) (modelTableType t)
  , not (tableTypeAgrees (modelTableType t) tableType) ]
  <> [ MODEL_META_FIELD_MISMATCH (codeName t) "modelSchemaName" (mismatch staged schemaName)
     | let staged = maybe "Nothing" (("Just " <>) . schemaText) (modelSchemaName t)
     , not (schemaNameAgrees (modelSchemaName t) schemaName) ]
  where
    mismatch staged r = "the plugin read '" <> staged <> "', which GHC resolves to " <> describeField r
    schemaText = \case
      SchemaLiteral s -> show s
      SchemaConfig v  -> v

-- | An absent binding agrees with 'Nothing': the class default is @Nothing@, and
-- 'unexpectedClasses' reports a class that is not the library's.
tableTypeAgrees :: Maybe String -> ResolvedField -> Bool
tableTypeAgrees staged r = case (staged, r) of
  (Nothing, FieldAbsent)  -> True
  (Nothing, FieldNothing) -> True
  (Just c, FieldJust (ValueConstructor c' origin "TableType")) -> c' == c && fromModule origin sequelizeModule
  _ -> False

-- | The staged value name keeps its qualifier as written; GHC's has none.
schemaNameAgrees :: Maybe ModelSchemaName -> ResolvedField -> Bool
schemaNameAgrees staged r = case (staged, r) of
  (Nothing, FieldAbsent)  -> True
  (Nothing, FieldNothing) -> True
  (Just (SchemaLiteral s), FieldJust (ValueLiteral s')) -> s' == s
  (Just (SchemaConfig v), FieldJust (ValueVariable n))  -> n == v || T.pack ('.' : n) `T.isSuffixOf` T.pack v
  _ -> False

describeField :: ResolvedField -> String
describeField = \case
  FieldAbsent           -> "no binding, the class's default"
  FieldNothing          -> "base's Nothing"
  FieldJust v           -> "base's Just applied to " <> describeValue v
  FieldForeign n origin -> "the '" <> n <> "' of " <> renderOrigin origin <> ", not base's"
  FieldOther e          -> "'" <> e <> "', which is not Nothing or Just applied to a value"

describeValue :: ResolvedValue -> String
describeValue = \case
  ValueConstructor c origin tc -> "the constructor " <> c <> " of the type " <> tc <> " of " <> renderOrigin origin
  ValuePatternSynonym p origin -> "the pattern synonym " <> p <> " of " <> renderOrigin origin
  ValueVariable v              -> "the variable " <> v
  ValueLiteral s               -> "the string literal " <> show s
  ValueOther e                 -> "'" <> e <> "'"

-- | The errors for a staged table's columns whose record fields, as GHC types them,
-- are not what the parse step read: its nullability rule, a @Maybe@ at the head of
-- the type inside @Columnar f@ or @C f@, any qualifier, read as written.
columnMismatches :: String -> [(ColumnInfo, ResolvedColumn)] -> [SqlSchemaError]
columnMismatches t cols =
  [ COLUMN_TYPE_MISMATCH t (hsField c) why
  | (c, r) <- cols
  , why <- case r of
      ColumnMissing -> ["GHC finds no field of that name in the table's record"]
      ColumnColumnar isMaybe ty
        | isMaybe == nullable c -> []
        | isMaybe   -> ["the plugin read it as not nullable, but GHC, expanding type synonyms, types it as '" <> ty <> "', with a base Maybe inside Columnar"]
        | otherwise -> ["the plugin read it as nullable, but GHC, expanding type synonyms, types it as '" <> ty <> "', with no base Maybe at the head of the type inside Columnar"]
      ColumnOther why -> [why]
  ]

-- | The errors for the staged tables annotated @{-# ANN type T largeRecord #-}@, each given as
-- the annotated type's name, where GHC says that @largeRecord@ is defined and where the type
-- constructor of its type is, whose @largeRecord@ or its options type is not the large-records
-- package's: the parse step takes the annotation, by name, for that plugin's.
foreignLargeRecords :: [String] -> [(String, Maybe Origin, Maybe Origin)] -> [SqlSchemaError]
foreignLargeRecords staged anns = nub
  [ UNEXTRACTED_TABLE t reasonForeignLargeRecord
  | (t, origin, typeOrigin) <- anns
  , t `elem` staged
  , not (all fromLargeRecords [origin, typeOrigin])
  ]
  where
    fromLargeRecords = \case
      Just (Origin (Just "large-records") _) -> True
      _                                      -> False

-- | A table's GHC.Generics instance as GHC resolves it.
data ResolvedGeneric
  = GenericSelectors [Maybe String]
    -- ^ One instance, whose representation is one record constructor: its selectors in
    --   order, 'Nothing' for a field without one.
  | GenericMetadata [String]
    -- ^ One instance, whose representation is large-generics' @ThroughLRGenerics@ of the
    --   table, as large-records generates it: the field names of its @MetadataOf@, in order.
  | GenericInstances [Maybe Origin] [Maybe Origin]
    -- ^ Not exactly one instance: where those that match and those that could are defined.
  | GenericOther String
    -- ^ Why its representation is not one record constructor's.
  deriving (Eq, Show)

-- | Beam pairs a table's columns with its representation's selectors, or a large-records
-- table's metadata names, by position, so they must be all the record's fields (@labels@)
-- in order; the staged columns are those fields, in order, less any the plugin leaves out
-- when @leftOut@ (allowUnknownColumnWrapper) allows it.
genericMismatches :: Bool -> String -> [String] -> [String] -> ResolvedGeneric -> [SqlSchemaError]
genericMismatches leftOut t labels staged = \case
  GenericSelectors sels
    | sels == map Just labels -> stagedInOrder
    | otherwise -> [ GENERIC_FIELD_MISMATCH t ("its representation's selectors, in order, are " <> listed sels
                                               <> ", not the record's fields " <> listed fields) ]
  GenericMetadata names
    | names == labels -> stagedInOrder
    | otherwise -> [ GENERIC_FIELD_MISMATCH t ("its large-records metadata names, in order, are " <> listed (map Just names)
                                               <> ", not the record's fields " <> listed fields) ]
  GenericInstances [] [] -> [GENERIC_FIELD_MISMATCH t "GHC finds no Generic instance for it"]
  GenericInstances matches unifiers ->
    [ GENERIC_FIELD_MISMATCH t ("GHC finds " <> show (length matches) <> " matching and " <> show (length unifiers)
                                <> " possibly matching Generic instances for it, not one (in "
                                <> intercalate "; " (map renderOrigin (matches <> unifiers)) <> ")") ]
  GenericOther why -> [GENERIC_FIELD_MISMATCH t why]
  where
    fields = map Just labels
    listed xs = "[" <> intercalate ", " (map (fromMaybe "(no selector)") xs) <> "]"
    stagedInOrder
      | if leftOut then staged `isSubsequenceOf` labels else staged == labels = []
      | leftOut = [ GENERIC_FIELD_MISMATCH t ("the plugin read its columns as " <> listed (map Just staged)
                                              <> ", which are not among the record's fields " <> listed fields <> " in that order") ]
      | otherwise = [ GENERIC_FIELD_MISMATCH t ("the plugin read its columns as " <> listed (map Just staged)
                                                <> ", not all the record's fields " <> listed fields
                                                <> " in order, and only allowUnknownColumnWrapper leaves a field out") ]

-- | The errors for a table's @cls@ instances at its EntityModification, each given as where
-- it is defined: anything but beam-core's one.
untrustedInstances :: String -> String -> [Maybe Origin] -> [SqlSchemaError]
untrustedInstances cls t = \case
  [o] | fromModule o beamCoreModule -> []
  []  -> [UNEXPECTED_INSTANCE cls t "GHC finds no instance for it"]
  [o] -> [UNEXPECTED_INSTANCE cls t ("GHC finds one, in " <> renderOrigin o)]
  os  -> [ UNEXPECTED_INSTANCE cls t ("GHC finds " <> show (length os) <> " that match or could match it, in "
                                      <> intercalate "; " (map renderOrigin os)) ]
