{-# LANGUAGE DeriveAnyClass,DeriveGeneric,OverloadedStrings,ScopedTypeVariables,TypeApplications,TypeFamilies,FlexibleContexts,CPP #-}
module CaseExtract.Plugin (plugin) where

#if __GLASGOW_HASKELL__ >= 900

import GHC.Plugins hiding ((<>),Case)
import GHC.Core.ConLike
import GHC.Tc.Types
import GHC.Driver.Plugins
import GHC.Types.Var
import GHC.Utils.Misc
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC
import GHC.Data.Bag (bagToList)

import GHC.Types.Basic
import GHC
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..),purePlugin)
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import qualified Data.IntMap.Internal as IntMap
import GHC as GhcPlugins
import GHC.Core.DataCon as GhcPlugins
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon as GhcPlugins
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc)
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Unit.Module.ModGuts
import GHC.Types.Name.Reader
import GHC.Types.Id
import GHC.Data.FastString
import GHC.Types.Literal

#else

import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn
import DataCon
import GHC.Generics (Generic)
import GHC.Hs
import GHC.Hs.Decls
import GHC.IO (unsafePerformIO)
import GhcPlugins (
    Arg (..),
    CommandLineOption,
    DataCon,
    DynFlags,
    FunctionOrData (..),
    HsParsedModule (..),
    Hsc,
    LitNumType (..),
    Literal (..),
    ModGuts (mg_binds, mg_loc, mg_module),
    ModSummary (..),
    Module (moduleName),
    Name,
    NamedThing (getName),
    Outputable (..),
    Plugin (..),
    SDoc,
    TyCon,
    Var,
    coVarDetails,
    collectArgs,
    dataConFieldLabels,
    dataConName,
    dataConOrigArgTys,
    dataConRepType,
    defaultPlugin,
    flLabel,
    getDynFlags,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    msHsFilePath,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDoc,
    showSDocUnsafe,
    tyConDataCons,
    tyConKind,
    tyConName,
    tyVarKind,
    typeEnvElts,
    unpackFS,
 )
import Id (Id, idType, isExportedId)
import Name (getSrcSpan)
import SrcLoc
import TcRnMonad
import TcRnTypes
import TyCoRep
import Unique (mkUnique)
import Var (isLocalId, varName, varType)
#endif

import qualified Data.Map.Strict as Map
import Data.List (intercalate, nub)
import System.IO (writeFile)
import Control.Monad (forM_, forM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath
import System.Directory.Internal.Prelude hiding (mapM, mapM_)

-- data ConditionalInfo = ConditionalInfo
--   { condLoc      :: SrcSpan        -- ^ Source location
--   , condNesting  :: Int            -- ^ Nesting level
--   , condType     :: CondType       -- ^ Type of conditional
--   , condScrutiny :: Maybe String   -- ^ The expression being evaluated
--   , condRelations :: [String]      -- ^ The patterns or conditions
--   , condActions  :: [String]       -- ^ The expressions to execute
--   , condFuncCalls :: [[String]]    -- ^ Function calls in each action
--   } deriving (Show)

-- instance Aeson.ToJSON ConditionalInfo where
--   toJSON ci = Aeson.object
--     [ "location" Aeson..= showSrcSpan (condLoc ci)
--     , "nesting_level" Aeson..= condNesting ci
--     , "conditional_type" Aeson..= show (condType ci)
--     , "scrutiny" Aeson..= condScrutiny ci
--     , "relations" Aeson..= condRelations ci
--     , "actions" Aeson..= condActions ci
--     , "function_calls" Aeson..= [Aeson.object [("branch", Aeson.toJSON i), ("calls", Aeson.toJSON calls)] 
--                                 | (i, calls) <- zip ([1..] :: [Int]) (condFuncCalls ci)]
--     ]

-- -- | Types of conditional statements we track
-- data CondType = IfThenElse | Case | Guard | PatternMatch | MultiWayIf
--   deriving (Show, Eq)

-- | Main plugin entry point
plugin :: Plugin
plugin = defaultPlugin
  { 
    -- typeCheckResultAction = analyzeConditions
  pluginRecompile = purePlugin
  , installCoreToDos = install
  }

-- -- | Main analysis function for the plugin
-- analyzeConditions :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
-- analyzeConditions opts modSummary tcEnv = do
--   dflags <- getDynFlags
--   let modName = moduleName $ ms_mod modSummary
--       binds = bagToList (tcg_binds tcEnv)
      
--   -- Analyze each binding
--   results <- forM binds $ \(bind) -> loopBinds bind
      
--   -- Output results to a JSON file
--   liftIO $ writeResultsJSON (moduleNameString modName) $ concat results
  
--   -- Return unchanged environment
--   return (tcEnv)

-- getLoc' = (la2r . getLoc)

-- loopBinds :: LHsBindLR GhcTc GhcTc -> TcM [(T.Text,[ConditionalInfo])]
-- loopBinds (L _ AbsBinds{abs_binds = binds}) = do
--   res <- mapM (loopBinds) $ bagToList binds
--   pure $ concat res
-- loopBinds (L location bind) =
--     case bind of
--         (FunBind _ id matches _) -> do
--             funName <- pure $ T.pack $ getOccString $ unLoc id
--             fName <- pure $ T.pack $ nameStableString $ getName id
--             name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr . la2r . getLoc) id)))
--             let (exprList :: [LHsExpr GhcTc]) = concat $ map (\x -> x ^? biplateRef ) $ unLoc $ mg_alts matches
--             conditionals <- mapM (\expr -> collectConditionals 0 expr) exprList
--             pure [(name,concat conditionals)]
--         x -> liftIO $ print (showSDocUnsafe $ ppr x) *> pure []


-- -- | Collects all conditional statements in an expression with their nesting levels
-- collectConditionals :: Int -> LHsExpr GhcTc -> TcM [ConditionalInfo]
-- collectConditionals nestingLevel expr = case unLoc expr of
--   -- If-then-else expression
--   HsIf _ cond thenExpr elseExpr -> do
--     dflags <- getDynFlags
--     let condStr = showSDoc dflags (ppr cond)
--         thenStr = showSDoc dflags (ppr thenExpr)
--         elseStr = showSDoc dflags (ppr elseExpr)
--         thenFuncs = extractFunctionCalls dflags thenExpr
--         elseFuncs = extractFunctionCalls dflags elseExpr
--         thisCondInfo = ConditionalInfo 
--                       { condLoc = getLocA expr
--                       , condNesting = nestingLevel
--                       , condType = IfThenElse
--                       , condScrutiny = Nothing
--                       , condRelations = [condStr, "otherwise"]
--                       , condActions = [thenStr, elseStr]
--                       , condFuncCalls = [thenFuncs, elseFuncs]
--                       }
--     thenConds <- collectConditionals (nestingLevel + 1) thenExpr
--     elseConds <- collectConditionals (nestingLevel + 1) elseExpr
--     return $ thisCondInfo : (thenConds ++ elseConds)
    
--   -- Multi-way if (HsMultiIf)
--   HsMultiIf _ grds -> do
--     dflags <- getDynFlags
    
--     -- Extract guard conditions, actions, and function calls
--     (conditions, actions, funcCalls) <- unzip3 <$> mapM (extractGuardActionAndCalls dflags) grds
    
--     let thisCondInfo = ConditionalInfo 
--                       { condLoc = getLocA expr
--                       , condNesting = nestingLevel
--                       , condType = MultiWayIf
--                       , condScrutiny = Nothing
--                       , condRelations = conditions
--                       , condActions = actions
--                       , condFuncCalls = funcCalls
--                       }
    
--     grdConds <- concat <$> mapM (collectConditionalsInGRHS (nestingLevel + 1)) grds
--     return $ thisCondInfo : grdConds
    
--   -- Case expression
--   HsCase _ scrut alts -> do
--     dflags <- getDynFlags
--     let scrutStr = showSDoc dflags (ppr scrut)
--         thisCondInfo = ConditionalInfo 
--                       { condLoc = getLocA expr
--                       , condNesting = nestingLevel
--                       , condType = Case
--                       , condScrutiny = Just scrutStr
--                       , condRelations = []
--                       , condActions = []
--                       , condFuncCalls = []
--                       }
--     -- (MatchGroup p (LHsExpr p))
--     -- Process alternatives to extract patterns, actions, and function calls
--     (patterns, actions, funcCalls) <- unzip3 <$> mapM (extractPatternActionAndCalls dflags) (unXRec @(GhcTc) $ mg_alts $ alts)
    
--     let updatedCondInfo = thisCondInfo { 
--                               condRelations = patterns, 
--                               condActions = actions,
--                               condFuncCalls = funcCalls
--                           }
    
--     altConds <- concat <$> mapM (collectConditionalsInAlt (nestingLevel + 1)) (unXRec @(GhcTc) $ mg_alts $ alts)
--     return $ updatedCondInfo : altConds
    
--   -- Lambda expression
--   HsLam _ mg -> do
--     matchConds <- collectConditionalsInMatchGroup nestingLevel mg
--     return matchConds
    
--   -- Let expression
--   HsLet _ binds body -> do
--     bindConds <- collectConditionalsInLocalBinds nestingLevel binds
--     bodyConds <- collectConditionals nestingLevel body
--     return $ bindConds ++ bodyConds
    
--   -- Do block
--   HsDo _ ctx stmts -> do
--     stmtConds <- collectConditionalsInStmts nestingLevel ctx (unXRec @(GhcTc) $ stmts)
--     return stmtConds
    
--   -- Application
--   HsApp _ e1 e2 -> do
--     conds1 <- collectConditionals nestingLevel e1
--     conds2 <- collectConditionals nestingLevel e2
--     return $ conds1 ++ conds2
    
--   -- Operator application
--   OpApp _ e1 op e2 -> do
--     conds1 <- collectConditionals nestingLevel e1
--     condOp <- collectConditionals nestingLevel op
--     conds2 <- collectConditionals nestingLevel e2
--     return $ conds1 ++ condOp ++ conds2
    
--   -- Parenthesized expression
--   HsPar _ e -> collectConditionals nestingLevel e
    
--   -- -- Record construction
--   -- RecordCon _ _ fields -> do
--   --   fieldConds <- concat <$> mapM (collectConditionalsInField nestingLevel) fields
--   --   return fieldConds
    
--   -- -- Record update
--   -- RecordUpd _ e fields -> do
--   --   eConds <- collectConditionals nestingLevel e
--   --   fieldConds <- concat <$> mapM (collectConditionalsInField nestingLevel) fields
--   --   return $ eConds ++ fieldConds
    
--   -- -- List comprehension
--   -- ListComp _ e stmts -> do
--   --   eConds <- collectConditionals nestingLevel e
--   --   stmtConds <- concat <$> mapM (collectConditionalsInStmt nestingLevel) stmts
--   --   return $ eConds ++ stmtConds
    
--   -- Section (partial application of operator)
--   SectionL _ x op -> do
--     xConds <- collectConditionals nestingLevel x
--     opConds <- collectConditionals nestingLevel op
--     return $ xConds ++ opConds
    
--   SectionR _ op x -> do
--     opConds <- collectConditionals nestingLevel op
--     xConds <- collectConditionals nestingLevel x
--     return $ opConds ++ xConds
    
--   -- Type application
--   HsAppType _ e _ -> collectConditionals nestingLevel e
    
--   -- Do block with explicit bind syntax
--   HsProc _ _ _ -> return []  -- For simplicity, not handling arrow syntax
    
--   -- -- Infix function application
--   -- InfixApp _ e1 op e2 -> do
--   --   conds1 <- collectConditionals nestingLevel e1
--   --   condOp <- collectConditionals nestingLevel op
--   --   conds2 <- collectConditionals nestingLevel e2
--   --   return $ conds1 ++ condOp ++ conds2
    
--   -- Type-constrained expression
--   ExprWithTySig _ e _ -> collectConditionals nestingLevel e
  
--   -- Dictionary application (used in typeclasses)
--   XExpr _ -> return []
    
--   -- Tuple expression
--   ExplicitTuple _ args _ -> do
--     argConds <- concat <$> mapM (collectConditionalsInTupleArg nestingLevel) (args)
--     return argConds
    
--   -- List expression
--   ExplicitList _ elems -> do
--     elemConds <- concat <$> mapM (collectConditionals nestingLevel) elems
--     return elemConds
    
--   -- Literals and variables don't have conditionals
--   HsLit _ _ -> return []
--   HsOverLit _ _ -> return []
--   HsVar _ _ -> return []
--   HsConLikeOut _ _ -> return []
--   -- HsRecSel _ _ -> return []
    
--   -- Other expressions (incomplete, but covers many cases)
--   _ -> return []

-- -- | Collect conditionals in a match group
-- collectConditionalsInMatchGroup :: Int -> MatchGroup GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInMatchGroup nestingLevel mg = do
--   let matches = mg_alts mg
--   concat <$> mapM (collectConditionalsInMatch nestingLevel) (unLoc matches)

-- -- | Collect conditionals in a single match
-- collectConditionalsInMatch :: Int -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInMatch nestingLevel match = do
--   dflags <- getDynFlags
--   let Match { m_pats = pats, m_grhss = grhss } = unLoc match
      
--   -- Check if this is a pattern match with multiple patterns or guards
--   matchConds <- if length pats > 1 || hasGuards grhss
--                 then do
--                   let patStr = showSDoc dflags (ppr pats)
                  
--                   -- Extract guard conditions if they exist
--                   (guardConds, guardActions, guardFuncCalls) <- extractGuardsInfo dflags grhss
                  
--                   -- Create a pattern match conditional
--                   let patMatchInfo = ConditionalInfo
--                                      { condLoc = getLocA match
--                                      , condNesting = nestingLevel
--                                      , condType = PatternMatch
--                                      , condScrutiny = Nothing
--                                      , condRelations = patStr : guardConds
--                                      , condActions = guardActions
--                                      , condFuncCalls = guardFuncCalls
--                                      }
--                   return [patMatchInfo]
--                 else
--                   return []
                  
--   -- Also collect conditionals from the body
--   bodyConds <- collectConditionalsInGRHSs nestingLevel grhss
--   return $ matchConds ++ bodyConds
  
--   where
--     hasGuards (GRHSs _ grhss _) = any hasGuardStatements grhss
--     hasGuardStatements (L _ (GRHS _ guards _)) = not (null guards)
    
--     extractGuardsInfo dflags (GRHSs _ grhss _) = do
--       results <- forM grhss $ \(L _ (GRHS _ guards body)) -> do
--         let guardStr = if null guards 
--                       then "otherwise" 
--                       else showSDoc dflags (ppr guards)
--             bodyStr = showSDoc dflags (ppr body)
--             funcCalls = extractFunctionCalls dflags body
--         return (guardStr, bodyStr, funcCalls)
--       return (unzip3 results)

-- -- | Collect conditionals in a tuple argument
-- collectConditionalsInTupleArg :: Int -> HsTupArg GhcTc -> TcM [ConditionalInfo]
-- collectConditionalsInTupleArg nestingLevel arg = case arg of
--   (Present _ e) -> collectConditionals nestingLevel e
--   _ -> return []

-- -- | Collect conditionals in a field
-- collectConditionalsInField :: Int -> LHsRecField GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInField nestingLevel field =
--   collectConditionals nestingLevel (hsRecFieldArg (unLoc field))

-- -- | Collect conditionals in guarded right-hand sides
-- collectConditionalsInGRHSs :: Int -> GRHSs GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInGRHSs nestingLevel (GRHSs _ grhss binds) = do
--   grhsConds <- concat <$> mapM (collectConditionalsInGRHS nestingLevel) grhss
--   bindConds <- collectConditionalsInLocalBinds nestingLevel binds
--   return $ grhsConds ++ bindConds

-- -- | Collect conditionals in a single guarded right-hand side
-- collectConditionalsInGRHS :: Int -> LGRHS GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInGRHS nestingLevel grhs = do
--   let GRHS _ guards body = unLoc grhs
--   guardConds <- concat <$> mapM (collectConditionalsInGuard nestingLevel) guards
--   bodyConds <- collectConditionals nestingLevel body
--   return $ guardConds ++ bodyConds

-- -- | Collect conditionals in a guard statement
-- collectConditionalsInGuard :: Int -> LStmt GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInGuard nestingLevel stmt = case unLoc stmt of
--   BodyStmt _ expr _ _ -> collectConditionals nestingLevel expr
--   _ -> return []

-- -- | Collect conditionals in a case alternative
-- collectConditionalsInAlt :: Int -> LMatch GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInAlt nestingLevel alt = collectConditionalsInMatch nestingLevel alt

-- -- | Collect conditionals in local bindings
-- collectConditionalsInLocalBinds :: Int -> HsLocalBinds GhcTc -> TcM [ConditionalInfo]
-- collectConditionalsInLocalBinds nestingLevel binds = case binds of
--   HsValBinds _ valBinds -> collectConditionalsInValBinds nestingLevel valBinds
--   _ -> return []

-- -- | Collect conditionals in value bindings
-- collectConditionalsInValBinds :: Int -> HsValBinds GhcTc -> TcM [ConditionalInfo]
-- collectConditionalsInValBinds nestingLevel valBinds = case valBinds of
--   ValBinds _ binds _ -> 
--     concat <$> mapM (collectConditionalsInBind nestingLevel) (bagToList binds)
--   _ -> pure []
--   -- XValBindsLR binds ->
--   --   concat <$> mapM (collectConditionalsInBind nestingLevel) 
--   --                   (concatMap bagToList (snd (unXRec @(GhcTc) binds)))

-- -- | Collect conditionals in a binding
-- collectConditionalsInBind :: Int -> LHsBind GhcTc -> TcM [ConditionalInfo]
-- collectConditionalsInBind nestingLevel bind = case unLoc bind of
--   FunBind { fun_matches = matches } -> 
--     collectConditionalsInMatchGroup nestingLevel matches
--   PatBind { pat_rhs = rhs } -> 
--     collectConditionalsInGRHSs nestingLevel rhs
--   _ -> 
--     return []

-- -- | Collect conditionals in statement lists (do-blocks)
-- collectConditionalsInStmts :: Int -> HsStmtContext GhcRn -> [LStmt GhcTc (LHsExpr GhcTc)] -> TcM [ConditionalInfo]
-- collectConditionalsInStmts nestingLevel _ stmts = 
--   concat <$> mapM (collectConditionalsInStmt nestingLevel) stmts

-- -- | Collect conditionals in a single statement
-- collectConditionalsInStmt :: Int -> LStmt GhcTc (LHsExpr GhcTc) -> TcM [ConditionalInfo]
-- collectConditionalsInStmt nestingLevel stmt = case unLoc stmt of
--   BindStmt _ _ expr -> collectConditionals nestingLevel expr
--   BodyStmt _ expr _ _ -> collectConditionals nestingLevel expr
--   LetStmt _ binds -> collectConditionalsInLocalBinds nestingLevel binds
--   _ -> return []

-- -- | Extract function calls from expressions
-- extractFunctionCalls :: DynFlags -> LHsExpr GhcTc -> [String]
-- extractFunctionCalls dflags expr = 
--   case unLoc expr of
--     -- Application: function applied to arguments
--     HsApp _ f arg -> 
--       extractFunctionName dflags f ++ 
--       extractFunctionCalls dflags f ++ 
--       extractFunctionCalls dflags arg
      
--     -- Operator application (+, -, etc)
--     OpApp _ left op right -> 
--       extractFunctionName dflags op ++
--       extractFunctionCalls dflags left ++
--       extractFunctionCalls dflags right
      
--     -- If expression
--     HsIf _ cond thenExpr elseExpr ->
--       extractFunctionCalls dflags cond ++
--       extractFunctionCalls dflags thenExpr ++
--       extractFunctionCalls dflags elseExpr
      
--     -- Case expression
--     HsCase _ scrut alts ->
--       extractFunctionCalls dflags scrut ++
--       concatMap (extractFunctionCallsFromAlt dflags) (unXRec @(GhcTc) $ mg_alts $ alts)
      
--     -- Let expression
--     HsLet _ binds body ->
--       extractFunctionCallsFromLocalBinds dflags binds ++
--       extractFunctionCalls dflags body
      
--     -- Do block
--     HsDo _ _ stmts ->
--       concatMap (extractFunctionCallsFromStmt dflags) (unXRec @(GhcTc) $ stmts)
      
--     -- Parenthesized expression
--     HsPar _ e -> extractFunctionCalls dflags e
    
--     -- -- Record construction and updates
--     -- RecordCon _ _ _ fields ->
--     --   concatMap (extractFunctionCallsFromField dflags) fields
--     -- RecordUpd _ expr fields ->
--     --   extractFunctionCalls dflags expr ++
--     --   concatMap (extractFunctionCallsFromField dflags) fields
      
--     -- Section (partial application of operator)
--     SectionL _ x op -> extractFunctionCalls dflags x ++ extractFunctionName dflags op
--     SectionR _ op x -> extractFunctionName dflags op ++ extractFunctionCalls dflags x
      
--     -- Lambda functions
--     HsLam _ mg -> extractFunctionCallsFromMatchGroup dflags mg
    
--     -- Literal expressions don't have function calls
--     HsLit _ _ -> []
--     HsOverLit _ _ -> []
    
--     -- For other cases, just return empty list for now
--     _ -> []

-- -- | Extract function name from expressions
-- extractFunctionName :: DynFlags -> LHsExpr GhcTc -> [String]
-- extractFunctionName dflags expr =
--   case unLoc expr of
--     -- Variable reference (likely a function)
--     HsVar _ (L _ name) -> [showSDoc dflags (ppr name)]
    
--     -- Record selector
--     -- HsRecSel _ field -> [showSDoc dflags (ppr field)]
    
--     -- Constructor used as a function
--     ExplicitTuple _ args _ -> 
--       if null args then ["()"] else []
    
--     ExplicitList _ elems ->
--       if null elems then ["[]"] else []
      
--     -- Operator in parentheses being used as a function
--     HsPar _ e -> extractFunctionName dflags e
      
--     -- If it's not a variable, it might be a complex expression that
--     -- evaluates to a function, but we can't easily extract its "name"
--     _ -> []

-- -- | Extract function calls from match group (used in lambda & case)
-- extractFunctionCallsFromMatchGroup :: DynFlags -> MatchGroup GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromMatchGroup dflags mg =
--   concatMap (extractFunctionCallsFromMatch dflags) (unLoc $ mg_alts mg)

-- -- | Extract function calls from a match
-- extractFunctionCallsFromMatch :: DynFlags -> LMatch GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromMatch dflags match =
--   extractFunctionCallsFromGRHSs dflags (m_grhss (unLoc match))

-- -- | Extract function calls from GRHSs
-- extractFunctionCallsFromGRHSs :: DynFlags -> GRHSs GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromGRHSs dflags (GRHSs _ grhss binds) =
--   concatMap (extractFunctionCallsFromGRHS dflags) grhss ++
--   extractFunctionCallsFromLocalBinds dflags binds

-- -- | Extract function calls from GRHS
-- extractFunctionCallsFromGRHS :: DynFlags -> LGRHS GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromGRHS dflags grhs =
--   let GRHS _ guards body = unLoc grhs
--   in concatMap (extractFunctionCallsFromGuard dflags) guards ++
--      extractFunctionCalls dflags body

-- -- | Extract function calls from case alternative
-- extractFunctionCallsFromAlt :: DynFlags -> LMatch GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromAlt dflags = extractFunctionCallsFromMatch dflags

-- -- | Extract function calls from guard
-- extractFunctionCallsFromGuard :: DynFlags -> LStmt GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromGuard dflags stmt = 
--   case unLoc stmt of
--     BodyStmt _ expr _ _ -> extractFunctionCalls dflags expr
--     _ -> []

-- -- | Extract function calls from local binds
-- extractFunctionCallsFromLocalBinds :: DynFlags -> HsLocalBinds GhcTc -> [String]
-- extractFunctionCallsFromLocalBinds dflags binds =
--   case binds of
--     HsValBinds _ valBinds -> extractFunctionCallsFromValBinds dflags valBinds
--     _ -> []

-- -- | Extract function calls from value binds
-- extractFunctionCallsFromValBinds :: DynFlags -> HsValBinds GhcTc -> [String]
-- extractFunctionCallsFromValBinds dflags valBinds =
--   case valBinds of
--     ValBinds _ binds _ -> 
--       concatMap (extractFunctionCallsFromBind dflags) (bagToList binds)
--     _ -> pure []
--     -- XValBindsLR binds ->
--     --   concatMap (extractFunctionCallsFromBind dflags) 
--     --            (concatMap bagToList (snd (unXRec @(GhcTc) binds)))

-- -- | Extract function calls from bind
-- extractFunctionCallsFromBind :: DynFlags -> LHsBind GhcTc -> [String]
-- extractFunctionCallsFromBind dflags bind =
--   case unLoc bind of
--     FunBind { fun_matches = matches } -> 
--       extractFunctionCallsFromMatchGroup dflags matches
--     PatBind { pat_rhs = rhs } -> 
--       extractFunctionCallsFromGRHSs dflags rhs
--     _ -> []

-- -- | Extract function calls from statement
-- extractFunctionCallsFromStmt :: DynFlags -> LStmt GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromStmt dflags stmt =
--   case unLoc stmt of
--     BindStmt _ _ expr -> extractFunctionCalls dflags expr
--     BodyStmt _ expr _ _ -> extractFunctionCalls dflags expr
--     LetStmt _ binds -> extractFunctionCallsFromLocalBinds dflags binds
--     _ -> []

-- -- | Extract function calls from record field
-- extractFunctionCallsFromField :: DynFlags -> LHsRecField GhcTc (LHsExpr GhcTc) -> [String]
-- extractFunctionCallsFromField dflags field =
--   extractFunctionCalls dflags (hsRecFieldArg (unLoc field))

-- -- | Helper functions to extract patterns, actions, and function calls from case alternatives
-- extractPatternActionAndCalls :: DynFlags -> LMatch GhcTc (LHsExpr GhcTc) -> TcM (String, String, [String])
-- extractPatternActionAndCalls dflags match = do
--   let Match { m_pats = pats, m_grhss = grhss } = unLoc match
--       patStr = intercalate ", " (map (showSDoc dflags . ppr) pats)
--       (bodyStr, funcCalls) = case grhss of
--                   GRHSs _ grhsList _ -> 
--                     case grhsList of
--                       (grhs:_) -> 
--                         let GRHS _ _ body = (unLoc grhs)
--                             bodyString = showSDocUnsafe (ppr body)
--                             calls = extractFunctionCalls dflags body
--                         in (bodyString, calls)
--                       [] -> ("<<empty>>", [])
--   return (patStr, bodyStr, funcCalls)

-- -- | Helper functions to extract conditions, actions, and function calls from guards
-- extractGuardActionAndCalls :: DynFlags -> LGRHS GhcTc (LHsExpr GhcTc) -> TcM (String, String, [String])
-- extractGuardActionAndCalls dflags grhs = do
--   let GRHS _ guards body = unLoc grhs
--       guardStr = case guards of
--                    (g:_) -> showSDoc dflags (ppr g)
--                    [] -> "<<empty>>"
--       bodyStr = showSDoc dflags (ppr body)
--       funcCalls = extractFunctionCalls dflags body
--   return (guardStr, bodyStr, funcCalls)

-- -- | Convert function info to JSON
-- functionToJSON :: (T.Text, [ConditionalInfo]) -> Aeson.Value
-- functionToJSON (fnName, conditionals) = Aeson.object
--   [ "function_name" Aeson..= fnName
--   , "conditionals" Aeson..= conditionals
--   ]

-- -- | Write results to a JSON file
-- writeResultsJSON :: String -> [(T.Text, [ConditionalInfo])] -> IO ()
-- writeResultsJSON modName results = do
--   let filename = modName ++ "_conditionals.json"
--       jsonObj = Aeson.object
--         [ "module" Aeson..= modName
--         , "functions" Aeson..= map functionToJSON results
--         ]
--       jsonBS = AesonPretty.encodePretty' prettyConfig jsonObj
      
--   BS.writeFile filename jsonBS
--   putStrLn $ "Conditional analysis written to " ++ filename
--   where
--     prettyConfig = AesonPretty.defConfig
--       { AesonPretty.confIndent = AesonPretty.Spaces 2
--       , AesonPretty.confCompare = compare
--       }

-- -- | Helper function to show source span in a readable format
-- showSrcSpan :: SrcSpan -> String
-- showSrcSpan span = case span of
--   RealSrcSpan s _ -> 
--     let file = unpackFS (srcSpanFile s)
--         startLine = srcSpanStartLine s
--         startCol = srcSpanStartCol s
--         endLine = srcSpanEndLine s
--         endCol = srcSpanEndCol s
--     in file ++ ":" ++ show startLine ++ ":" ++ show startCol ++ 
--        "-" ++ show endLine ++ ":" ++ show endCol
--   _ -> "unknown location"

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "configExtract" (buildCfgPass args) : todos)


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e


getFilePath :: SrcSpan -> String
#if __GLASGOW_HASKELL__ >= 900
getFilePath (RealSrcSpan rSSpan _) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = showSDocUnsafe $ ppr $ fs
#else
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs
#endif

checkIfExists :: T.Text -> [T.Text] -> Bool
checkIfExists query [] = False
checkIfExists query (x : xs) = if query `T.isInfixOf` x then True else checkIfExists query xs

buildCfgPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
            [] -> "./tmp/configExtract/"
            [local] -> local
            _ -> error "unexpected no of arguments"
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ GhcPlugins.moduleName $ mg_module guts
            moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
        createDirectoryIfMissing True (takeDirectory moduleLoc)
        removeIfExists (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL")
        let fBinds = flattenBinds binds
        coreFunctionsList <-
                    mapM
                        ( \(b, e) -> do
                            let nssName = (T.pack $ nameStableString $ idName b)
                            let funName = (T.pack $ showSDocUnsafe $ ppr $ idName b)
                            allCaseRelations <- pure $ collectAllCaseAndRelations e
                            pure (funName,allCaseRelations)
                        )
                        (fBinds)
        mapM_   ( \(k,v) ->
                        (liftIO . BS.appendFile (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL") . (<> "\n") . Aeson.encode) (Map.singleton k (reverse v))
                    )
                    (coreFunctionsList)
        pure ()
    return guts

getEitherTextTypeFromAlt (DataAlt x) = T.pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt (LitAlt x) = T.pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt _ = "DEFAULT"

collectAllVars :: CoreExpr -> [Id]
collectAllVars d =
    ok d
  where
    ok :: CoreExpr -> [Id]
    ok e = go e

    go (Var v) = [v]
    go (Cast e _) = go e

    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        ok scrut <> concatMap (\((Alt zz lv rhs)) -> getDataAltId zz <> (lv) <> go rhs) alts
#else
        ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getDataAltId zz <> (lv) <> go rhs) alts
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> [b] <> go e) prs
    go (Lit{}) = []
    go (Type{}) = []
    go (Coercion{}) = []

getLitAlt (LitAlt x) = [x]
getLitAlt _ = []

getDataAlt (DataAlt x) = [tyConKind $ dataConTyCon x]
getDataAlt _ = []

getDataAltId (DataAlt x) = [dataConWorkId x]
getDataAltId _ = []

-- collectAllLits :: CoreExpr -> [Literal]
-- collectAllLits d =
--     ok d
--   where
--     ok :: CoreExpr -> [Literal]
--     ok e = go e

--     go (Var v) = []
--     go (Cast e _) = go e
--     go (Case scrut _ _ alts) =
-- #if __GLASGOW_HASKELL__ >= 900
--         ok scrut <> concatMap (\((Alt zz lv rhs)) -> getLitAlt zz <> go rhs) alts
-- #else
--         ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getLitAlt zz <> go rhs) alts
-- #endif
--     go (Tick t e)= go e
--     go (Lam x e) = go e
--     go (App f e) = go f <> go e
--     go (Let (NonRec _ r) e) = go e <> ok r
--     go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
--     go (Lit literal) = [literal]
--     go (Type{}) = []
--     go (Coercion{}) = []

-- collectAllTypes :: CoreExpr -> [Type]
-- collectAllTypes d =
--     ok d
--   where
--     ok :: CoreExpr -> [Type]
--     ok e = go e

--     go (Var v) = []
--     go (Cast e _) = go e
--     go (Case scrut _ _ alts) =
-- #if __GLASGOW_HASKELL__ >= 900
--         ok scrut <> concatMap (\((Alt zz lv rhs)) -> getDataAlt zz <> go rhs) alts
-- #else
--         ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getDataAlt zz <> go rhs) alts
-- #endif
--     go (Tick t e) = go e
--     go (Lam x e) = go e
--     go (App f e) = go f <> go e
--     go (Let (NonRec _ r) e) = go e <> ok r
--     go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
--     go (Lit literal) = []
--     go (Type _type) = [_type]
--     go (Coercion{}) = []


data CaseCollate = CaseCollate (T.Text,T.Text,T.Text) T.Text [(T.Text,[CaseCollate],[T.Text])]
    deriving (Show,Generic)

instance Aeson.ToJSON CaseCollate where
    toJSON (CaseCollate (condition,type_of_condition,inputs) id_details (x:xs)) = Aeson.object ["id_details" Aeson..= Aeson.toJSON id_details,"scrutiny_type" Aeson..= Aeson.toJSON inputs,"case_match_output_type" Aeson..= Aeson.toJSON type_of_condition ,"condition" Aeson..= (T.replace "\n" "" condition), "relations" Aeson..= map (\(r,a,action) -> Aeson.object ["relation" Aeson..= Aeson.toJSON r ,"action"Aeson..= Aeson.toJSON action , "next" Aeson..= Aeson.toJSON a]) (x:xs)]
    toJSON (CaseCollate (condition,type_of_condition,inputs) id_details [x]) = Aeson.object ["id_details" Aeson..= Aeson.toJSON id_details,"scrutiny_type" Aeson..= Aeson.toJSON inputs,"case_match_output_type" Aeson..= Aeson.toJSON type_of_condition,"condition" Aeson..= (T.replace "\n" "" condition), "relations" Aeson..= map (\(r,a,action) -> Aeson.object ["relation" Aeson..= Aeson.toJSON r ,"action"Aeson..= Aeson.toJSON action, "next" Aeson..= Aeson.toJSON a]) [x]]
    toJSON (CaseCollate (condition,type_of_condition,inputs) id_details []) = Aeson.object ["id_details" Aeson..= Aeson.toJSON id_details,"scrutiny_type" Aeson..= Aeson.toJSON inputs,"case_match_output_type" Aeson..= Aeson.toJSON type_of_condition,"condition" Aeson..= (T.replace "\n" "" condition)]


collectAllCaseAndRelations :: CoreExpr -> [CaseCollate]
collectAllCaseAndRelations d =
    ok d
  where
    ok :: CoreExpr -> [CaseCollate]
    ok e = go e

    go (Var v) = []
    go (Cast e _) = go e
    go (Case scrut b _type alts) =
#if __GLASGOW_HASKELL__ >= 900
        (ok scrut) <> ([ CaseCollate
            (T.pack $ showSDocUnsafe $ ppr scrut,(T.pack $ showSDocUnsafe $ ppr _type),T.pack $ showSDocUnsafe $ ppr $ varType b)
            (T.pack $ showSDocUnsafe $ ppr $ nameSrcSpan $ varName b)
            ( map
                ( \(Alt zz _ rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase,(map (T.pack . nameStableString . idName) $ collectAllVars rhs))
                )
                alts
            )
        ])
#else
        (ok scrut) <> [ CaseCollate
            (pack $ showSDocUnsafe $ ppr scrut,T.pack $ showSDocUnsafe $ ppr _type,T.pack $ showSDocUnsafe $ ppr $ varType b)
            (T.pack $ showSDocUnsafe $ ppr $ nameSrcSpan $ varName b)
            ( map
                ( \(zz, _, rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase,(map (pack . nameStableString . idName) $ collectAllVars rhs ))
                )
                alts
            )
        ]
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
    go (Lit literal) = []
    go (Type _type) = []
    go (Coercion{}) = []