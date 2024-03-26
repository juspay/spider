{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Logerr.Plugin (plugin) where

import Bag (bagToList)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import Data.List (nub)
import Debug.Trace (traceShowId, trace)
import GHC
  ( GRHS (..),
    GRHSs (grhssGRHSs),
    GenLocated (L),
    GhcTc,
    HsBindLR (..),
    HsConDetails (..),
    HsConPatDetails,
    HsExpr (..),
    HsRecField' (..),
    HsRecFields (..),
    LGRHS,
    LHsExpr,
    LHsRecField,
    LMatch,
    LPat,
    Match (m_grhss),
    MatchGroup (..),
    Name,
    Pat (..),
    PatSynBind (..),
    noLoc, Module (moduleName, moduleUnitId), moduleNameString,
    RdrName(..), Located(..)
  )
import GHC.Hs.Binds
  ( LHsBindLR,
    HsBind
  )
import GhcPlugins (Var (varName), isExportedId, varUnique, getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..), RdrName(..))
import HscTypes (ModSummary (..), hpm_module, HsParsedModule, Hsc)
import Name (nameStableString, pprOccName, occName, nameModule, occNameString, nameOccName, nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin, parsedResultAction)
import TcRnTypes (TcGblEnv (..), TcM)
import TcRnMonad (failWith, addErr, addErrAt, addErrs)
import SrcLoc (mkSrcLoc, mkSrcSpan, decomposeSrcSpan)
import FastString (mkFastString)
import Module (moduleNameFS, unitIdFS, pprModule, moduleUnitId, unitIdString)
import OrdList
import Avail (AvailInfo(..))
import Unique (getUnique)
import UniqSet (pprUniqSet)
import NameSet (emptyNameSet)
import Prelude hiding (id,writeFile)
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Lazy.Char8 (pack, fromStrict)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Exception (try, SomeException)
import Outputable as OP (ppr, showSDocUnsafe, Outputable, text, withPprStyle, PprStyle(..))
import qualified Outputable as OP ((<>))
import GhcPlugins (installCoreToDos, CoreToDo(..), CoreM, ModGuts(..), Bind(..), CoreBind(..), CoreExpr(..), OccName(..), Id(..), Expr(..), getHscEnv, mgLookupModule, hsc_mod_graph, 
                    getDynFlags, noSrcSpan)
import HscTypes (throwOneError)
import ErrUtils (mkPlainErrMsg)

plugin :: Plugin
plugin = defaultPlugin {
  -- parsedResultAction = printTree,
    -- typeCheckResultAction = logerr,
    installCoreToDos = install,
    pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

showOutputable :: (MonadIO m, Outputable a) => a -> m ()
showOutputable = liftIO . putStr . showSDocUnsafe . ppr

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

stop :: TcM ()
stop = failWith (OP.text "Stop here for now")

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    -- Add our custom pass after CoreTidy
    return $ todo ++ [CoreDoPluginPass "MyPlugin" myPluginPass]

throwCompilationError :: String -> CoreM a
throwCompilationError errMsg = do
  dynFlags <- getDynFlags 
  throwOneError $ mkPlainErrMsg dynFlags noSrcSpan (OP.text errMsg)

-- Custom transformation pass
myPluginPass :: ModGuts -> CoreM ModGuts
myPluginPass guts = do
  hscEnv <- getHscEnv
  let moduleName' = moduleNameString $ moduleName $ mg_module guts
      modulePath = maybe "default" ms_hspp_file $ mgLookupModule (hsc_mod_graph hscEnv)(mg_module guts)
      moduleNameFS' = (mkFastString $ modulePath)
    
  binds' <- mapM transformBind (mg_binds guts)
  liftIO $ do
    writeFile ("test/output/" <> moduleName' <> "_core_binds.json") (pack $ show $ binds')
  
  -- Throwing error in this kind of plugin
  -- throwCompilationError "Error Generated through Plugin" 

  return guts

-- Transformation function
transformBind :: CoreBind -> CoreM [(String, [String])]
transformBind exp@(NonRec bndr expr) = do
    let fnName = (show . getUnique) bndr <> ":" <> (nameStableString . varName) bndr
    lst <- processExp expr    
    pure [(fnName, lst)]
transformBind exp@(Rec pairs) =
    concat <$> mapM (\(x, y) -> transformBind $ NonRec x y) pairs

processExp :: Expr Var -> CoreM [String]
processExp (Var v) = let fnName = (show . getUnique) v <> ":" <> (nameStableString . varName) v in pure [fnName]
processExp (Lit l) = pure ["Literal = " <> showS l]
processExp (App expr arg) = do
  ls1 <- processExp expr
  ls2 <- processExp arg
  pure $ ls1 <> ls2
processExp (Lam v expr) = do
  let fnName = (show . getUnique) v <> ":" <> (nameStableString . varName) v
  ls1 <- processExp expr
  pure $ fnName : ls1
processExp (Let bnd expr) = processExp expr
processExp (Case expr v typ alts) = do
  let fnName = (show . getUnique) v <> ":" <> (nameStableString . varName) v
  ls1 <- processExp expr
  pure $ fnName : ls1
processExp (Cast expr coerc) = processExp expr
processExp (Tick tickish expr) = processExp expr
-- processExp (Type typ) = _
-- processExp (Coercion coerc) = _
processExp _ = pure []

printTree :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
printTree _ modSummary pm = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
  let mod = hpm_module pm
  liftIO $ do
    print "\nModule = "
    writeFile ("test/output/" <> moduleName' <> "_mod.hs") (pack $ showS $ mod)
  pure pm

logerr :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
logerr _ modSummary tcEnv = do
  let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      modulePath = ms_hspp_file modSummary
      moduleNameFS' = (mkFastString $ ms_hspp_file modSummary)

  -- showOutputable modSummary
  
  let ls1 = fromOLReverse $ tcg_dus tcEnv
  let pprFn name = let  pkgName = moduleUnitId $ nameModule name
                        moduleName' = moduleNameString $ moduleName $ nameModule name
                        nameStr = occNameString $ nameOccName name
                        qualifiedName = unitIdString pkgName ++ ":" ++ moduleName' ++ "." ++ nameStr
                    in OP.text qualifiedName
  let pprFn1 = OP.text . nameStableString
  let fn1 = (\(defs, uses) -> (showSDocUnsafe $ pprUniqSet pprFn1 (fromMaybe emptyNameSet defs), showSDocUnsafe $ pprUniqSet pprFn1 uses))
  let ls2 = map fn1 ls1
  liftIO $ do
    writeFile ("test/output/" <> moduleName' <> "_defUses.js") (pack $ show ls2)

  -- liftIO $ print "\nRDR_ENV = "
  -- showOutputable $ tcg_rdr_env tcEnv

  -- liftIO $ do 
  --   print "\tcg_type_env tcEnv "
  --   writeFile ("test/output/" <> moduleName' <> "_typeEnv.json") (pack $ showS $ tcg_type_env tcEnv)
  -- showOutputable $ tcg_type_env tcEnv

  liftIO $ do 
    print "\ntcg_rdr_env tcEnv "
    writeFile ("test/output/" <> moduleName' <> "_rdrEnv.js") (pack $ showS $ tcg_rdr_env tcEnv)

  liftIO $ do
    print "\nBinds = "
    writeFile ("test/output/" <> moduleName' <> "_binds.js") (pack $ showS $ tcg_binds tcEnv)

  liftIO $ do
    print "\nBinds = "
    let pprExpNames expo = case expo of
                        Avail nm -> Just $ (show . getUnique) nm <> ":" <> nameStableString nm
                        _ -> Nothing
    writeFile ("test/output/" <> moduleName' <> "_exports.js") (pack $ show $ catMaybes $ map pprExpNames $ tcg_exports tcEnv)
  
  let stmts = (bagToList $ tcg_binds tcEnv) ^? biplateRef :: [LHsExpr GhcTc]
      !ios = catMaybes $ map checkLetBind stmts
  if length ios > 0
    then liftIO $ print ios
    else pure ()

  -- let localBnds = (bagToList $ tcg_binds tcEnv) ^? biplateRef :: [HsBind GhcTc]
  --     !ios2 = catMaybes $ map checkLocalBind localBnds
  -- if length ios2 > 0
  --   then liftIO $ print ios2
  --   else pure ()

  -- liftIO $ print "\nGLOBAL_RDR_ENV = "
  -- liftIO $ writeFile (modulePath <> "/test.json") (encode $ showS $ tcg_rdr_env tcEnv)
  -- showOutputable $ tcg_rdr_env tcEnv

  -- ** Different Ways to Throw Errors **
  -- failWith (OP.text "Triggering compilation failure for logerr")
  -- addErr (OP.text "Triggering compilation failure for logerr")
  -- addErrAt (mkSrcSpan (mkSrcLoc moduleNameFS' 10 1) (mkSrcLoc moduleNameFS' 20 40)) (OP.text "Triggering compilation failure for logerr")
  -- addErrs [
  --   (mkSrcSpan (mkSrcLoc moduleNameFS' 12 1) (mkSrcLoc moduleNameFS' 20 40), OP.text "Triggering compilation failure 1 for logerr"),
  --   (mkSrcSpan (mkSrcLoc moduleNameFS' 15 1) (mkSrcLoc moduleNameFS' 18 20), OP.text "Triggering compilation failure 2 for logerr")
  --   ]

  -- stop
  let localBnds = (bagToList $ tcg_binds tcEnv) ^? biplateRef :: [LHsBindLR GhcTc GhcTc]
      localBnds' = (bagToList $ tcg_binds tcEnv)
  depsMapList <- liftIO $ mapM loopOverLHsBindLR $ localBnds
  liftIO $ do
      print ("\ngenerated dependancy for module: " <> moduleName' <> " at path: " <> modulePath)
      writeFile ("test/output/" <> moduleName' <> "_deps.json") (encode $ nub depsMapList)
  return tcEnv

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [(String, [Maybe String])]
loopOverLHsBindLR bnd@(L _ (FunBind _ id matches _ _)) = do
  -- showOutputable bnd
  -- print $ (nameStableString . varName . unLoc) id
  -- print ("FunBind" :: String)
  let funName = (show . getUnique . varName . unLoc) id <> ":" <> (nameStableString . varName . unLoc) id <> ":" <> (if isExportedId (unLoc id) then "Global" else "Local")
      matchList = mg_alts matches
      list = map processMatch (unLoc matchList)
  pure [(funName, concat list)]
loopOverLHsBindLR bnd@(L _ (PatBind _ _ pat_rhs _)) = do
  -- print ("patBind" :: String)
  let l = concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [("", l)]
loopOverLHsBindLR bnd@(L _ VarBind {var_rhs = rhs}) = do
  -- print ("varBind" :: String)
  pure [("", processExpr rhs)]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  -- print ("absBind" :: String)
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  -- print ("patSynBind PSB" :: String)
  let list = map (\id -> Just $ (show . getUnique . varName) id <> ":" <> (nameStableString . varName) id <> ":" <> (if isExportedId id then "Global" else "Local")) $ processPat def
  pure [("", list)]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  -- print ("patSynBind XPatSynBind" :: String)
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  -- print ("XHsBindsLR" :: String)
  pure []

checkLocalBind :: HsBind GhcTc -> Maybe String
checkLocalBind bnd@(FunBind _ id matches _ _) = do
  Just $ (nameStableString . varName . unLoc) id
checkLocalBind _ = Nothing

checkLetBind :: LHsExpr GhcTc -> Maybe String
checkLetBind (L _ ap@(HsApp _ funl funr)) = do
  if showS funl == "logErrorT"
    then case unLoc funr of
      HsVar _ lvar -> Just $ "FunApp = " <> (nameStableString . varName . unLoc) lvar
      _ -> Nothing
    else Nothing
checkLetBind (L _ ap@(HsDo _ _ exprLStmt)) = do
  Just $ "DoApp = " <> showS ap
checkLetBind (L _ ap@(HsLet _ exprLStmt func)) = do
    Just $ "LetApp = " <> showS ap
checkLetBind _ = Nothing

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> [Maybe String]
processMatch (L _ match) =
  let grhss = m_grhss match
   in concatMap processGRHS $ grhssGRHSs grhss

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [Maybe String]
processGRHS (L _ (GRHS _ _ body)) = processExpr body
processGRHS _ = []

processExpr :: LHsExpr GhcTc -> [Maybe String]
processExpr (L _ (HsVar _ (L _ var))) =
  let name = (show . getUnique . varName) var <> ":" <> (nameStableString $ varName var) <> ":" <> (if isExportedId var then "Global" else "Local")
   in [Just name]
processExpr (L _ (HsUnboundVar _ _)) = []
processExpr (L _ (HsApp _ funl funr)) =
  processExpr funl <> processExpr funr
processExpr (L _ (OpApp _ funl funm funr)) =
  processExpr funl <> processExpr funm <> processExpr funr
processExpr (L _ (NegApp _ funl _)) =
  processExpr funl
processExpr (L _ (HsTick _ _ fun)) =
  processExpr fun
processExpr (L _ (HsStatic _ fun)) =
  processExpr fun
processExpr (L _ (HsWrap _ _ fun)) =
  processExpr (noLoc fun)
processExpr (L _ (HsBinTick _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExplicitList _ _ funList)) =
  concatMap processExpr funList
processExpr (L _ (HsTickPragma _ _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsSCC _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsCoreAnn _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExprWithTySig _ fun _)) =
  processExpr fun
processExpr (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap processExpr stmts
processExpr (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExpr func <> nub (concatMap processExpr stmts)
processExpr (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl] <> stmts)
processExpr (L _ (ExplicitSum _ _ _ fun)) = processExpr fun
processExpr (L _ (SectionR _ funl funr)) = processExpr funl <> processExpr funr
processExpr (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
processExpr (L _ (HsPar _ fun)) = processExpr fun
processExpr (L _ (HsAppType _ fun _)) = processExpr fun
processExpr (L _ (HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmtsR)
processExpr (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [Maybe String]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr fun

processPat :: LPat GhcTc -> [Var]
processPat (L _ pat) = case pat of
  ConPatIn _ details -> processDetails details
  VarPat _ (L _ var) -> [var]
  ParPat _ pat' -> processPat pat'
  _ -> []

processDetails :: HsConPatDetails GhcTc -> [Var]
processDetails (PrefixCon args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [Var]
processPatField (L _ HsRecField {hsRecFieldArg = arg}) = processPat arg