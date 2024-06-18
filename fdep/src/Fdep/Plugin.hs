{-# LANGUAGE DataKinds #-}

module Fdep.Plugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import Data.List (nub)
import Debug.Trace (traceShowId)
import GHC
  ( GRHS (..),
    GRHSs (..),
    GenLocated (L),
    HsValBinds (..),
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
    noLoc, moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass, XXExprGhcTc(..), HsWrap(..)
  )
import GHC.Hs.Binds
import Prelude hiding (id,writeFile)
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe)
import Control.Exception (try,SomeException)
import Control.Monad (foldM,when)
import Data.List
import Data.List.Extra (replace,splitOn)
import Data.Maybe (fromJust,isJust,mapMaybe)
import Fdep.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent
import System.Directory
import GHC.Hs.Utils as GHCHs
import GHC.Core.TyCo.Ppr (pprSigmaType)
import Data.Bool (bool)
import qualified Data.Map as Map
import GHC.Tc.Types (TcGblEnv, TcM)
import GHC.Driver.Plugins (CommandLineOption, PluginRecompile(..), pluginRecompile, typeCheckResultAction, Plugin, defaultPlugin)
import GHC.Unit.Module.ModSummary (ms_mod_name, ModSummary (..))
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Types.SrcLoc (unLoc, getLoc)
import GHC.Types.Id (idName, idType)
import GHC.Utils.Outputable (showSDocUnsafe, ppr)
import GHC.Types.Name (pprPrefixName, nameStableString, varName)
import GHC.Plugins (getOccString)
import Language.Haskell.Syntax.Pat (LHsRecUpdField)
import Language.Haskell.Syntax.Expr (LHsRecUpdProj)


plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = fDep
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
  liftIO $ forkIO $ do
      let prefixPath = case opts of
                            []    -> "/tmp/fdep/"
                            local : _ -> local
          moduleName' = moduleNameString $ ms_mod_name modSummary
          modulePath = prefixPath <> ms_hspp_file modSummary
      depsMapList <- mapM loopOverLHsBindLR $ bagToList $ tcg_binds tcEnv
      let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
      print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path)
      createDirectoryIfMissing True path
      writeFile ((modulePath) <> ".json") (encodePretty $ concat depsMapList)
      writeFile ((modulePath) <> ".missing.signatures.json") $
        encodePretty
          $ Map.fromList
            $ map (\element -> (\(x,y) -> (x,typeSignature y)) $ filterForMaxLenTypSig element)
              $ groupBy (\a b -> (srcSpan a) == (srcSpan b))
                $ dumpMissingTypeSignatures tcEnv
  return tcEnv
    where
      filterForMaxLenTypSig :: [MissingTopLevelBindsSignature] -> (String, MissingTopLevelBindsSignature)
      filterForMaxLenTypSig x =
          case x of
            [el] -> (srcSpan $ el,el)
            [el1,el2] -> (srcSpan el1,bool (el2) (el1) ((length $ typeSignature $ el1) > (length $ typeSignature $ el2)))
            (xx:xs) -> (\(y,yy) -> (srcSpan xx,bool (yy) (xx) ((length $ typeSignature $ xx) > (length $ typeSignature $ yy)))) $ filterForMaxLenTypSig xs

dumpMissingTypeSignatures :: TcGblEnv -> [MissingTopLevelBindsSignature]
dumpMissingTypeSignatures gbl_env =
  let binds    =  ((collectHsBindsBinders CollWithDictBinders) $ tcg_binds $ gbl_env)
      whereBinds  = concatMap (\x -> ((concatMap (collectHsBindsBinders CollWithDictBinders) $ processHsLocalBindsForWhereFunctions $ processMatchForWhereFunctions x)))  ((bagToList $ tcg_binds $ gbl_env) ^? biplateRef :: [LMatch GhcTc (LHsExpr GhcTc)])
  in nub $ mapMaybe add_bind_warn (binds <> whereBinds)
  where
    add_bind_warn :: Id -> Maybe MissingTopLevelBindsSignature
    add_bind_warn id
      = let name    = idName id
            ty = (idType id)
            ty_msg  = pprSigmaType ty
        in add_warn (showSDocUnsafe $ ppr $ nameSrcSpan $ getName name) (showSDocUnsafe $ pprPrefixName name) (showSDocUnsafe $ ppr $ ty_msg)

    add_warn "<no location info>" msg ty_msg = Nothing
    add_warn "<wired into compiler>" msg ty_msg = Nothing
    add_warn _ msg "*" = Nothing
    add_warn _ msg "* -> *" = Nothing
    add_warn _ msg ('_':xs) = Nothing
    add_warn name msg ty_msg
      = if "$" `isPrefixOf` msg
          then Nothing
          else Just $ MissingTopLevelBindsSignature { srcSpan = (name), typeSignature = (msg <> " :: " <> ty_msg)}

    processMatchForWhereFunctions :: LMatch GhcTc (LHsExpr GhcTc) -> HsLocalBindsLR GhcTc GhcTc
    processMatchForWhereFunctions (L _ match) = (grhssLocalBinds (m_grhss match))

    processHsLocalBindsForWhereFunctions :: HsLocalBindsLR GhcTc GhcTc -> [LHsBindsLR GhcTc GhcTc]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (ValBinds _ x _)) = [x]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (XValBindsLR (NValBinds x _))) = map (\(_,binds) -> binds) $ x
    processHsLocalBindsForWhereFunctions x = []

transformFromNameStableString :: (Maybe String,Maybe String) -> Maybe FunctionInfo
transformFromNameStableString (Just str,Just loc) =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in Just $ if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) loc else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) loc

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ (FunBind _ id matches _)) = do
  let funName = getOccString $ unLoc id
      matchList = mg_alts matches
  (list,funcs) <- foldM (\(x,y) xx -> do
                                  (l,f) <- processMatch xx
                                  pure $ (x <> l,y <> f)
                        ) ([],[]) (unLoc matchList)
  let listTransformed = map transformFromNameStableString $ nub $ list
  pure [(Function funName listTransformed (nub funcs) (showSDocUnsafe $ ppr $ getLoc id))]
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = do
  let l = map transformFromNameStableString $ concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [(Function "" l [] "")]
loopOverLHsBindLR (L _ VarBind {var_rhs = rhs}) = do
  pure [(Function "" (map transformFromNameStableString $ processExpr rhs) [] "")]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  let list = map transformFromNameStableString $ map (\(n,srcLoc) -> (Just $ traceShowId $ nameStableString n, srcLoc)) $ processPat def
  pure [(Function "" list [] "")]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  pure []

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([(Maybe String,Maybe String)],[Function])
processMatch (L _ match) = do
  whereClause <- processHsLocalBinds $ grhssLocalBinds (m_grhss match)
  pure $ (concatMap processGRHS $ grhssGRHSs (m_grhss match),whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [(Maybe String,Maybe String)]
processGRHS (L _ (GRHS _ _ body)) = processExpr body
processGRHS _ = []

processHsLocalBinds :: HsLocalBindsLR GhcTc GhcTc -> IO [Function]
processHsLocalBinds (HsValBinds _ (ValBinds _ x y)) = do
  res <- mapM loopOverLHsBindLR $ bagToList $ x
  pure $ concat res
processHsLocalBinds (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
  res <- foldM (\acc (recFlag,binds) -> do 
                  funcs <- mapM loopOverLHsBindLR $ bagToList binds
                  pure (acc <> funcs)
                ) [] x
  pure $ concat res
processHsLocalBinds x =
  pure []

processExpr :: LHsExpr GhcTc -> [(Maybe String,Maybe String)]
processExpr x@(L _ (HsVar _ (L _ var))) =
  let name = nameStableString $ getName var
  in [(Just name, Just $ showSDocUnsafe $ ppr $ getLoc x)]
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
processExpr (L loc (XExpr (WrapExpr (HsWrap _ fun)))) =
  processExpr (L loc fun)
processExpr (L _ (HsBinTick _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExplicitList _ funList)) =
  concatMap processExpr funList
-- processExpr (L _ (HsTickPragma _ _ _ _ fun)) =
--   processExpr fun
-- processExpr (L _ (HsSCC _ _ _ fun)) =
--   processExpr fun
-- processExpr (L _ (HsCoreAnn _ _ _ fun)) =
--   processExpr fun
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
processExpr (L _ (HsIf _ funl funm funr)) = nub (concatMap processExpr $ [funl, funm, funr])
processExpr (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl] <> stmts)
processExpr (L _ (ExplicitSum _ _ _ fun)) = processExpr fun
processExpr (L _ (SectionR _ funl funr)) = processExpr funl <> processExpr funr
processExpr (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
-- processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = 
    processExpr rupd_expr <> case rupd_flds of
        Left fields  -> concatMap extractLHsRecUpdField fields
        Right fields -> concatMap extractLHsRecUpdProj fields
-- processExpr (L _ (HsGetField _ gf_expr gf_field)) =
--     nub (processExpr gf_expr <> gf_field (processExpr gf_expr))
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
processExpr (L _ (HsTcBracketOut _ _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr _ = []

-- extractField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(Maybe String, Maybe String)]
-- extractField (L _ (HsRecField _ _ fun _)) = processExpr fun

extractLHsRecUpdField :: LHsRecUpdField GhcTc -> [(Maybe String, Maybe String)]
extractLHsRecUpdField (L _ (HsRecField _ _ fun _)) = processExpr fun

extractLHsRecUpdProj :: LHsRecUpdProj GhcTc -> [(Maybe String, Maybe String)]
extractLHsRecUpdProj (L _ (HsRecField _ _ fun _)) = processExpr fun

processPat :: LPat GhcTc -> [(Name, Maybe String)]
processPat (L _ pat) = case pat of
  ConPat _ _ pat_args -> processDetails pat_args
  VarPat _ x -> let var = unLoc x in [(idName var, Just $ showSDocUnsafe $ ppr $ getLoc x)]
  ParPat _ pat' -> processPat pat'
  _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name,Maybe String)]
processDetails (PrefixCon _ args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name,Maybe String)]
processPatField (L _ HsRecField {hsRecFieldArg = arg}) = processPat arg