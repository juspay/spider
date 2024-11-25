{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass, CPP, TypeApplications #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, RecordWildCards, PatternSynonyms, PartialTypeSignatures #-}

module Variable.Trace where

import Control.Reference (biplateRef, (^?))
import GHC hiding (typeKind)
import Data.Aeson
import GHC.Generics (Generic)
-- import Data.Aeson as A
import Data.Generics.Uniplate.Data ()
import Data.List
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Data
-- import Control.Monad.Extra (anyM)
import Data.List.Extra (replace,splitOn)
-- import Data.Aeson.Encode.Pretty (encodePretty)
-- import Control.Monad.Extra (filterM, ifM)
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy as B
import Control.Exception (try,SomeException)
import System.Directory (createDirectoryIfMissing, doesFileExist )
-- import Data.Yaml
-- import qualified Data.ByteString.Lazy.Char8 as Char8
#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.ConLike
import qualified GHC.Utils.Outputable as OP
import GHC.Tc.Utils.Monad (addErrs)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types
import GHC.Driver.Plugins
import GHC.Types.Var
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.Name hiding (varName)
import Control.Monad.IO.Class (liftIO)
import GHC.Utils.Misc
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Unit.Module.ModSummary (msHsFilePath)
#else
import ConLike
import HscTypes (ModSummary (..),msHsFilePath)
import qualified Outputable as OP
import TcRnMonad ( addErrs)
import Bag (bagToList)
import TcRnTypes (TcGblEnv (..), TcM)
import GhcPlugins hiding ((<>)) --(Plugin(..), defaultPlugin, purePlugin, CommandLineOption, ModSummary(..), Module (..), liftIO, moduleNameString, showSDocUnsafe, ppr, nameStableString, varName)
#endif
import Debug.Trace
import GHC.Types.Unique (getUnique, getKey)
import Data.Unique (hashUnique)
import Language.Haskell.Syntax.Expr
import GHC hiding (exprType)
import GHC.Core.TyCo.Rep
import GHC.Tc.Types.Evidence
import GHC.Tc.Types
import qualified Data.ByteString.Lazy as B

pkg_names = [""]

mainPackage = "euler-api-order"

-- mainPackage = "main"

plugin :: Plugin
plugin =
    defaultPlugin {
      typeCheckResultAction = variableCollect
    , pluginRecompile = purePlugin
    }


variableCollect :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
variableCollect opts modSummary tcEnv = do
    let prefixPath = "./tmp/varTrace/"
        moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> msHsFilePath modSummary
        path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
    liftIO $ createDirectoryIfMissing True path
    allVars <- mapM (loopOverLHsBindLR moduleName') (bagToList $ tcg_binds tcEnv)
    -- print 
    -- liftIO $ print ("Complete Before",HM.unions allVars)
    let completeH = lookUpAndConcat $ HM.unions allVars
    let completeHM = HM.map (\(WrapperFunInfo fun x) -> WrapperFunInfo fun $ filter (\funVal -> not (module_name funVal == "WhereClause")) x) completeH
    -- liftIO $ print ("Complete",completeHM)
    filteredHM :: [(String, WrapperFunInfo)]  <- 
             mapM (\(functionName',functionValueList) -> do
                            res <- concat <$> (mapM (\functionValue -> 
                                        if ((any (\y -> (y `isInfixOf` package_name functionValue && package_name functionValue /= "")) pkg_names ) ||  mainPackage `isInfixOf` package_name functionValue) 
                                            then pure ([functionValue])
                                            -- else if mainPackage `isInfixOf` package_name functionValue
                                            --     then do
                                            --         val <- readDump (module_name functionValue) (functionName functionValue)
                                            --         let args = HM.fromList $ zip (arguments val) (arguments functionValueList)
                                            --         -- liftIO $ print ("ARGUE", functionName functionValue, args)
                                            --         pure $ map (\x -> x{mainArgs = map (\(_,y) -> maybe (trace ("NOTFOUND " ++ y ++ show args )( ("NOTFOUND " ++ show args,y))) (\found -> (found,y)) $ HM.lookup y args) $ mainArgs x}) (funDep val)
                                                    -- pure (funDep val)
                                            else do
                                              -- liftIO $ print ("ARGUE 2",functionValue)
                                              pure mempty

                                         ) $ funDep functionValueList)
                            pure (functionName', WrapperFunInfo (arguments functionValueList) res)
                    ) $ HM.toList completeHM
-- package_name :: String
--   , module_name :: String
--   , functionName    :: String
--   , funArgs :: [String]
    liftIO $ B.writeFile (modulePath <> ".json") $ (encode $ HM.fromList filteredHM)
    pure tcEnv

lookUpAndConcat :: HM.HashMap String WrapperFunInfo -> HM.HashMap String WrapperFunInfo
lookUpAndConcat hm = HM.map (\val -> WrapperFunInfo (arguments val) $ nub $ foldl (\acc x -> nub $ acc ++ lookupEachKey (arguments val) hm [] x) [] (funDep val) ) hm

lookupEachKey :: [String] -> HM.HashMap String WrapperFunInfo -> [String] -> FunctionDetailsInfo -> [FunctionDetailsInfo]
lookupEachKey argList hm alreadyVisited x = case HM.lookup (functionName x) hm of
  Just val -> [x] ++ if functionName x `elem` (alreadyVisited) then [] else (nub $ concat $ (lookupEachKey argList hm (alreadyVisited ++ ([functionName x])) <$> (nub (funDep val))))
  Nothing ->
    let allArgs = HM.fromList $ zip (snd <$> mainArgs x) argList
    in [x{mainArgs = map (\(val, y) -> maybe (val,y) (\found -> (val, found)) $ HM.lookup y allArgs) $ mainArgs x}]


resolveArgs :: HM.HashMap String FunctionInfo -> String -> [String] -> [FunctionDetailsInfo] -> String -> [FunctionDetailsInfo]
resolveArgs hm key argsInput acc modName =
    case HM.lookup key hm of
        Just val ->
          let argsIn = filter (\x -> x `elem` (args val ++ [funName val])) argsInput
          in if not $ null argsIn then
              nub $ acc ++ [transformFromNameStableString (funName val) (args val) (nub $ zip (args val ++ [funName val]) argsIn) modName] else 
               let resil = foldl' (\inAcc inArgs -> inAcc ++ resolveArgs hm inArgs argsInput acc modName) acc (args val ++ [funName val]) 
               in if length resil == 0 then acc else acc ++ [transformFromNameStableString (funName val) (args val) ((nub $ concat $ mainArgs <$> resil)) modName]
        Nothing -> acc

resolveArgs2 :: HM.HashMap String FunctionInfo -> String -> [String] -> [FunctionInfo] -> [FunctionInfo]
resolveArgs2 hm key argsInput acc =
    case HM.lookup key hm of
        Just val ->
          let argsIn = filter (\x -> x `elem` (args val ++ [funName val])) argsInput 
          in if not $ null argsIn then
              acc ++ [val{orgArgs = argsIn}] else 
               let resil = foldl' (\inAcc inArgs -> inAcc ++ resolveArgs2 hm inArgs argsInput acc) acc (args val ++ [funName val]) 
               in if length resil == 0 then acc else acc ++ [val{orgArgs = nub $ concat $ orgArgs <$> resil}]
        Nothing -> acc


moduleNameToLocalPath :: String -> String
moduleNameToLocalPath x = ("./tmp/varTrace/" <> "src/" <> (intercalate "/" . splitOn "." $ x) ++ ".hs.json")

readDump :: String -> String -> TcM WrapperFunInfo
readDump moduleName funName = do
    eitherRes <- liftIO $ (try $ B.readFile (moduleNameToLocalPath moduleName) :: IO (Either SomeException B.ByteString))
    either (\x -> do
          -- liftIO $ print $ "ERROR: " ++ show x
          pure $ WrapperFunInfo [] []) (\res -> case decode res of
        Just (val :: HM.HashMap String WrapperFunInfo) -> pure $ fromMaybe (WrapperFunInfo [] []) $ HM.lookup funName val
        Nothing -> pure $ WrapperFunInfo [] []) eitherRes

-- ([("$_in$list","")],["num_a12Q","num2_a12R"]
-- ,fromList [("y_a12V",FunctionInfo {name = "$_in$y", funName = "SomeType", args = ["num_a12Q"]}),("lastStatmentPlugin0",FunctionInfo {name = "lastStatmentPlugin", funName = "$_in$a", args = []}),("a_a12S",FunctionInfo {name = "$_in$a", funName = "getval", args = ["num_a12Q"]}),("x_a12U",FunctionInfo {name = "lastStatmentPlugin", funName = "$main$Sample$getValueIn", args = ["a_a12S"]}),("lastStatmentPlugin1",FunctionInfo {name = "lastStatmentPlugin", funName = "$main$Sample$getValueIn", args = ["a_a12S"]}),("c_a12T",FunctionInfo {name = "$_in$c", funName = "show", args = ["num_a12Q"]})])

loopOverLHsBindLR :: String -> LHsBindLR GhcTc GhcTc -> TcM (HM.HashMap String WrapperFunInfo)
loopOverLHsBindLR modName x@(L _ AbsBinds {abs_binds = binds}) = do
   let funName' = concatMap (getFunctionName) $ bagToList binds
       allArgs1 = concat $ mapMaybe loopOverFunBind $ bagToList binds
       allArgs :: [String] = fst <$> allArgs1
       allLetPats :: HM.HashMap String FunctionInfo = (foldl' (\acc val -> processAllLetPats acc False allArgs val) HM.empty (bagToList binds))
   let listArgs :: [FunctionDetailsInfo] = nub $ HM.foldlWithKey (\acc key val -> (resolveArgs allLetPats key allArgs acc modName)) ([] ::[FunctionDetailsInfo] ) allLetPats
  --  liftIO $ print (funName', allArgs, allLetPats)
  --  liftIO $ print ("ALL arges", allLetPats)
  --  liftIO $ print ("ALL arge", nub listArgs)
   let argsHm = HM.fromList allArgs1
   let mapped = listArgs -- nub $ map (\x -> transformFromNameStableString (funName x) (args x) (map (\orgi -> ("",orgi)) $ orgArgs x) modName) listArgs
   let mappedARgs =  map (\x -> functionName $ transformFromNameStableString (x) [] [] modName) $ snd <$> allArgs1
  --  liftIO $ print ("ALL argesss", nub mapped)
   let finalResult = nub $ filter (\x -> any (\y -> y `isInfixOf` package_name x && package_name x /= "") (pkg_names ++ [mainPackage])) mapped
   let finalResultMapped = nub $ map (\x -> x{mainArgs = filter (not . null) $ map (\(val,y) -> (val,functionName $ transformFromNameStableString (fromMaybe y $ HM.lookup y argsHm) [] [] modName)) (mainArgs x) }) finalResult

    --    allResult = foldl' (\acc ())
--     name :: String
--     , funName :: String
--     , args :: [String]
   
--    liftIO $ mapM (\x@(FunctionInfo name funName args) -> do 
--                 mapM (\y ->
--                     let detailInfo = transformFromNameStableString (funName y) (args y)
--                     if (package_name detailInfo) == "main" 
--                         then do
--                             fileData <- readDump (module_name detailInfo)
--                             filter (\y@(FunctionInfo targetName targetFunName targerArgs) -> ) fileData
--                         else pure mempty
--                     ) (args x)
--             ) listArgs
  --  liftIO $ print ("Final", nub finalResultMapped, finalResult )
  --  liftIO $ print ("Final", map (\x -> x{funArgs = [], mainArgs =[]})(finalResultMapped))
   pure $ HM.singleton (if null funName' then "" else replace "$_in$" "" $ fst $ head funName') $ WrapperFunInfo (mappedARgs) (nub finalResultMapped)--HM.empty -- HM.insert (funName) (allArgs, HM.empty
-- loopOverLHsBindLR (L _ x@(FunBind _ _ matches _)) = 
loopOverLHsBindLR _ _ = pure HM.empty

getFunctionName :: LHsBindLR GhcTc GhcTc -> [(String,String)]
#if __GLASGOW_HASKELL__ < 900
getFunctionName (L _ (FunBind _ idt _ _ _)) = [(nameStableString $ getName idt,mempty)]
#else
getFunctionName (L _ (FunBind _ idt _ _)) = [(nameStableString $ getName idt,mempty)]
#endif
getFunctionName (L _ (VarBind{var_id = var})) = [(nameStableString $ varName var,showSDocUnsafe $ ppr $ idType var)]
getFunctionName (L _ (PatBind{})) = []
getFunctionName (L _ (AbsBinds{abs_binds = binds})) = concatMap getFunctionName $ bagToList binds
getFunctionName _ = []


getStableName idT =
  let name = nameStableString $ getName idT
  in if "$_in" `isPrefixOf` name then showS idT else name

getStableVarName idT =
  let name = nameStableString $ varName idT
  in if "$_in" `isPrefixOf` name then showS idT else name

getFunctionNameWithArgs :: HM.HashMap String FunctionInfo -> Bool -> LHsBindLR GhcTc GhcTc -> ([(String, (String, String,[String]))], HM.HashMap String FunctionInfo)
#if __GLASGOW_HASKELL__ < 900
getFunctionNameWithArgs hm orgNameNeeded (L _ (FunBind _ idt _ _ _)) = [((getStableName idt), (showS idt, showSDocUnsafe <$> ppr <$> unLoc $ mg_alts matches,mempty ))]
#else
getFunctionNameWithArgs hm orgNameNeeded (L _ (FunBind _ idt matches _)) =
    let valArgs = mapMaybe getFnNameWithAllArgs (matches ^? biplateRef)
        filteredList = filter (\(name,_) -> (fst $ head valArgs) /= name) valArgs
        changeVALS = map (\(x,y) -> (getStableVarName $ unLoc x,showS <$> y)) filteredList--(valArgs)
        changedHM = HM.toList $ foldl' (\acc (name,args) -> maybe (HM.insert (name) (args) acc) (\val -> if length val > length args then acc else HM.insert (name) (args) acc) $ HM.lookup (name) acc ) HM.empty (changeVALS)
        firstEl = if null valArgs then Nothing else Just $ head valArgs
       -- changedHM = HM.toList $ foldl' (\acc (name,args) -> maybe (HM.insert (showS name) (showS <$> args) acc) (\val -> if length val > length args then acc else HM.insert (showS name) (showS <$> args) acc) $ HM.lookup (showS name) acc ) HM.empty valArgs
        -- funName = fromMaybe "" $ nameStableString <$> varName <$> unLoc <$> (fst <$> firstEl) 
        funName = fromMaybe "" $ getStableVarName <$> unLoc <$> (fst <$> firstEl) 
        args = map showS $ fromMaybe [] $ snd <$> firstEl
        finalHM = snd $ foldl' (\(ind,acc) (name,args) -> (ind + 1,HM.insert (showS idt ++ "$letInside" ++ show ind ++ name ) (FunctionInfo (showS idt ++ "$letInside") (name) (args) []) acc)) (0 :: Int,hm) changedHM
    in ([((getStableName idt), (showS idt, funName, args))], finalHM)
#endif
getFunctionNameWithArgs hm orgNameNeeded x@(L _ (VarBind{var_id = var})) = ([((getStableName var), (showS x, showSDocUnsafe $ ppr var, mempty ))], hm)
getFunctionNameWithArgs hm _ (L _ (PatBind{})) = ([], hm)
getFunctionNameWithArgs hm orgNameNeeded (L _ (AbsBinds{abs_binds = binds, abs_exports = new})) = 
    let pol = abe_poly <$> new
        mon = abe_mono <$> new
        allmaps = foldl (\acc (x,y) -> HM.insert (showS y) (showS x) acc) HM.empty $ zip (pol) ( mon)
        tampRes = (map (getFunctionNameWithArgs hm orgNameNeeded) $ bagToList binds)
        resul = concat $ fst <$> tampRes
        changedResult = map (\a@(x,(y,z,args)) -> maybe a (\val -> (x,(val,z,args))) $ HM.lookup y allmaps) resul
    in (changedResult, HM.unions $ snd <$> tampRes)
-- getFunctionNameWithArgs (L _ (RecordCon _ (L _ (iD)) rcon_flds)) = Just ((extractRecordBinds (rcon_flds)), False)
getFunctionNameWithArgs hm _ (L _ v) = ([], hm)

getFnNameWithAllArgs :: HsExpr GhcTc -> Maybe (Located Var, [HsExpr GhcTc])
getFnNameWithAllArgs (HsVar _ v)                      = Just (getLocated v, [])
getFnNameWithAllArgs (HsConLikeOut _ cl)             = (\clId -> (noExprLoc clId, [])) <$> conLikeWrapId cl
getFnNameWithAllArgs (HsAppType _ (L _ expr) _)             = getFnNameWithAllArgs expr
getFnNameWithAllArgs expr@(HsApp _ (L _ funl@(HsVar _ v)) (L _ funr)) = Just (getLocated v, [funr])
getFnNameWithAllArgs expr@(HsApp _ (L _ funl) (L _ funr))              = do
  let res = getFnNameWithAllArgs funl
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> Just (fnName, ls ++ [funr])
getFnNameWithAllArgs expr@(OpApp _ (L _ funl) (L _ op) (L _ funr)) = do
  case op of
    (HsVar _ v)                -> Just (getLocated v, [funl,funr])
    (PatHsWrap _ (HsVar _ var)) -> Just (getLocated var, [funl,funr])
    expanded                                -> Nothing
getFnNameWithAllArgs (PatHsWrap _ expr) = getFnNameWithAllArgs (expr)
getFnNameWithAllArgs (HsCase _ (L _ funl) exprLStmt) = do
  let res = getFnNameWithAllArgs funl 
  case res of
    Nothing -> Nothing
    Just (fnName, ls) -> do
      let exprs = exprLStmt ^? biplateRef :: [HsExpr GhcTc]
      Just (fnName, ls <> exprs)
getFnNameWithAllArgs (ap@(PatHsExpansion orig expanded)) =
  case (orig, expanded) of
    ((OpApp _ _ op _), (HsApp _ (L _ (HsApp _ op' funl)) funr)) -> case showS op of
      "($)" -> getFnNameWithAllArgs ((HsApp noExtFieldOrAnn funl funr))
      "(<$>)" -> getFnNameWithAllArgs ((HsApp noExtFieldOrAnn funl funr))
      _ -> getFnNameWithAllArgs (expanded)
    _ -> getFnNameWithAllArgs (expanded)
getFnNameWithAllArgs (x@(RecordCon _ (L _ cl) rcon_flds)) =
  let res = (rcon_flds ^? biplateRef)
      maybeVal = noExprLoc <$> conLikeWrapId cl
  in  maybe Nothing (\val -> Just (val, res)) maybeVal
    --  Just (getLocated v, rcon_flds)

-- getFnNameWithAllArgs (L loc doB@(HsDo _ _ exprLStmt)) = do
--     let val = extractExprsFromStmtLRHsExpr exprLStmt
getFnNameWithAllArgs (x)                            = Nothing

noExtFieldOrAnn :: EpAnn a
noExtFieldOrAnn = noAnn

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

getLocated :: GenLocated (SrcSpanAnn' a) e -> Located e
getLocated ap = L (getLocA ap) (unLoc ap)

noExprLoc :: a -> Located a
noExprLoc = noLoc


conLikeWrapId :: ConLike -> Maybe Var
conLikeWrapId (RealDataCon dc) = Just (dataConWrapId dc)
conLikeWrapId _ = Nothing


loopOverFunBind :: LHsBindLR GhcTc GhcTc -> (Maybe [(String,String)])
loopOverFunBind (L _ x@(FunBind _ _ matches _)) = do
   let inte = unLoc $ mg_alts matches
   if null inte then Nothing else do
       let y = mapMaybe (loopOverVarPat False) $ m_pats $ unLoc $ head inte
       Just y
loopOverFunBind (L _ x) = do
   Nothing

loopOverVarPat :: Bool -> LPat GhcTc -> Maybe (String,String)
loopOverVarPat orgNameNeeded v@(L _ (VarPat _ (name@(L _ var)))) = Just $ (showS $ getLocated name, getStableVarName var)
loopOverVarPat _ (L _ x) =  Nothing

processExpr :: Bool -> LHsExpr GhcTc -> [(String,String)]
processExpr orgNameNeeded exprSt =
 case exprSt of
    x@(L _ (HsVar _ (L _ var))) ->
      let name = (getStableVarName var,showSDocUnsafe $ ppr $ idType var)
      in [name]
    (L _ (HsUnboundVar _ _)) -> []
    (L _ (HsApp _ funl funr)) -> processExpr orgNameNeeded funl <> processExpr orgNameNeeded funr
    (L _ (OpApp _ funl funm funr)) -> processExpr orgNameNeeded funl <> processExpr orgNameNeeded funm <> processExpr orgNameNeeded funr
    (L _ (NegApp _ funl _)) -> processExpr orgNameNeeded funl
    (L _ (HsTick _ _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsStatic _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsBinTick _ _ _ fun)) -> processExpr orgNameNeeded fun
#if __GLASGOW_HASKELL__ < 900
    (L _ (HsTickPragma _ _ _ _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsSCC _ _ _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsCoreAnn _ _ _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsWrap _ _ fun)) -> processExpr orgNameNeeded (noLoc fun)
--      (L _ (HsWrap _ _ fun)) ->
--   processExpr (noLoc fun)
    (L _ (ExplicitList _ _ funList)) -> concatMap (processExpr orgNameNeeded) funList
    (L _ (HsIf _ exprLStmt funl funm funr)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) $ [funl, funm, funr] <> stmts)
    (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) ->
      let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
          stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) (stmtsL <> stmtsR))
--      (L _ (HsIf _ exprLStmt funl funm funr)) ->
--   let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
--    in nub (concatMap (processExpr orgNameNeeded) $ [funl, funm, funr] <> stmts)
    (L _ (RecordUpd _ rupd_expr rupd_flds)) -> processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
#else
    (L _ (ExplicitList _ funList)) -> concatMap (processExpr orgNameNeeded) funList
    (L _ (HsIf exprLStmt funl funm funr)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) $ [funl, funm, funr] <> stmts)
    (L _ (HsTcBracketOut _ _ exprLStmtL exprLStmtR)) ->
      let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
          stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) (stmtsL <> stmtsR))
    (L _ (RecordUpd _ rupd_expr rupd_flds)) -> processExpr orgNameNeeded rupd_expr <> extractLHsRecUpdField orgNameNeeded rupd_flds
#endif
    (L _ (ExprWithTySig _ fun _)) -> processExpr orgNameNeeded fun
    (L _ (HsDo _ _ exprLStmt)) ->
      let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      in nub $ concatMap (processExpr orgNameNeeded) stmts
    (L _ (HsLet _ exprLStmt func)) ->
      let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      in processExpr orgNameNeeded func <> nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsMultiIf _ exprLStmt)) ->
      let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsCase _ funl exprLStmt@(MG _ (L _ _) _))) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) $ [funl] <> stmts)
    (L _ (ExplicitSum _ _ _ fun)) -> processExpr orgNameNeeded fun
    (L _ (SectionR _ funl funr)) -> processExpr orgNameNeeded funl <> processExpr orgNameNeeded funr
    (L _ (ExplicitTuple _ exprLStmt _)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsPar _ fun)) -> processExpr orgNameNeeded fun
    (L _ (HsAppType _ fun _)) -> processExpr orgNameNeeded fun
    (L _ (HsLamCase _ exprLStmt)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsLam _ exprLStmt)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    x@(L _ (HsLit _ liter)) ->
  -- let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
  -- let literals = 
      [(showSDocUnsafe $ ppr liter,mempty)]
    (L _ (HsOverLit _ exprLStmt)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsRecFld _ exprLStmt)) ->
      let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmts)
    (L _ (HsSpliceE exprLStmtL exprLStmtR)) ->
      let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
          stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) (stmtsL <> stmtsR))
    (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) ->
      let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
          stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) (stmtsL <> stmtsR))
    (L _ (ArithSeq _ Nothing exprLStmtR)) ->
      let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded) stmtsR)
    (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) ->
      let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
          stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
      in nub (concatMap (processExpr orgNameNeeded)  (stmtsL <> stmtsR))
--      (L _ (RecordCon _ (L _ (iD)) rcon_flds)) -> Just ((extractRecordBinds (rcon_flds)), False)
--      (L _ (RecordUpd _ rupd_expr rupd_flds)
--      (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) ->

-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
    x@(L _ (HsConLikeOut _ (RealDataCon liter))) -> [(showSDocUnsafe $ ppr liter,mempty)]
    x@(L _ (HsConLikeOut _ (liter))) -> [(showSDocUnsafe $ ppr liter,mempty)]
    (L _ x@(XExpr ((WrapExpr (HsWrap hsWrapper hsExpr))))) -> processExpr orgNameNeeded (wrapXRec @(GhcTc) hsExpr)
    (L _ _) -> []

processLetExpr :: HM.HashMap String FunctionInfo -> Bool -> [String] -> _ -> HM.HashMap String FunctionInfo
-- processLetExpr (L _ (HsLet _ exprLStmt func)) =
--   let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
--    in trace ("RECHHHH" ) $ processExpr func <> nub (concatMap processExpr stmts)
processLetExpr hm orgNameNeeded allArgs (L _ (HsDo _ smtContext (L _ exprLStmt))) =
    foldl' (\acc (L _ x) -> extractExprsFromStmtLRHsExpr acc orgNameNeeded allArgs x) hm exprLStmt
-- processLetExpr (L _ (XExpr overLitVal)) = processXXExpr overLitVal
processLetExpr hm orgNameNeeded _ (L _ (HsLet _ exprLStmt func)) = do
  let val = processHsLocalBinds hm orgNameNeeded exprLStmt
  let vals = mapMaybe getFnNameWithAllArgs (func ^? biplateRef)
      changeVALS = map (\(x,y) -> (getStableVarName $ unLoc x,showS <$> y)) (vals)
      changedHM = HM.toList $ foldl' (\acc (name,args) -> maybe (HM.insert (name) (args) acc) (\val -> if length val > length args then acc else HM.insert (name) (args) acc) $ HM.lookup (name) acc ) HM.empty (changeVALS)
  snd $ foldl' (\(ind,acc) (name,args) -> (ind + 1,HM.insert ("lastStatmentPlugin" ++ name ++ show ind) (FunctionInfo ("lastStatmentPlugin") (name) (args) []) acc)) (0 :: Int,val) changedHM
  -- HM.union 
processLetExpr hm _ _ (L _ x) = hm

-- processXXExpr (WrapExpr (HsWrap hsWrapper hsExpr)) =
--     processExpr (wrapXRec @(GhcTc) hsExpr)
-- processXXExpr (ExpansionExpr (HsExpanded _ expansionExpr)) =
--     concatMap (processExpr . (wrapXRec @(GhcTc))) [expansionExpr]

-- processSynExpr keyFunction path (SyntaxExprTc { syn_expr      = expr}) = processExpr (wrapXRec @GhcTc $ expr)
-- processSynExpr _ _ _ = []

getNameFromPat :: Bool -> LPat GhcTc -> String
getNameFromPat orgNameNeeded (L _ (VarPat _ var)) = getStableName var
getNameFromPat _ (L _ x) = ""

extractExprsFromStmtLRHsExpr ::  HM.HashMap String FunctionInfo -> Bool -> [String] -> StmtLR GhcTc GhcTc (LHsExpr GhcTc) -> HM.HashMap String FunctionInfo
extractExprsFromStmtLRHsExpr hm orgNameNeeded allArgs stmt = case stmt of
    x@(LastStmt _ body _ retExpr) -> do
        let vals = mapMaybe getFnNameWithAllArgs (body ^? biplateRef)
            changeVALS = map (\(x,y) -> (getStableVarName $ unLoc x,showS <$> y)) (vals)
            changedHM = HM.toList $ foldl' (\acc (name,args) -> maybe (HM.insert (name) (args) acc) (\val -> if length val > length args then acc else HM.insert (name) (args) acc) $ HM.lookup (name) acc ) HM.empty (changeVALS)
        snd $ foldl' (\(ind,acc) (name,args) -> (ind + 1,HM.insert ("lastStatmentPlugin" ++ name ++ show ind) (FunctionInfo ("lastStatmentPlugin") (name) (args) []) acc)) (0 :: Int,hm) changedHM
        -- processExpr body
        -- processSynExpr retExpr
    BindStmt _ pat x@(L _ (HsDo _ _ (body))) -> do
        let allLetPats = processLetExpr hm orgNameNeeded allArgs x
            allReqArgs = HM.keys hm ++ allArgs
        let listArgs :: [FunctionInfo] = HM.foldlWithKey (\acc key val -> (resolveArgs2 allLetPats key (allReqArgs) acc)) ([] ::[FunctionInfo] ) allLetPats
            result = filter (\fun -> name fun == "lastStatmentPlugin") listArgs
        -- let val = map getFnNameWithAllArgs (body ^? biplateRef)
        (if null result then hm else HM.insert (if showS pat == "_" then name (head result) ++ "DO" ++ showS pat else showS pat) (head result) hm)
    BindStmt _ pat x@(L _ (XExpr (ExpansionExpr (HsExpanded _ (HsApp _ fun y@(L _ (HsDo _ _ (body)))))))) -> do
        let vals = mapMaybe getFnNameWithAllArgs (fun ^? biplateRef)
            changeVALS = map (\(x,y) -> (getStableVarName $ unLoc x,showS <$> y)) (vals)
            changedHM = HM.toList $ foldl' (\acc (name,args) -> maybe (HM.insert (name) (args) acc) (\val -> if length val > length args then acc else HM.insert (name) (args) acc) $ HM.lookup (name) acc ) HM.empty (changeVALS)
            res = snd $ foldl' (\(ind,acc) (name,args) -> (ind + 1,HM.insert ("lastStatmentPluginOut" ++ show ind) (FunctionInfo ("lastStatmentPluginOut") (name) (args) []) acc)) (0 :: Int,hm) changedHM
        let allLetPats = HM.union res (processLetExpr hm orgNameNeeded allArgs (y))
            allReqArgs = HM.keys hm ++ allArgs
        let listArgs :: [FunctionInfo] = HM.foldlWithKey (\acc key val -> (resolveArgs2 allLetPats key (allReqArgs) acc)) ([] ::[FunctionInfo] ) allLetPats
            result = filter (\fun -> name fun == "lastStatmentPluginOut") listArgs
            final = snd $ foldl' (\(ind,acc) val -> (ind+1,HM.insert ("lastDo" ++ show ind) val acc)) (0,(if null result then hm else HM.insert (if showS pat == "_" then name (head result) ++ "DOAPP" ++ showS pat else showS pat) (head result) hm)) listArgs
        -- let val = map getFnNameWithAllArgs (body ^? biplateRef)
        final
        -- trace (showS body ++ "BIND " ++  show (toConstr body)) $ maybe mempty (\(name,args) -> HM.insert (showS pat) (FunctionInfo (getNameFromPat pat) (nameStableString $ varName $ unLoc name) (showS <$> args)) mempty) $ trace ("ALLFUNS" ++ showS val) val
    BindStmt _ pat body@(L _ x) -> do
        let val = mapMaybe getFnNameWithAllArgs (body ^? biplateRef)
        HM.unions $ map (\(name,args) -> HM.insert (if showS pat == "_" then (getStableVarName $ unLoc name) ++ "DO" ++ showS pat else showS pat) (FunctionInfo (getNameFromPat orgNameNeeded pat) (getStableVarName $ unLoc name) (showS <$> args) []) hm) val
        -- trace (showS pat ++ "  bodyyyy " ++ showS val) mempty
        -- extractExprsFromPat pat
        -- processExpr body
    -- ApplicativeStmt _ args mJoin -> do
    --     map (\(op, arg) -> do
    --                 processSynExpr op
    --                 extractExprFromApplicativeArg arg) args
    --     case mJoin of
    --         Just m -> processSynExpr m
    --         _ -> pure ()
    x@(BodyStmt _ body _ guardOp) -> do
        let val = mapMaybe getFnNameWithAllArgs (body ^? biplateRef)
        snd $ foldl' (\(ind,acc) (name,args) -> (ind+1,HM.insert ("middleSTatment" ++ show ind) (FunctionInfo "middleSTatment" (showS name) (showS <$> args) []) acc)) (0,hm) val
        -- processExpr body
        -- processSynExpr guardOp
    y@(LetStmt _ x@binds) ->
       let val = processHsLocalBinds hm orgNameNeeded binds
       in val
    -- ParStmt _ blocks _ bindOp -> do
    --     map (extractExprsFromParStmtBlock) blocks
    --     processSynExpr bindOp
    -- TransStmt{..} -> do
    --     map (extractExprsFromStmtLRHsExpr . unLoc) trS_stmts
    --     processExpr trS_using
    --     map (processExpr) (trS_by)
    --     processSynExpr trS_ret
    --     processSynExpr trS_bind
    --     processExpr (wrapXRec @GhcTc trS_fmap)
    -- RecStmt{..} -> do
    --     map (extractExprsFromStmtLRHsExpr . unLoc) (unXRec @GhcTc $ recS_stmts)
    --     processSynExpr recS_bind_fn
    --     processSynExpr recS_ret_fn
    --     processSynExpr recS_mfix_fn
    -- XStmtLR _ -> pure ()
    x -> hm

processHsLocalBinds :: HM.HashMap String FunctionInfo -> Bool -> HsLocalBindsLR GhcTc GhcTc -> HM.HashMap String FunctionInfo
processHsLocalBinds hm orgNameNeeded (HsValBinds _ (ValBinds _ x y)) = do
    let includeEV = (map (getFunctionNameWithArgs hm orgNameNeeded) $ bagToList x)
    let vals = concat $ fst <$> includeEV
        allVals = mapMaybe (getFnNameWithAllArgs) $ ((bagToList x) ^? biplateRef)
    -- let (names, dtType) = (concat $ fst <$> vals, concat $ snd <$> vals)
    HM.union (HM.unions $ snd <$> includeEV) $ foldl' (\acc ((name),(uni, expr,exprType)) -> HM.insert uni (FunctionInfo name expr exprType []) acc ) hm vals
    -- fol modHm allVals
processHsLocalBinds hm orgNameNeeded (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
  --  concat $ 
   let vals' = map (\(recFlag, binds) -> 
                              let x = (map (getFunctionNameWithArgs hm orgNameNeeded) $ bagToList binds)
                              in (concat (fst <$> x),HM.unions (snd <$> x))
                              ) (x)
       vals = concat (fst <$> vals')
       mergedHM = HM.unions $ [hm] <> (snd <$> vals')
--    let (names, dtType) = (concat $ fst <$> vals, concat $ snd <$> vals)
   foldl (\acc ((name),(uni, expr,exprType)) -> HM.insert uni (FunctionInfo name expr exprType []) acc ) (mergedHM) vals
--    HM.insert names (dtType , concatMap processExpr $ (y ^? biplateRef :: [LHsExpr GhcTc]) ) HM.empty
processHsLocalBinds hm orgNameNeeded x = hm

processAllLetPats :: HM.HashMap String FunctionInfo -> Bool -> [String] -> LHsBindLR GhcTc GhcTc -> HM.HashMap String FunctionInfo
#if __GLASGOW_HASKELL__ >= 900
processAllLetPats hm orgNameNeeded allArgs x@(L _ (FunBind _ name matches _)) = do
#else
processAllLetPats hm orgNameNeeded allArgs (L _ (FunBind _ name matches _ _)) = do
#endif
    let newHm = HM.unions $ map (\x -> processHsLocalBinds hm True $ grhssLocalBinds $ m_grhss $ unLoc x) (unLoc $ mg_alts matches)
    (foldl' (\acc val -> processLetExpr acc orgNameNeeded allArgs val) newHm (x ^? biplateRef :: [LHsExpr GhcTc]))
    -- if null inte
    --     then []
    --     else concatMap (\x@(L _ y) -> processLetExpr x) inte 
processAllLetPats hm _ _ (L _ _) = hm

#if __GLASGOW_HASKELL__ >= 900
extractLHsRecUpdField :: Bool -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> [(String,String)]
extractLHsRecUpdField orgNameNeeded fields =
  case fields of
    Left fun -> concatMap (processExprCases orgNameNeeded) (fun)
    Right x ->
      let yn = (map (GHC.unXRec @(GhcTc)) x)
      in concatMap (processExprUps orgNameNeeded) (yn )
#else
extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(String,String)]
extractLHsRecUpdField (L _ (HsRecField {hsRecFieldArg = fun})) = processExpr fun
#endif

-- processGRHS :: Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO ()
-- processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
-- processGRHS _ _ _ = pure mempty

-- processHsLocalBinds :: Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
-- processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
--     void $ mapM (loopOverLHsBindLR con (Just keyFunction) path) $ bagToList $ x
-- processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
--     void $ mapM (\(recFlag, binds) -> void $ mapM (loopOverLHsBindLR con (Just keyFunction) path) $ bagToList binds) ( x)
-- processHsLocalBinds _ _ _ = pure mempty

#if __GLASGOW_HASKELL__ >= 900
processExprUps :: Bool -> HsRecField' id (GenLocated SrcSpanAnnA (HsExpr GhcTc)) -> [(String,String)]
processExprUps orgNameNeeded (HsRecField {hsRecFieldArg = fun}) = processExpr orgNameNeeded fun

processExprCases :: Bool -> GenLocated l (HsRecField' id (GenLocated SrcSpanAnnA (HsExpr GhcTc))) -> [(String,String)]
processExprCases orgNameNeeded (L _ (HsRecField {hsRecFieldArg = fun})) = processExpr orgNameNeeded fun
#endif

transformFromNameStableString :: (String) -> [String] -> [(String, String)] -> String -> FunctionDetailsInfo
transformFromNameStableString ( str) isF orgArgs modName =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in if length parts == 2 then  FunctionDetailsInfo "" (parts !! 0) (parts !! 1) isF orgArgs
     else if length parts == 3 then FunctionDetailsInfo (parts !! 0) (parts !! 1) (parts !! 2) isF orgArgs
     else FunctionDetailsInfo mainPackage "WhereClause" str isF orgArgs

data FunctionInfo = FunctionInfo
    { name :: String
    , funName :: String
    , args :: [String]
    , orgArgs :: [String]
    }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)

data WrapperFunInfo = WrapperFunInfo
  { arguments :: [String]
  , funDep :: [FunctionDetailsInfo]
  }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)

data FunctionDetailsInfo = FunctionDetailsInfo
  { package_name :: String
  , module_name :: String
  , functionName    :: String
  , funArgs :: [String]
  , mainArgs :: [(String, String)]
  } deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON, FromJSON)


pattern PatHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsWrap wrapper expr <- (XExpr (WrapExpr (HsWrap wrapper expr))) 

pattern PatHsExpansion :: HsExpr GhcRn -> HsExpr GhcTc -> HsExpr GhcTc
pattern PatHsExpansion orig expanded <- (XExpr (ExpansionExpr (HsExpanded orig expanded)))

