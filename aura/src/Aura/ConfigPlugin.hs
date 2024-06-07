{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# OPTIONS_GHC -ddump-parsed-ast #-}

module Aura.ConfigPlugin where

import GhcPlugins hiding ((<>)) --(Plugin(..), defaultPlugin, purePlugin, CommandLineOption, ModSummary(..), Module (..), liftIO, moduleNameString, showSDocUnsafe, ppr, nameStableString, varName)
import TcRnTypes (TcGblEnv (..), TcM)
import Control.Concurrent.MVar (newMVar, MVar, takeMVar, readMVar)
import GHC.IO (unsafePerformIO)
import Control.Reference (biplateRef, (^?))
import GHC
import Bag (bagToList,listToBag)
import Data.Aeson
import Data.Generics.Uniplate.Data ()
import TcRnMonad ( addErrs)
import qualified Outputable as OP
import Data.List
import Data.Maybe (mapMaybe, catMaybes, isJust, fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Bool (bool)
import Data.Data
import Data.List.Extra (replace,splitOn)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Extra (anyM, foldM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Control.Exception (try,SomeException)
import System.Directory ( doesDirectoryExist, listDirectory, createDirectoryIfMissing )
import GHC.Generics (Generic)
import Data.Char (toLower)
import Debug.Trace (traceShowId)

plugin :: Plugin
plugin =
    defaultPlugin {
      typeCheckResultAction = checkIntegrity
    , pluginRecompile = purePlugin
    }

checkIntegrity :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
checkIntegrity opts modSummary tcEnv = do
    let prefixPath = case opts of
                    [] -> "/tmp/env/"
                    local : _ -> local
    let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> ms_hspp_file modSummary
    let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
    liftIO $ createDirectoryIfMissing True path
    (result) <- liftIO $ catMaybes <$> mapM (loopOverLHsBindLRBefore prefixPath moduleName') (bagToList $ tcg_binds tcEnv)
    let finalResult = foldl (\acc (EnvList name env) -> maybe (HM.insert name env acc) (\val -> HM.insert name (nub $ env ++ val) acc) $ HM.lookup name acc ) HM.empty $ map (\(FunctionInfo _ _ name env _) -> EnvList name env) $ concat (result)
    (getAllFuns) <- liftIO $ foldM (\acc x -> loopAndColect acc x) HM.empty (bagToList $ tcg_binds tcEnv)
    let combination = lookUpAndConcat getAllFuns
    -- liftIO $ print  $ "Generating 1" ++ show (combination, finalResult)
    liftIO $ writeFile (modulePath <> ".jsonl") ( show (combination, getAllFuns, finalResult))
    let (getAllNewList) = HM.foldlWithKey (\acc key val -> loopOverEverything acc key (nub val)) finalResult combination
    liftIO $ B.writeFile (modulePath <> ".json") (encodePretty getAllNewList )
    liftIO $ print $  "Completed generating " ++ moduleName' ++ " for path: " ++ (modulePath <> ".json")
    pure tcEnv

lookUpAndConcat hm = HM.map (\val -> nub $ foldl (\acc x -> nub $ acc ++ lookupEachKey hm [] x) val val ) hm

lookupEachKey :: HM.HashMap String [FunctionInfo] -> [String] -> FunctionInfo -> [FunctionInfo]
lookupEachKey hm alreadyVisited x = case HM.lookup (name x) hm of
  Just val -> [x] ++ if name x `elem` (alreadyVisited) then [] else (nub $ concat $ (lookupEachKey hm (alreadyVisited ++ ([name x])) <$> (nub (val))))
  Nothing -> [x]

changeModName moduleName' (FunctionInfo x y z _ z2) = FunctionInfo x (if y == "_in" then moduleName' else y) z [] z2


checkInOtherMods :: String -> String -> FunctionInfo -> IO (Maybe [String])
checkInOtherMods path moduleName (FunctionInfo x y z envList fileName) = do
--    liftIO $ print (moduleName,y)
   if (((not $ "euler" `isInfixOf` x ) || y == moduleName)) then do
    --  liftIO $ print $ "Omiting " ++ show (y, x, z)
     pure Nothing
   else do
    let srcPath =
          if "euler-events-hs" `isInfixOf` x 
                    then "events"
                    else if "euler-hs" `isInfixOf` x 
                      then "euler-hs" 
                      else if "euler-db" `isInfixOf` x 
                      then "euler-db"
                      else if "euler-webse" `isInfixOf` x 
                      then "euler-web"
                      else "src"
    fileContents <- liftIO $ (try $ B.readFile (path ++ (srcPath) ++ "/" ++  (intercalate "/" . splitOn "." $ y) ++ ".hs.json") :: IO (Either SomeException B.ByteString))
    -- liftIO $ print fileContents
    either (\e -> do
         y <- liftIO $ listDirectory $ path ++ (srcPath)
         liftIO $ print (fileContents, z, x,y)
         pure Nothing) (\contents -> 
        maybe (pure Nothing) 
            (\list -> do
                -- liftIO $ print list
                pure $ Just $ maybe [] (\l -> l ++ envList) $ HM.lookup z list ) (Aeson.decode contents :: Maybe (HM.HashMap String [String]))) fileContents


checkInOtherModsAfter :: [FunctionInfo]  -> FunctionInfo -> IO (Maybe [String])
checkInOtherModsAfter funInfos (FunctionInfo x y z envListOrg _) = do
    let envList =  map (\(FunctionInfo _ _ name env _) -> EnvList name env) $ funInfos
    pure $ maybe Nothing
            (\list -> Just $ foldl (\acc allList -> if z == (_name allList) then (listEnv allList) ++ acc ++ envListOrg else acc) [] list  ) (Just envList)


extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [FunctionInfo]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr fun

processExprWithARgs :: LHsExpr GhcTc -> [FunctionInfo]
processExprWithARgs (L _ (HsApp _ funl lit)) = 
    let exprs = processExpr funl
        fstf = (concat $ (map processExprAsAn $  lit ^? biplateRef))
        fstl = ((mapMaybe getLiteral $  lit ^? biplateRef))
        fstt = fstf ++ fstl
    in map (\x -> x{envList = fstt}  ) (exprs)
processExprWithARgs x@(L _ (HsVar _ y@(L _ var))) = 
  let name = transformFromNameStableString ((nameStableString $ varName var),[]) (showSDocUnsafe $ ppr $ getLoc $ x)
  in [name] -- trace (show (toConstr val,showSDocUnsafe $ ppr val)) []
processExprWithARgs x = (\y-> transformFromNameStableString (y,[]) (showSDocUnsafe $ ppr $ getLoc $ x)) <$> processExprAsAn x

processExpr :: LHsExpr GhcTc -> [FunctionInfo]
processExpr x@(L _ (HsVar _ y@(L _ var))) =
  let name = transformFromNameStableString ((nameStableString $ varName var),[]) (showSDocUnsafe $ ppr $ getLoc $ x)
  in [name]
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

allRedisFuns = ["rhget", "rget", "rxread"]

processExprFun :: LHsExpr GhcTc -> [String]
processExprFun (L _ (HsApp _ funl lit)) = 
    let exprs = processExpr funl
        fsts = (concat $ (map processExprAsAn $  lit ^? biplateRef))
        lats = ((mapMaybe getLiteral $  lit ^? biplateRef))
        fstt = fsts ++ lats
    in if any (\x -> isInfixOf "euler-hs" (package_name x ) && any (\redisVal -> isInfixOf redisVal (toLower <$> name x)) allRedisFuns) (exprs) then fstt else []
processExprFun (L _ val) = [] -- trace (show (toConstr val,showSDocUnsafe $ ppr val)) []


processExprAsAn :: LHsExpr GhcTc -> [String]
processExprAsAn x@(L _ (HsVar _ y@(L _ var))) =
  let name = nameStableString $ varName var
  in [name]
processExprAsAn (L _ (HsUnboundVar _ _)) = []
processExprAsAn (L _ (HsApp _ funl funr)) =
  processExprAsAn funl <> processExprAsAn funr
processExprAsAn (L _ (OpApp _ funl funm funr)) =
  processExprAsAn funl <> processExprAsAn funm <> processExprAsAn funr
processExprAsAn (L _ (NegApp _ funl _)) =
  processExprAsAn funl
processExprAsAn (L _ (HsTick _ _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (HsStatic _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (HsWrap _ _ fun)) =
  processExprAsAn (noLoc fun)
processExprAsAn (L _ (HsBinTick _ _ _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (ExplicitList _ _ funList)) =
  concatMap processExprAsAn funList
processExprAsAn (L _ (HsTickPragma _ _ _ _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (HsSCC _ _ _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (HsCoreAnn _ _ _ fun)) =
  processExprAsAn fun
processExprAsAn (L _ (ExprWithTySig _ fun _)) =
  processExprAsAn fun
processExprAsAn (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap processExprAsAn stmts
processExprAsAn (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExprAsAn func <> nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn $ [funl, funm, funr] <> stmts)
processExprAsAn (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn $ [funl] <> stmts)
processExprAsAn (L _ (ExplicitSum _ _ _ fun)) = processExprAsAn fun
processExprAsAn (L _ (SectionR _ funl funr)) = processExprAsAn funl <> processExprAsAn funr
processExprAsAn (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
-- processExprAsAn (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExprAsAn rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
processExprAsAn (L _ (HsPar _ fun)) = processExprAsAn fun
processExprAsAn (L _ (HsAppType _ fun _)) = processExprAsAn fun
processExprAsAn (L _ (HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmts)
processExprAsAn (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn (stmtsL <> stmtsR))
processExprAsAn (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn (stmtsL <> stmtsR))
processExprAsAn (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn stmtsR)
processExprAsAn (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn (stmtsL <> stmtsR))
processExprAsAn (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExprAsAn (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExprAsAn _ = []


getLiteral :: HsLit GhcTc -> Maybe String
getLiteral (((HsString _ fstt))) = Just $ unpackFS fstt
getLiteral txt = Just $ showSDocUnsafe $ ppr txt

isVarPat :: LPat GhcTc -> Bool
isVarPat (L _ pat) = case pat of
  VarPat _ x@(L _ var) -> True
  WildPat _ -> True
  _ -> False


getDataTypeDetails :: HsExpr GhcTc -> Maybe (Bool, Bool)
getDataTypeDetails (RecordCon _ (L _ (iD)) rcon_flds) = Just ((extractRecordBinds (rcon_flds)), False)
getDataTypeDetails (RecordUpd _ rupd_expr rupd_flds) = Just (False, (getFieldUpdates rupd_flds))
getDataTypeDetails _ = Nothing
-- inferFieldType :: Name -> String
-- inferFieldTypeFieldOcc (L _ (FieldOcc _ (L _ rdrName))) = handleRdrName rdrName
-- inferFieldTypeAFieldOcc = (handleRdrName . rdrNameAmbiguousFieldOcc . unLoc)

handleRdrName :: RdrName -> String
handleRdrName x = 
    case x of 
        Unqual occName          ->               ("$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        Qual moduleName occName -> ((moduleNameString moduleName) <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        Orig module' occName    -> ((moduleNameString $ moduleName module') <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <>  "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        Exact name              -> nameStableString name

getFieldUpdates :: [LHsRecUpdField GhcTc] -> Bool
getFieldUpdates fields = any (\x -> x == True) $ map extractField fields
    where
    extractField :: LHsRecUpdField GhcTc -> Bool
    extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) = isInfixOf "status" (showSDocUnsafe $ ppr lbl) && any (\x -> isInfixOf x (map toLower $ showSDocUnsafe $ ppr expr)) ["failure", "failed", "success"]

extractRecordBinds :: HsRecFields GhcTc (LHsExpr GhcTc) -> Bool
extractRecordBinds (HsRecFields{rec_flds = fields}) =
    any (\x -> x==True) $ map extractField fields
    where
    extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> Bool
    extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) = isInfixOf "status" (showSDocUnsafe $ ppr lbl) && any (\x -> isInfixOf x (map toLower $ showSDocUnsafe $ ppr expr)) ["failure", "failed", "success"]
            -- then (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr lbl) (inferFieldTypeFieldOcc lbl))
            -- else (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr $ unLoc expr) (inferFieldTypeFieldOcc lbl))

getFunctionName :: LHsBindLR GhcTc GhcTc -> [String]
getFunctionName (L _ x@(FunBind fun_ext id matches _ _)) = [nameStableString $ getName id]
getFunctionName (L _ (VarBind{var_id = var, var_rhs = expr, var_inline = inline})) = [nameStableString $ varName var]
getFunctionName (L _ (PatBind{pat_lhs = pat, pat_rhs = expr})) = [""]
getFunctionName (L _ (AbsBinds{abs_binds = binds})) = concatMap getFunctionName $ bagToList binds
getFunctionName _ = []

loopOverEverything :: HM.HashMap String [String] -> String -> [FunctionInfo] -> (HM.HashMap String [String])
loopOverEverything allFunsList key value = do
    let val = foldl (\acc val -> acc ++( nub $ concatNate allFunsList val)) [] value
    -- trace (show (allFunsList,key,value)) $
    if (any (\x -> x `elem` HM.keys allFunsList) (name <$> value)) || (not $ null val)
        then
            case HM.lookup key allFunsList of
                Just exist -> HM.insert key (nub $ val ++ exist) allFunsList
                Nothing -> HM.insert key (nub val) allFunsList
            else allFunsList
--   let fname = (getFunctionName x)
--       allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
--       allNrFuns = filter (\fInfo -> "euler-db" `isInfixOf` package_name fInfo) $  ((concatMap processExprWithARgs allVals))
--       allPreicousFuns = getAllPreviousFuns allNrFuns allFunsList
--   liftIO $ print (allNrFuns, allFunsList, allPreicousFuns)
--   allFunsList
-- loopOverEverything allFunsList _ _ = allFunsList

concatNate allFunsList val =
    case HM.lookup (name val) allFunsList of
        Just allFuns -> nub $ allFuns ++ envList val
        Nothing -> []

loopAndColect :: HM.HashMap String [FunctionInfo] -> LHsBindLR GhcTc GhcTc -> IO ((HM.HashMap String [FunctionInfo]))
loopAndColect allFunsList x@(L _ AbsBinds {abs_binds = binds}) = do
  let fname = map name $ map (\y -> transformFromNameStableString (y , []) (showSDocUnsafe $ ppr $ getLoc $ x)) $ (getFunctionName x)
      allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
  let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
--   liftIO $ print (fname, showSDocUnsafe $ ppr x)
  let allBinds = concat $ mapMaybe loopOverFunBind (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      allNrFuns = nub $ ((concatMap processExprWithARgs allVals))
  let name' = map (\funInfo -> funInfo{envList = 
               nub $ concat $ map (\env ->
                case HM.lookup env allLetPats of
                    Just found -> found
                    Nothing -> [env]) $ filter ((\x -> not ( x `elem` allBinds) && not (x == "$euler-webservice-6.1.1-inplace$Euler.WebService.Config.Constants$kvRedis") && ("euler" `isInfixOf` x || "_in" `isInfixOf` x || not ("$" `isInfixOf` x)))) $ envList funInfo  } ) allNrFuns
  pure $ foldl (\acc val -> HM.insert (val) (nub $ name') acc) allFunsList fname
loopAndColect allFunsList _ = pure allFunsList

getAllPreviousFuns :: [FunctionInfo] -> HM.HashMap String [String] -> [FunctionInfo]
getAllPreviousFuns funInfo allFunsList = do
    map (\x@(FunctionInfo _ _ _ list _) -> x{envList = concat $ map (\l -> getAllEnvList l allFunsList) list}) funInfo

getAllEnvList :: String -> HM.HashMap String [String] -> [String]
getAllEnvList name allFunsList =
    case HM.lookup name allFunsList of
        Just value -> value ++ (concat $ map (\x -> getAllEnvList x allFunsList) value)
        Nothing -> []

processAllLetPats :: LHsBindLR GhcTc GhcTc -> (Maybe (String, [String]))
processAllLetPats (L _ x@(FunBind _ name matches _ _)) = do
    -- let (GRHS _ _ val) = 
    -- let y = unLoc val
    -- let next = matchs ^? biplateRef
    let inte = unLoc $ mg_alts matches
    if null inte then Nothing
       else Just (nameStableString $ varName $ unLoc name, map (\(GRHS _ _ val) -> showSDocUnsafe $ ppr val) $ map unLoc $ grhssGRHSs $ m_grhss $ unLoc $ head $ inte )
    -- pure Nothing
processAllLetPats (L _ x) = do
    -- liftIO $ print $ "Processss for " <> (show (toConstr x, showSDocUnsafe $ ppr x))
    Nothing

-- processNextFunBind :: LHsBindLR GhcTc GhcTc -> IO (Maybe String)
-- processNextFunBind (L _ x@(FunBind _ _ matches _ _)) = do

loopOverLHsBindLRBefore :: String -> String -> LHsBindLR GhcTc GhcTc -> IO (Maybe [FunctionInfo])
loopOverLHsBindLRBefore path moduleName x@(L _ AbsBinds {abs_binds = binds}) = do
  let fname = (getFunctionName x)
--   print $ ("Data for ") ++ show (moduleName, fname)
  let allBinds = concat $ mapMaybe loopOverFunBind (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
  let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))

  let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
      allFuns = (concatMap processExprFun allVals)
      allNrFuns = ((concatMap processExprWithARgs allVals))
--   liftIO $ print $ "Processing for" ++ (show (fname, allBinds))
--   print allNrFuns
  allExFuns <- concat <$> catMaybes <$> mapM (checkInOtherMods path moduleName) allNrFuns
--   print ("CHeck", getFunctionName x, allFuns, showSDocUnsafe $ ppr allVals) 
  let name' = map (\funInfo -> funInfo{envList =
               concat $ map (\env ->
                case HM.lookup env allLetPats of
                    Just found -> found
                    Nothing -> [env]) 
                 $ filter ((\x -> not ( x `elem` allBinds) && not (x == "$euler-webservice-6.1.1-inplace$Euler.WebService.Config.Constants$kvRedis") && (("euler" `isInfixOf` x || "_in" `isInfixOf` x || not ("$" `isInfixOf` x))))) $ envList funInfo  } ) 
                 $ filter (\(FunctionInfo _ _ funName list _) -> ("serviceconf" `isInfixOf` (toLower <$> (funName))) || not (null list) )
                 $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns ++ allFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) fname
--   print $ "DATA FOR " ++ show (name')
--   let name' = filter (\(FunctionInfo _ _ _ list _) -> not $ null list) $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns ++ allFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) (getFunctionName x)
  pure $ if null name' then Nothing else Just name'
loopOverLHsBindLRBefore _ _ (L _ x) = do
    -- liftIO $ print $ "Processing for" ++ show (toConstr x , showSDocUnsafe $ ppr x)
    pure Nothing


loopOverFunBind :: LHsBindLR GhcTc GhcTc -> (Maybe [String])
loopOverFunBind (L _ x@(FunBind _ _ matches _ _)) = do
    let inte = unLoc $ mg_alts matches
    if null inte then Nothing else do
        let y = mapMaybe loopOverVarPat $ m_pats $ unLoc $ head inte
        Just y
loopOverFunBind (L _ x) = do
    Nothing

loopOverVarPat :: LPat GhcTc -> Maybe String
loopOverVarPat (L _ (VarPat _ (L _ name))) = Just $ nameStableString $ varName name
loopOverVarPat _ = Nothing

data CompileError = CompileError
  {
    pkg_name :: String,
    mod_name :: String,
    err_msg :: String,
    src_span :: SrcSpan
  } deriving (Eq, Show)

instance ToJSON CompileError where
  toJSON (CompileError pkg modName errMsg srcLoc) =
    object [ "package_name"   .= pkg
           , "module_name"    .= modName
           , "error_message"  .= errMsg
           , "src_span"       .= show srcLoc
           ]

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  , envList :: [String]
  , src_loc :: String
  } deriving (Generic, Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

data EnvList = EnvList
  { _name :: String
  , listEnv :: [String]
  } deriving (Generic,Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

data UpdateInfo = UpdateInfo
  { createdRecordsFun :: [FunctionInfo]
  , updatedRecordsFun :: [FunctionInfo]
  , updatedFailurs :: [FunctionInfo]
  } deriving (Show, Eq, Ord)

data UpdateInfoAsText = UpdateInfoAsText
  { createdRecords :: [String]
  , updatedRecords :: [String]
  , updatedFailures :: [String]
  } deriving (Generic, Show, Eq, Ord)
    deriving (ToJSON, FromJSON)

data TypeOfUpdate = Update | Create | Default | NoChange

mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ err_msg err)

transformFromNameStableString :: (String,[String]) -> String ->  FunctionInfo
transformFromNameStableString ( str, list) loc =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) list loc
     else if length parts == 3 then FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) list loc
     else FunctionInfo "" "" "" list loc
