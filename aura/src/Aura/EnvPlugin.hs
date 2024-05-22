{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# OPTIONS_GHC -ddump-parsed-ast #-}

module Aura.EnvPlugin where

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
import Data.Maybe (mapMaybe, catMaybes, isJust)
import qualified Data.HashMap.Strict as HM
import Data.Bool (bool)
import Data.Data
import Data.List.Extra (replace,splitOn)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Extra (anyM)
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
    -- liftIO $ print (modulePath,moduleName')
    let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
    let allExprs = ((bagToList $ tcg_binds tcEnv) ^? biplateRef :: [LHsExpr GhcTc])
    liftIO $ createDirectoryIfMissing True path
    liftIO $ print $  "generating " ++ moduleName'
    (result) <- liftIO $ catMaybes <$> mapM (loopOverLHsBindLRBefore prefixPath moduleName') (bagToList $ tcg_binds tcEnv)
    (res) <- liftIO $ catMaybes <$> mapM (loopOverLHsBindLRAfter prefixPath (concat result)) (bagToList $ tcg_binds tcEnv)
    -- liftIO $ B.writeFile (modulePath <> ".json") (encodePretty $ map (\(FunctionInfo _ _ name env _) -> EnvList name env) $ concat result)
    let finalResult = foldl (\acc (EnvList name env) -> maybe (HM.insert name env acc) (\val -> HM.insert name (nub $ env ++ val) acc) $ HM.lookup name acc ) HM.empty $ map (\(FunctionInfo _ _ name env _) -> EnvList name env) $ concat (result ++ res)
    liftIO $ B.writeFile (modulePath <> ".json") (encodePretty finalResult )
    liftIO $ print $  "Completed generating " ++ moduleName' ++ " for path: " ++ (modulePath <> ".json")
    -- liftIO $ writeFile (modulePath <> ".txt") (show samp)
    pure tcEnv

changeModName moduleName' (FunctionInfo x y z _ z2) = FunctionInfo x (if y == "_in" then moduleName' else y) z [] z2


checkInOtherMods :: String -> String -> FunctionInfo -> IO (Maybe [String])
checkInOtherMods path moduleName (FunctionInfo x y z _ fileName) = do
--    liftIO $ print (moduleName,y)
   if (((not $ "euler" `isInfixOf` x ) || y == moduleName)) then do
    --  liftIO $ print $ "Omiting " ++ show (y, x, z)
     pure Nothing
   else do
    let srcPath = if "euler-events-hs" `isInfixOf` x 
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
         liftIO $ print (fileContents, z, x)
         pure Nothing) (\contents -> 
        maybe (pure Nothing) 
            (\list -> do
                -- liftIO $ print list
                pure $ Just $ maybe [] id $ HM.lookup z list ) (Aeson.decode contents :: Maybe (HM.HashMap String [String]))) fileContents


checkInOtherModsAfter :: [FunctionInfo]  -> FunctionInfo -> IO (Maybe [String])
checkInOtherModsAfter funInfos (FunctionInfo x y z _ _) = do
    let envList =  map (\(FunctionInfo _ _ name env _) -> EnvList name env) $ funInfos
    pure $ maybe Nothing
            (\list -> Just $ foldl (\acc allList -> if z == (_name allList) then (listEnv allList) ++ acc else acc) [] list  ) (Just envList)


extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [FunctionInfo]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr fun


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

processExprFun :: LHsExpr GhcTc -> [String]
processExprFun (L _ (HsApp _ funl (L _ (HsLit _ (HsString _ fstt))))) =
    let exprs = processExpr funl
    in if any (\x -> isInfixOf "lookupenv" ( toLower <$> x) || isInfixOf "Env" ( toLower <$> x)) $ (name <$> exprs) then [unpackFS fstt] else []
processExprFun (L _ (HsApp _ funl (L _ (HsVar _ y@(L _ var))))) =
    let exprs = processExpr funl
        fstt = (nameStableString $ varName var)
    in if any (\x -> isInfixOf "lookupenv" ( toLower <$> x) || isInfixOf "Env" ( toLower <$> x)) $ ( name <$> exprs) then [fstt] else []
processExprFun (L _ (HsApp _ funl lit)) = 
    let exprs = processExpr funl
        fstt = mapMaybe getLiteral $  lit ^? biplateRef
    in if any (\x -> isInfixOf "lookupenv" ( toLower <$> x) || isInfixOf "Env" ( toLower <$> x)) $ ( name <$> exprs) then fstt else []
processExprFun _ = []


getLiteral :: HsLit GhcTc -> Maybe String
getLiteral (((HsString _ fstt))) = Just $ unpackFS fstt
getLiteral _ = Nothing

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

loopOverLHsBindLRDoc :: String -> LHsBindLR GhcTc GhcTc -> IO (Maybe ([String],[FunctionInfo]))
loopOverLHsBindLRDoc path x@(L _ AbsBinds {abs_binds = binds}) = do
  let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
    --   allFuns = (concatMap processExprFun allVals)
      allNrFuns = ((concatMap processExpr allVals))
--   allExFuns <- concat <$> catMaybes <$> mapM (checkInOtherMods path) allNrFuns
--   print ("CHeck", allFuns,allNrFuns, showSDocUnsafe $ ppr allVals, allExFuns)
    --   extraList = getExtraList allExFuns
  pure $ Just (getFunctionName x, allNrFuns)
--   pure $ if null name' then Nothing else Just name'
--   if 
--           then Just name'  else Nothing
loopOverLHsBindLRDoc _ x = pure Nothing

loopOverLHsBindLRAfter :: String -> [FunctionInfo] -> LHsBindLR GhcTc GhcTc -> IO (Maybe [FunctionInfo])
loopOverLHsBindLRAfter path funInfos x@(L _ AbsBinds {abs_binds = binds}) = do
  let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
      allFuns = (concatMap processExprFun allVals)
      allNrFuns = ((concatMap processExpr allVals))
  allExFuns <- concat <$> catMaybes <$> mapM (checkInOtherModsAfter funInfos) allNrFuns
  let name' = filter (\(FunctionInfo _ _ _ list _) -> not $ null list) $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) (getFunctionName x)
--   let name' = filter (\(FunctionInfo _ _ _ list _) -> not $ null list) $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns ++ allFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) (getFunctionName x)
  pure $ if null name' then Nothing else Just name'
loopOverLHsBindLRAfter _ _ x = pure Nothing

loopOverLHsBindLRBefore :: String -> String -> LHsBindLR GhcTc GhcTc -> IO (Maybe [FunctionInfo])
loopOverLHsBindLRBefore path moduleName x@(L _ AbsBinds {abs_binds = binds}) = do
  let fname = (getFunctionName x)
--   liftIO $ print $ "Processing for" ++ (show $ getFunctionName x)
  let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
      allFuns = (concatMap processExprFun allVals)
      allNrFuns = ((concatMap processExpr allVals))
  allExFuns <- concat <$> catMaybes <$> mapM (checkInOtherMods path moduleName) allNrFuns
--   print ("CHeck",allNrFuns, getFunctionName x, allExFuns) 
  let name' = filter (\(FunctionInfo _ _ _ list _) -> not $ null list) $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns ++ allFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) fname
--   let name' = filter (\(FunctionInfo _ _ _ list _) -> not $ null list) $ map (\y -> transformFromNameStableString (y , (nub $ (allExFuns ++ allFuns))) (showSDocUnsafe $ ppr $ getLoc $ x)) (getFunctionName x)
  pure $ if null name' then Nothing else Just name'
loopOverLHsBindLRBefore _ _ x = pure Nothing

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
  in if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) list loc else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) list loc
