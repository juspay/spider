{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Fdep.Plugin (plugin,collectDecls) where

import Control.Concurrent ( forkIO )
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Aeson ( encode, Value(String, Object), ToJSON(toJSON) )
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (toStrict, writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data ()
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time ( diffUTCTime, getCurrentTime )
import Fdep.Types
    ( PFunction(PFunction),
      FunctionInfo(FunctionInfo) )
import Text.Read (readMaybe)
import Prelude hiding (id, writeFile)
import qualified Prelude as P
import qualified Data.List.Extra as Data.List
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import System.Directory ( createDirectoryIfMissing )
import System.Environment (lookupEnv)
import GHC.IO (unsafePerformIO)
#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..))
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import GHC.Hs.Expr
#else
import qualified Data.HashMap.Strict as HM
import Bag (bagToList)
import DynFlags ()
import GHC
import GHC.Hs.Binds
    ( HsBindLR(PatBind, FunBind, AbsBinds, VarBind, PatSynBind,
               XHsBindsLR, fun_id, abs_binds, var_rhs),
      LHsBindLR,
      HsValBindsLR(XValBindsLR, ValBinds),
      HsLocalBindsLR(HsValBinds),
      NHsValBindsLR(NValBinds),
      PatSynBind(XPatSynBind, PSB, psb_def) )
import GHC.Hs.Decls
    ( HsDecl(SigD, TyClD, InstD, DerivD, ValD), LHsDecl )
import GhcPlugins (HsParsedModule, Hsc, Plugin (..), PluginRecompile (..), Var (..), getOccString, hpm_module, ppr, showSDocUnsafe)
import HscTypes (ModSummary (..),msHsFilePath)
import Name (nameStableString)
import Outputable ()
import Plugins (CommandLineOption, defaultPlugin)
import SrcLoc ( GenLocated(L), getLoc, noLoc, unLoc )
import TcRnTypes (TcGblEnv (..), TcM)
import StringBuffer
#endif

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Exception (try, Handler(..), catches)
import qualified Database.SQLite.Simple as SQLite

maxRetries :: Int
maxRetries = 5

initialDelayMicros :: Int
initialDelayMicros = 100000

plugin :: Plugin
plugin =
    defaultPlugin
        { typeCheckResultAction = fDep
        , pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectDecls
        }

filterList :: [Text]
filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
    , "showList"
    , "toConstr"
    , "toDomResAcc"
    , "toEncoding"
    , "toEncodingList"
    , "toEnum"
    , "toForm"
    , "toHaskellString"
    , "toInt"
    , "toJSON"
    , "toJSONList"
    , "toJSONWithOptions"
    , "encodeJSON"
    , "gfoldl"
    , "ghmParser"
    , "gmapM"
    , "gmapMo"
    , "gmapMp"
    , "gmapQ"
    , "gmapQi"
    , "gmapQl"
    , "gmapQr"
    , "gmapT"
    , "parseField"
    , "parseJSON"
    , "parseJSONList"
    , "parseJSONWithOptions"
    , "hasField"
    , "gunfold"
    , "getField"
    , "_mapObjectDeep'"
    , "_mapObjectDeep"
    , "_mapObjectDeepForSnakeCase"
    , "!!"
    , "/="
    , "<"
    , "<="
    , "<>"
    , "<$"
    , "=="
    , ">"
    , ">="
    , "readsPrec"
    , "readPrec"
    , "toDyn"
    , "fromDyn"
    , "fromDynamic"
    , "compare"
    , "readListPrec"
    , "toXml"
    , "fromXml"
    ]

collectDecls :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectDecls opts modSummary hsParsedModule = do
    _ <- liftIO $
        forkIO $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fdep/"
                    local : _ -> local
                modulePath = prefixPath <> msHsFilePath modSummary
                path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                declsList = hsmodDecls $ unLoc $ hpm_module hsParsedModule
            createDirectoryIfMissing True path
            functionsVsCodeString <- mapM getDecls declsList
            writeFile (modulePath <> ".function_code.json") (encodePretty $ Map.fromList $ concat functionsVsCodeString)
    pure hsParsedModule

getDecls :: LHsDecl GhcPs -> IO [(Text, PFunction)]
getDecls x = do
    case x of
        (L _ (TyClD _ _)) -> pure mempty
        (L _ (InstD _ _)) -> pure mempty
        (L _ (DerivD _ _)) -> pure mempty
        (L _ (ValD _ bind)) -> pure $ getFunBind bind
        (L _ (SigD _ _)) -> pure mempty
        _ -> pure mempty
  where
    getFunBind f@FunBind{fun_id = funId} = [((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ getLoc' funId), PFunction ((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ getLoc' funId)) (T.pack $ showSDocUnsafe $ ppr f) (T.pack $ getLoc' funId))]
    getFunBind _ = mempty

shouldForkPerFile :: Bool
shouldForkPerFile = readBool $ unsafePerformIO $ lookupEnv "SHOULD_FORK"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldGenerateFdep :: Bool
shouldGenerateFdep = readBool $ unsafePerformIO $ lookupEnv "GENERATE_FDEP"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldLog :: Bool
shouldLog = readBool $ unsafePerformIO $ lookupEnv "ENABLE_LOGS"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool _ = False

websocketPort :: Int
websocketPort = maybe 8000 (fromMaybe 8000 . readMaybe) $ unsafePerformIO $ lookupEnv "SERVER_PORT"

websocketHost :: String
websocketHost = fromMaybe "localhost" $ unsafePerformIO $ lookupEnv "SERVER_HOST"

decodeBlacklistedFunctions :: IO [Text]
decodeBlacklistedFunctions = do
    mBlackListedFunctions <- lookupEnv "BLACKLIST_FUNCTIONS_FDEP"
    pure $ case mBlackListedFunctions of
        Just val' ->
            case A.decode $ BL.fromStrict $ encodeUtf8 (T.pack val') of
                Just val -> filterList <> val
                _ -> filterList
        _ -> filterList

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    when (shouldGenerateFdep) $
        liftIO $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fdep/"
                    local : _ -> local
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> msHsFilePath modSummary
            let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
            when shouldLog $ print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
            createDirectoryIfMissing True path
            t1 <- getCurrentTime
            mapM_ (\x -> loopOverLHsBindLR Nothing (T.pack ("/" <> modulePath <> ".json")) x) $ bagToList $ tcg_binds tcEnv
            t2 <- getCurrentTime
            when shouldLog $ print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path <> " total-timetaken: " <> show (diffUTCTime t2 t1))
    return tcEnv

withRetry :: SQLite.Connection -> IO a -> IO a
withRetry conn action = retry 0 initialDelayMicros
  where
    retry attempt delay
      | attempt >= maxRetries = error "Max retries reached"
      | otherwise = do
          result <- try action
          case result of
            Left e -> handleError e attempt delay
            Right value -> return value

    handleError e attempt delay = do
      case e of
        SQLite.SQLError SQLite.ErrorBusy _ _ -> do
          putStrLn $ "Database locked, retrying (attempt " ++ show (attempt + 1) ++ ")"
          threadDelay delay
          retry (attempt + 1) (delay * 2)
        _ -> error $ "SQLite error: " ++ show e

sendTextData' :: Text -> Text -> IO ()
sendTextData' path data_ = do
    SQLite.withConnection (tail $ (T.unpack path) <> "_fdep.db") $ \conn -> do
        withRetry conn $ do
            SQLite.execute_ conn createTableQuery
            SQLite.execute conn insertQuery (path, data_)
  where
    createTableQuery = "CREATE TABLE IF NOT EXISTS fdep (path TEXT, data TEXT)"
    insertQuery = "INSERT INTO fdep (path, data) VALUES (?, ?)"

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args

loopOverLHsBindLR :: (Maybe Text) -> Text -> LHsBindLR GhcTc GhcTc -> IO ()
#if __GLASGOW_HASKELL__ >= 900
loopOverLHsBindLR mParentName path (L _ x@(FunBind fun_ext id matches _)) = do
#else
loopOverLHsBindLR mParentName path (L _ x@(FunBind fun_ext id matches _ _)) = do
#endif
    funName <- evaluate $ force $ T.pack $ getOccString $ unLoc id
    fName <- evaluate $ force $ T.pack $ nameStableString $ getName id
    let matchList = mg_alts matches
    if funName `elem` (unsafePerformIO $ decodeBlacklistedFunctions) || ("$_in$$" `T.isPrefixOf` fName)
        then pure mempty
        else do
            when (shouldLog) $ print ("processing function: " <> fName)
#if __GLASGOW_HASKELL__ >= 900
            name <- evaluate $ force (fName <> "**" <> (T.pack (getLoc' id)))
#else
            name <- evaluate $ force (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr . getLoc) id)))
#endif
            typeSignature <- evaluate $ force $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
            nestedNameWithParent <- evaluate $ force $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            data_ <- evaluate $ force $ (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String nestedNameWithParent), ("typeSignature", String typeSignature)])
            t1 <- getCurrentTime

            void $ mapM (processMatch (nestedNameWithParent) path) (unLoc matchList)

            t2 <- getCurrentTime
            when (shouldLog) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
loopOverLHsBindLR mParentName path (L _ AbsBinds{abs_binds = binds}) =
    void $ mapM (loopOverLHsBindLR mParentName path) $ bagToList binds
loopOverLHsBindLR _ _ _ = pure mempty
-- loopOverLHsBindLR _ _ _ (L _ VarBind{var_rhs = rhs}) = pure mempty
-- loopOverLHsBindLR _ _ _ (L _ (PatSynBind _ PSB{psb_def = def})) = pure mempty
-- loopOverLHsBindLR _ _ _ (L _ (PatSynBind _ (XPatSynBind _))) = pure mempty
-- loopOverLHsBindLR _ _ _ (L _ (XHsBindsLR _)) = pure mempty
-- loopOverLHsBindLR _ _ _ (L _ (PatBind _ _ pat_rhs _)) = pure mempty

processMatch :: Text -> Text -> LMatch GhcTc (LHsExpr GhcTc) -> IO ()
processMatch keyFunction path (L _ match) = do
#if __GLASGOW_HASKELL__ >= 900
    whereClause <- processHsLocalBinds keyFunction path $ grhssLocalBinds (m_grhss match)
#else
    whereClause <- processHsLocalBinds keyFunction path $ unLoc $ grhssLocalBinds (m_grhss match)
#endif
    void $ mapM (processGRHS keyFunction path) $ grhssGRHSs (m_grhss match)
    pure mempty

processGRHS :: Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO ()
processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
processGRHS _ _ _ = pure mempty

processHsLocalBinds :: Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
    void $ mapM (loopOverLHsBindLR (Just keyFunction) path) $ bagToList $ x
processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
    void $ mapM (\(recFlag, binds) -> void $ mapM (loopOverLHsBindLR (Just keyFunction) path) $ bagToList binds) ( x)
processHsLocalBinds _ _ _ = pure mempty

processExpr :: Text -> Text -> LHsExpr GhcTc -> IO ()
processExpr keyFunction path x@(L _ (HsVar _ (L _ var))) = do
    let name = T.pack $ nameStableString $ varName var
        _type = T.pack $ showSDocUnsafe $ ppr $ varType var
    expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ x, Just _type, mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr _ _ (L _ (HsUnboundVar _ _)) = pure mempty
processExpr keyFunction path (L _ (HsApp _ funl funr)) = do
    processExpr keyFunction path funl
    processExpr keyFunction path funr
processExpr keyFunction path (L _ (OpApp _ funl funm funr)) = do
    processExpr keyFunction path funl
    processExpr keyFunction path funm
    processExpr keyFunction path funr
processExpr keyFunction path (L _ (NegApp _ funl _)) =
    processExpr keyFunction path funl
processExpr keyFunction path (L _ (HsTick _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsStatic _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsBinTick _ _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (ExprWithTySig _ fun _)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsDo _ smtContext exprLStmt)) =
    let stmts = (smtContext ^? biplateRef :: [LHsExpr GhcTc]) <> (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) ( stmts)
processExpr keyFunction path (L _ (HsLet _ exprLStmt func)) = do
#if __GLASGOW_HASKELL__ >= 900
    processHsLocalBinds keyFunction path exprLStmt
#else
    processHsLocalBinds keyFunction path (unLoc exprLStmt)
#endif
    processExpr keyFunction path func
processExpr keyFunction path (L _ (HsMultiIf _ exprLStmt)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
     in void $ mapM (processExpr keyFunction path) ( stmts)
processExpr keyFunction path (L _ (HsCase _ funl exprLStmt)) = do
    processExpr keyFunction path funl
    void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
processExpr keyFunction path (L _ (ExplicitSum _ _ _ fun)) = processExpr keyFunction path fun
processExpr keyFunction path (L _ (SectionR _ funl funr)) = processExpr keyFunction path funl <> processExpr keyFunction path funr
processExpr keyFunction path (L _ (HsPar _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsAppType _ fun _)) = processExpr keyFunction path fun
processExpr keyFunction path (L _ x@(HsLamCase _ exprLStmt)) =
    void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
processExpr keyFunction path (L _ x@(HsLam _ exprLStmt)) =
    void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
processExpr keyFunction path y@(L _ x@(HsLit _ hsLit)) = do
    expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path y@(L _ x@(HsOverLit _ overLitVal)) = do
    expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr overLitVal), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
processExpr keyFunction path (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
processExpr keyFunction path y@(L _ x@(HsConLikeOut _ hsType)) = do
    expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path y@(L _ x@(HsIPVar _ implicit)) = do
    expr <- pure $ transformFromNameStableString (Just $ ("$_implicit$" <> T.pack (showSDocUnsafe $ ppr implicit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path (L _ (SectionL _ funl funr)) = processExpr keyFunction path funl <> processExpr keyFunction path funr

#if __GLASGOW_HASKELL__ > 900
processExpr keyFunction path (L _ (HsGetField _ exprLStmt _)) =
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
        stmtsNoLoc = exprLStmt ^? biplateRef :: [HsExpr GhcTc]
     in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (wrapXRec @(GhcTc)) stmtsNoLoc)))
processExpr keyFunction path (L _ (ExplicitList _ funList)) =
    void $ mapM (processExpr keyFunction path) funList
processExpr keyFunction path (L _ (HsPragE _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsProc _ lPat fun)) = do
    let stmts = lPat ^? biplateRef :: [LHsExpr GhcTc]
        stmts' = fun ^? biplateRef :: [LHsExpr GhcTc]
    void $ mapM (processExpr keyFunction path) (stmts <> stmts')
processExpr keyFunction path (L _ (HsIf exprLStmt funl funm funr)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) $ [funl, funm, funr] <> stmts
processExpr keyFunction path (L _ (HsTcBracketOut b mQW exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
processExpr keyFunction path (L _ (ArithSeq _ Nothing exprLStmtR)) =
    let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
        stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsR <> ((map (wrapXRec @(GhcTc)) $ stmtsRNoLoc)))
processExpr keyFunction path (L _ (HsRecFld _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (exprLStmt ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) ( (stmts  <> (map (wrapXRec @(GhcTc)) stmtsNoLoc)))
processExpr keyFunction path (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsLNoLoc = (exprLStmtL ^? biplateRef :: [HsExpr GhcTc])
        stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (map (wrapXRec @(GhcTc)) $ (stmtsLNoLoc <> stmtsRNoLoc))
processExpr keyFunction path (L _ x@(RecordCon expr (L _ (iD)) rcon_flds)) = do
    let stmts = (rcon_flds ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (rcon_flds ^? biplateRef :: [HsExpr GhcTc])
        stmtsNoLocexpr = (expr ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmts <> (map (wrapXRec @(GhcTc)) (stmtsNoLoc <> stmtsNoLocexpr)))
processExpr keyFunction path (L _ (RecordUpd _ rupd_expr rupd_flds)) =
    let stmts = (rupd_flds ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (rupd_flds ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmts <> (map (wrapXRec @(GhcTc)) stmtsNoLoc))
processExpr keyFunction path (L _ (ExplicitTuple _ exprLStmt _)) =
    let l = (exprLStmt)
    in void $ mapM (\x ->
            case x of
                (Present _ exprs) -> processExpr keyFunction path exprs
                _ -> pure ()) l
processExpr keyFunction path y@(L _ (XExpr overLitVal)) = do
    processXXExpr keyFunction path overLitVal
processExpr keyFunction path y@(L _ x@(HsOverLabel _ fs)) = do
    expr <- transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path (L _ x) = pure ()
    -- let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
    --     stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
    -- in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (wrapXRec @(GhcTc)) stmtsNoLoc)))

processXXExpr :: Text -> Text -> XXExprGhcTc -> IO ()
processXXExpr keyFunction path (WrapExpr (HsWrap hsWrapper hsExpr)) = do
    processExpr keyFunction path (wrapXRec @(GhcTc) hsExpr)
processXXExpr keyFunction path x =
    let stmtsL = (x ^? biplateRef :: [HsExpr GhcTc])
    in void $ mapM (processExpr keyFunction path . (wrapXRec @(GhcTc))) ( stmtsL)

#else
processExpr keyFunction path (L _ (ExplicitTuple _ exprLStmt _)) =
    let l = (unLoc <$> exprLStmt)
    in void $ mapM (\x ->
            case x of
                (Present _ exprs) -> processExpr keyFunction path exprs
                _ -> pure ()) l
processExpr keyFunction path (L _ (ExplicitList _ _ funList)) =
    void $ mapM (processExpr keyFunction path) ( funList)
processExpr keyFunction path (L _ (HsTickPragma _ _ _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsSCC _ _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ (HsCoreAnn _ _ _ fun)) =
    processExpr keyFunction path fun
processExpr keyFunction path (L _ x@(HsWrap _ _ fun)) =
    processExpr keyFunction path (noLoc fun)
processExpr keyFunction path (L _ (HsIf _ exprLStmt funl funm funr)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) $ [funl, funm, funr] <> stmts
processExpr keyFunction path (L _ (HsTcBracketOut b exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
processExpr keyFunction path (L _ (ArithSeq _ Nothing exprLStmtR)) =
    let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
        stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsR <> ((map noLoc) $ stmtsRNoLoc))
processExpr keyFunction path (L _ (HsRecFld _ exprLStmt)) =
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (exprLStmt ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) ( (stmts  <> (map noLoc) stmtsNoLoc))
processExpr keyFunction path (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
        stmtsLNoLoc = (exprLStmtL ^? biplateRef :: [HsExpr GhcTc])
        stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR <> (map noLoc $ (stmtsLNoLoc <> stmtsRNoLoc)))
processExpr keyFunction path (L _ x@(Recordexpr (L _ (iD)) rcon_flds)) = do
    let stmts = (rcon_flds ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (rcon_flds ^? biplateRef :: [HsExpr GhcTc])
        stmtsNoLocexpr = (expr ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmts <> (map noLoc) (stmtsNoLoc <> stmtsNoLocexpr))
processExpr keyFunction path (L _ (RecordUpd _ rupd_expr rupd_flds)) =
    let stmts = (rupd_flds ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (rupd_flds ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) (stmts <> (map noLoc) stmtsNoLoc)
processExpr keyFunction path y@(L _ (XExpr overLitVal)) =
    let stmts = (overLitVal ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (overLitVal ^? biplateRef :: [HsExpr GhcTc])
    in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (noLoc) stmtsNoLoc)))
processExpr keyFunction path y@(L _ x@(HsOverLabel _ mIdp fs)) = do
    print $ showSDocUnsafe $ ppr mIdp
    expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
    sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr keyFunction path (L _ x) =
    let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
        stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
     in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (noLoc) stmtsNoLoc)))
#endif



#if __GLASGOW_HASKELL__ > 900
getLocTC' = (showSDocUnsafe . ppr . la2r . getLoc)
getLoc'   = (showSDocUnsafe . ppr . la2r . getLoc)
#else
getLocTC' = (showSDocUnsafe . ppr . getLoc)
getLoc' = (showSDocUnsafe . ppr . getLoc)
#endif