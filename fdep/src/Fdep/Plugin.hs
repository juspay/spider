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
import Language.Haskell.Syntax.Pat
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

sendTextData' :: WS.Connection -> Text -> Text -> IO ()
sendTextData' conn path data_ = do
    t1 <- getCurrentTime
    res <- try $ WS.sendTextData conn data_
    case res of
        Left (err :: SomeException) -> do
            when (shouldLog) $ print err
            withSocketsDo $ WS.runClient websocketHost websocketPort (T.unpack path) (\nconn -> WS.sendTextData nconn data_)
        Right _ -> pure ()
    t2 <- getCurrentTime
    when (shouldLog) $ print ("websocket call timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1))

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    when (shouldGenerateFdep) $
        liftIO $ bool P.id (void . forkIO) shouldForkPerFile $ do
            let prefixPath = case opts of
                    [] -> "/tmp/fdep/"
                    local : _ -> local
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> msHsFilePath modSummary
            let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
            when shouldLog $ print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
            createDirectoryIfMissing True path
            t1 <- getCurrentTime
            withSocketsDo $ do
                eres <- try $ WS.runClient websocketHost websocketPort ("/" <> modulePath <> ".json") (\conn -> do mapM_ (loopOverLHsBindLR conn Nothing (T.pack ("/" <> modulePath <> ".json"))) ((bagToList $ tcg_binds tcEnv)))
                case eres of
                    Left (err :: SomeException) -> when shouldLog $ print err
                    Right _ -> pure ()
            t2 <- getCurrentTime
            when shouldLog $ print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path <> " total-timetaken: " <> show (diffUTCTime t2 t1))
    return tcEnv

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args

loopOverLHsBindLR :: WS.Connection -> (Maybe Text) -> Text -> LHsBindLR GhcTc GhcTc -> IO ()
loopOverLHsBindLR con mParentName path (L _ AbsBinds{abs_binds = binds}) =
    mapM_ (loopOverLHsBindLR con mParentName path) $ bagToList binds
#if __GLASGOW_HASKELL__ >= 900
loopOverLHsBindLR con mParentName path (L _ x@(FunBind fun_ext id matches _)) = do
#else
loopOverLHsBindLR con mParentName path (L _ x@(FunBind fun_ext id matches _ _)) = do
#endif
    funName <- pure $ T.pack $ getOccString $ unLoc id
    fName <- pure $ T.pack $ nameStableString $ getName id
    let matchList = mg_alts matches
    if funName `elem` (unsafePerformIO $ decodeBlacklistedFunctions) || ("$_in$$" `T.isPrefixOf` fName)
        then pure mempty
        else do
            when (shouldLog) $ print ("processing function: " <> fName)
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack (getLoc' id)))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr . getLoc) id)))
#endif
            typeSignature <- pure $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            data_ <- pure (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String nestedNameWithParent), ("typeSignature", String typeSignature)])
            t1 <- getCurrentTime
            sendTextData' con path data_
            mapM_ (processMatch (nestedNameWithParent) path) (unLoc matchList)
            t2 <- getCurrentTime
            when (shouldLog) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
    where
        processMatch :: Text -> Text -> LMatch GhcTc (LHsExpr GhcTc) -> IO ()
        processMatch keyFunction path (L _ match) = do
#if __GLASGOW_HASKELL__ >= 900
            whereClause <- processHsLocalBinds keyFunction path $ grhssLocalBinds (m_grhss match)
#else
            whereClause <- processHsLocalBinds keyFunction path $ unLoc $ grhssLocalBinds (m_grhss match)
#endif
            mapM_ (processGRHS keyFunction path) $ grhssGRHSs (m_grhss match)

        processGRHS :: Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO ()
        processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
        processGRHS _ _ _ = pure mempty

        processHsLocalBinds :: Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
        processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
            void $ mapM (loopOverLHsBindLR con (Just keyFunction) path) $ bagToList $ x
        processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
            void $ mapM (\(recFlag, binds) -> void $ mapM (loopOverLHsBindLR con (Just keyFunction) path) $ bagToList binds) ( x)
        processHsLocalBinds _ _ _ = pure mempty

        processExpr :: Text -> Text -> LHsExpr GhcTc -> IO ()
        processExpr keyFunction path x@(L _ (HsVar _ (L _ var))) = do
            let name = T.pack $ nameStableString $ varName var
                _type = T.pack $ showSDocUnsafe $ ppr $ varType var
            expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ x, Just _type, mempty)
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
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
        processExpr keyFunction path (L _ (HsLet _ exprLStmt func)) = do
#if __GLASGOW_HASKELL__ >= 900
            processHsLocalBinds keyFunction path exprLStmt
#else
            processHsLocalBinds keyFunction path (unLoc exprLStmt)
#endif
            processExpr keyFunction path func
        processExpr keyFunction path (L _ (HsMultiIf _ exprLStmt)) =
            mapM_ (processGRHS keyFunction path) exprLStmt
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
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path y@(L _ x@(HsOverLit _ overLitVal)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr overLitVal), mempty)
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
            let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
                stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
        processExpr keyFunction path y@(L _ x@(HsConLikeOut _ hsType)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path y@(L _ x@(HsIPVar _ implicit)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_implicit$" <> T.pack (showSDocUnsafe $ ppr implicit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ (SectionL _ funl funr)) = do
            processExpr keyFunction path funl
            processExpr keyFunction path funr
#if __GLASGOW_HASKELL__ > 900
        processExpr keyFunction path (L _ (HsDo _ smtContext ((L _ stmt)))) =
            mapM_ (\(L _ x ) -> extractExprsFromStmtLRHsExpr keyFunction path x) $ stmt
        processExpr keyFunction path (L _ (HsGetField _ exprLStmt _)) =
            processExpr keyFunction path exprLStmt
        processExpr keyFunction path (L _ (ExplicitList _ funList)) =
            void $ mapM (processExpr keyFunction path) funList
        processExpr keyFunction path (L _ (HsPragE _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsProc _ lPat fun)) = do
            extractExprsFromPat keyFunction path lPat
            (extractExprsFromLHsCmdTop keyFunction path fun)
        processExpr keyFunction path (L _ (HsIf _ funl funm funr)) =
            void $ mapM (processExpr keyFunction path) $ [funl, funm, funr]
        processExpr keyFunction path (L _ (ArithSeq hsexpr exprLStmtL exprLStmtR)) = do
            processExpr keyFunction path $ wrapXRec @(GhcTc) hsexpr
            case exprLStmtL of
                Just epr -> processExpr keyFunction path $ wrapXRec @GhcTc $ syn_expr epr
                Nothing -> pure ()
            case exprLStmtR of
                From l -> processExpr keyFunction path l
                FromThen l r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path r
                FromTo l r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path r
                FromThenTo l m r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path m
                    processExpr keyFunction path r
        processExpr keyFunction path (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
            let stmtsLNoLoc = (exprLStmtL ^? biplateRef :: [HsExpr GhcTc])
                stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (map (wrapXRec @(GhcTc)) $ (stmtsLNoLoc <> stmtsRNoLoc))
        processExpr keyFunction path (L _ (HsRecFld _ exprLStmt)) =
            let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
                stmtsNoLoc = (exprLStmt ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (stmts  <> (map (wrapXRec @(GhcTc)) stmtsNoLoc))
        processExpr keyFunction path (L _ x@(RecordCon expr (L _ (iD)) rcon_flds)) =
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
            in mapM_ (\x ->
                    case x of
                        (Present _ exprs) -> processExpr keyFunction path exprs
                        _ -> pure ()) l
        processExpr keyFunction path y@(L _ (XExpr overLitVal)) = do
            processXXExpr keyFunction path overLitVal
        processExpr keyFunction path y@(L _ x@(HsOverLabel _ fs)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ (HsTcBracketOut b mQW exprLStmtL exprLStmtR)) =
            let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
                stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
        processExpr keyFunction path (L _ x) =
            let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
                stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (wrapXRec @(GhcTc)) stmtsNoLoc)))
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
            mapM_ (processExpr keyFunction path) $ [funl, funm, funr]
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
        processExpr keyFunction path (L _ x@(Recordexpr (L _ (iD)) rcon_flds)) =
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
            sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ x) =
            let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
                stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (noLoc) stmtsNoLoc)))
#endif
#if __GLASGOW_HASKELL__ > 900

        extractExprsFromLHsCmdTop :: Text -> Text -> LHsCmdTop GhcTc -> IO ()
        extractExprsFromLHsCmdTop keyFunction path (L _ cmdTop) = 
            case cmdTop of
                HsCmdTop _ cmd -> extractExprsFromLHsCmd keyFunction path cmd
                XCmdTop _ -> pure ()

        extractExprsFromLHsCmd :: Text -> Text ->  LHsCmd GhcTc -> IO ()
        extractExprsFromLHsCmd keyFunction path (L _ cmd) = extractExprsFromHsCmd keyFunction path cmd

        extractExprsFromCmdLStmt :: Text -> Text -> CmdLStmt GhcTc -> IO ()
        extractExprsFromCmdLStmt keyFunction path (L _ stmt) = extractExprsFromStmtLR keyFunction path stmt

        extractExprsFromMatchGroup :: Text -> Text -> MatchGroup GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatchGroup keyFunction path (MG _ (L _ matches) _) = mapM_ (extractExprsFromMatch keyFunction path) matches

        extractExprsFromMatch :: Text -> Text ->  LMatch GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatch keyFunction path (L _ (Match _ _ _ grhs)) = extractExprsFromGRHSs keyFunction path grhs

        extractExprsFromGRHSs :: Text -> Text ->  GRHSs GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHSs keyFunction path (GRHSs _ grhss _) = mapM_ (extractExprsFromGRHS keyFunction path)  grhss
        extractExprsFromGRHSs keyFunction path (XGRHSs _) = pure ()

        extractExprsFromGRHS :: Text -> Text ->  LGRHS GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHS keyFunction path (L _ (GRHS _ _ body)) = extractExprsFromLHsCmd keyFunction path body
        extractExprsFromGRHS keyFunction path (L _ (XGRHS _)) = pure ()

        extractExprsFromStmtLR :: Text -> Text -> StmtLR GhcTc GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromStmtLR keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body -> do
                extractExprsFromPat keyFunction path pat
                extractExprsFromLHsCmd keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                mapM_ (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path binds
            ParStmt _ blocks _ bindOp -> do
                mapM_ (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                mapM_ (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (trS_stmts)
                processExpr keyFunction path trS_using
                mapM_ (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path  trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (wrapXRec @GhcTc trS_fmap)
            RecStmt{..} -> do
                mapM_ (extractExprsFromStmtLR keyFunction path . unLoc) (unLoc recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            XStmtLR _ -> pure ()

        extractExprsFromParStmtBlock :: Text -> Text -> ParStmtBlock GhcTc GhcTc -> IO ()
        extractExprsFromParStmtBlock keyFunction path (ParStmtBlock _ stmts _ _) =
            mapM_ (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) stmts

        processSynExpr keyFunction path (SyntaxExprTc { syn_expr      = expr}) = processExpr keyFunction path (wrapXRec @GhcTc $ expr)
        processSynExpr _ _ _ = pure ()

        extractExprsFromStmtLRHsExpr :: Text -> Text -> StmtLR GhcTc GhcTc (LHsExpr GhcTc) -> IO ()
        extractExprsFromStmtLRHsExpr keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body -> do
                extractExprsFromPat keyFunction path pat
                processExpr keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                mapM_ (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path binds
            ParStmt _ blocks _ bindOp -> do
                mapM_ (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                mapM_ (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) trS_stmts
                processExpr keyFunction path trS_using
                mapM_ (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (wrapXRec @GhcTc trS_fmap)
            RecStmt{..} -> do
                mapM_ (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (unXRec @GhcTc $ recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            XStmtLR _ -> pure ()

        extractExprsFromHsCmd :: Text -> Text -> HsCmd GhcTc -> IO ()
        extractExprsFromHsCmd keyFunction path cmd = case cmd of
            HsCmdArrApp _ f arg _ _ ->
                void $ mapM (processExpr keyFunction path) [f, arg]
            HsCmdArrForm _ e _ _ cmdTops -> do
                mapM_ (extractExprsFromLHsCmdTop keyFunction path) cmdTops
                processExpr keyFunction path e
            HsCmdApp _ cmd' e -> do
                extractExprsFromLHsCmd keyFunction path cmd'
                processExpr keyFunction path e
            HsCmdLam _ mg -> extractExprsFromMatchGroup keyFunction path mg
            HsCmdPar _ cmd' ->
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdCase _ e mg -> do
                extractExprsFromMatchGroup keyFunction path mg
                processExpr keyFunction path e
            HsCmdLamCase _ mg ->
                extractExprsFromMatchGroup keyFunction path mg
            HsCmdIf _ _ predExpr thenCmd elseCmd -> do
                extractExprsFromLHsCmd keyFunction path elseCmd
                extractExprsFromLHsCmd keyFunction path thenCmd
                processExpr keyFunction path predExpr
            HsCmdLet _ binds cmd' -> do
                processHsLocalBinds keyFunction path binds
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdDo _ stmts ->
                mapM_ (extractExprsFromCmdLStmt keyFunction path )(unLoc stmts)
            XCmd _ -> pure ()

        extractExprsFromPat :: Text -> Text -> LPat GhcTc -> IO ()
        extractExprsFromPat keyFunction path y@(L _ pat) =
            case pat of
                WildPat hsType     -> do
                    expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                VarPat _ var    -> processExpr keyFunction path ((wrapXRec @(GhcTc)) (HsVar noExtField (var)))
                LazyPat _ p   -> (extractExprsFromPat keyFunction path) p
                AsPat _ var p   -> do
                    processExpr keyFunction path ((wrapXRec @(GhcTc)) (HsVar noExtField (var)))
                    (extractExprsFromPat keyFunction path) p
                ParPat _ p    -> (extractExprsFromPat keyFunction path) p
                BangPat _ p   -> (extractExprsFromPat keyFunction path) p
                ListPat _ ps  -> mapM_ (extractExprsFromPat keyFunction path) ps
                TuplePat _ ps _ -> mapM_ (extractExprsFromPat keyFunction path) ps
                SumPat hsTypes p _ _ -> do 
                    mapM_ (\hsType -> do
                                expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                                sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                            ) hsTypes
                    (extractExprsFromPat keyFunction path) p
                ConPat {pat_args = args} -> (extractExprsFromHsConPatDetails args)
                ViewPat hsType expr p -> do
                    expr' <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr')])
                    processExpr keyFunction path expr
                    (extractExprsFromPat keyFunction path) p
                SplicePat _ splice -> mapM_ (processExpr keyFunction path) $ extractExprsFromSplice splice
                LitPat _ hsLit     -> do
                    expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
                    sendTextData' con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                NPat _ (L _ overLit) _ _ -> do
                    extractExprsFromOverLit overLit
                NPlusKPat _ _ (L _ overLit) _ _ _ ->
                    extractExprsFromOverLit overLit
                SigPat _ p _   -> (extractExprsFromPat keyFunction path) p
                XPat _         -> pure ()
            where
            extractExprsFromOverLit :: HsOverLit GhcTc -> IO ()
            extractExprsFromOverLit (OverLit _ _ e) = processExpr keyFunction path $ wrapXRec @(GhcTc) e

            extractExprsFromHsConPatDetails :: HsConPatDetails GhcTc -> IO ()
            extractExprsFromHsConPatDetails (PrefixCon _ args) = mapM_ (extractExprsFromPat keyFunction path) args
            extractExprsFromHsConPatDetails x@(RecCon (HsRecFields {rec_flds = flds})) =
                let stmtsLNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
                in mapM_ (processExpr keyFunction path) $ map (wrapXRec @(GhcTc)) stmtsLNoLoc
            extractExprsFromHsConPatDetails (InfixCon p1 p2) = do
                (extractExprsFromPat keyFunction path) p1
                (extractExprsFromPat keyFunction path) p2

        extractExprFromApplicativeArg :: Text -> Text -> ApplicativeArg GhcTc -> IO ()
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgOne _ lpat expr _) = do 
            processExpr keyFunction path expr
            extractExprsFromPat keyFunction path lpat
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgMany _ exprLStmt stmts lpat _) = do
            processExpr keyFunction path (wrapXRec @(GhcTc) stmts)
            mapM_ (extractExprsFromStmtLRHsExpr keyFunction path) (map (unLoc) exprLStmt)
            extractExprsFromPat keyFunction path lpat

        extractLStmt :: Text -> Text -> GenLocated l (StmtLR GhcTc GhcTc (GenLocated SrcSpanAnnA (HsCmd GhcTc))) -> IO ()
        extractLStmt keyFunction path (L _ smtlr) = extractExprsFromStmtLR keyFunction path smtlr

        extractExprsFromSplice :: HsSplice GhcTc -> [LHsExpr GhcTc]
        extractExprsFromSplice (HsTypedSplice _ _ _ e) = [e]
        extractExprsFromSplice (HsUntypedSplice _ _ _ e) = [e]
        extractExprsFromSplice (HsQuasiQuote _ _ _ _ _) = []
        extractExprsFromSplice (HsSpliced _ _ _) = []

        processXXExpr :: Text -> Text -> XXExprGhcTc -> IO ()
        processXXExpr keyFunction path (WrapExpr (HsWrap hsWrapper hsExpr)) =
            processExpr keyFunction path (wrapXRec @(GhcTc) hsExpr)
        processXXExpr keyFunction path (ExpansionExpr (HsExpanded _ expansionExpr)) =
            mapM_ (processExpr keyFunction path . (wrapXRec @(GhcTc))) [expansionExpr]
loopOverLHsBindLR _ _ _ (L _ VarBind{var_rhs = rhs}) = pure mempty
loopOverLHsBindLR _ _ _ (L _ (PatSynBind _ PSB{psb_def = def})) = pure mempty
loopOverLHsBindLR _ _ _ (L _ (PatSynBind _ (XPatSynBind _))) = pure mempty
loopOverLHsBindLR _ _ _ (L _ (XHsBindsLR _)) = pure mempty
loopOverLHsBindLR _ _ _ (L _ (PatBind _ _ pat_rhs _)) = pure mempty
loopOverLHsBindLR _ _ _ _ = pure mempty

getLocTC' = (showSDocUnsafe . ppr . la2r . getLoc)
getLoc'   = (showSDocUnsafe . ppr . la2r . getLoc)
#else
getLocTC' = (showSDocUnsafe . ppr . getLoc)
getLoc' = (showSDocUnsafe . ppr . getLoc)
#endif