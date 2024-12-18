{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns,PartialTypeSignatures #-}
{-# OPTIONS_GHC -Werror=unused-imports -Werror=incomplete-patterns -Werror=name-shadowing #-}
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
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data ()
import Data.List.Extra (splitOn,nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time ( diffUTCTime, getCurrentTime )
import Fdep.Types
import Text.Read (readMaybe)
import Prelude hiding (id, writeFile,span)
import qualified Prelude as P
import qualified Data.List.Extra as Data.List
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
-- import System.Directory ( createDirectoryIfMissing )
import System.Environment (lookupEnv)
import GHC.IO (unsafePerformIO)
#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Hs.Pat
import GHC.Unit.Types
import GHC
import GHC.Types.SourceText
import GHC.Driver.Plugins
import GHC.Types.Name.Reader
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
#else
import TyCoRep
import DataCon
import qualified Data.HashMap.Strict as HM
import Bag (bagToList)
import DynFlags ()
import GHC
import BasicTypes
import GhcPlugins (tyConName,rdrNameOcc,occNameString,RdrName (..),HsParsedModule, Hsc, Plugin (..), PluginRecompile (..), Var (..), getOccString, hpm_module, ppr, showSDocUnsafe)
import HscTypes (msHsFilePath)
import Name (nameStableString)
import Outputable ()
import Plugins (CommandLineOption, defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
#endif

plugin :: Plugin
plugin =
    defaultPlugin
        { typeCheckResultAction = fDep
        , pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectDecls
        , desugarResultAction = handleWarns
        }

handleWarns :: [CommandLineOption] -> (Maybe ModSummary) -> TcGblEnv -> ModGuts -> Hsc ModGuts
handleWarns _ _ _ x =do
    dflags <- getDynFlags
    logger <- getLogger
    warnings <- getWarnings
    liftIO $ printOrThrowWarnings logger dflags warnings
    clearWarnings
    x

    where
        getWarnings :: Hsc WarningMessages
        getWarnings = Hsc $ \_ w -> return (w, w)

        clearWarnings :: Hsc ()
        clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)


sendFileToWebSocketServer :: CliOptions -> Text -> _ -> IO ()
sendFileToWebSocketServer cliOptions path data_ =
    withSocketsDo $ do
        eres <- try $
            WS.runClient
                (fromMaybe (host cliOptions) websocketHost)
                (fromMaybe (port cliOptions) websocketPort)
                (T.unpack path)
                (\conn -> do
                    res <- try $ WS.sendTextData conn data_
                    case res of
                        Left (err :: SomeException) ->
                            when (shouldLog || Fdep.Types.log cliOptions) $ print err
                        Right _ -> pure ()
                )
        case eres of
            Left (err :: SomeException) ->
                when (shouldLog || Fdep.Types.log cliOptions) $ print err
            Right _ -> pure ()

collectDecls :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectDecls opts modSummary hsParsedModule = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) -> 
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    _ <- liftIO $
        forkIO $ do
            let prefixPath = path cliOptions
                modulePath = prefixPath <> msHsFilePath modSummary
            let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                declsList = hsmodDecls $ unLoc $ hpm_module hsParsedModule
            -- createDirectoryIfMissing True path
            (functionsVsCodeString,typesCodeString,classCodeString,instanceCodeString) <- processDecls declsList
            let importsList = concatMap (fromGHCImportDecl) (hsmodImports $ unLoc $ hpm_module hsParsedModule)
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".module_imports.json") (decodeUtf8 $ toStrict $ encodePretty $ importsList)
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".function_code.json") (decodeUtf8 $ toStrict $ encodePretty $ Map.fromList functionsVsCodeString)
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".types_code.json") (decodeUtf8 $ toStrict $ encodePretty $ typesCodeString)
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".class_code.json") (decodeUtf8 $ toStrict $ encodePretty $ classCodeString)
            sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".instance_code.json") (decodeUtf8 $ toStrict $ encodePretty $ instanceCodeString)
            -- writeFile (modulePath <> ".module_imports.json") (encodePretty $ importsList)
            -- writeFile (modulePath <> ".function_code.json") (encodePretty $ Map.fromList functionsVsCodeString)
            -- writeFile (modulePath <> ".types_code.json") (encodePretty $ typesCodeString)
            -- writeFile (modulePath <> ".class_code.json") (encodePretty $ classCodeString)
            -- writeFile (modulePath <> ".instance_code.json") (encodePretty $ instanceCodeString)
    pure hsParsedModule

fromGHCImportDecl :: LImportDecl GhcPs -> [SimpleImportDecl]
fromGHCImportDecl (L _span ImportDecl{..}) = [SimpleImportDecl {
    moduleName' = moduleNameToText (unLoc ideclName),
    packageName = fmap stringLiteralToText ideclPkgQual,
#if __GLASGOW_HASKELL__ >= 900
    isBootSource = case ideclSource of
            IsBoot -> True
            NotBoot -> False,
#else
    isBootSource = ideclSource,
#endif
    isSafe = ideclSafe,
    qualifiedStyle = convertQualifiedStyle ideclQualified,
    isImplicit = ideclImplicit,
    asModuleName = fmap (moduleNameToText . unLoc) ideclAs,
    hidingSpec = case ideclHiding of
        Nothing -> Nothing
        Just (isHiding, names) -> Just $ HidingSpec {
            isHiding = isHiding,
            names = convertLIEsToText names
        },
    line_number = spanToLine _span
}]
fromGHCImportDecl (L span (XImportDecl _)) = []

moduleNameToText :: ModuleName -> T.Text
moduleNameToText = T.pack . moduleNameString

stringLiteralToText :: StringLiteral -> T.Text
stringLiteralToText StringLiteral {sl_st} =
    case sl_st  of
        SourceText s -> T.pack s
        _ -> T.pack "NoSourceText"

convertQualifiedStyle :: ImportDeclQualifiedStyle -> QualifiedStyle
convertQualifiedStyle GHC.NotQualified     = Fdep.Types.NotQualified
convertQualifiedStyle QualifiedPre     = Fdep.Types.Qualified
convertQualifiedStyle QualifiedPost    = Fdep.Types.Qualified

-- (GenLocated (Anno [GenLocated l (IE GhcPs)]) [GenLocated l (IE GhcPs)])
convertLIEsToText :: _ -> [T.Text]
convertLIEsToText lies = 
#if __GLASGOW_HASKELL__ >= 900
    concatMap (ieNameToText . unLoc) (unXRec @(GhcPs) lies)
#else
    concatMap (ieNameToText . unLoc) (unLoc lies)
#endif
  where
    ieNameToText :: IE GhcPs -> [T.Text]
    ieNameToText x = map rdrNameToText $ ieNames x

    rdrNameToText = T.pack . occNameString . rdrNameOcc

processDecls :: [LHsDecl GhcPs] -> IO ([(Text, PFunction)], [PType], [PClass], [PInstance])
processDecls decls = do
    results <- mapM getDecls' decls
    pure ( concatMap (\(f,_,_,_) -> f) results
         , concatMap (\(_,t,_,_) -> t) results
         , concatMap (\(_,_,c,_) -> c) results
         , concatMap (\(_,_,_,i) -> i) results
         )

#if __GLASGOW_HASKELL__ >= 900
spanToLine :: _ -> (Int,Int)
spanToLine s = (srcSpanStartLine $ la2r s,srcSpanEndLine $ la2r s)
#else
spanToLine :: SrcSpan -> (Int,Int)
spanToLine (UnhelpfulSpan _) = (-1,-1)
spanToLine (RealSrcSpan s) = (srcSpanStartLine s,srcSpanEndLine s)
-- srcLocSpan :: SrcLoc -> SrcSpan
-- srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
-- srcLocSpan (RealSrcLoc l) = RealSrcSpan (realSrcLocSpan l)
#endif


-- Modified function to extract all declarations
getDecls' :: LHsDecl GhcPs -> IO ([(Text, PFunction)], [PType], [PClass], [PInstance])
getDecls' x = case x of
    (L span (TyClD _ decl)) -> pure (mempty, getTypeDecl span decl, getClassDecl span decl, mempty)
    (L span (InstD _ inst)) -> pure (mempty, mempty, mempty, getInstDecl span inst)
    (L span (DerivD _ _)) -> pure mempty4
    (L span (ValD _ bind)) -> pure (getFunBind span bind, mempty, mempty, mempty)
    (L span (SigD _ _)) -> pure mempty4
    _ -> pure mempty4
  where
    mempty4 = (mempty, mempty, mempty, mempty)

    -- Extract function bindings (original code)
    getFunBind _span f@FunBind{fun_id = funId} = 
        [( T.pack (showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> T.pack (getLoc' funId)
         , PFunction 
             (T.pack (showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> T.pack (getLoc' funId))
             (T.pack $ showSDocUnsafe $ ppr f)
             (T.pack $ getLoc' funId)
             (spanToLine _span)
         )]
    getFunBind _ _ = mempty

    -- Extract type and newtype declarations
    getTypeDecl :: _ -> TyClDecl GhcPs -> [PType]
    getTypeDecl _span decl@DataDecl{tcdLName = L l name} =
        [PType 
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) $ l))
#endif
            (spanToLine _span)
        ]
    getTypeDecl _span decl@SynDecl{tcdLName = L l name} =
        [PType
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) $ l))
#endif
            (spanToLine _span)
        ]
    getTypeDecl _ _ = mempty

    -- Extract class declarations
    getClassDecl :: _ -> TyClDecl GhcPs -> [PClass]
    getClassDecl _span decl@ClassDecl{tcdLName = L l name} =
        [PClass
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) l))
#endif
            (spanToLine _span)
        ]
    getClassDecl _ _ = mempty

    -- Extract instance declarations
    getInstDecl :: _ -> InstDecl GhcPs -> [PInstance]
    getInstDecl _span decl@(ClsInstD _ ClsInstDecl{cid_poly_ty = ty}) =
        [PInstance
            (T.pack $ showSDocUnsafe $ ppr ty)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA _span))
#else
            (T.pack ((showSDocUnsafe . ppr) _span))
#endif
            (spanToLine _span)
        ]
    getInstDecl _ _ = mempty

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

websocketPort :: Maybe Int
websocketPort = maybe Nothing (readMaybe) $ unsafePerformIO $ lookupEnv "SERVER_PORT"

websocketHost :: Maybe String
websocketHost = unsafePerformIO $ lookupEnv "SERVER_HOST"

sendTextData' :: CliOptions -> WS.Connection -> Text -> Text -> IO ()
sendTextData' cliOptions conn path data_ = do
    -- t1 <- getCurrentTime
    res <- try $ WS.sendTextData conn data_
    case res of
        Left (err :: SomeException) -> do
            when (shouldLog || Fdep.Types.log cliOptions) $ print err
            appendFile "error.log" ((T.unpack path) <> "," <> (T.unpack data_) <> "\n")
            withSocketsDo $ WS.runClient (fromMaybe (host cliOptions) websocketHost) (fromMaybe (port cliOptions) websocketPort) (T.unpack path) (\nconn -> WS.sendTextData nconn data_)
        Right _ -> pure ()
            -- t2 <- getCurrentTime
            -- when (shouldLog || Fdep.Types.log cliOptions) $ print ("websocket call timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1))

-- default options
-- "{\"path\":\"/tmp/fdep/\",\"port\":9898,\"host\":\"localhost\",\"log\":true}"
defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {path="/tmp/fdep/",port=4444,host="::1",log=False,tc_funcs=Just False}

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) ->
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    when (shouldGenerateFdep) $
        liftIO $ bool P.id (void . forkIO) shouldForkPerFile $ do
            let prefixPath = path cliOptions
                moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> msHsFilePath modSummary
            let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
            when (shouldLog || Fdep.Types.log cliOptions) $ print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
            -- createDirectoryIfMissing True path
            t1 <- getCurrentTime
            withSocketsDo $ do
                eres <- try $
                    WS.runClient
                        (fromMaybe (host cliOptions) websocketHost)
                        (fromMaybe (port cliOptions) websocketPort)
                        ("/" <> modulePath <> ".json")
                        (\conn ->
                            mapM_
                                (loopOverLHsBindLR cliOptions conn Nothing (T.pack ("/" <> modulePath <> ".json")))
                                (bagToList $ tcg_binds tcEnv)
                        )
                case eres of
                    Left (err :: SomeException) ->
                        when (shouldLog || Fdep.Types.log cliOptions) $ print err
                        --appendFile "error.log" (show err <> "\n")
                    Right _ -> pure ()
            t2 <- getCurrentTime
            when (shouldLog || Fdep.Types.log cliOptions) $ print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path <> " total-timetaken: " <> show (diffUTCTime t2 t1))
    return tcEnv

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args
transformFromNameStableString (_,_,_,_) = Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tail' [] = []
tail' [x] = []
tail' (x:xs) = xs

maybeBool (Just v) = v
maybeBool _ = False

processAndSendTypeDetails :: CliOptions -> WS.Connection -> Text -> Text -> [(Type)] -> IO ()
processAndSendTypeDetails cliOptions con path keyFunction typesUsed =
    let details = concat $ map getTypeDetails typesUsed
        functionInfoList = nub $ map (\(name,_type) -> transformFromNameStableString ((Just $ T.pack $ name) ,Nothing ,(Just $ T.pack $ show $ toConstr _type),[])) details
    in mapM_ (\expr -> sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])) functionInfoList

getTypeDetails :: Type -> [(String,Type)]
getTypeDetails ty = map (\x -> (nameStableString $ tyConName x,ty)) $ tyConsOfType ty

tyConsOfType :: Type -> [TyCon]
tyConsOfType ty = case ty of
    TyConApp tc tys -> tc : concatMap tyConsOfType tys
    AppTy t1 t2     -> tyConsOfType t1 ++ tyConsOfType t2
#if __GLASGOW_HASKELL__ >= 900
    FunTy _ _ t1 t2 -> tyConsOfType t1 ++ tyConsOfType t2
#else
    FunTy _ t1 t2 -> tyConsOfType t1 ++ tyConsOfType t2
#endif
    ForAllTy _ t    -> tyConsOfType t
    CastTy t _      -> tyConsOfType t
    CoercionTy _    -> []
    LitTy _         -> []
    TyVarTy _       -> []

loopOverLHsBindLR :: CliOptions -> WS.Connection -> (Maybe Text) -> Text -> LHsBindLR GhcTc GhcTc -> IO ()
loopOverLHsBindLR cliOptions con mParentName path (L _ AbsBinds{abs_binds = binds}) =
    mapM_ (loopOverLHsBindLR cliOptions con mParentName path) $ bagToList binds
loopOverLHsBindLR cliOptions con mParentName _path (L location bind) = do
    let typesUsed = (map varType $ (bind ^? biplateRef :: [Var])) <> (map idType $ (bind ^? biplateRef :: [Id])) <> (bind ^? biplateRef :: [Type])
    case bind of
#if __GLASGOW_HASKELL__ >= 900
        (FunBind _ id matches _) -> do
#else
        (FunBind _ id matches _ _) -> do
#endif
            funName <- pure $ T.pack $ getOccString $ unLoc id
            fName <- pure $ T.pack $ nameStableString $ getName id
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack (getLoc' id)))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr . getLoc) id)))
#endif
            let matchList = mg_alts matches
            if not $ (maybeBool $ tc_funcs cliOptions)
                then
                    when (not $ "$$" `T.isInfixOf` name) $ do
                        when (shouldLog || Fdep.Types.log cliOptions) $ print ("processing function: " <> fName)
                        typeSignature <- pure $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
                        nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
                        data_ <- pure (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String nestedNameWithParent), ("typeSignature", String typeSignature)])
                        t1 <- getCurrentTime
                        sendTextData' cliOptions con _path data_
                        processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
                        mapM_ (\x -> do
                                    eres :: Either SomeException () <- try $ processMatch (nestedNameWithParent) _path x
                                    case eres of
                                        Left err -> do
                                            when (shouldLog || Fdep.Types.log cliOptions) $ print (err,name)
                                            pure ()--appendFile "error.log" (show (err,funName) <> "\n")
                                        Right _ -> pure ()
                                ) (unLoc matchList)
                        t2 <- getCurrentTime
                        when (shouldLog || Fdep.Types.log cliOptions) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
                else do
                    when (shouldLog || Fdep.Types.log cliOptions) $ print ("processing function: " <> fName)
                    typeSignature <- pure $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
                    nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
                    data_ <- pure (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String nestedNameWithParent), ("typeSignature", String typeSignature)])
                    t1 <- getCurrentTime
                    sendTextData' cliOptions con _path data_
                    processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
                    mapM_ (\x -> do
                                eres :: Either SomeException () <- try $ processMatch (nestedNameWithParent) _path x
                                case eres of
                                    Left err -> do
                                        when (shouldLog || Fdep.Types.log cliOptions) $ print (err,name)
                                        pure ()--appendFile "error.log" (show (err,funName) <> "\n")
                                    Right _ -> pure ()
                            ) (unLoc matchList)
                    t2 <- getCurrentTime
                    when (shouldLog || Fdep.Types.log cliOptions) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
        (VarBind{var_id = var, var_rhs = expr}) -> do
            let stmts = (expr ^? biplateRef :: [LHsExpr GhcTc])
                fName = T.pack $ nameStableString $ getName var 
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) $ locA location)))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) location)))
#endif
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            if (maybeBool $ tc_funcs cliOptions)
                then mapM_ (processExpr (nestedNameWithParent) _path) (stmts)
                else when (not $ "$$" `T.isInfixOf` name) $
                        mapM_ (processExpr (nestedNameWithParent) _path) (stmts)
        (PatBind{pat_lhs = pat, pat_rhs = expr}) -> do
            let stmts = (expr ^? biplateRef :: [LHsExpr GhcTc])
                ids = (pat ^? biplateRef :: [LIdP GhcTc])
                fName = (maybe (T.pack "::") (T.pack . nameStableString . getName) $ (headMaybe ids))
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) $ locA location)))
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            if (maybeBool $ tc_funcs cliOptions)
                then mapM_ (processExpr nestedNameWithParent _path) (stmts <> map (\v -> wrapXRec @(GhcTc) $ HsVar noExtField v) (tail' ids))
                else when (not $ "$$" `T.isInfixOf` name) $
                        mapM_ (processExpr nestedNameWithParent _path) (stmts <> map (\v -> wrapXRec @(GhcTc) $ HsVar noExtField v) (tail' ids))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) location)))
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            if (maybeBool $ tc_funcs cliOptions)
                then mapM_ (processExpr nestedNameWithParent _path) (stmts <> map (\v -> noLoc $ HsVar noExtField v) (tail' ids))
                else when (not $ "$$" `T.isInfixOf` name) $
                        mapM_ (processExpr nestedNameWithParent _path) (stmts <> map (\v -> noLoc $ HsVar noExtField v) (tail' ids))
#endif
        _ -> pure ()
    where
        processMatch :: Text -> Text -> LMatch GhcTc (LHsExpr GhcTc) -> IO ()
        processMatch keyFunction path (L _ match) = do
#if __GLASGOW_HASKELL__ >= 900
            processHsLocalBinds keyFunction path $ grhssLocalBinds (m_grhss match)
#else
            processHsLocalBinds keyFunction path $ unLoc $ grhssLocalBinds (m_grhss match)
#endif
            mapM_ (processGRHS keyFunction path) $ grhssGRHSs (m_grhss match)

        processGRHS :: Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO ()
        processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
        processGRHS _ _ _ = pure mempty

        processHsLocalBinds :: Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
        processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
            void $ mapM (loopOverLHsBindLR cliOptions con (Just keyFunction) path) $ bagToList $ x
        processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
            void $ mapM (\(recFlag, binds) -> void $ mapM (loopOverLHsBindLR cliOptions con (Just keyFunction) path) $ bagToList binds) ( x)
        processHsLocalBinds _ _ _ = pure mempty

        processExpr :: Text -> Text -> LHsExpr GhcTc -> IO ()
        processExpr keyFunction path x@(L _ (HsVar _ (L _ var))) = do
            let name = T.pack $ nameStableString $ varName var
                _type = T.pack $ showSDocUnsafe $ ppr $ varType var
            expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ x, Just _type, mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
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
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path y@(L _ x@(HsOverLit _ overLitVal)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr overLitVal), mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
            let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
                stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
        processExpr keyFunction path y@(L _ x@(HsConLikeOut _ hsType)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path y@(L _ x@(HsIPVar _ implicit)) = do
            expr <- pure $ transformFromNameStableString (Just $ ("$_implicit$" <> T.pack (showSDocUnsafe $ ppr implicit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
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
        processExpr keyFunction path x@(L _ (HsRecFld _ exprLStmt)) = getDataTypeDetails keyFunction path x
        processExpr keyFunction path y@(L _ x@(RecordCon expr (L _ (iD)) rcon_flds)) = getDataTypeDetails keyFunction path y
        processExpr keyFunction path x@(L _ (RecordUpd _ rupd_expr rupd_flds)) = getDataTypeDetails keyFunction path x
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
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
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
        processExpr keyFunction path (L _ x@(RecordCon expr (L _ (iD)) rcon_flds)) =
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
            expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processExpr keyFunction path (L _ x) =
            let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
                stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (noLoc) stmtsNoLoc)))
#endif
        getDataTypeDetails :: Text -> Text -> LHsExpr GhcTc -> IO ()
#if __GLASGOW_HASKELL__ >= 900 
        getDataTypeDetails keyFunction path (L _ (RecordCon _ (iD) rcon_flds)) = 
            (extractRecordBinds keyFunction path (T.pack $ nameStableString $ getName (GHC.unXRec @(GhcTc) iD)) (rcon_flds))
#else
        getDataTypeDetails keyFunction path (L _ (RecordCon _ (iD) rcon_flds)) = (extractRecordBinds keyFunction path (T.pack $ nameStableString $ getName (GHC.unLoc iD)) (rcon_flds))
#endif
        getDataTypeDetails keyFunction path y@(L _ (RecordUpd x@(RecordUpdTc rupd_cons rupd_in_tys rupd_out_tys rupd_wrap) rupd_expr rupd_flds)) = do
            let names = (x ^? biplateRef :: [DataCon])
                types = (x ^? biplateRef :: [Type])
            mapM_ (\xx -> do
                let name = T.pack $ nameStableString $ dataConName $ xx
                    _type = T.pack $ showSDocUnsafe $ ppr $ dataConRepType xx
                expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                ) names
            mapM_ (\xx -> do
                expr <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr xx)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr xx), mempty)
                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                ) types
            (getFieldUpdates y keyFunction path (T.pack $ showSDocUnsafe $ ppr rupd_expr) rupd_flds)
        getDataTypeDetails keyFunction path y@(L _ (HsRecFld _ (Unambiguous id' lnrdrname))) = do
            let name = T.pack $ nameStableString $ varName id'
                _type = T.pack $ showSDocUnsafe $ ppr $ varType id'
            expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
            -- case reLocN lnrdrname of
            --     (L l rdrname) -> do
            --         print $ (handleRdrName rdrname,showSDocUnsafe $ ppr id')
        getDataTypeDetails keyFunction path y@(L _ (HsRecFld _ (Ambiguous   id'  lnrdrname))) = do
            let name = T.pack $ nameStableString $ varName id'
                _type = T.pack $ showSDocUnsafe $ ppr $ varType id'
            expr <- pure $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
            -- case reLocN lnrdrname of
            --     (L l rdrname) -> do
            --         print $ (handleRdrName rdrname,showSDocUnsafe $ ppr id')
        getDataTypeDetails keyFunction path (L _ y@(HsRecFld _ _)) = pure ()
        getDataTypeDetails keyFunction path _ = pure ()

        -- inferFieldType :: Name -> String
        inferFieldTypeFieldOcc (L _ (FieldOcc _ (L _ rdrName))) = handleRdrName rdrName
        inferFieldTypeFieldOcc (L _ (XFieldOcc _)) = mempty--handleRdrName rdrName
        inferFieldTypeAFieldOcc = (handleRdrName . rdrNameAmbiguousFieldOcc . unLoc)

        handleRdrName :: RdrName -> String
        handleRdrName rdrName = case rdrName of
                Exact name -> nameStableString name  -- For exact names
                Qual mod' occ -> moduleNameString mod' ++ "." ++ occNameString occ  -- For qualified names
                Unqual occ -> occNameString occ  -- For unqualified names
                Orig mod' occ -> moduleNameString (moduleName mod') ++ "." ++ occNameString occ  -- For original names
        -- handleRdrName :: RdrName -> String
        -- handleRdrName x =
        --     case x of
        --         Unqual occName -> ("$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Qual moduleName occName -> ((moduleNameString moduleName) <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Orig module' occName -> ((moduleNameString $ moduleName module') <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Exact name -> nameStableString name

#if __GLASGOW_HASKELL__ >= 900
        getFieldUpdates :: GenLocated (SrcSpanAnn' a) e -> Text -> Text -> Text -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> IO ()
        getFieldUpdates _ keyFunction path type_ fields =
            case fields of
                Left x -> (mapM_ (extractField)) x
                Right x -> (mapM_ (processRecordProj) x)
            where
            processRecordProj :: LHsRecProj GhcTc (LHsExpr GhcTc) -> IO ()
            processRecordProj (L _ (HsRecField { hsRecFieldAnn, hsRecFieldLbl=lbl , hsRecFieldArg=expr ,hsRecPun=pun })) = do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                case lbl of
                    (L _ (FieldLabelStrings ll)) -> mapM_ (processHsFieldLabel keyFunction path) ll
                    _ -> pure ()
                processExpr keyFunction path expr

            -- extractField :: HsRecUpdField GhcTc -> IO ()
            extractField y@(L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeAFieldOcc lbl)
                processExpr keyFunction path expr
                expr' <- evaluate $ force $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ getLocTC' $ y), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr')])

        processHsFieldLabel :: Text -> Text -> Located (HsFieldLabel GhcTc) -> IO ()
        processHsFieldLabel keyFunction path y@(L l x@(HsFieldLabel _ (L _ hflLabel))) = do
            expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_fieldName$" <> (T.pack $ showSDocUnsafe $ ppr hflLabel)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
        processHsFieldLabel keyFunction path (L _ (XHsFieldLabel _)) = pure ()
#else
        getFieldUpdates :: _ -> Text -> Text -> Text -> [LHsRecUpdField GhcTc]-> IO ()
        getFieldUpdates y keyFunction path type_ fields = mapM_ extractField fields
            where
            extractField :: LHsRecUpdField GhcTc -> IO ()
            extractField (L l x@(HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr', hsRecPun = pun})) =do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeAFieldOcc lbl)
                processExpr keyFunction path expr'
                expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ getLocTC' y), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
#endif

        extractRecordBinds :: Text -> Text -> Text ->  HsRecFields GhcTc (LHsExpr GhcTc) -> IO ()
        extractRecordBinds keyFunction path type_ (HsRecFields{rec_flds = fields}) =
            mapM_ extractField fields
            where
            extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> IO ()
            extractField (L l x@(HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) = do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeFieldOcc lbl)
                processExpr keyFunction path expr
                expr' <- evaluate $ force $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ showSDocUnsafe $ ppr $ l), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr')])

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
                    sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
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
                                sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                            ) hsTypes
                    (extractExprsFromPat keyFunction path) p
                ConPat {pat_args = args} -> (extractExprsFromHsConPatDetails keyFunction path args)
                ViewPat hsType expr p -> do
                    expr' <- pure $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr')])
                    processExpr keyFunction path expr
                    (extractExprsFromPat keyFunction path) p
                SplicePat _ splice -> mapM_ (processExpr keyFunction path) $ extractExprsFromSplice splice
                LitPat _ hsLit     -> do
                    expr <- pure $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
                    sendTextData' cliOptions con path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
                NPat _ (L _ overLit) _ _ -> do
                    extractExprsFromOverLit overLit
                NPlusKPat _ _ (L _ overLit) _ _ _ ->
                    extractExprsFromOverLit overLit
                SigPat _ p _   -> (extractExprsFromPat keyFunction path) p
                XPat _         -> pure ()
            where
            extractExprsFromOverLit :: HsOverLit GhcTc -> IO ()
            extractExprsFromOverLit (OverLit _ _ e) = processExpr keyFunction path $ wrapXRec @(GhcTc) e

            extractExprsFromHsConPatDetails :: Text -> Text -> HsConPatDetails GhcTc -> IO ()
            extractExprsFromHsConPatDetails keyFunction' path' (PrefixCon _ args) = mapM_ (extractExprsFromPat keyFunction' path') args
            extractExprsFromHsConPatDetails keyFunction' path' z@(RecCon (HsRecFields {})) =
                mapM_ (extractExprsFromPat keyFunction' path') $ hsConPatArgs z
            extractExprsFromHsConPatDetails keyFunction' path' (InfixCon p1 p2) = do
                (extractExprsFromPat keyFunction' path') p1
                (extractExprsFromPat keyFunction' path') p2

        extractExprFromApplicativeArg :: Text -> Text -> ApplicativeArg GhcTc -> IO ()
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgOne _ lpat expr _) = do 
            processExpr keyFunction path expr
            extractExprsFromPat keyFunction path lpat
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgMany _ exprLStmt stmts lpat _) = do
            processExpr keyFunction path (wrapXRec @(GhcTc) stmts)
            mapM_ (extractExprsFromStmtLRHsExpr keyFunction path) (map (unLoc) exprLStmt)
            extractExprsFromPat keyFunction path lpat

        -- extractLStmt :: Text -> Text -> GenLocated l (StmtLR GhcTc GhcTc (GenLocated SrcSpanAnnA (HsCmd GhcTc))) -> IO ()
        -- extractLStmt keyFunction path (L _ smtlr) = extractExprsFromStmtLR keyFunction path smtlr

        extractExprsFromSplice :: HsSplice GhcTc -> [LHsExpr GhcTc]
        extractExprsFromSplice (HsTypedSplice _ _ _ e) = [e]
        extractExprsFromSplice (HsUntypedSplice _ _ _ e) = [e]
        extractExprsFromSplice (HsQuasiQuote _ _ _ _ _) = []
        extractExprsFromSplice (HsSpliced _ _ _) = []
        extractExprsFromSplice _ = []

        processXXExpr :: Text -> Text -> XXExprGhcTc -> IO ()
        processXXExpr keyFunction path (WrapExpr (HsWrap hsWrapper hsExpr)) =
            processExpr keyFunction path (wrapXRec @(GhcTc) hsExpr)
        processXXExpr keyFunction path (ExpansionExpr (HsExpanded _ expansionExpr)) =
            mapM_ (processExpr keyFunction path . (wrapXRec @(GhcTc))) [expansionExpr]

getLocTC' :: GenLocated (SrcSpanAnn' a) e -> String
getLocTC' = (showSDocUnsafe . ppr . la2r . getLoc)

getLoc' :: GenLocated (SrcSpanAnn' a) e -> String
getLoc'   = (showSDocUnsafe . ppr . la2r . getLoc)
#else
getLocTC' = (showSDocUnsafe . ppr . getLoc)
getLoc' = (showSDocUnsafe . ppr . getLoc)
#endif