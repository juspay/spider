{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass,ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns,PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Endpoints.Plugin where

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Data.FastString (unpackFS)
import qualified Data.Text as T
import GHC.Types.TypeEnv
import Data.Data ( Data(toConstr) )
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..))
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc,Outputable)
import GHC.Data.Bag
import System.Directory ( createDirectoryIfMissing )
import Control.Monad.IO.Class (liftIO)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import GHC.Data.Bag (bagToList)
import qualified Data.Aeson.KeyMap as HM
import GHC.Hs.Expr
import Language.Haskell.Syntax.Pat
import GHC as GhcPlugins
import GHC.Generics (Generic)
import Data.Aeson
import GHC.Core.DataCon as GhcPlugins
import GHC.Core.TyCon as GhcPlugins
import GHC.Core.TyCo.Rep
import GHC.Driver.Session
import qualified Data.ByteString as DBS
import qualified Data.Map as Map
import Data.Maybe
import Data.List (intercalate,isInfixOf,foldl')
import GHC.Core.Type
import Control.Applicative ((<|>))
import Debug.Trace (traceShowId,trace)
import Control.Monad (when)
import Control.Concurrent (MVar,putMVar,takeMVar,readMVar,newMVar)
import GHC.IO (unsafePerformIO)
import qualified Data.Aeson.Key as Key
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Prelude hiding (log)
import qualified Network.WebSockets as WS
import Control.Exception
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.List.Extra as Data.List
import Network.Socket (withSocketsDo)
import Data.Maybe
import System.Environment (lookupEnv)
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory (createDirectoryIfMissing)
import Data.Hashable (hash)

plugin :: Plugin
plugin =
    defaultPlugin
        {
        pluginRecompile = (\_ -> return NoForceRecompile)
        , typeCheckResultAction = collectTypesTC
        }


shouldLog :: Bool
shouldLog = readBool $ unsafePerformIO $ lookupEnv "ENABLE_LOGS"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool _ = False

fdepSocketPath :: Maybe FilePath
fdepSocketPath = unsafePerformIO $ lookupEnv "FDEP_SOCKET_PATH"

connectToUnixSocket :: FilePath -> IO NS.Socket
connectToUnixSocket socketPath = do
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.connect sock (NS.SockAddrUnix socketPath)
    return sock

sendFileToWebSocketServer :: CliOptions -> T.Text -> _ -> IO ()
sendFileToWebSocketServer cliOptions path data_ =
    -- Use the Unix Domain Socket implementation
    sendViaUnixSocket cliOptions path data_

sendViaUnixSocket :: CliOptions -> T.Text -> T.Text -> IO ()
sendViaUnixSocket cliOptions path data_ = do
    -- Create message with path as header for routing
    let message = encodeUtf8 $ path <> "****" <> data_
    
    -- Get socket path from environment or config
    let socketPathToUse = fromMaybe (Endpoints.Plugin.path cliOptions) fdepSocketPath
    
    -- Try to send data
    res <- try $ do
        sock <- connectToUnixSocket socketPathToUse
        NSB.sendAll sock (message)
        NS.close sock
    
    case res of
        Left (err :: SomeException) -> do
            appendFile "error.log" ((T.unpack path) <> "," <> (T.unpack data_) <> "\n")
            let errorDir = "fdep_recovery"
            createDirectoryIfMissing True errorDir
            let timestamp = show $ hash $ T.unpack path
            appendFile (errorDir <> "/" <> timestamp <> ".json") (T.unpack data_ <> "\n")
    
        Right _ -> 
            print $ "Successfully sent data to " <> path


defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {path="./tmp/fdep/",port=4444,host="::1",log=False,tc_funcs=Just False}

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcg = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) -> 
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    dflags <- getDynFlags
    _ <- liftIO $
            do
                let prefixPath = (path $ cliOptions)
                    moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> msHsFilePath modSummary
                    typeEnv = tcg_type_env tcg
                let path = (intercalate "/" . init . splitOn "/") modulePath
                servantAPIs <- mapM (\x ->
                                            case x of
                                                ATyCon tyCon -> getAPITypes tyCon
                                                _ -> pure mempty
                                        ) (typeEnvElts typeEnv)
                -- hm <- takeMVar cachedTypes
                -- putMVar cachedTypes $ (foldl' (\hm (name,ty) -> HM.insert (Key.fromText $ T.pack $ showSDocUnsafe $ ppr name) ty hm) hm (concat servantAPIs))
                parsedEndpoints <-  processServantApis $ concat servantAPIs
                -- createDirectoryIfMissing True path
                -- DBS.writeFile (modulePath <> ".api-spec.json") (DBS.toStrict $ encode $ parsedEndpoints)
                sendFileToWebSocketServer cliOptions (T.pack $ "/" <> modulePath <> ".module_apis.json") (decodeUtf8 $ (DBS.toStrict $ encode $ parsedEndpoints))
    return tcg

mergeEndpoints x = x

isServantAPI :: Type -> Bool
isServantAPI ty = go ty
    where
    go :: Type -> Bool
    go t = case splitTyConApp_maybe t of
        Just (tyCon', args) ->
            let tyConName' = getOccString (GhcPlugins.tyConName tyCon')
            in isServantCombinator tyConName' || any go args
        Nothing -> False

    isServantCombinator :: String -> Bool
    isServantCombinator name = name `elem` [
        ":>", ":<|>", "Get", "Post", "Put", "Delete", "Patch",
        "Capture", "QueryParam", "QueryParams", "QueryFlag",
        "Header", "ReqBody", "StreamBody", "Raw", "EmptyAPI"
        ]

getAPITypes :: GhcPlugins.TyCon -> IO [(Name,Type)]
getAPITypes tyCon' = do
    let name = GhcPlugins.tyConName tyCon'
        tyConStr = showSDocUnsafe (ppr name)
        tyConKind' = tyConKind tyCon'
        kindStr = showSDocUnsafe (ppr tyConKind')
        dataCons = tyConDataCons tyCon'
    case synTyConRhs_maybe tyCon' of
        Just rhs ->
            if isServantAPI rhs
                then do
                    pure $ [(name, rhs)]
                else pure $ mempty
        Nothing -> pure mempty

data HttpMethod = Get | Post | Put | Delete | Patch
    deriving (Show, Eq,Data)

-- data ResponseType = JSON | Text | CUSTOM Text deriving (Show)

data Endpoint = Endpoint
    { method :: String
    , path' :: [String]
    , queryParams :: [String]
    , headers :: [String]
    , responseStatus :: String
    , responseContentType :: Maybe String
    , responseBody :: Maybe String
    , requestBody  :: Maybe String
    , requestContentType  :: Maybe String
    } deriving (Generic,Show)

deriving instance ToJSON Endpoint

-- processServantApis :: [(Name,Type)] -> IO [(String,[Endpoint])]
processServantApis ts = do
    res <- mapM (\(name',type') -> do
                        res <- parseApiType type'
                        endpoints <- (maybe mempty parseApiDefinition) res
                        pure (showSDocUnsafe $ ppr name',endpoints)
                    ) ts
    pure $ Map.fromList res

mergeAllURLOptions :: [ApiComponent] -> [Endpoint]
mergeAllURLOptions list =
    let (headers,queryParams,verbs,captures,reqBodyList,contentTypeList) = processList list
    in concatMap (\x ->
        case x of
            (Verb method status responseType responseBody) -> [(Endpoint method captures queryParams headers status responseType responseBody (if null $ concat reqBodyList then Nothing else Just $ concat reqBodyList) (if null $ concat contentTypeList then Nothing else Just $ concat contentTypeList))]
            _ -> mempty
    ) verbs
    where
        processList list =
            foldl' (\(rh,qp,verb,capture,reqBodyList,contentTypeList) x ->
                case x of
                    (QueryParam p) -> (rh,qp <> [p],verb,capture,reqBodyList,contentTypeList)
                    (Header p) -> (rh <> [p], qp,verb,capture,reqBodyList,contentTypeList)
                    (Verb method status responseType responseBody) -> (rh,qp,verb <> [x],capture,reqBodyList,contentTypeList)
                    (ReqBody contentType reqBody) -> (rh,qp,verb,capture,(reqBodyList <> [reqBody]),contentTypeList <> [contentType])
                    (Group "" l)  ->
                        let (a,b,c,cc,reqBodyL,contentTypeL) = processList l
                        in (rh <> a,qp <> b,verb <> c,capture <>cc,(reqBodyList <> reqBodyL),(contentTypeList <> contentTypeL))
                    (Group p [])  -> (rh,qp,verb,capture <> [p],reqBodyList,contentTypeList)
                    (Group x l)  ->
                        let (a,b,c,cc,reqBodyL,contentTypeL) = processList l
                        in (rh <> a,qp <> b,verb <> c,capture <> cc <> [x],(reqBodyList <> reqBodyL),contentTypeList<>contentTypeL)
                    (Tag p) -> (rh,qp,verb,capture,reqBodyList,contentTypeList)
                    _ -> (rh,qp,verb,capture,reqBodyList,contentTypeList)
                ) ([],[],[],([] :: [String]),[],[]) list

parseApiDefinition :: ApiComponent -> IO [Endpoint]
parseApiDefinition (Path p) = pure [Endpoint mempty [p] [] mempty "" Nothing Nothing Nothing Nothing]
parseApiDefinition (Verb method status responseType _) = pure [Endpoint (method) [] [] mempty status (responseType) Nothing Nothing Nothing]
parseApiDefinition (Group p []) = pure [Endpoint mempty [p] [] mempty mempty Nothing Nothing Nothing Nothing]
parseApiDefinition (Group "" [(Verb method status responseType responseBody)]) = pure [Endpoint (method) [] [] mempty status (responseType) responseBody Nothing Nothing]
parseApiDefinition (Group "" [(ReqBody contentType reqBody),(Verb method status responseType responseBody)]) = pure [Endpoint (method) [] [] mempty status (responseType) responseBody (Just reqBody) (Just contentType)]
-- parseApiDefinition (Group "" [(Verb method status responseType responseBody)]) = pure [Endpoint (method) [] [] mempty status (responseType) responseBody]
parseApiDefinition (Group "" (x:xs)) = do
    pure $ mergeAllURLOptions (x:xs)
parseApiDefinition (Group p [(ReqBody contentType reqBody),(Verb method status responseType responseBody)]) = pure [Endpoint (method) [p] [] mempty status (responseType) responseBody (Just reqBody) (Just contentType)]
parseApiDefinition (Group p [(Verb method status responseType responseBody)]) = pure [Endpoint (method) [p] [] mempty status (responseType) responseBody Nothing Nothing]
parseApiDefinition x@(Group p comps) = do
    endpointsList <- mapM parseApiDefinition comps
    let filteredList = concat $ filter (not . null) endpointsList
    case filteredList of
        [] -> do
            pure [Endpoint mempty [p] [] mempty mempty Nothing Nothing Nothing Nothing]
        [(endpoint)] -> pure [(\(Endpoint method path qp h rs rt rb reqBody contentType) -> Endpoint method ([p] <> path) qp h rs rt rb reqBody contentType) endpoint]
        _ -> pure $ (map (\endpoint -> endpoint { path' = [p] <> (path' endpoint) })) filteredList
parseApiDefinition (Alternative comps) = concat <$> mapM parseApiDefinition comps
parseApiDefinition _ = pure []

-- parseApiDefinition :: ApiComponent -> IO [[String]]
-- parseApiDefinition (Path p) = pure [[p]]
-- parseApiDefinition (Capture p) = pure [["{" ++ p ++ "}"]]
-- parseApiDefinition (QueryParam p) = pure mempty--[["?" ++ p ++ "={" ++ p ++ "}"]]
-- parseApiDefinition (Header p) = pure mempty--pure [["@" ++ p]]
-- parseApiDefinition ReqBody = pure mempty
-- parseApiDefinition (Verb method status responseType responseBody) = pure [[("->" ++ method ++ "->" ++ status ++ "->" ++ responseType ++ "->" ++ responseBody)]]
-- parseApiDefinition (Group (p) []) = pure [[p]]
-- parseApiDefinition (Group ("") [(Verb method status responseType responseBody)]) = do
--     pure [[("->" ++ method ++ "->" ++ status ++ "->" ++ responseType ++ "->" ++ responseBody)]]
-- parseApiDefinition (Group (p) [(Verb method status responseType responseBody)]) = do
--     pure [[p ++ "/" ++ ("->" ++ method ++ "->" ++ status ++ "->" ++ responseType ++ "->" ++ responseBody)]]
-- parseApiDefinition (Group (p) comps) = do
--     pathsList <- mapM parseApiDefinition comps
--     let filteredList = filter (\x ->
--                                 case x of
--                                     [] -> False
--                                     [[]] -> False
--                                     _ -> True
--                     ) pathsList
--     print (toConstr $ head comps,filteredList,pathsList)
--     case pathsList of
--         [] ->
--             pure $ [[p]]
--         [[[x]]] -> do
--             case p of
--                 "" -> pure [[x]]
--                 _  -> pure [[p <> "/" <> x]]
--         (x:xs) ->
--             case p of
--                 "" -> pure $ concat filteredList
--                 _  -> pure $ concatMap (map (map ((p <> "/") <>))) filteredList
-- parseApiDefinition (Alternative comps) = concat <$> mapM parseApiDefinition comps
-- parseApiDefinition _ = pure mempty

data ApiComponent
  = Path String
  | Capture String
  | QueryParam String
  | Header String
  | AddArgs String
  | ReqBody String String
  | Verb String String (Maybe String) (Maybe String)
  | Group String [ApiComponent]
  | Alternative [ApiComponent]
  | Tag String
  deriving (Generic,Show,Data,ToJSON)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe [x] = Nothing
tailMaybe (x:xs) = Just xs

-- extractResponseTypeToLink :: Type  -> String -> IO (Maybe ResponseType)
-- extractResponseTypeToLink (TyConApp tyCon args) rawStr =
--     case showSDocUnsafe $ ppr $ (tyCon) of
--         "Headers" ->
--             case args of
--                 [(TyConApp tyConh headers),resp] -> do
--                     pure $ Just $ ResponseType (map (showSDocUnsafe . ppr) headers) [((showSDocUnsafe . ppr) resp)] rawStr
--                 _ -> pure $ Just $ ResponseType mempty mempty rawStr
--         _ -> pure $ Just $ ResponseType mempty mempty rawStr

-- extractRequestTypeToLink x@(TyConApp tyCon args) raw = pure $ Just $ RequestType [showSDocUnsafe $ ppr x] (raw)

parseApiType :: Type -> IO (Maybe ApiComponent)
parseApiType ty = do
    case splitTyConApp_maybe ty of
        Just (tyCon, args) -> do
            let tyConName' = getOccString (tyConName tyCon)
            case tyConName' of
                ":>" -> do
                    res <- catMaybes <$> mapM parseApiType args
                    case res of
                        (Path p : rest) -> pure $ Just $ Group p rest
                        (Capture p : rest) -> pure $ Just $ Group ("{" ++ p ++ "}") rest
                        _ -> do
                            if null res
                                then pure Nothing
                                else pure $ Just $ Group "" res
                ":<|>" -> do
                    res <- mapM parseApiType args
                    pure $ Just $ Alternative (catMaybes res)
                "Header'" -> do
                    res <- mapM parseApiType args
                    case catMaybes res of
                        [Path p] -> pure $ Just $ Header p
                        _ -> pure Nothing
                "Capture'" -> do
                    res <- mapM parseApiType args
                    -- print $ (tyConName',showSDocUnsafe $ ppr $ ty,map (\x -> (showSDocUnsafe $ ppr x,toConstr x)) args,res)
                    case catMaybes res of
                        [Path p] -> pure $ Just $ Group ("{" ++ p ++ "}") mempty
                        _ -> pure Nothing
                "QueryParam'" -> do
                    res <- mapM parseApiType args
                    case catMaybes res of
                        [Path p] -> pure $ Just $ QueryParam p
                        _ -> pure Nothing
                "QueryFlag" -> do
                    res <- mapM parseApiType args
                    case catMaybes res of
                        [Path p] -> pure $ Just $ QueryParam p
                        _ -> pure Nothing
                "AddArgs" -> do
                    res <- mapM parseApiType args
                    -- print ("----AddArgs",null (catMaybes res),catMaybes res,map (\x -> (showSDocUnsafe $ ppr x,toConstr x)) args)
                    if null (catMaybes res)
                        then pure Nothing
                        else pure $ Just $ Group "" $ catMaybes res
                "Verb" -> do
                    res <- mapM (\x -> pure $ (showSDocUnsafe $ ppr x)) args
                    case res of
                        ["StdMethod",method,statusCode,encodeType,responseTypeName] -> pure $ Just $ Verb method statusCode (Just encodeType) (Just $ showSDocUnsafe $ ppr $ last $ args)
                        _ -> pure Nothing
                "ReqBody'" -> do
                    res <- mapM (\x -> pure $ (showSDocUnsafe $ ppr x)) args
                    case res of
                        [("'[Required, Strict]"),reqContentType,reqBody] -> pure $ Just $ ReqBody (reqContentType) reqBody
                        _ -> pure Nothing
                "Tag" -> pure $ Just $ Tag (showSDocUnsafe $ ppr args)
                "TYPE" -> do
                    res <- mapM parseApiType args
                    if null (catMaybes res)
                        then pure Nothing
                        else pure $ Just $ Group "" $ catMaybes res
                ":" -> do
                    res <- mapM parseApiType args
                    if null (catMaybes res)
                        then pure Nothing
                        else pure $ Just $ Group "" $ catMaybes res
                _ -> do
                    -- print ("------",tyConName',showSDocUnsafe $ ppr tyCon,map (\x -> (showSDocUnsafe $ ppr x,toConstr x)) args)
                    pure Nothing
        Nothing -> do
            case isStrLitTy ty of
                Just fs -> pure $ Just $ Path (unpackFS fs)
                Nothing -> pure Nothing

isTyConApp :: Type -> Bool
isTyConApp (TyConApp _ _) = True
isTyConApp _              = False

data CliOptions = CliOptions {
    path :: FilePath,
    port :: Int,
    host :: String,
    log :: Bool,
    tc_funcs :: Maybe Bool
} deriving (Show,Generic,ToJSON,FromJSON)
#endif