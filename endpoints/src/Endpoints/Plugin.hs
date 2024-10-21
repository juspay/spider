{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
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
#endif

plugin :: Plugin
plugin =
    defaultPlugin
        {
        pluginRecompile = (\_ -> return NoForceRecompile)
        , typeCheckResultAction = collectTypesTC
        }

collectTypesTC :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectTypesTC opts modSummary tcg = do
    dflags <- getDynFlags
    _ <- liftIO $
            do
                let prefixPath = case opts of
                        [] -> "./tmp/endpoints/"
                        local : _ -> local
                    moduleName' = moduleNameString $ GhcPlugins.moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> msHsFilePath modSummary
                    typeEnv = tcg_type_env tcg
                    path = (intercalate "/" . init . splitOn "/") modulePath
                servantAPIs <- mapM (\x ->
                                            case x of
                                                ATyCon tyCon -> getAPITypes tyCon
                                                _ -> pure mempty
                                        ) (typeEnvElts typeEnv)
                parsedEndpoints <-  processServantApis $ filter (\(x,y) -> "EulerAPI" == (showSDocUnsafe $ ppr x)) $ concat servantAPIs
                print (encode parsedEndpoints)
                -- print $ map (\(x,y) -> (x,map showEndpoint y))  parsedEndpoints
                createDirectoryIfMissing True path
                -- DBS.writeFile (modulePath <> ".types.json") (DBS.toStrict $ encode $ Map.fromList $ Prelude.concat types)
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
    , path :: [String]
    , queryParams :: [String]
    , headers :: [String]
    , responseStatus :: String
    , responseBody :: String
    , requestBody :: String
    } deriving (Generic,Show)

deriving instance ToJSON Endpoint
deriving instance ToJSON ApiComponent

-- processServantApis :: [(Name,Type)] -> IO [(String,[Endpoint])]
processServantApis ts = do
    res <- mapM (\(name',type') -> do
                        res <- parseApiType type'
                        endpoints <- (maybe mempty parseApiDefinition) res
                        pure (showSDocUnsafe $ ppr name',endpoints)
                    ) ts
    pure res

mergeAllURLOptions :: [ApiComponent] -> [Endpoint]
mergeAllURLOptions list =
    let (headers,queryParams,verbs) = processList list
    in map (\(Verb method status responseType responseBody) -> (Endpoint method mempty queryParams headers status responseType responseBody)) verbs
    where
        processList list =
            foldl' (\(rh,qp,verb) x ->
                case x of
                    (QueryParam p) -> (rh,qp <> [p],verb)
                    (Header p) -> (rh <> [p], qp,verb)
                    (Verb method status responseType responseBody) -> (rh,qp,verb <> [x])
                    (Group "" l)  ->
                        let (a,b,c) = processList l
                        in (rh <> a,qp <> b,verb <> c)
                    _ -> (rh,qp,verb)
                ) ([],[],[]) list

parseApiDefinition :: ApiComponent -> IO [Endpoint]
parseApiDefinition (Path p) = pure [Endpoint mempty [p] [] mempty "" "" ""]
parseApiDefinition (Verb method status responseType _) = pure [Endpoint (method) [] [] mempty status (responseType) mempty]
parseApiDefinition (Group p []) = pure [Endpoint mempty [p] [] mempty mempty mempty mempty]
parseApiDefinition (Group "" [Verb method status responseType responseBody]) = pure [Endpoint (method) [] [] mempty status (responseType) responseBody]
parseApiDefinition (Group "" (x:xs)) = pure $ mergeAllURLOptions (x:xs)
parseApiDefinition (Group p [Verb method status responseType responseBody]) = pure [Endpoint (method) [p] [] mempty status (responseType) responseBody]
parseApiDefinition (Group p comps) = do
    endpointsList <- mapM parseApiDefinition comps
    let filteredList = concat $ filter (not . null) endpointsList
    case filteredList of
        [] -> pure [Endpoint mempty [p] [] mempty mempty mempty mempty]
        [(endpoint)] -> pure [(\(Endpoint method path qp h rs rt rb) -> Endpoint method ([p] <> path) qp h rs rt rb) endpoint]
        _ -> pure $ (map (\endpoint -> endpoint { path = [p] <> (path endpoint) })) filteredList
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
  | ReqBody String
  | Verb String String String String
  | Group String [ApiComponent]
  | Alternative [ApiComponent]
  | Tag String
  deriving (Generic,Show,Data)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe [x] = Nothing
tailMaybe (x:xs) = Just xs

parseApiType :: Type -> IO (Maybe ApiComponent)
parseApiType ty = case splitTyConApp_maybe ty of
  Just (tyCon, args) ->
    let tyConName' = getOccString (tyConName tyCon)
    in case tyConName' of
        ":>" -> do
            res <- catMaybes <$> mapM parseApiType args
            case res of
                (Path p : rest) -> pure $ Just $ Group p rest
                (Capture p : rest) -> pure $ Just $ Group ("{" ++ p ++ "}") rest
                -- (QueryParam p : rest) -> pure $ Just $ Group ("?" ++ p ++ "={" ++ p ++ "}") rest
                -- (Tag t : rest) -> pure $ Just $ Group ("<" ++ t ++ ">") rest
                -- (TypeApp "ReqBody" [_, _] : rest) -> pure $ Just $ Group "" (ReqBody : rest)
                -- (TypeApp "Get" _ : rest) -> pure $ Just $ Group "" (Verb "'GET" "" "" "" : rest)
                -- (TypeApp "Post" _ : rest) -> pure $ Just $ Group "" (Verb "'POST" "" "" "" : rest)
                -- (TypeApp "Put" _ : rest) -> pure $ Just $ Group "" (Verb "'PUT" "" "" "" : rest)
                -- (TypeApp "Delete" _ : rest) -> pure $ Just $ Group "" (Verb "'DELETE" "" "" "" : rest)
                _ -> pure $ Just $ Group "" res
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
            case catMaybes res of
                [Path p] -> pure $ Just $ Capture p
                _ -> pure Nothing
        "QueryParam'" -> do
            res <- mapM parseApiType args
            case catMaybes res of
                [Path p] -> pure $ Just $ QueryParam p
                _ -> pure Nothing
        "AddArgs" -> do
            res <- mapM parseApiType args
            case catMaybes res of
                [Path p] -> pure $ Just $ AddArgs p
                _ -> pure Nothing
        "Verb" -> do
            res <- mapM (\x -> pure $ (showSDocUnsafe $ ppr x)) args
            case res of
                ["StdMethod",method,statusCode,encodeType,responseTypeName] -> pure $ Just $ Verb method statusCode encodeType (showSDocUnsafe $ ppr $ last $ args)
                _ -> pure Nothing
        -- "ReqBody'" -> pure $ Just $ ReqBody (showSDocUnsafe $ ppr ty)
        -- "Get" -> pure $ Just (Verb "'GET" "" "" "")
        -- "Post" -> pure $ Just (Verb "'POST" "" "" "")
        -- "Put" -> pure $ Just (Verb "'PUT" "" "" "")
        -- "Delete" -> pure $ Just (Verb "'DELETE" "" "" "")
        "Tag" -> pure $ Just $ Tag (showSDocUnsafe $ ppr args)
        _ -> do 
            -- print (tyConName',(map (\x -> (toConstr x,showSDocUnsafe $ ppr x)) args))
            pure Nothing
  Nothing -> case isStrLitTy ty of
    Just fs -> pure $ Just $ Path (unpackFS fs)
    Nothing -> pure Nothing