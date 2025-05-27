{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Fdep.PostgreSQL where

import Control.Concurrent
import Control.Exception (bracket, catch, SomeException, try)
import Control.Monad (void, when, forM_,forever, unless)
import Data.Aeson (encode, toJSON, object, (.=))
import Data.ByteString.Lazy (toStrict,fromStrict)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Notification
import Data.Pool
import System.Environment (lookupEnv)
import GHC.IO (unsafePerformIO)
import Fdep.Types
import Data.Aeson
import Control.Concurrent.STM.TQueue
import Data.IORef
import GHC.Conc
import Database.PostgreSQL.Simple.SqlQQ
import Data.String (fromString)

#if __GLASGOW_HASKELL__ >= 900
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif

data DBConfig = DBConfig
    { dbHost :: String
    , dbPort :: Int
    , dbUser :: String
    , dbPassword :: String
    , dbName :: String
    , dbPoolSize :: Int
    } deriving (Show)

defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig
    { dbHost = "localhost"
    , dbPort = 5432
    , dbUser = "postgres"
    , dbPassword = "postgres"
    , dbName = "fdep"
    , dbPoolSize = 50
    }

{-# NOINLINE globalDBPool #-}
globalDBPool :: MVar (Maybe (Pool Connection))
globalDBPool = unsafePerformIO $ newMVar Nothing

getDBConfig :: IO DBConfig
getDBConfig = do
    host <- lookupEnv "FDEP_DB_HOST"
    port <- lookupEnv "FDEP_DB_PORT"
    user <- lookupEnv "FDEP_DB_USER"
    pass <- lookupEnv "FDEP_DB_PASSWORD"
    name <- lookupEnv "FDEP_DB_NAME"
    poolSize <- lookupEnv "FDEP_DB_POOL_SIZE"
    
    return $ DBConfig
        { dbHost = maybe (dbHost defaultDBConfig) id host
        , dbPort = maybe (dbPort defaultDBConfig) read port
        , dbUser = maybe (dbUser defaultDBConfig) id user
        , dbPassword = maybe (dbPassword defaultDBConfig) id pass
        , dbName = maybe (dbName defaultDBConfig) id name
        , dbPoolSize = maybe (dbPoolSize defaultDBConfig) read poolSize
        }

getDBPool :: IO (Pool Connection)
getDBPool = do
    maybePool <- takeMVar globalDBPool
    case maybePool of
        Just pool -> do
            putMVar globalDBPool (Just pool)
            return pool
        Nothing -> do
            config <- getDBConfig
            pool <- createPool
                (createConnection config)
                close
                1
                60
                (dbPoolSize config)
            putMVar globalDBPool (Just pool)
            return pool

sendTextData' :: CliOptions -> Text -> Text -> Text -> IO ()
sendTextData' cliOptions moduleName path data_ = insertToPostgreSQL cliOptions moduleName path data_

createConnection :: DBConfig -> IO Connection
createConnection DBConfig{..} = connect defaultConnectInfo
    { connectHost = dbHost
    , connectPort = fromIntegral dbPort
    , connectUser = dbUser
    , connectPassword = dbPassword
    , connectDatabase = dbName
    }

insertToPostgreSQL :: CliOptions -> Text -> Text -> Text -> IO ()
insertToPostgreSQL _cliOptions moduleName path content = void $ forkIO $ do
    pool <- getDBPool
    result <- try $ withResource pool $ \conn -> do
        let (table, itemType) = determineTableAndType path
        case table of
            "haskell_code" -> do
                void $ execute conn [sql|
                    INSERT INTO haskell_code (module_path, module_name, item_type, data) 
                    VALUES (?, ?, ?, ?::jsonb)
                |] (path, moduleName, itemType, content)
            _ -> do
                void $ execute conn [sql|
                    INSERT INTO fdep_data (path, data) 
                    VALUES (?, ?)
                |] (path, content)

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "PostgreSQL error: " ++ show e
            appendFile "fdep_fallback.log" $ T.unpack path ++ "|" ++ T.unpack content ++ "\n" ++ (show e)
        Right _ -> return ()

determineTableAndType :: Text -> (Text, Text)
determineTableAndType path
    | "module_imports" `T.isInfixOf` path = ("haskell_code", "module_import")
    | "function_code" `T.isInfixOf` path = ("haskell_code", "function")
    | "types_code" `T.isInfixOf` path = ("haskell_code", "type")
    | "class_code" `T.isInfixOf` path = ("haskell_code", "class")
    | "instance_code" `T.isInfixOf` path = ("haskell_code", "instance")
    | "hs.json" `T.isInfixOf` path = ("haskell_code", "dependency")
    | "function_dependencies" `T.isInfixOf` path = ("haskell_code", "dependency")
    | "function_instance_mapping" `T.isInfixOf` path = ("haskell_code", "function_instance_mapping")
    | otherwise = ("fdep_data", "")