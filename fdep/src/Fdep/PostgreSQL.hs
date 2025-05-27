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

sendTextData' :: CliOptions -> Text -> Text -> IO ()
sendTextData' cliOptions path data_ = do
    insertToPostgreSQL cliOptions path data_

createConnection :: DBConfig -> IO Connection
createConnection DBConfig{..} = connect defaultConnectInfo
    { connectHost = dbHost
    , connectPort = fromIntegral dbPort
    , connectUser = dbUser
    , connectPassword = dbPassword
    , connectDatabase = dbName
    }

-- Extract item name from path
extractItemName :: Text -> Maybe Text
extractItemName path = do
    let parts = T.splitOn "/" path
    if length parts >= 2
    then Just (last parts)
    else Nothing

-- Extract module path from full path
extractModulePath :: Text -> Text
extractModulePath path = 
    let parts = T.splitOn "/" path
        cleanParts = filter (not . T.null) parts
    in if length cleanParts >= 2
       then T.intercalate "/" (init cleanParts)
       else path

insertToPostgreSQL :: CliOptions -> Text -> Text -> IO ()
insertToPostgreSQL _cliOptions path content = do
    pool <- getDBPool
    result <- try $ withResource pool $ \conn -> do
        let (table, itemType) = determineTableAndType path
        case table of
            "haskell_code" -> do
                let (modulePath, itemName) =
                        fromMaybe (extractModulePath path, fromMaybe path (extractItemName path))
                                  (parseModuleAndName path)
                void $ execute conn [sql|
                    INSERT INTO haskell_code (module_path, item_name, item_type, data) 
                    VALUES (?, ?, ?, ?::jsonb)
                |] (modulePath, itemName, itemType, content)
            _ -> do
                void $ execute conn [sql|
                    INSERT INTO fdep_data (path, data) 
                    VALUES (?, ?)
                |] (path, content)

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "PostgreSQL error: " ++ show e
            appendFile "fdep_fallback.log" $ T.unpack path ++ "|" ++ T.unpack content ++ "\n"
        Right _ -> return ()

-- insertToPostgreSQL :: CliOptions -> Text -> Text -> IO ()
-- insertToPostgreSQL cliOptions path content = do
--     pool <- getDBPool
--     result <- try $ withResource pool $ \conn -> do
--         let (table, itemType) = determineTableAndType path
--         case table of
--             "haskell_code" -> do
--                 -- Try to parse module and item name
--                 case parseModuleAndName path of
--                     Just (modulePath, itemName) -> do
--                         -- Insert into haskell_code table
--                         execute conn [sql|
--                             INSERT INTO haskell_code (module_path, item_name, item_type, data) 
--                             VALUES (?, ?, ?, ?::jsonb)
--                             ON CONFLICT (module_path, item_name, item_type) 
--                             DO UPDATE SET data = EXCLUDED.data, updated_at = CURRENT_TIMESTAMP
--                         |] (modulePath, itemName, itemType, content)
--                     Nothing -> do
--                         -- Fallback: try to extract from path
--                         case extractItemName path of
--                             Just itemName -> do
--                                 let modulePath = extractModulePath path
--                                 execute conn [sql|
--                                     INSERT INTO haskell_code (module_path, item_name, item_type, data) 
--                                     VALUES (?, ?, ?, ?::jsonb)
--                                     ON CONFLICT (module_path, item_name, item_type) 
--                                     DO UPDATE SET data = EXCLUDED.data, updated_at = CURRENT_TIMESTAMP
--                                 |] (modulePath, itemName, itemType, content)
--                             Nothing -> do
--                                 -- Final fallback to fdep_data
--                                 execute conn [sql|
--                                     INSERT INTO fdep_data (path, data) 
--                                     VALUES (?, ?) 
--                                     ON CONFLICT (path) 
--                                     DO UPDATE SET data = EXCLUDED.data
--                                 |] (path, content)
            
--             "fdep_data" -> do
--                 -- Insert into generic data table
--                 execute conn [sql|
--                     INSERT INTO fdep_data (path, data) 
--                     VALUES (?, ?) 
--                     ON CONFLICT (path) 
--                     DO UPDATE SET data = EXCLUDED.data
--                 |] (path, content)
            
--             _ -> do
--                 -- Shouldn't happen, but fallback to fdep_data
--                 execute conn [sql|
--                     INSERT INTO fdep_data (path, data) 
--                     VALUES (?, ?) 
--                     ON CONFLICT (path) 
--                     DO UPDATE SET data = EXCLUDED.data
--                 |] (path, content)
    
--     case result of
--         Left (e :: SomeException) -> do
--             print $ "PostgreSQL error: " ++ show e
--             appendFile "fdep_fallback.log" $ T.unpack path ++ "|" ++ T.unpack content ++ "\n"
--         Right val -> do
--             -- print (val,content)
--             return ()

determineTableAndType :: Text -> (Text, Text)
determineTableAndType path
    | "module_imports" `T.isInfixOf` path = ("haskell_code", "module_import")
    | "function_code" `T.isInfixOf` path = ("haskell_code", "function")
    | "types_code" `T.isInfixOf` path = ("haskell_code", "type")
    | "class_code" `T.isInfixOf` path = ("haskell_code", "class")
    | "instance_code" `T.isInfixOf` path = ("haskell_code", "instance")
    | "hs.json" `T.isInfixOf` path = ("haskell_code", "dependency")
    | "function_dependencies" `T.isInfixOf` path = ("haskell_code", "dependency")
    | otherwise = ("fdep_data", "")

parseModuleAndName :: Text -> Maybe (Text, Text)
parseModuleAndName path = do
    let parts = T.splitOn "/" path
    if length parts >= 2
    then Just (T.intercalate "/" (init parts), last parts)
    else Nothing


parseModulePath :: Text -> (Text, Text)
parseModulePath path = 
    let cleaned = T.replace "//" "/" $ T.replace "/./" "/" path
        parts = T.splitOn "/" cleaned
    in if null parts 
       then ("", "")
       else (T.intercalate "/" (init parts), last parts)