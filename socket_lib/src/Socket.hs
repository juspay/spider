{-# LANGUAGE ScopedTypeVariables,PartialTypeSignatures,OverloadedStrings,CPP #-}
module Socket where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)
import Data.Text
import Control.Exception
import Data.Maybe
import Data.Text.Encoding
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Control.Concurrent (forkIO)
import Control.Monad (void)

#if __GLASGOW_HASKELL__ >= 900
import qualified Data.Aeson.KeyMap as HM
import qualified Data.Aeson.Key as HM
#else
import qualified Data.HashMap.Internal as HM
#endif

shouldFork :: Bool
shouldFork = 
    case toLower $ pack $ fromMaybe ("False") $ unsafePerformIO $ lookupEnv "SHOULD_FORK" of
        ("true" :: Text) -> True
        _ -> False

forkWrap action = if shouldFork then void $ forkIO action else action 

fdepSocketPath :: Maybe FilePath
fdepSocketPath = unsafePerformIO $ lookupEnv "FDEP_SOCKET_PATH"

sendTextData' :: _ -> NS.Socket -> Text -> Text -> IO ()
sendTextData' _ sock _ data_ = do
    NSB.sendAll sock (encodeUtf8 $ data_ <> "\n")

connectToUnixSocket :: FilePath -> IO NS.Socket
connectToUnixSocket socketPath = do
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.connect sock (NS.SockAddrUnix socketPath)
    return sock

sendPathPerformAction :: FilePath -> FilePath -> (NS.Socket -> IO ()) -> IO ()
sendPathPerformAction path socketPath action = do
    res <- try $ withUnixSocket socketPath $ \sock -> do
                    NSB.sendAll sock (encodeUtf8 $ pack path <> "\n")
                    ackResponse <- NSB.recv sock 1024
                    case ackResponse of
                        "ACK\n" -> action sock
                        _ -> pure ()
    case res of
        Left (err :: SomeException) -> pure ()
        Right _ -> pure ()

withUnixSocket :: FilePath -> (NS.Socket -> IO ()) -> IO ()
withUnixSocket socketPath action = 
    bracket
        (connectToUnixSocket socketPath)
        NS.close
        action

sendViaUnixSocket :: FilePath -> Text -> Text -> IO ()
sendViaUnixSocket socketPath path data_ =
    let socketPathToUse = fromMaybe (socketPath) fdepSocketPath
    in sendPathPerformAction (unpack path) socketPathToUse (\sock -> NSB.sendAll sock (encodeUtf8 $ data_ <> "\n"))

transformPayload :: (Show a) => Text -> Text -> a -> Value -> Text
transformPayload path key payload_type value = 
#if __GLASGOW_HASKELL__ >= 900
        (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String key), ((HM.fromString $ show payload_type), value)])
#else
        (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String key), ((pack $ show payload_type), value)])
#endif
    