{-# LANGUAGE ScopedTypeVariables,PartialTypeSignatures,OverloadedStrings #-}
module Socket where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)
import Data.Text
import Control.Exception
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)

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
                        _ -> print $ "Unexpected path response: " <> show ackResponse
    case res of
        Left (err :: SomeException) -> print $ "Error sending data: " <> (pack $ show err)
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