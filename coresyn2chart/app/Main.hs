module Main where

import System.Directory ( doesDirectoryExist, listDirectory, createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.Environment ( getArgs )
import Control.Monad ( forM, foldM )
import Data.List ( isSuffixOf )
import Syn2Chart.Types ( LBind, Function(Function) )
import Data.Aeson ( eitherDecode, encode )
import Data.ByteString.Lazy (readFile, toStrict)
import Syn2Chart.Traversal
import qualified Data.Functor
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString as DBS
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import System.IO (IOMode(WriteMode))
import System.IO.Extra (withBinaryFile)
import Data.ByteString.Builder
import System.Directory.Extra (removeFile)
import Control.Exception ( catch, throwIO )
import System.Directory.Internal.Prelude (isDoesNotExistError)
import qualified Data.HashMap.Strict as HM

getBase64FunctionName :: String -> String
getBase64FunctionName = unpack . decodeUtf8 . BS.encode . toStrict . encode

writeWithHandle :: FilePath -> Builder -> IO ()
writeWithHandle path tx =
  withBinaryFile path WriteMode $ \h ->
  hPutBuilder h tx

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

main :: IO ()
main = do
  args <- getArgs
  let prefixPath =
              case args of
                []  -> "/tmp/coresyn2chart/"
                [x] -> x
                _ -> error "unexpected no of arguments"
  files <- getDirectoryContentsRecursive prefixPath
  let jsonFiles = filter (".lbind.ast.show.json" `isSuffixOf`) files
  print $ Prelude.length jsonFiles
  binds <- processDumpFiles jsonFiles
  createDirectoryIfMissing True prefixPath
  removeIfExists (prefixPath <> "data.jsonL")
  print $ Prelude.length binds
  r <- foldM (\acc (_,x) -> do
    DBS.appendFile (prefixPath <> "data.jsonL") (toStrict (encode x)Prelude.<> "\n")
    pure (acc <> [x])
    ) [] $ HM.toList $ HM.fromList $ map (\functionData@(Function functionName _ _ _)-> (functionName,functionData)) $ translateCoreProgramToCFG binds
  print $ Prelude.length r

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    names <- listDirectory dir
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getDirectoryContentsRecursive path
            else return [path]
    return (concat paths)

processDumpFiles :: [String] -> IO [LBind]
processDumpFiles files = do
  contents <- mapM (\file -> do
                      content <- Data.ByteString.Lazy.readFile file
                      case eitherDecode content of
                        Right (val :: [LBind]) -> pure val
                        Left err -> do
                          print err
                          print file
                          pure $ []
                    ) files
  pure $ concat contents