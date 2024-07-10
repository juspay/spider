module Fdep.Group where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.Map as Map
import Data.Aeson.Encode.Pretty (encodePretty)
import Fdep.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

processDumpFile :: Text -> Text -> Text -> IO (Text,Map.Map Text Function)
processDumpFile toReplace baseDirPath path = do
  let module_name = T.replace toReplace ""
                      $ T.replace "/" "."
                        $ if (("src/")) `T.isInfixOf` (path)
                            then last (T.splitOn ("src/") (T.replace baseDirPath "" path))
                          else if (("src-generated/")) `T.isInfixOf` (path)
                              then last (T.splitOn ("src-generated/") (T.replace baseDirPath "" path))
                          else if (("src-extras/")) `T.isInfixOf` (path)
                              then last (T.splitOn ("src-extras/") (T.replace baseDirPath "" path))
                          else T.replace baseDirPath "" path
  parserCodeExists <- doesFileExist (T.unpack $ T.replace ".json" ".function_code.json" path)
  contentHM <- if parserCodeExists
                    then do
                      parsercode <- B.readFile $ T.unpack $ T.replace ".json" ".function_code.json" path
                      case Aeson.decode parsercode of
                        (Just (x :: HM.HashMap Text PFunction)) -> pure $ x
                        Nothing -> pure $ HM.empty
                    else pure HM.empty
  content <- B.readFile $ T.unpack path
  decodedContent <- case Aeson.decode content of
      (Just (x :: HM.HashMap Text Function)) -> pure $ x
      Nothing -> pure $ HM.empty
  let d = Map.fromList $ filter (\x -> fst x /= "") $ map (\(name,x) -> (name,updateCodeString (name) x contentHM)) $ HM.toList $ decodedContent
  pure (module_name, d)
  where
    updateCodeString functionName functionObject contentHM =
      case HM.lookup functionName contentHM of
        Just val -> functionObject {stringified_code = (parser_stringified_code val)}
        Nothing -> functionObject

run :: Maybe String -> IO ()
run bPath = do
  let baseDirPath =
        case bPath of
            Just val -> val
            _ -> "/tmp/fdep/"
  files <- getDirectoryContentsRecursive baseDirPath
  let jsonFiles = map T.pack $ filter (\x -> (".hs.json" `T.isSuffixOf`) $ T.pack x) files
  (functionGraphs) <- mapM (processDumpFile ".hs.json" (T.pack baseDirPath)) jsonFiles
  B.writeFile (baseDirPath <> "data.json") (encodePretty (Map.fromList functionGraphs))

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
