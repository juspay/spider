module Fdep.Group where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List.Extra (replace,splitOn)
import System.Environment (lookupEnv)
import Fdep.Types
import qualified Data.HashMap.Strict as HM

processDumpFile :: String -> String -> FilePath -> IO (String,Map.Map String Function)
processDumpFile toReplace baseDirPath path = do
  let module_name = replace toReplace ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  parserCodeExists <- doesFileExist (replace ".json" ".function_code.json" path)
  contentHM <- if parserCodeExists
                    then do
                      parsercode <- B.readFile $ replace ".json" ".function_code.json" path
                      case Aeson.decode parsercode of
                        (Just (x :: HM.HashMap String PFunction)) -> pure $ x
                        Nothing -> pure $ HM.empty
                    else pure HM.empty
  content <- B.readFile path
  let d = Map.fromList $ filter (\x -> fst x /= "") $ map (\x -> (function_name x,updateCodeString (function_name x) x contentHM)) $ fromMaybe [] (Aeson.decode content :: Maybe [Function])
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
  let jsonFiles = filter (\x -> (".hs.json" `isSuffixOf`) $ x) files
  (functionGraphs) <- mapM (processDumpFile ".hs.json" baseDirPath) jsonFiles
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
