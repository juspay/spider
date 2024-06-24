module FieldInspector.Group where

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
import FieldInspector.Types

processDumpFileFieldUsage :: String -> FilePath -> IO (String,Map.Map String [FieldUsage])
processDumpFileFieldUsage baseDirPath path = do
  let module_name = replace ".hs.fieldUsage.json" ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else if (("test/")) `isInfixOf` (path)
                              then last (splitOn ("test/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  putStrLn module_name
  content <- B.readFile path
  let d = fromMaybe mempty (Aeson.decode content :: Maybe (Map.Map String [FieldUsage]))
  pure (module_name, d)

processDumpFileTypes :: String -> FilePath -> IO (String,Map.Map String TypeInfo)
processDumpFileTypes baseDirPath path = do
  let module_name = replace ".hs.types.json" ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else if (("test/")) `isInfixOf` (path)
                              then last (splitOn ("test/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  putStrLn module_name
  content <- B.readFile path
  let d = fromMaybe mempty (Aeson.decode content :: Maybe (Map.Map String TypeInfo))
  pure (module_name, d)

processDumpFileTypesParser :: String -> FilePath -> IO (String,Map.Map String TypeInfo)
processDumpFileTypesParser baseDirPath path = do
  let module_name = replace ".hs.types.parser.json" ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else if (("test/")) `isInfixOf` (path)
                              then last (splitOn ("test/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  putStrLn module_name
  content <- B.readFile path
  let d = fromMaybe mempty (Aeson.decode content :: Maybe (Map.Map String TypeInfo))
  pure (module_name, d)

run :: Maybe String -> IO ()
run bPath = do
  let baseDirPath =
        case bPath of
            Just val -> val
            _ -> "/tmp/fieldInspector/"
  files <- getDirectoryContentsRecursive baseDirPath

  let jsonFiles = filter (\x -> (".hs.fieldUsage.json" `isSuffixOf`) $ x) files
  fieldUsage <- mapM (processDumpFileFieldUsage baseDirPath) jsonFiles
  B.writeFile (baseDirPath <> "/" <> "fieldUsage-data.json") (encodePretty (Map.fromList fieldUsage))

  let jsonFiles = filter (\x -> (".hs.types.json" `isSuffixOf`) $ x) files
  typeData <- mapM (processDumpFileTypes baseDirPath) jsonFiles
  B.writeFile (baseDirPath <> "/" <> "types-data.json") (encodePretty (Map.fromList typeData))

  let jsonFiles = filter (\x -> (".hs.types.parser.json" `isSuffixOf`) $ x) files
  typeData <- mapM (processDumpFileTypesParser baseDirPath) jsonFiles
  B.writeFile (baseDirPath <> "/" <> "types-parser-data.json") (encodePretty (Map.fromList typeData))

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
