{-# LANGUAGE OverloadedStrings #-}

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

type ModuleName = String
type FunctionGraphSet = Map.Map String [FunctionInfo]

data FunctionInfo = FunctionInfo
  { package :: String
  , module' :: String
  , name    :: String
  } deriving (Show, Eq, Ord)

instance Aeson.ToJSON FunctionInfo where
  toJSON (FunctionInfo pkg moduleName name) =
    Aeson.object [ "package" Aeson..= pkg
                 , "module"  Aeson..= moduleName
                 , "name"    Aeson..= name
                 ]

baseDir :: IO String
baseDir = do 
  res <- lookupEnv "REPO_DIR"
  pure $ fromMaybe "./" res

processDumpFile :: FilePath -> IO (ModuleName, FunctionGraphSet)
processDumpFile path = do
  baseDirPath <- baseDir
  let module_name = replace ".hs.json" ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  putStrLn module_name
  content <- B.readFile path
  let d = fromMaybe [] (Aeson.decode content :: Maybe [[(String, [String])]])
  let functionGraph = foldr processEntry Map.empty d
  let functionGraphSet = Map.mapWithKey processSet functionGraph
  return (module_name, functionGraphSet)
  where
    processEntry [] acc = acc
    processEntry ((caller, callees):_) acc
      | caller /= "" = Map.insertWith (++) caller (filter validCallee callees) acc
      | otherwise = acc
    validCallee callee = not ("_sys" `isInfixOf` (callee)) && not ("_in" `isInfixOf` (callee))
    processSet _ callees = map processFunctionInfo (Set.toList $ Set.fromList callees)
    processFunctionInfo func =
      let parts = splitOn ("$") func
          func_name = if length parts > 4 then concat (drop 3 parts) else parts !! 3
      in FunctionInfo (parts !! 1) (parts !! 2) func_name

main :: IO ()
main = do

  baseDirPath <- baseDir
  files <- getDirectoryContentsRecursive baseDirPath
  let jsonFiles = filter (\x -> (".hs.json" `isSuffixOf`) $ x) files
  functionGraphs <- mapM processDumpFile jsonFiles
  let data' = Map.fromList functionGraphs
  B.writeFile "data.json" (encodePretty data')

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
