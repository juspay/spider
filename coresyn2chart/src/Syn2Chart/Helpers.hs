module Syn2Chart.Helpers where

import qualified Data.Text as T
import Syn2Chart.Types
import Data.Text (pack)
import qualified Data.HashMap.Strict as HM
import Universum (Hashable, MonadIO (liftIO), newMVar)
import Crypto.Hash
import Data.Aeson (encode, eitherDecodeStrict)
import Data.ByteString.Lazy (toStrict)
import Universum.String hiding (toStrict)
import System.Directory.Internal.Prelude
import System.Directory
import Streamly
import Streamly.Prelude
import Control.Monad (forM)
import System.FilePath ((</>))
import Syn2Chart.Traversal (translateCoreProgramToCFG)
import Control.Concurrent (MVar)
import qualified Data.HashSet as HashSet

readModifiedFunctionsList :: IO [(T.Text,[T.Text])]
readModifiedFunctionsList = do
  content <- readFile "flows_to_be_tested.json"
  case eitherDecodeStrict $ encodeUtf8 content of
    Right (val :: [EndPointsModified]) -> 
      pure $ Prelude.map (\x -> 
                let mName = moduleName' x 
                    endpoint = mName <> "$" <> apiHandler x
                    fModified = Prelude.concatMap (\y -> Prelude.map (\z -> (moduleName y) <> "$" <> z) (functions' y)) $ functionsModified x
                in (endpoint,fModified)
              ) val
    _ -> pure []

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

readConfigFile :: String -> IO Config
readConfigFile configPath = do
  content <- readFile configPath
  case eitherDecodeStrict $ encodeUtf8 content of
    Left err -> error err
    Right val -> pure val

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

processDumpFiles :: String -> String -> MVar (HashSet.HashSet Text) -> IO [(Text,Function)]
processDumpFiles file _ recursiveFunctionsMvar = do
  content <- readFile file
  r <- toList $ serially $ Streamly.Prelude.mapM (\line -> do
                case eitherDecodeStrict $ toStrict $ encodeUtf8 line of
                  Right (bind :: LBind) -> do
                    r <- liftIO $ translateCoreProgramToCFG [bind] recursiveFunctionsMvar
                    pure $ Prelude.map (\functionData@(Function _name _type _ _ _ _ _ _) -> (_name,functionData)) r
                  Left err -> do
                    print err
                    print file
                    pure []
          ) (fromList $ Prelude.lines content)
  pure $ concat r

checkForErrorFlows :: [Edge] -> Bool
checkForErrorFlows = Prelude.any (\(edge,_,_) -> Prelude.any (`T.isInfixOf` edge) errorFlow)
  where
    errorFlow :: [Text]
    errorFlow = ["throwException","handleClientError"]

shouldExpandThese :: Edge -> Bool
shouldExpandThese (name,_,_) = not $ Prelude.any (\(_,func) -> func `T.isInfixOf` name) [("euler-hs" :: [Char],"forkFlow")]

functionsToFilterFromPath :: Edge -> Bool
functionsToFilterFromPath (_,True,_) = True
functionsToFilterFromPath (name,False,_) = Prelude.any (`T.isInfixOf` name) ["$_sys$"]

hashToText :: (Foldable t, Show a) => t a -> Text
hashToText strs = pack $ Prelude.show (hash (toStrict $ encode $ Prelude.concatMap Prelude.show strs) :: Digest  SHA3_256)

hashToTextText :: Text -> Text
hashToTextText strs = pack $ Prelude.show (hash (toStrict $ encode strs) :: Digest  SHA3_256)

lookupAndInsertIntoMVar :: (Applicative f, Eq k1, Eq k2,Hashable k1,Hashable k2) => HM.HashMap k1 (HM.HashMap k2 [a]) -> (k1, k2) -> a -> f (HM.HashMap k1 (HM.HashMap k2 [a]))
lookupAndInsertIntoMVar caseMatches (prevCase,prevRelation) node = do
  pure $
    HM.insert
      prevCase
      (case HM.lookup prevCase caseMatches of
        Just val ->
          HM.insert
            prevRelation
            (case HM.lookup prevRelation val of
              Just l -> l <> [node]
              Nothing -> [node]
            )
            val
        Nothing ->
          HM.insert
            prevRelation
            [node]
            HM.empty
      )
      caseMatches

checkAndReturnIfInCaseMemory :: HM.HashMap Text Text -> Maybe CaseExtract -> Maybe Text
checkAndReturnIfInCaseMemory caseMemory (Just key) = HM.lookup (generateHash key) caseMemory
checkAndReturnIfInCaseMemory  _ _ = Nothing

addToCaseMemory :: HM.HashMap Text Text -> Maybe CaseExtract -> Text -> Text -> HM.HashMap Text Text
addToCaseMemory caseMemory (Just key) relationName _
  = HM.insert (generateHash key) relationName caseMemory
addToCaseMemory hm _ _ _ = hm