{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import System.Directory ( createDirectoryIfMissing )
import Control.Monad (unless, when,foldM,void)
import Data.List (isSuffixOf, isInfixOf ) 
import Syn2Chart.Types ( Function(..), Config (..), Edge, NodeTypes (..), CaseCache,Relation)
import Data.Aeson ( encode, ToJSON (toJSON), Value (..), object, (.=) )
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as DBS
import Data.Text (Text,pack)
import System.Directory.Internal.Prelude (getArgs)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Default (Default (..))
import Control.Concurrent.MVar
    ( putMVar, readMVar, takeMVar, newMVar, MVar )
import Universum (liftIO)
import Control.Exception.Base (evaluate)
import Control.DeepSeq (force)
import Database.Bolt
import qualified Data.HashSet as HashSet
import Streamly
import Streamly.Prelude ( mapM_, drain, fromList,mapM,toList )
import Prelude hiding (mapM,mapM_)
import Neo4j.Helpers
import Syn2Chart.Helpers
import GHC.Generics

main :: IO ()
main = do
  args <- getArgs
  config <- case args of
              [x] -> readConfigFile x
              _ -> error "pass config file path"
  let prefixPath = dumpDir config

  files <- getDirectoryContentsRecursive prefixPath
  let jsonFiles = filter (".lbind.ast.show.jsonL" `isSuffixOf`) files
  when (shouldLog  config) $ print ("found " <> show (Prelude.length jsonFiles) <> " files")

  createDirectoryIfMissing True prefixPath
  when (shouldLog  config) $ print ("loading the files from dump" :: String)

  recursiveFunctionsMVar <- newMVar HashSet.empty
  binds <- toList $ serially $ mapM (\x -> processDumpFiles x prefixPath recursiveFunctionsMVar) (fromList jsonFiles)
  
  let hmBinds = HM.fromList $ filter (not . shouldFilter) $ concat binds

  pipe <- do
    if graphDBEnabled config
      then do
        Just <$> (connect $ def { user = "neo4j", password = "parole-delete-riviera-parker-pegasus-8198" , host = "localhost"})
      else pure Nothing

  clearDB pipe

  when (cleanUp config) $ do
    removeIfExists (prefixPath <> "data.jsonL")
    removeIfExists (prefixPath <> "data-lbind.jsonL")
    removeIfExists (prefixPath <> "data-path.jsonL")
    removeIfExists (prefixPath <> "data-dump.jsonL")
    removeIfExists (prefixPath <> "top-lvl-binds.json")
    removeIfExists (prefixPath <> "function-flows-cnt.txt")
    removeIfExists (prefixPath <> "data-lbind-not-rec.jsonL")
    removeIfExists (prefixPath <> "data-lbind-same-rec.jsonL")
    removeIfExists (prefixPath <> "recursive-binds.json")

  when (shouldLog  config) $ print ("created the top-lvl-binds for reference at: " <> prefixPath)

  DBS.writeFile (prefixPath <> "top-lvl-binds.json") (toStrict $ encode $ HM.keys hmBinds)

  recursiveFunctionsHashSet <- readMVar recursiveFunctionsMVar

  DBS.writeFile (prefixPath <> "recursive-binds.json") (toStrict $ encode $ HashSet.toList recursiveFunctionsHashSet)
  
  functionsToGeneratePaths <- if (diff config) then readModifiedFunctionsList else pure mempty
  
  let bindsToGenerateFor =
        if (diff config)
          then filter (\(k,_) -> any (\x -> x `isInfixOf` (T.unpack k)) (map (T.unpack . fst) functionsToGeneratePaths)) $ HM.toList hmBinds
          else if null (endpoints config)
            then HM.toList hmBinds
          else filter (\(k,_) -> any (T.unpack k `isInfixOf`) (endpoints config)) $ HM.toList hmBinds
  when (shouldLog  config) $ print ("found the following endpoints" :: String,show $ map fst bindsToGenerateFor)

  drain $ serially $ mapM_ (\(name,functionData) -> do
    liftIO $ do
      let modifiedFunctionsHashSet :: HashSet.HashSet Text = if (diff config) then HashSet.fromList $ (concatMap (snd) functionsToGeneratePaths) else (HashSet.empty)
      when (shouldLog  config) $ print ("processing function: " <> name)
      DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode functionData) Prelude.<> "\n")
      pathsMvar <- newMVar 1
      relationsMvar <- newMVar HashSet.empty
      convertBindToEdgesList modifiedFunctionsHashSet prefixPath functionData hmBinds pathsMvar pipe relationsMvar recursiveFunctionsHashSet
      paths <- readMVar pathsMvar
      DBS.appendFile (prefixPath <> "function-flows-cnt.txt") ((toStrict $ encode $ "got " <> show paths <> " paths for the function: " <> T.unpack name) <> "\n")
    ) (fromList bindsToGenerateFor)
  where
    shouldFilter x =
      let n =  fst x
      in ("$_in$$" `T.isInfixOf` n || "$_sys$" `T.isInfixOf` n || "$$" `T.isInfixOf` n)

type CRelation = Text
type CUniqueName = Text
type SrcSpan = Text

data CNode =
        CFunction CUniqueName (Maybe SrcSpan) Bool (Maybe Text) (Maybe Text)
        | CDecision Text Text Text CRelation [CNode]
        | CEnd [CNode]
        | CStart Text
  deriving (Generic,Eq,Show)

type CPath = [CNode]

instance ToJSON CNode where
  toJSON (CFunction name mSrcSpan isRecursive funciton argument) = object [
      "_name" .= String name
      , "_typeNode" .= String "Function"
      , "mSrcSpan" .= toJSON mSrcSpan
      , "isRecursive" .= toJSON isRecursive
      , "funciton" .= toJSON funciton
      , "argument" .= toJSON argument
    ]
  toJSON (CEnd context) =
        object
          [
            "context" .= map toJSON context
            , "_typeNode" .= String "End"
          ]
  toJSON (CStart name) =
        object
          [
            "root" .= String name
            , "_typeNode" .= String "Start"
          ]
  toJSON (CDecision id_ pprE t relation context) =
        object
          [ "_id" .= String id_
            , "_prettyPrintCode" .= String pprE
            , "_typeNode" .= String "Decision"
            , "type" .= String t
            , "_relation" .= String relation
            , "context" .= toJSON context
          ]

createCNode :: Function -> Maybe Text -> IO [CNode]
createCNode (Function name _ False _ mSrcSpan isRecursive funciton argument) _ = pure [CFunction name mSrcSpan isRecursive funciton argument]
createCNode (CaseFunction id_ _ pprE t _ _ _) (Just relation) = pure [CDecision id_ pprE t relation []]
createCNode _ _ = pure []

groupWithContext :: [CNode] -> [CNode]
groupWithContext  path = go [] path []
  where
    go :: [CNode] -> [CNode] -> [CNode] -> [CNode]
    go contextAcc [] compressedPath = compressedPath <> [CEnd contextAcc]
    go contextAcc (x@(CFunction _ _ _ _ _):xs) compressedPath = go (contextAcc <> [x]) xs compressedPath
    go contextAcc ((CDecision id_ pprE t relation context):xs) compressedPath = go [] xs (compressedPath <> [CDecision id_ pprE t relation (context <> contextAcc)])
    go _ (x@(CStart _):xs) compressedPath = go [] xs (compressedPath <> [x])

convertBindToEdgesList :: HashSet.HashSet Text -> String -> Function -> HM.HashMap Text Function -> MVar Int -> Maybe Pipe -> MVar (HashSet.HashSet Text) -> HashSet.HashSet Text ->  IO ()
convertBindToEdgesList modifiedFunctionsHashSet prefixPath (Function pName _ pIsCase pChildren _ isRecursive funciton argument) hmBinds mvar pipe relationsMvar recursiveFunctionsHashSet = do
  insertNode pipe NStartFunction (pName,pIsCase,pName) ""
  void $ go HM.empty [CStart pName] (pName,pName) HM.empty True [(pName,pIsCase,pName)] Nothing [] pChildren [] HashSet.empty
  where
    go :: CaseCache -> CPath -> (Text,Text) -> HM.HashMap Text Text -> Bool -> [Edge] -> Maybe Text -> [Text] -> [Function] -> [Function] -> HashSet.HashSet Text -> IO CaseCache
    go caseMatchMemoryHM path _ _ _ prev relation acc [] [] _ = do
      unless (checkForErrorFlows prev) $ do
        currentNodeHash <- evaluate $ force $ hashToTextText $ (pack $ show acc)
        insertNode pipe NEnd ("END",False,currentNodeHash <> pName) (pack $ show acc)
        createRelationship pipe True False (last prev) ("END",False,currentNodeHash <> pName) relation
        l <- takeMVar mvar
        print (l,hashToTextText (pack $ show path))
        -- DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ encode prev Prelude.<> "\n")
        if HashSet.null modifiedFunctionsHashSet
          then
            DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (map toJSON $ groupWithContext path) Prelude.<> "\n")
          else if any (\x -> HashSet.member x modifiedFunctionsHashSet) (acc)
            then DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (map toJSON $ groupWithContext path) Prelude.<> "\n")
          else
            pure ()
        -- DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (caseMatchMemoryHM) Prelude.<> "\n")
        putMVar mvar (l + 1)
      pure caseMatchMemoryHM
    go caseMatchMemoryHM path (prevCase,prevRelation) caseMemory isStart prev relation acc [] futureFunctions visitedHashSet = do
      go caseMatchMemoryHM path (prevCase,prevRelation) caseMemory isStart prev relation acc futureFunctions [] visitedHashSet
    go caseMatchMemoryHM''' path (prevCase,prevRelation) caseMemory isStart prev relation acc currentNodes futureFunctions visitedHashSet = do
      foldM (\caseMatchMemoryHM x -> do
          case x of
            (Function _name _type isCase childChildren srcSpan isRecursive funciton argument) -> do
              currentNodeHash <- evaluate $ force $ hashToText (show prev <> show [(_name,isCase,_name,relation)])
              if not (checkForErrorFlows [(_name,isCase,currentNodeHash)])
                then
                  if shouldExpandThese (_name,isCase,"")
                      then do
                        case HM.lookup _name hmBinds of
                          Just functionData@(Function __name __type _isCase _childChildren ss isRecursive' funciton argument) -> do
                            if _name /= pName && not (HashSet.member _name visitedHashSet)
                              then do
                                -- DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode functionData) Prelude.<> "\n")
                                -- DBS.appendFile (prefixPath <> "data-lbind-not-rec.jsonL") (toStrict (encode $ _name <> pack (show srcSpan)) Prelude.<> "\n")
                                caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM (prevCase,prevRelation) (Function __name __type _isCase [] ss isRecursive' funciton argument)
                                newFunctionNode <- createCNode (Function __name __type _isCase _childChildren ss isRecursive funciton argument) Nothing
                                go caseMatchMemoryHM' (path <> newFunctionNode) (prevCase,prevRelation) caseMemory isStart prev relation (acc <> [pack (show (_name))]) _childChildren (childChildren <> futureFunctions) (HashSet.insert _name visitedHashSet)
                              else do
                                -- DBS.appendFile (prefixPath <> "data-lbind-same-rec.jsonL") (toStrict (encode $ _name <> pack (show srcSpan)) Prelude.<> "\n")
                                caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM (prevCase,prevRelation) (Function __name __type _isCase [] ss isRecursive' funciton argument)
                                newFunctionNode <- createCNode (Function _name __type _isCase _childChildren ss isRecursive funciton argument) Nothing
                                go caseMatchMemoryHM' (path <> newFunctionNode) (prevCase,prevRelation) caseMemory isStart prev relation (acc <> [pack (show (_name))]) childChildren futureFunctions visitedHashSet
                          _ -> do
                            caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM (prevCase,prevRelation) (Function _name _type isCase [] srcSpan isRecursive funciton argument)
                            -- DBS.appendFile (prefixPath <> "data-lbind-rec.jsonL") (toStrict (encode $ _name <> pack (show srcSpan)) Prelude.<> "\n")
                            newFunctionNode <- createCNode (Function _name _type isCase childChildren srcSpan isRecursive funciton argument) Nothing
                            go caseMatchMemoryHM' (path <> newFunctionNode) (prevCase,prevRelation) caseMemory isStart prev relation (acc <> [pack (show (_name))]) childChildren futureFunctions visitedHashSet
                      else do
                        caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM (prevCase,prevRelation) (Function _name _type isCase [] srcSpan isRecursive funciton argument)
                        newFunctionNode <- createCNode (Function _name _type isCase childChildren srcSpan isRecursive funciton argument) Nothing
                        go caseMatchMemoryHM' (path <> newFunctionNode) (prevCase,prevRelation) caseMemory isStart prev relation (acc <> [pack (show (_name))]) childChildren futureFunctions visitedHashSet
                else do
                  print ("Not processing since error flows" :: [Char],(_name,isCase,currentNodeHash))
                  insertNode pipe NError (_name,isCase,currentNodeHash <> pName) (pack $ show acc)
                  createRelationshipError pipe False isStart (last prev) (_name,isCase,currentNodeHash <> pName) relation
                  pure caseMatchMemoryHM
            (CaseFunction _id caseExtract _name _type isCase childChildren srcSpan) -> do
              currentNodeHash <- evaluate $ force $ pack $ show _id
              insertNode pipe NFunction (_name,isCase,currentNodeHash<> pName) (pack $ show acc)
              createRelationship pipe False isStart (last prev) (_name,isCase,currentNodeHash <> pName) relation
              mPrevRelation <- pure Nothing--checkAndReturnIfInCaseMemory caseMemory caseExtract
              case HM.lookup (_id <> _name) caseMatchMemoryHM of
                Just val -> do
                  print ("Case is already cached " <> " id: " <> _id <> " name: " <> _name)
                  -- generateCachedPath val caseMatchMemoryHM path (prevCase,prevRelation) prev relation acc currentNodes futureFunctions visitedHashSet
                  pure caseMatchMemoryHM
                Nothing -> do
                  case mPrevRelation of
                    Just prevRelation' -> do
                      foldM (\caseMatchMemoryHM'' (CaseRelation __name __type _isCase _childChildren _) -> do
                          caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM'' (prevCase,prevRelation') ((CaseFunction _id caseExtract _name _type isCase [] srcSpan))
                          newFunctionNode <- createCNode (CaseFunction _id caseExtract _name _type isCase childChildren srcSpan) (Just __name)
                          currentPathHash <- evaluate $ force $ hashToTextText $ pack $ show (path <> newFunctionNode)
                          l <- takeMVar relationsMvar
                          if prevRelation' == __name && not (HashSet.member currentPathHash l)
                            then do
                              putMVar relationsMvar $ HashSet.insert currentPathHash l
                              go caseMatchMemoryHM' (path <> newFunctionNode) ((_id <> _name),__name) caseMemory False (prev <> [(_name,isCase,currentNodeHash<> pName)]) (Just (__name <> " :: " <> __type)) [] _childChildren futureFunctions (HashSet.insert currentPathHash visitedHashSet)
                            else do
                              putMVar relationsMvar l
                              pure caseMatchMemoryHM'
                        ) HM.empty childChildren
                    Nothing ->
                      foldM (\caseMatchMemoryHM'' (CaseRelation __name __type _isCase _childChildren _) -> do
                          caseMatchMemoryHM' <- lookupAndInsertIntoMVar caseMatchMemoryHM'' (prevCase,prevRelation) ((CaseFunction _id caseExtract _name _type isCase [] srcSpan))
                          newFunctionNode <- createCNode (CaseFunction _id caseExtract _name _type isCase childChildren srcSpan) (Just __name)
                          currentPathHash <- evaluate $ force $ hashToTextText $ pack $ show newFunctionNode
                          l <- takeMVar relationsMvar
                          if not $ HashSet.member currentPathHash l
                            then do
                              putMVar relationsMvar $ HashSet.insert currentPathHash l
                              go caseMatchMemoryHM' (path <> newFunctionNode) ((_id <> _name),__name) (addToCaseMemory caseMemory caseExtract __name __type) False (prev <> [(_name,isCase,currentNodeHash<> pName)]) (Just (__name <> " :: " <> __type)) [] _childChildren futureFunctions ((HashSet.insert currentPathHash visitedHashSet))
                            else do
                              putMVar relationsMvar l
                              pure caseMatchMemoryHM'
                        ) caseMatchMemoryHM childChildren
        ) caseMatchMemoryHM''' currentNodes

    generateCachedPath :: HM.HashMap Relation [Function] -> CaseCache -> CPath -> (Text,Text)  -> [Edge] -> Maybe Text -> [Text] -> [Function] -> [Function] -> HashSet.HashSet Text -> IO ()
    generateCachedPath val caseMatchMemoryHM path (prevCase,prevRelation) prev relation acc currentNodes futureFunctions visitedHashSet  = do
      case HM.lookup prevRelation val of
        Just rExpansion -> do
          (didPathEnd,path,prev,acc,visitedHashSet) <- foldM (\(foundNextCase,path,prev,acc,(visitedHashSet)) x -> do
                case x of
                  (Function _name _type isCase childChildren srcSpan isRecursive funciton argument) -> do
                    newFunctionNode <- createCNode (Function _name _type isCase childChildren srcSpan isRecursive funciton argument) Nothing
                    pure $ ((False || foundNextCase),(path <> newFunctionNode),prev,(acc <> [pack (show (_name))]),(HashSet.insert _name visitedHashSet))
                  x@(CaseFunction _id caseExtract _name _type isCase childChildren srcSpan)           -> do
                    currentNodeHash <- evaluate $ force $ pack $ show _id
                    newFunctionNode <- createCNode (CaseFunction _id caseExtract _name _type isCase childChildren srcSpan) (Just _name)
                    currentPathHash <- evaluate $ force $ hashToTextText $ pack $ show newFunctionNode
                    case HM.lookup (_id <> _name) caseMatchMemoryHM of
                      Just v -> do
                        mapM_ (\(CaseRelation __name __type _isCase _childChildren _) ->
                                  generateCachedPath v caseMatchMemoryHM (path <> newFunctionNode) (_id <> _name,__name) (prev <> [(_name,isCase,currentNodeHash <> pName)]) (Just __name) acc _childChildren [] visitedHashSet
                              ) (fromList childChildren)
                      Nothing -> print ("path not traversed " <> (show x))
                    pure $ (True,(path <> newFunctionNode),(prev <> [(_name,isCase,currentNodeHash <> pName)]),acc,(HashSet.insert currentPathHash visitedHashSet))
              ) (False,path,prev,acc,(visitedHashSet)) rExpansion
          when (didPathEnd) $ do
            unless (checkForErrorFlows prev) $ do
              currentNodeHash <- evaluate $ force $ hashToTextText (pack $ show acc)
              insertNode pipe NEnd ("END",False,currentNodeHash <> pName) (pack $ show acc)
              createRelationship pipe True False (last prev) ("END",False,currentNodeHash <> pName) relation
              l <- takeMVar mvar
              print (l,hashToTextText (pack $ show path))
              -- DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ encode prev Prelude.<> "\n")
              DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (map toJSON $ groupWithContext path) Prelude.<> "\n")
              -- DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (caseMatchMemoryHM) Prelude.<> "\n")
              putMVar mvar (l + 1)
        Nothing ->
          print "case is not stored"
      -- DBS.appendFile (prefixPath <> "data-path.jsonL") (toStrict $ encode (map toJSON $ groupWithContext path) Prelude.<> "\n")