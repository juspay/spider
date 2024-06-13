
module Neo4j.Helpers where

import Database.Bolt
import Syn2Chart.Types
import qualified Data.Map as Map
import Data.Text
import Control.Monad
import Data.Bool (bool)
import Control.Exception (try)
import Exception (SomeException)

insertNode :: Maybe Pipe -> NodeTypes -> Edge -> Text -> IO ()
insertNode Nothing _ _ _ = pure ()
insertNode (Just pipe) typeOfNode (name,isCase,uniqueId) context = do
  let params = Map.fromList [ ("name", T name)
                , ("isCase", B isCase)
                , ("uniqueId", T uniqueId)
                , ("context", T context)
                ]
      cypher = "CREATE (n:"<> pack (show typeOfNode) <> " {name: $name, isCase: $isCase, uniqueId: $uniqueId, context: $context}) RETURN n"
  eRes <- try $ run pipe $ queryP cypher params
  case eRes of
    Left (_ :: SomeException) -> pure ()
    _ -> pure ()

createRelationship :: Maybe Pipe -> Bool -> Bool -> Edge -> Edge -> Maybe Text -> IO ()
createRelationship Nothing _ _ _ _ _ = pure ()
createRelationship (Just pipe) isEnd isStart (pName,pIsCase,uniqueId1) (cName,cIsCase,uniqueId2) mRelation = do
  let params = Map.fromList [("pName", T pName) , ("pIsCase", B pIsCase) , ("uniqueId1", T uniqueId1), ("cName", T cName) , ("cIsCase", B cIsCase) , ("uniqueId2", T uniqueId2)]
      prop = "RELATED_TO {edge: " <> maybe "\'NO_MATCH\'" (pack . show) mRelation <> "}"
      cypher = "MATCH (a:" <> bool (pack $ show NFunction) (pack $ show NStartFunction) isStart <> " {name: $pName, isCase: $pIsCase, uniqueId: $uniqueId1}), (b:" <> bool (pack $ show NFunction) (pack $ show NEnd) isEnd <> " {name: $cName, isCase: $cIsCase, uniqueId: $uniqueId2}) \
              \WHERE NOT EXISTS((a)-[:" <> prop <> "]->(b)) \
              \CREATE (a)-[r:" <> prop <> "]->(b) \
              \RETURN r"
  void $ run pipe $ queryP cypher params

createRelationshipError :: Maybe Pipe -> Bool -> Bool -> Edge -> Edge -> Maybe Text -> IO ()
createRelationshipError Nothing _ _ _ _ _ = pure ()
createRelationshipError (Just pipe) isEnd isStart (pName,pIsCase,uniqueId1) (cName,cIsCase,uniqueId2) mRelation = do
  let params = Map.fromList [("pName", T pName) , ("pIsCase", B pIsCase) , ("uniqueId1", T uniqueId1), ("cName", T cName) , ("cIsCase", B cIsCase) , ("uniqueId2", T uniqueId2)]
      prop = "RELATED_TO {edge: " <> maybe "\'NO_MATCH\'" (pack . show) mRelation <> "}"
      cypher = "MATCH (a:" <> bool (pack $ show NFunction) (pack $ show NStartFunction) isStart <> " {name: $pName, isCase: $pIsCase, uniqueId: $uniqueId1}), (b:" <> (pack $ show $ NError) <> " {name: $cName, isCase: $cIsCase, uniqueId: $uniqueId2}) \
              \WHERE NOT EXISTS((a)-[:" <> prop <> "]->(b)) \
              \CREATE (a)-[r:" <> prop <> "]->(b) \
              \RETURN r"
  void $ run pipe $ queryP cypher params

clearDB :: Maybe Pipe -> IO ()
clearDB Nothing = pure ()
clearDB (Just pipe)  = do
  let cypher =  "MATCH (n) DETACH DELETE n"
  void $ run pipe $ queryP cypher mempty

-- CREATE CONSTRAINT ON (n:Function) ASSERT n. IS UNIQUE
-- CREATE CONSTRAINT ON (n:StartFunction) ASSERT n. IS UNIQUE
-- CREATE CONSTRAINT ON (n:END) ASSERT n. IS UNIQUE

-- MATCH (n)
-- DETACH DELETE n

-- MATCH (n)
-- OPTIONAL MATCH (n)-[r]->(m)
-- RETURN n,r,m