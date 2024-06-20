{-# LANGUAGE BangPatterns #-}
module Syn2Chart.Traversal where
import Syn2Chart.Types
    ( Function(..),
      LBind(..),
      LAltCon,
      LExpr(..),
      extractNameFromLAltCon )
import Data.Text (Text,pack)
import Data.Time
import Control.Concurrent (MVar, takeMVar, putMVar)
import qualified Data.HashSet as HashSet


translateCoreProgramToCFG :: [LBind] -> MVar (HashSet.HashSet Text) -> IO [Function]
translateCoreProgramToCFG a recursiveFunctionsMvar = do
  r <- mapM traverseBindings a
  pure $ concat r
    where
      traverseForFlowsLExpr :: LExpr -> [Function] -> IO [Function]
      traverseForFlowsLExpr x@(LVar name tykind srcSpan isLocal isExported) argument = do
        pure [Function name tykind False argument (Just srcSpan) False Nothing Nothing]
      traverseForFlowsLExpr x@LLit {} argument = pure argument
      traverseForFlowsLExpr (LType _) argument = pure argument
      traverseForFlowsLExpr x@(LApp (LVar name tykind srcSpan isLocal isExported) (LLit typeOfLit val isFunc) functionPpr argumentPpr) argument = do 
        pure [Function name "" False [Function val typeOfLit False argument Nothing False (Just argumentPpr) (Just functionPpr)] Nothing False Nothing Nothing]
      traverseForFlowsLExpr x@(LApp (LVar name tykind srcSpan isLocal isExported) (LVar name1 tykind1 srcSpan1 isLocal1 isExported1) functionPpr argumentPpr) argument = do
        pure [Function name tykind False [Function name1 tykind1 False argument (Just srcSpan1) False (Just argumentPpr) (Just functionPpr)] (Just srcSpan) False Nothing Nothing]
      traverseForFlowsLExpr x@(LApp (LVar name tykind srcSpan isLocal isExported) (LType _) functionPpr argumentPpr) argument = do
        pure [Function name tykind False argument (Just srcSpan) False (Just (functionPpr)) (Just argumentPpr)]
      traverseForFlowsLExpr x@(LApp f a functionPpr argumentPpr) argument = do
          arg <- traverseForFlowsLExpr a argument
          r <- traverseForFlowsLExpr f arg
          pure [Function "" "" False r Nothing False (Just (functionPpr)) (Just argumentPpr)]
      traverseForFlowsLExpr x@(LLam e a) argument = do
        (\x -> pure [Function e "" False (x) Nothing False Nothing Nothing]) =<< (traverseForFlowsLExpr a argument)
      traverseForFlowsLExpr (LLet b e) argument = do
        arg <- traverseForFlowsLExpr e argument
        traverseBindingsInternal b arg
      traverseForFlowsLExpr (LCase id_ caseExtract _ pprE _ t alts) argument = do
        r <- (mapM (\x -> countAlt t x argument) alts)
        pure [CaseFunction id_ caseExtract pprE t True (concat r) Nothing]
      traverseForFlowsLExpr (LUnhandled _ _) argument = pure $ argument

      traverseBindings :: LBind -> IO [Function]
      traverseBindings (LNonRec name tyVarK expr) = do
        r <- traverseForFlowsLExpr expr []
        pure [Function name tyVarK False r Nothing False Nothing Nothing]
      traverseBindings (LRec bs) =
        mapM (\(name,tyVarK,expr) -> do
          recursiveFunctions <- takeMVar recursiveFunctionsMvar
          putMVar recursiveFunctionsMvar (HashSet.insert name recursiveFunctions)
          r <- traverseForFlowsLExpr expr []
          pure $ Function name tyVarK False r Nothing True Nothing Nothing
          ) bs
      traverseBindings _ = pure $ []

      traverseBindingsInternal :: LBind -> [Function] -> IO [Function]
      traverseBindingsInternal (LNonRec name tyVarK expr) argument = do
        r <- traverseForFlowsLExpr expr argument
        pure [Function name tyVarK False r Nothing False Nothing Nothing]
      traverseBindingsInternal (LRec bs) argument =
          mapM (\(name,tyVarK,expr) -> do
            recursiveFunctions <- takeMVar recursiveFunctionsMvar
            putMVar recursiveFunctionsMvar (HashSet.insert name recursiveFunctions)
            r <- traverseForFlowsLExpr expr argument
            pure $ Function name tyVarK False r Nothing True Nothing Nothing
          ) bs
      traverseBindingsInternal LNull argument = pure argument

      countAlt :: Text -> (LAltCon, [LExpr], LExpr) -> [Function] -> IO [Function]
      countAlt t (p, [], e) args = do
        r <- traverseForFlowsLExpr e args
        pure [CaseRelation (extractNameFromLAltCon p) t True (r) Nothing]
      countAlt t (p, [LVar name tykind srcSpan isLocal isExported], e) args = do
        r <- traverseForFlowsLExpr e args
        pure [CaseRelation (extractNameFromLAltCon p) t True (r) (Just srcSpan)]
      countAlt t (p, x@((LVar name tykind srcSpan isLocal isExported):xs), e) args = do
        r <- traverseForFlowsLExpr e args
        pure [CaseRelation (extractNameFromLAltCon p) t True (r) (Just srcSpan) ]
      countAlt t (p, _, e) args = do
        r <- traverseForFlowsLExpr e args
        pure [CaseRelation (extractNameFromLAltCon p) t True (r) Nothing]