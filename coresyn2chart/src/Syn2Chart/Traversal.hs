module Syn2Chart.Traversal where
import Syn2Chart.Types

translateCoreProgramToCFG :: [LBind] -> [Function]
translateCoreProgramToCFG = concatMap traverseBindings

traverseForFlowsLExpr :: LExpr -> [Function] -> [Function]
traverseForFlowsLExpr (LVar name tykind) argument = [Function name tykind False argument]
traverseForFlowsLExpr (LLit _) argument = argument
traverseForFlowsLExpr (LType _) argument = argument
traverseForFlowsLExpr (LApp (LVar name _) (LLit x)) argument = [Function name "" False [Function x "" False argument]]
traverseForFlowsLExpr (LApp (LVar name _) (LVar x _)) argument = [Function name "" False [Function x "" False argument]]
traverseForFlowsLExpr (LApp (LVar name _) (LType _)) argument = [Function name "" False argument]
traverseForFlowsLExpr (LApp f a) argument =
    let arg = traverseForFlowsLExpr a argument
        fun = traverseForFlowsLExpr f arg
    in fun
traverseForFlowsLExpr (LLam e a) argument = [Function e "" False (traverseForFlowsLExpr a argument)]
traverseForFlowsLExpr (LLet b e) argument =
  let arg = traverseForFlowsLExpr e argument
  in traverseBindingsInternal b arg
traverseForFlowsLExpr (LCase e _ t alts) argument = [Function e t True (concatMap (\x -> countAlt t x argument) alts)]
traverseForFlowsLExpr (LUnhandled _ _) argument = argument

traverseBindings :: LBind -> [Function]
traverseBindings (LNonRec name tyVarK expr) =
  [Function name tyVarK False $ traverseForFlowsLExpr expr []]
traverseBindings (LRec bs) =
  map (\(name,tyVarK,expr) -> Function name tyVarK False $ traverseForFlowsLExpr expr []) bs

traverseBindingsInternal :: LBind -> [Function] -> [Function]
traverseBindingsInternal (LNonRec _ tyVarK expr) argument = traverseForFlowsLExpr expr argument
traverseBindingsInternal (LRec bs) argument = concatMap (\(name,tyVarK,expr) -> traverseForFlowsLExpr expr argument) bs
traverseBindingsInternal LNull argument = argument

countAlt :: String -> (LAltCon, [LExpr], LExpr) -> [Function] -> [Function]
countAlt t (p, [], e) args = [Function (extractNameFromLAltCon p) t True $ traverseForFlowsLExpr e args]
countAlt t (p, val, e) args = [Function (extractNameFromLAltCon p) t True $ traverseForFlowsLExpr e args]

