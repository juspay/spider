{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Syn2Chart.Plugin (plugin) where

import Syn2Chart.Types
import Control.Monad
import CoreMonad
import CoreSyn hiding (TB)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile, toStrict,readFile)
import Data.Data
import GhcPlugins hiding (TB,(<>))
import qualified Data.Map as Map
import Prelude hiding (id)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Data.List.Extra (replace,intercalate,splitOn)
import System.Directory (createDirectoryIfMissing)
import Control.Concurrent


plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
    , pluginRecompile = GhcPlugins.purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "CoreSyn2Chart" (buildCfgPass args) : todos)

buildCfgPass ::  [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
                        []    -> "/tmp/coresyn2chart/"
                        [local] -> local
                        _ -> error "unexpected no of arguments"
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ moduleName $ mg_module guts
            moduleLoc = prefixPath Prelude.<> (getFilePath $ mg_loc guts)
        print ("generated coreAST for module: " <> moduleN <> " at path: " <> moduleLoc)
        createDirectoryIfMissing True ((intercalate "/" . reverse . tail . reverse . splitOn "/") moduleLoc)
        !_ <- Prelude.writeFile (moduleLoc Prelude.<> ".ast.show.json")
                (unpack $ decodeUtf8 $ toStrict $ encodePretty $ Map.fromList (concatMap bindToJSON binds))
        Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".lbind.ast.show.json") (encodePretty $ map (toJSON . toLBind) binds)
        -- res <- Data.ByteString.Lazy.readFile (moduleLoc Prelude.<> ".lbind.ast.show.json")
        -- case eitherDecode $ res of
        --     Right (_ :: [LBind]) -> print ("generated binds for " Prelude.<> moduleN)
        --     Left err -> print err
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

toLexpr :: (Expr Var) -> LExpr
toLexpr (Var x)         = LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)
toLexpr x@(Lit _)       = LLit (showSDocUnsafe $ ppr x)
toLexpr (Type id)       = LType (showSDocUnsafe $ ppr id)
toLexpr (App func args) = LApp (toLexpr func) (toLexpr args)
toLexpr (Lam func args) = LLam (nameStableString (idName func)) (toLexpr args)
toLexpr (Let func args) = LLet (toLBind func) (toLexpr args)
toLexpr (Case condition bind _type alts) = LCase (replace "\n" "" $ showSDocUnsafe $ ppr $ condition) (nameStableString (idName bind)) (showSDocUnsafe $ ppr _type) (map toLAlt alts)
toLexpr v = LUnhandled (show $ toConstr v) (showSDocUnsafe $ ppr v)

toLAlt :: (AltCon, [Var], CoreExpr) -> (LAltCon, [LExpr], LExpr)
toLAlt ((DataAlt dataCon), val, e) = (LDataAlt (showSDocUnsafe $ ppr dataCon), map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)
toLAlt ((LitAlt lit), val, e) = (LLitAlt (showSDocUnsafe $ ppr lit), map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)
toLAlt (DEFAULT, val, e) = (LDEFAULT, map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)

toLBind :: CoreBind -> LBind
toLBind (NonRec binder expr) = (LNonRec (nameStableString $ idName binder) (showSDocUnsafe $ ppr $ tyVarKind binder) (toLexpr expr))
toLBind (Rec binds) = LRec (map (\(b, e) -> (nameStableString (idName b),(showSDocUnsafe $ ppr $ tyVarKind b), toLexpr e)) binds)