{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns,PartialTypeSignatures #-}
{-# OPTIONS_GHC -Werror=unused-imports -Werror=incomplete-patterns -Werror=name-shadowing #-}
{-# LANGUAGE CPP #-}

module Fdep.Plugin (plugin,collectDecls) where

import Socket
-- import Control.Concurrent ( forkIO )
import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Control.Reference (biplateRef, (^?))
import Data.Aeson ( encode, ToJSON(toJSON) )
import Data.Bool
import Data.ByteString.Lazy (toStrict)
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data ()
import Data.List.Extra (splitOn,nub)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time ( diffUTCTime, getCurrentTime )
import Digraph
import Fdep.Types
import GHC.IO (unsafePerformIO)
import Prelude hiding (id, writeFile,span)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.Extra as Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Prelude as P
import Socket
import System.Environment (lookupEnv)
import TcRnMonad
#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.Opt.Monad
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type hiding (tyConsOfType)
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Driver.Env
import GHC.Driver.Plugins
import GHC.Hs.Pat
import GHC.Tc.Types
import GHC.Tc.Utils.TcType
import GHC.Types.Id
import GHC.Types.Name hiding (varName)
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.Var
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import qualified Data.Aeson.KeyMap as HM
#else
import Bag (bagToList)
import BasicTypes
import CoreMonad
import CoreSyn
import Data.IORef
import DataCon
import DynFlags ()
import GHC
import GhcPlugins hiding ((<>),tyConsOfType,tyConsOfType)
import Outputable ()
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import TcType
import TyCoRep
#endif

plugin :: Plugin
plugin =
    defaultPlugin
        { typeCheckResultAction = fDep
        , pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectDecls
        , installCoreToDos = installInstanceTracker
        }

installInstanceTracker :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installInstanceTracker cli todos = do
    return (CoreDoPluginPass "Instance Usage Tracker" (instanceTrackerPass cli) : todos)

#if __GLASGOW_HASKELL__ >= 900
getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan _) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = showSDocUnsafe $ ppr $ fs
#else
getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs
#endif

instanceTrackerPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
instanceTrackerPass opts guts = do
    let cliOptions = case opts of 
                    [] ->  defaultCliOptions
                    (local : _) -> 
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    let prefixPath = path cliOptions
        allBinds = mg_binds guts
        moduleN = moduleNameString $ moduleName $ mg_module guts
        moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
    liftIO $ sendViaUnixSocket (prefixPath) (T.pack $ moduleLoc Prelude.<> ".function_instance_mapping.json") (decodeUtf8 $ toStrict $ encode $ Map.fromList $ concat $ map (toLBind) allBinds)
    pure guts

toLBind :: CoreBind -> [(String,[(String,String)])]
toLBind (NonRec binder expr) = [(nameStableString $ idName binder,filter (\(name,_) -> "$f" `Data.List.isPrefixOf` name) $ map (\x -> (showSDocUnsafe $ ppr $ varName x,showSDocUnsafe $ ppr $ varType x)) (expr ^? biplateRef :: [Id]))]
toLBind (Rec binds) = map (\(b, e) -> (nameStableString $ idName b,filter (\(name,_) -> "$f" `Data.List.isPrefixOf` name) $ map (\x -> (showSDocUnsafe $ ppr $ varName x,showSDocUnsafe $ ppr $ varType x)) (e ^? biplateRef :: [Id])) ) binds


collectDecls :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectDecls opts modSummary hsParsedModule = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) -> 
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    _ <- liftIO $ do
            let prefixPath = path cliOptions
                modulePath = prefixPath <> msHsFilePath modSummary
            let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                declsList = hsmodDecls $ unLoc $ hpm_module hsParsedModule
            -- createDirectoryIfMissing True path
            (functionsVsCodeString,typesCodeString,classCodeString,instanceCodeString) <- processDecls declsList
            let importsList = concatMap (fromGHCImportDecl) (hsmodImports $ unLoc $ hpm_module hsParsedModule)
            sendViaUnixSocket (Fdep.Types.path cliOptions) (T.pack $ modulePath <> ".module_imports.json") (decodeUtf8 $ toStrict $ A.encode $ importsList)
            sendViaUnixSocket (Fdep.Types.path cliOptions) (T.pack $ modulePath <> ".function_code.json") (decodeUtf8 $ toStrict $ A.encode $ Map.fromList functionsVsCodeString)
            sendViaUnixSocket (Fdep.Types.path cliOptions) (T.pack $ modulePath <> ".types_code.json") (decodeUtf8 $ toStrict $ A.encode $ typesCodeString)
            sendViaUnixSocket (Fdep.Types.path cliOptions) (T.pack $ modulePath <> ".class_code.json") (decodeUtf8 $ toStrict $ A.encode $ classCodeString)
            sendViaUnixSocket (Fdep.Types.path cliOptions) (T.pack $ modulePath <> ".instance_code.json") (decodeUtf8 $ toStrict $ A.encode $ instanceCodeString)
            -- writeFile (modulePath <> ".module_imports.json") (encodePretty $ importsList)
            -- writeFile (modulePath <> ".function_code.json") (encodePretty $ Map.fromList functionsVsCodeString)
            -- writeFile (modulePath <> ".types_code.json") (encodePretty $ typesCodeString)
            -- writeFile (modulePath <> ".class_code.json") (encodePretty $ classCodeString)
            -- writeFile (modulePath <> ".instance_code.json") (encodePretty $ instanceCodeString)
    pure hsParsedModule

fromGHCImportDecl :: LImportDecl GhcPs -> [SimpleImportDecl]
fromGHCImportDecl (L _span ImportDecl{..}) = [SimpleImportDecl {
    moduleName' = moduleNameToText (unLoc ideclName),
    packageName = fmap stringLiteralToText ideclPkgQual,
#if __GLASGOW_HASKELL__ >= 900
    isBootSource = case ideclSource of
            IsBoot -> True
            NotBoot -> False,
#else
    isBootSource = ideclSource,
#endif
    isSafe = ideclSafe,
    qualifiedStyle = convertQualifiedStyle ideclQualified,
    isImplicit = ideclImplicit,
    asModuleName = fmap (moduleNameToText . unLoc) ideclAs,
    hidingSpec = case ideclHiding of
        Nothing -> Nothing
        Just (isHiding, names) -> Just $ HidingSpec {
            isHiding = isHiding,
            names = convertLIEsToText names
        },
    line_number = spanToLine _span
}]
fromGHCImportDecl (L span (XImportDecl _)) = []

moduleNameToText :: ModuleName -> T.Text
moduleNameToText = T.pack . moduleNameString

stringLiteralToText :: StringLiteral -> T.Text
stringLiteralToText StringLiteral {sl_st} =
    case sl_st  of
        SourceText s -> T.pack s
        _ -> T.pack "NoSourceText"

convertQualifiedStyle :: ImportDeclQualifiedStyle -> QualifiedStyle
convertQualifiedStyle GHC.NotQualified     = Fdep.Types.NotQualified
convertQualifiedStyle QualifiedPre     = Fdep.Types.Qualified
convertQualifiedStyle QualifiedPost    = Fdep.Types.Qualified

-- (GenLocated (Anno [GenLocated l (IE GhcPs)]) [GenLocated l (IE GhcPs)])
convertLIEsToText :: _ -> [T.Text]
convertLIEsToText lies = 
#if __GLASGOW_HASKELL__ >= 900
    concatMap (ieNameToText . unLoc) (unXRec @(GhcPs) lies)
#else
    concatMap (ieNameToText . unLoc) (unLoc lies)
#endif
  where
    ieNameToText :: IE GhcPs -> [T.Text]
    ieNameToText x = map rdrNameToText $ ieNames x

    rdrNameToText = T.pack . occNameString . rdrNameOcc

processDecls :: [LHsDecl GhcPs] -> IO ([(Text, PFunction)], [PType], [PClass], [PInstance])
processDecls decls = do
    results <- mapM getDecls' decls
    pure ( concatMap (\(f,_,_,_) -> f) results
         , concatMap (\(_,t,_,_) -> t) results
         , concatMap (\(_,_,c,_) -> c) results
         , concatMap (\(_,_,_,i) -> i) results
         )

#if __GLASGOW_HASKELL__ >= 900
spanToLine :: _ -> (Int,Int)
spanToLine s = (srcSpanStartLine $ la2r s,srcSpanEndLine $ la2r s)
#else
spanToLine :: SrcSpan -> (Int,Int)
spanToLine (UnhelpfulSpan _) = (-1,-1)
spanToLine (RealSrcSpan s) = (srcSpanStartLine s,srcSpanEndLine s)
-- srcLocSpan :: SrcLoc -> SrcSpan
-- srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
-- srcLocSpan (RealSrcLoc l) = RealSrcSpan (realSrcLocSpan l)
#endif


-- Modified function to extract all declarations
getDecls' :: LHsDecl GhcPs -> IO ([(Text, PFunction)], [PType], [PClass], [PInstance])
getDecls' x = case x of
    (L span (TyClD _ decl)) -> pure (mempty, getTypeDecl span decl, getClassDecl span decl, mempty)
    (L span (InstD _ inst)) -> pure (mempty, mempty, mempty, getInstDecl span inst)
    (L span (DerivD _ _)) -> pure mempty4
    (L span (ValD _ bind)) -> pure (getFunBind span bind, mempty, mempty, mempty)
    (L span (SigD _ _)) -> pure mempty4
    _ -> pure mempty4
  where
    mempty4 = (mempty, mempty, mempty, mempty)

    -- Extract function bindings (original code)
    getFunBind _span f@FunBind{fun_id = funId} = 
        [( T.pack (showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> T.pack (getLoc' funId)
         , PFunction 
             (T.pack (showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> T.pack (getLoc' funId))
             (T.pack $ showSDocUnsafe $ ppr f)
             (T.pack $ getLoc' funId)
             (spanToLine _span)
         )]
    getFunBind _ _ = mempty

    -- Extract type and newtype declarations
    getTypeDecl :: _ -> TyClDecl GhcPs -> [PType]
    getTypeDecl _span decl@DataDecl{tcdLName = L l name} =
        [PType 
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) $ l))
#endif
            (spanToLine _span)
        ]
    getTypeDecl _span decl@SynDecl{tcdLName = L l name} =
        [PType
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) $ l))
#endif
            (spanToLine _span)
        ]
    getTypeDecl _ _ = mempty

    -- Extract class declarations
    getClassDecl :: _ -> TyClDecl GhcPs -> [PClass]
    getClassDecl _span decl@ClassDecl{tcdLName = L l name} =
        [PClass
            (T.pack $ showSDocUnsafe $ ppr name)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA l))
#else
            (T.pack ((showSDocUnsafe . ppr) l))
#endif
            (spanToLine _span)
        ]
    getClassDecl _ _ = mempty

    -- Extract instance declarations
    getInstDecl :: _ -> InstDecl GhcPs -> [PInstance]
    getInstDecl _span decl@(ClsInstD _ ClsInstDecl{cid_poly_ty = ty}) =
        [PInstance
            (T.pack $ showSDocUnsafe $ ppr ty)
            (T.pack $ showSDocUnsafe $ ppr decl)
#if __GLASGOW_HASKELL__ >= 900
            (T.pack ((showSDocUnsafe . ppr) $ locA _span))
#else
            (T.pack ((showSDocUnsafe . ppr) _span))
#endif
            (spanToLine _span)
        ]
    getInstDecl _ _ = mempty

shouldForkPerFile :: Bool
shouldForkPerFile = readBool $ unsafePerformIO $ lookupEnv "SHOULD_FORK"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldGenerateFdep :: Bool
shouldGenerateFdep = readBool $ unsafePerformIO $ lookupEnv "GENERATE_FDEP"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True   
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldLog :: Bool
shouldLog = readBool $ unsafePerformIO $ lookupEnv "ENABLE_LOGS"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool _ = False

-- default options
-- "{\"path\":\"/tmp/fdep/\",\"port\":9898,\"host\":\"localhost\",\"log\":true}"
defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {
    path="./tmp/fdep/",
    port=4444,
    host="::1",
    log=False,
    tc_funcs=Just False
}

filterList :: [Text]
filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
    , "showList"
    , "toConstr"
    , "toDomResAcc"
    , "toEncoding"
    , "toEncodingList"
    , "toEnum"
    , "toForm"
    , "toHaskellString"
    , "toInt"
    , "toJSON"
    , "toJSONList"
    , "toJSONWithOptions"
    , "encodeJSON"
    , "gfoldl"
    , "ghmParser"
    , "gmapM"
    , "gmapMo"
    , "gmapMp"
    , "gmapQ"
    , "gmapQi"
    , "gmapQl"
    , "gmapQr"
    , "gmapT"
    , "parseField"
    , "parseJSON"
    , "parseJSONList"
    , "parseJSONWithOptions"
    , "hasField"
    , "gunfold"
    , "getField"
    , "_mapObjectDeep'"
    , "_mapObjectDeep"
    , "_mapObjectDeepForSnakeCase"
    , "!!"
    , "/="
    , "<"
    , "<="
    , "<>"
    , "<$"
    , "=="
    , ">"
    , ">="
    , "readsPrec"
    , "readPrec"
    , "toDyn"
    , "fromDyn"
    , "fromDynamic"
    , "compare"
    , "readListPrec"
    , "toXml"
    , "fromXml"
    ]

{-# NOINLINE globalCompletionState #-}
globalCompletionState :: IORef (Set.Set String, Int)
globalCompletionState = unsafePerformIO $ newIORef (Set.empty, 0)

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    let cliOptions = case opts of
                    [] ->  defaultCliOptions
                    (local : _) ->
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (val :: CliOptions) -> val
                                    Nothing -> defaultCliOptions
    hscEnv <- getTopEnv
    let moduleGraph = hsc_mod_graph hscEnv
        modSummaries = mgModSummaries moduleGraph
        modNames = map (moduleNameString . ms_mod_name) modSummaries
        sortedModules = map (moduleNameString . ms_mod_name) (flattenSCCs $ topSortModuleGraph True moduleGraph Nothing)
        totalModules = length sortedModules
        currentModuleName = moduleNameString $ moduleName $ ms_mod modSummary
        isLastModule = case lastMaybe sortedModules of
            Just lastMod -> lastMod == currentModuleName
            Nothing -> False
    
    when (shouldGenerateFdep) $ do
        liftIO $ atomicModifyIORef' globalCompletionState $ \(completed, total) ->
            if total == 0 then ((completed, totalModules), ()) else ((completed, total), ())
        
        if isLastModule then
            liftIO $ do
                processModule cliOptions modSummary tcEnv
                atomicModifyIORef' globalCompletionState $ \(completed, total) ->
                    ((Set.insert currentModuleName completed, total), ())
                waitForAllModules totalModules
                writeIORef globalCompletionState (Set.empty, 0)
        else
            liftIO $ (bool P.id (void . forkIO) shouldForkPerFile) $ do
                processModule cliOptions modSummary tcEnv
                atomicModifyIORef' globalCompletionState $ \(completed, total) ->
                    ((Set.insert currentModuleName completed, total), ())
    
    return tcEnv

waitForAllModules :: Int -> IO ()
waitForAllModules expectedTotal = do
    (completed, _) <- readIORef globalCompletionState
    let completedCount = Set.size completed
    if completedCount >= expectedTotal
        then do
            print $ "All " <> show expectedTotal <> " modules completed!"
            return ()
        else do
            threadDelay 1000000  -- Wait 1000ms
            waitForAllModules expectedTotal

processModule :: CliOptions -> ModSummary -> TcGblEnv -> IO ()
processModule cliOptions modSummary tcEnv = do
    let prefixPath = path cliOptions
        moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> msHsFilePath modSummary
        wsPath = modulePath <> ".json"
        pathStr = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
    
    when (shouldLog || Fdep.Types.log cliOptions) $ 
        print ("generating dependancy for module: " <> moduleName' <> " at path: " <> pathStr)
    
    t1 <- getCurrentTime
    let socketPathToUse = fromMaybe (path cliOptions) fdepSocketPath
    sendPathPerformAction wsPath socketPathToUse (\sock -> 
        void $ mapM (loopOverLHsBindLR cliOptions sock Nothing (T.pack wsPath)) (bagToList $ tcg_binds tcEnv))
    t2 <- getCurrentTime
    
    when (shouldLog || Fdep.Types.log cliOptions) $ 
        print ("generated dependancy for module: " <> moduleName' <> " at path: " <> pathStr <> " total-timetaken: " <> show (diffUTCTime t2 t1))

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe FunctionInfo
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) loc args
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
    in Just $ if length parts == 2 then FunctionInfo "" (parts !! 0) (parts !! 1) (fromMaybe "<unknown>" _type) "<no location info>" args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) (fromMaybe "<unknown>" _type) "<no location info>" args
transformFromNameStableString (_,_,_,_) = Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tail' [] = []
tail' [x] = []
tail' (x:xs) = xs

maybeBool (Just v) = v
maybeBool _ = False

processAndSendTypeDetails :: CliOptions -> _ -> Text -> Text -> [(Type)] -> IO ()
processAndSendTypeDetails cliOptions con path keyFunction typesUsed =
    let details = concat $ map getTypeDetails typesUsed
        functionInfoList = nub $ map (\(name,_type) -> transformFromNameStableString ((Just $ T.pack $ name) ,Nothing ,(Just $ T.pack $ show $ toConstr _type),[])) details
    in void $ mapM (\expr -> sendTextData' cliOptions con path (transformPayload path keyFunction EXPR (toJSON expr))) functionInfoList

getTypeDetails :: Type -> [(String,Type)]
getTypeDetails ty = map (\x -> (nameStableString $ tyConName x,ty)) $ tyConsOfType ty

tyConsOfType :: Type -> [(TyCon)]
tyConsOfType ty = case ty of
    TyConApp tc tys -> tc : concatMap tyConsOfType tys
    AppTy t1 t2     -> tyConsOfType t1 ++ tyConsOfType t2
#if __GLASGOW_HASKELL__ >= 900
    FunTy _ _ t1 t2 -> tyConsOfType t1 ++ tyConsOfType t2
#else
    FunTy _ t1 t2 -> tyConsOfType t1 ++ tyConsOfType t2
#endif
    ForAllTy _ t    -> tyConsOfType t
    CastTy t _      -> tyConsOfType t
    CoercionTy _    -> []
    LitTy _         -> []
    TyVarTy _       -> []


processFunctionInputOutput :: Type -> CliOptions -> _ -> Text -> Text -> IO ()
processFunctionInputOutput type_ cliOptions con _path nestedNameWithParent = do
    let info = toJSON $ HM.fromList $ extractDetailsFromBind (type_)
    data_ <- pure (transformPayload _path nestedNameWithParent FUNCTION_IO info)
    sendTextData' cliOptions con _path data_

#if __GLASGOW_HASKELL__ >= 900
scaledThing' = scaledThing
extractDetailsFromBind :: Type -> [(HM.Key,A.Value)]
#else
scaledThing' a = a
extractDetailsFromBind :: Type -> [(Text,A.Value)]
#endif
extractDetailsFromBind ty =
    let -- Split the type into quantified variables, constraints, and the core function type
        (tyVars, constraints, coreTy) = tcSplitSigmaTy ty
        -- Process each type
        processType ty' =
            let (headTy, argTys) = splitAppTys ty'
            in (showSDocUnsafe $ ppr headTy, map (showSDocUnsafe . ppr) argTys)
        -- Process arguments and result
        (argTys', resTy) = splitFunTys coreTy
        processedArgs = map (processType . scaledThing') argTys'
        processedRes = processType resTy
        -- Convert quantified variables and constraints to strings
        tyVarStrs = map (showSDocUnsafe . ppr) tyVars
        constraintStrs = map (showSDocUnsafe . ppr) constraints
        -- Combine the type's metadata into a human-readable format
        quantifiedPart = if null tyVarStrs then "" else "forall " ++ unwords tyVarStrs ++ ". "
        constraintPart = if null constraintStrs then "" else Data.List.intercalate ", " constraintStrs ++ " => "
    in [("inputs",toJSON processedArgs), ("outputs",toJSON (quantifiedPart ++ constraintPart ++ fst processedRes, snd processedRes))]

data PayloadType = TYPE_SIGNATURE | EXPR | FUNCTION_IO

instance Show PayloadType where
  show TYPE_SIGNATURE = "typeSignature"
  show EXPR           = "expr"
  show FUNCTION_IO    = "functionIO"

loopOverLHsBindLR :: CliOptions -> _ -> (Maybe Text) -> Text -> LHsBindLR GhcTc GhcTc -> IO ()
loopOverLHsBindLR cliOptions con mParentName path (L _ AbsBinds{abs_binds = binds}) =
    void $ mapM (loopOverLHsBindLR cliOptions con mParentName path) $ bagToList binds
loopOverLHsBindLR cliOptions con mParentName _path (L location bind) = do
    let typesUsed = (map varType $ (bind ^? biplateRef :: [Var])) <> (map idType $ (bind ^? biplateRef :: [Id])) <> (bind ^? biplateRef :: [Type])
    case bind of
#if __GLASGOW_HASKELL__ >= 900
        (FunBind _ id matches _) -> do
#else
        (FunBind _ id matches _ _) -> do
#endif
            funName <- pure $ T.pack $ getOccString $ unLoc id
            fName <- pure $ T.pack $ nameStableString $ getName id
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack (getLoc' id)))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr . getLoc) id)))
#endif
            let matchList = mg_alts matches
            if funName `elem` (filterList)
                then pure mempty
                else
                    if not $ (maybeBool $ tc_funcs cliOptions)
                        then
                            when (not $ "$$" `T.isInfixOf` name) $ do
                                when (shouldLog || Fdep.Types.log cliOptions) $ print ("processing function: " <> fName)
                                typeSignature <- pure $ toJSON $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
                                nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
                                data_ <- pure $ transformPayload _path nestedNameWithParent TYPE_SIGNATURE typeSignature
                                t1 <- getCurrentTime
                                processFunctionInputOutput (varType (unLoc id)) cliOptions con _path nestedNameWithParent
                                sendTextData' cliOptions con _path data_
                                processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
                                void $ mapM (\x -> do
                                            eres :: Either SomeException () <- try $ processMatch (nestedNameWithParent) _path x
                                            case eres of
                                                Left err -> do
                                                    when (shouldLog || Fdep.Types.log cliOptions) $ print (err,name)
                                                    pure ()--appendFile "error.log" (show (err,funName) <> "\n")
                                                Right _ -> pure ()
                                        ) (unLoc matchList)
                                t2 <- getCurrentTime
                                when (shouldLog || Fdep.Types.log cliOptions) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
                        else do
                            when (shouldLog || Fdep.Types.log cliOptions) $ print ("processing function: " <> fName)
                            typeSignature <- pure $ toJSON $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
                            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
                            data_ <- pure $ transformPayload _path nestedNameWithParent TYPE_SIGNATURE typeSignature
                            t1 <- getCurrentTime
                            sendTextData' cliOptions con _path data_
                            processFunctionInputOutput (varType (unLoc id)) cliOptions con _path nestedNameWithParent
                            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
                            void $ mapM (\x -> do
                                        eres :: Either SomeException () <- try $ processMatch (nestedNameWithParent) _path x
                                        case eres of
                                            Left err -> do
                                                when (shouldLog || Fdep.Types.log cliOptions) $ print (err,name)
                                                pure ()--appendFile "error.log" (show (err,funName) <> "\n")
                                            Right _ -> pure ()
                                    ) (unLoc matchList)
                            t2 <- getCurrentTime
                            when (shouldLog || Fdep.Types.log cliOptions) $ print $ "processed function: " <> fName <> " timetaken: " <> (T.pack $ show $ diffUTCTime t2 t1)
        (VarBind{var_id = var, var_rhs = expr}) -> do
            let stmts = (expr ^? biplateRef :: [LHsExpr GhcTc])
                fName = T.pack $ nameStableString $ getName var 
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) $ locA location)))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) location)))
#endif
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            processFunctionInputOutput (varType (var)) cliOptions con _path nestedNameWithParent
            if (maybeBool $ tc_funcs cliOptions)
                then void $ mapM (processExpr (nestedNameWithParent) _path) (stmts)
                else when (not $ "$$" `T.isInfixOf` name) $
                        void $ mapM (processExpr (nestedNameWithParent) _path) (stmts)
        (PatBind{pat_lhs = pat, pat_rhs = expr,pat_ext=pat_ext}) -> do
            let stmts = (expr ^? biplateRef :: [LHsExpr GhcTc])
                ids = (pat ^? biplateRef :: [LIdP GhcTc])
                fName = (maybe (T.pack "::") (T.pack . nameStableString . getName) $ (headMaybe ids))
#if __GLASGOW_HASKELL__ >= 900
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) $ locA location)))
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            processFunctionInputOutput (pat_ext) cliOptions con _path nestedNameWithParent
            if (maybeBool $ tc_funcs cliOptions)
                then void $ mapM (processExpr nestedNameWithParent _path) (stmts <> map (\v -> wrapXRec @(GhcTc) $ HsVar noExtField v) (tail' ids))
                else when (not $ "$$" `T.isInfixOf` name) $
                        void $ mapM (processExpr nestedNameWithParent _path) (stmts <> map (\v -> wrapXRec @(GhcTc) $ HsVar noExtField v) (tail' ids))
#else
            name <- pure (fName <> "**" <> (T.pack ((showSDocUnsafe . ppr) location)))
            nestedNameWithParent <- pure $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            processAndSendTypeDetails cliOptions con _path nestedNameWithParent typesUsed
            if (maybeBool $ tc_funcs cliOptions)
                then void $ mapM (processExpr nestedNameWithParent _path) (stmts <> map (\v -> noLoc $ HsVar noExtField v) (tail' ids))
                else when (not $ "$$" `T.isInfixOf` name) $
                        void $ mapM (processExpr nestedNameWithParent _path) (stmts <> map (\v -> noLoc $ HsVar noExtField v) (tail' ids))
#endif
        _ -> pure ()
    where
        processMatch :: Text -> Text -> LMatch GhcTc (LHsExpr GhcTc) -> IO ()
        processMatch keyFunction path (L _ match) = do
#if __GLASGOW_HASKELL__ >= 900
            processHsLocalBinds keyFunction path $ grhssLocalBinds (m_grhss match)
#else
            processHsLocalBinds keyFunction path $ unLoc $ grhssLocalBinds (m_grhss match)
#endif
            void $ mapM (processGRHS keyFunction path) $ grhssGRHSs (m_grhss match)

        processGRHS :: Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO ()
        processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
        processGRHS _ _ _ = pure mempty

        processHsLocalBinds :: Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
        processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
            void $ mapM (loopOverLHsBindLR cliOptions con (Just keyFunction) path) $ bagToList $ x
        processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
            void $ mapM (\(recFlag, binds) -> void $ mapM (loopOverLHsBindLR cliOptions con (Just keyFunction) path) $ bagToList binds) ( x)
        processHsLocalBinds _ _ _ = pure mempty

        processExpr :: Text -> Text -> LHsExpr GhcTc -> IO ()
        processExpr keyFunction path x@(L _ (HsVar _ (L _ var))) = do
            let name = T.pack $ nameStableString $ varName var
                _type = T.pack $ showSDocUnsafe $ ppr $ varType var
            expr <- pure $ toJSON $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ x, Just _type, mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr _ _ (L _ (HsUnboundVar _ _)) = pure mempty
        processExpr keyFunction path (L _ (HsApp _ funl funr)) = do
            processExpr keyFunction path funl
            processExpr keyFunction path funr
        processExpr keyFunction path (L _ (OpApp _ funl funm funr)) = do
            processExpr keyFunction path funl
            processExpr keyFunction path funm
            processExpr keyFunction path funr
        processExpr keyFunction path (L _ (NegApp _ funl _)) =
            processExpr keyFunction path funl
        processExpr keyFunction path (L _ (HsTick _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsStatic _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsBinTick _ _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (ExprWithTySig _ fun _)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsLet _ exprLStmt func)) = do
#if __GLASGOW_HASKELL__ >= 900
            processHsLocalBinds keyFunction path exprLStmt
#else
            processHsLocalBinds keyFunction path (unLoc exprLStmt)
#endif
            processExpr keyFunction path func
        processExpr keyFunction path (L _ (HsMultiIf _ exprLStmt)) =
            void $ mapM (processGRHS keyFunction path) exprLStmt
        processExpr keyFunction path (L _ (HsCase _ funl exprLStmt)) = do
            processExpr keyFunction path funl
            void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
        processExpr keyFunction path (L _ (ExplicitSum _ _ _ fun)) = processExpr keyFunction path fun
        processExpr keyFunction path (L _ (SectionR _ funl funr)) = processExpr keyFunction path funl <> processExpr keyFunction path funr
        processExpr keyFunction path (L _ (HsPar _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsAppType _ fun _)) = processExpr keyFunction path fun
        processExpr keyFunction path (L _ x@(HsLamCase _ exprLStmt)) =
            void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
        processExpr keyFunction path (L _ x@(HsLam _ exprLStmt)) =
            void $ mapM (processMatch keyFunction path) (unLoc $ mg_alts exprLStmt)
        processExpr keyFunction path y@(L _ x@(HsLit _ hsLit)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr keyFunction path y@(L _ x@(HsOverLit _ overLitVal)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr overLitVal), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        -- processExpr keyFunction path (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
        --     let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        --         stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
        --     in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
        processExpr keyFunction path (L _ (HsSpliceE _ splice)) =
            void $ mapM (processExpr keyFunction path) (extractExprsFromSplice splice)
        processExpr keyFunction path y@(L _ x@(HsConLikeOut _ hsType)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr keyFunction path y@(L _ x@(HsIPVar _ implicit)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_implicit$" <> T.pack (showSDocUnsafe $ ppr implicit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr keyFunction path (L _ (SectionL _ funl funr)) = do
            processExpr keyFunction path funl
            processExpr keyFunction path funr
#if __GLASGOW_HASKELL__ > 900
        processExpr keyFunction path (L _ (HsDo _ smtContext ((L _ stmt)))) =
            void $ mapM (\(L _ x ) -> extractExprsFromStmtLRHsExpr keyFunction path x) $ stmt
        processExpr keyFunction path (L _ (HsGetField _ exprLStmt _)) =
            processExpr keyFunction path exprLStmt
        processExpr keyFunction path (L _ (ExplicitList _ funList)) =
            void $ mapM (processExpr keyFunction path) funList
        processExpr keyFunction path (L _ (HsPragE _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsProc _ lPat fun)) = do
            extractExprsFromPat keyFunction path lPat
            (extractExprsFromLHsCmdTop keyFunction path fun)
        processExpr keyFunction path (L _ (HsIf _ funl funm funr)) =
            void $ mapM (processExpr keyFunction path) $ [funl, funm, funr]
        processExpr keyFunction path (L _ (ArithSeq hsexpr exprLStmtL exprLStmtR)) = do
            processExpr keyFunction path $ wrapXRec @(GhcTc) hsexpr
            case exprLStmtL of
                Just epr -> processExpr keyFunction path $ wrapXRec @GhcTc $ syn_expr epr
                Nothing -> pure ()
            case exprLStmtR of
                From l -> processExpr keyFunction path l
                FromThen l r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path r
                FromTo l r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path r
                FromThenTo l m r -> do
                    processExpr keyFunction path l
                    processExpr keyFunction path m
                    processExpr keyFunction path r
        processExpr keyFunction path (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
            let stmtsLNoLoc = (exprLStmtL ^? biplateRef :: [HsExpr GhcTc])
                stmtsRNoLoc = (exprLStmtR ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (map (wrapXRec @(GhcTc)) $ (stmtsLNoLoc <> stmtsRNoLoc))
        processExpr keyFunction path x@(L _ (HsRecFld _ exprLStmt)) = getDataTypeDetails keyFunction path x
        processExpr keyFunction path y@(L _ x@(RecordCon expr (L _ (iD)) rcon_flds)) = getDataTypeDetails keyFunction path y
        processExpr keyFunction path x@(L _ (RecordUpd _ rupd_expr rupd_flds)) = getDataTypeDetails keyFunction path x
        processExpr keyFunction path (L _ (ExplicitTuple _ exprLStmt _)) =
            let l = (exprLStmt)
            in void $ mapM (\x ->
                    case x of
                        (Present _ exprs) -> processExpr keyFunction path exprs
                        _ -> pure ()) l
        processExpr keyFunction path y@(L _ (XExpr overLitVal)) = do
            processXXExpr keyFunction path overLitVal
        processExpr keyFunction path y@(L _ x@(HsOverLabel _ fs)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr keyFunction path (L _ (HsTcBracketOut b mQW exprLStmtL exprLStmtR)) =
            let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
                stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (stmtsL <> stmtsR)
        processExpr keyFunction path (L _ x) =
            let stmts = (x ^? biplateRef :: [LHsExpr GhcTc])
                stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) ( (stmts <> (map (wrapXRec @(GhcTc)) stmtsNoLoc)))
#else
        processExpr keyFunction path (L _ (ExplicitTuple _ exprLStmt _)) =
            let l = (unLoc <$> exprLStmt)
            in void $ mapM (\x ->
                    case x of
                        (Present _ exprs) -> processExpr keyFunction path exprs
                        _ -> pure ()) l
        processExpr keyFunction path (L _ (ExplicitList _ _ funList)) =
            void $ mapM (processExpr keyFunction path) ( funList)
        processExpr keyFunction path (L _ (HsTickPragma _ _ _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsSCC _ _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ (HsCoreAnn _ _ _ fun)) =
            processExpr keyFunction path fun
        processExpr keyFunction path (L _ x@(HsWrap _ _ fun)) =
            processExpr keyFunction path (noLoc fun)
        processExpr keyFunction path (L _ (HsIf _ exprLStmt funl funm funr)) =
            void $ mapM (processExpr keyFunction path) $ [funl, funm, funr]
        processExpr keyFunction path (L _ (HsTcBracketOut _ bracket pending)) = do
            -- processBracket keyFunction path bracket
            void $ mapM (processPendingSplice keyFunction path) pending
        processExpr keyFunction path (L _ (ArithSeq _ Nothing arithSeqInfo)) =
            processArithSeqInfo keyFunction path arithSeqInfo
        processExpr keyFunction path x@(L _ (HsRecFld _ exprLStmt)) =
            getDataTypeDetails keyFunction path x
        processExpr keyFunction path (L _ (HsRnBracketOut _ bracket pending)) = pure ()
            -- processBracket keyFunction path bracket
            -- void $ mapM (processExpr keyFunction path) pending
        processExpr keyFunction path y@(L _ x@(RecordCon _ (L _ (iD)) rcon_flds)) = 
            getDataTypeDetails keyFunction path y
        processExpr keyFunction path x@(L _ (RecordUpd _ rupd_expr rupd_flds)) = 
            getDataTypeDetails keyFunction path x
        processExpr keyFunction path y@(L _ (XExpr _)) =
            let stmts = (y ^? biplateRef :: [HsExpr GhcTc])
            in void $ mapM (processExpr keyFunction path) (noLoc <$> stmts)
        processExpr keyFunction path y@(L _ x@(HsOverLabel _ mIdp fs)) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_overLabel$" <> (T.pack $ showSDocUnsafe $ ppr fs)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processExpr keyFunction path (L _ (HsDo _ smtContext ((L _ stmt)))) =
            void $ mapM (\(L _ x ) -> extractExprsFromStmtLRHsExpr keyFunction path x) $ stmt
        processExpr keyFunction path (L _ x) = do
            print $ ("UNHANDLED PATTERN",toConstr x)
            let stmtsNoLoc = (x ^? biplateRef :: [HsExpr GhcTc])
            void $ mapM (processExpr keyFunction path) ((map (noLoc) stmtsNoLoc))

        processArithSeqInfo :: Text -> Text -> ArithSeqInfo GhcTc -> IO ()
        processArithSeqInfo keyFunction path arithInfo = case arithInfo of
            From expr -> processExpr keyFunction path expr
            FromThen expr1 expr2 -> do
                processExpr keyFunction path expr1
                processExpr keyFunction path expr2
            FromTo expr1 expr2 -> do
                processExpr keyFunction path expr1
                processExpr keyFunction path expr2
            FromThenTo expr1 expr2 expr3 -> do
                processExpr keyFunction path expr1
                processExpr keyFunction path expr2
                processExpr keyFunction path expr3

        processPendingSplice :: Text -> Text -> PendingTcSplice -> IO ()
        processPendingSplice keyFunction path (PendingTcSplice _ expr) = 
            processExpr keyFunction path expr

        processBracket :: Text -> Text -> HsBracket GhcTc -> IO ()
        processBracket keyFunction path bracket = case bracket of
            ExpBr _ expr -> processExpr keyFunction path expr
            PatBr _ pat -> extractExprsFromPat keyFunction path pat
            DecBrL _ decls -> void $ mapM (processDecl keyFunction path) $ decls
            -- DecBrG _ decls -> void $ mapM (processDecl keyFunction path) $ decls  
            TypBr _ _ -> pure ()
            VarBr _ _ _ -> pure ()
            TExpBr _ expr -> processExpr keyFunction path expr
            _ -> pure ()

        processDecl :: Text -> Text -> LHsDecl GhcTc -> IO ()
        processDecl keyFunction path (L _ decl) = case decl of
            ValD _ b -> processHsBind keyFunction path b
            TyClD _ tyClDecl -> processTyClDecl keyFunction path tyClDecl
            InstD _ instDecl -> processInstDecl keyFunction path instDecl
            ForD _ forDecl -> processForDecl keyFunction path forDecl
            SigD _ sig -> processSig keyFunction path sig
            _ -> pure ()

        -- Process bindings
        processHsBind :: Text -> Text -> HsBindLR GhcTc GhcTc -> IO ()
        processHsBind keyFunction path b = loopOverLHsBindLR cliOptions con (Just keyFunction) path (noLoc b)

        -- Process type class declarations
        processTyClDecl :: Text -> Text -> TyClDecl GhcTc -> IO ()
        processTyClDecl keyFunction path tyClDecl = case tyClDecl of
            DataDecl{tcdDataDefn = defn} -> processDataDefn keyFunction path defn
            ClassDecl{tcdSigs = sigs, tcdMeths = meths} -> do
                void $ mapM (processSig keyFunction path . unLoc) sigs
                void $ mapM (processHsBind keyFunction path . unLoc) (bagToList meths)
            _ -> pure ()

        -- Process data definitions
        processDataDefn :: Text -> Text -> HsDataDefn GhcTc -> IO ()
        processDataDefn keyFunction path (HsDataDefn{dd_cons = cons}) =
            void $ mapM (processConDecl keyFunction path) cons
        processDataDefn keyFunction path _ = pure ()

        -- Process constructor declarations
        processConDecl :: Text -> Text -> LConDecl GhcTc -> IO ()
        processConDecl keyFunction path (L _ conDecl) = case conDecl of
            ConDeclGADT{con_args = args} -> processConDeclDetails keyFunction path args
            ConDeclH98{con_args = args} -> processConDeclDetails keyFunction path args
            _ -> pure ()

        processConDeclDetails :: Text -> Text -> HsConDeclDetails GhcTc -> IO ()
        processConDeclDetails keyFunction path details = case details of
            PrefixCon args -> void $ mapM (processLHsType keyFunction path) args
            RecCon (L _ recordFields) -> void $ mapM (processConDeclField keyFunction path) recordFields
            InfixCon arg1 arg2 -> do
                processLHsType keyFunction path arg1
                processLHsType keyFunction path arg2

        processConDeclField :: Text -> Text -> LConDeclField GhcTc -> IO ()
        processConDeclField keyFunction path (L _ (ConDeclField _ _ fieldType _)) =
            processLHsType keyFunction path fieldType
        processConDeclField keyFunction path (L _ (XConDeclField {})) = pure ()

        -- Process types
        processLHsType :: Text -> Text -> LHsType GhcTc -> IO ()
        processLHsType keyFunction path (L _ hsType) = 
            processType keyFunction path hsType

        processType :: Text -> Text -> HsType GhcTc -> IO ()
        processType keyFunction path hsType = case hsType of
            HsAppTy _ t1 t2 -> do
                processLHsType keyFunction path t1
                processLHsType keyFunction path t2
            HsFunTy _ t1 t2 -> do
                processLHsType keyFunction path t1
                processLHsType keyFunction path t2
            HsListTy _ t -> processLHsType keyFunction path t
            HsTupleTy _ _ ts -> void $ mapM (processLHsType keyFunction path) ts
            _ -> pure ()

        -- Process instance declarations
        processInstDecl :: Text -> Text -> InstDecl GhcTc -> IO ()
        processInstDecl keyFunction path instDecl = case instDecl of
            ClsInstD _ clsInst -> processClsInstDecl keyFunction path clsInst
            _ -> pure ()

        processClsInstDecl :: Text -> Text -> ClsInstDecl GhcTc -> IO ()
        processClsInstDecl keyFunction path (ClsInstDecl{cid_binds = binds}) =
            void $ mapM (processHsBind keyFunction path . unLoc) (bagToList binds)
        processClsInstDecl keyFunction path (XClsInstDecl{}) = pure ()

        -- Process foreign declarations
        processForDecl :: Text -> Text -> ForeignDecl GhcTc -> IO ()
        processForDecl keyFunction path _ = pure () -- Usually no expressions in foreign decls

        -- Process signatures
        processSig :: Text -> Text -> Sig GhcTc -> IO ()
        processSig keyFunction path sig = case sig of
            TypeSig _ _ hsType -> processLHsSigWcType keyFunction path hsType
            _ -> pure ()

        processLHsSigWcType :: Text -> Text -> LHsSigWcType GhcTc -> IO ()
        processLHsSigWcType keyFunction path (HsWC _ (HsIB _ hsType)) =
            processLHsType keyFunction path hsType
        processLHsSigWcType keyFunction path _ = pure ()

        extractExprsFromLHsCmdTop :: Text -> Text -> LHsCmdTop GhcTc -> IO ()
        extractExprsFromLHsCmdTop keyFunction path (L _ cmdTop) = 
            case cmdTop of
                HsCmdTop _ cmd -> extractExprsFromLHsCmd keyFunction path cmd
                _ -> pure ()

        extractExprsFromLHsCmd :: Text -> Text ->  LHsCmd GhcTc -> IO ()
        extractExprsFromLHsCmd keyFunction path (L _ cmd) = extractExprsFromHsCmd keyFunction path cmd

        extractExprsFromCmdLStmt :: Text -> Text -> CmdLStmt GhcTc -> IO ()
        extractExprsFromCmdLStmt keyFunction path (L _ stmt) = extractExprsFromStmtLR keyFunction path stmt

        extractExprsFromMatchGroup :: Text -> Text -> MatchGroup GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatchGroup keyFunction path (MG _ (L _ matches) _) = void $ mapM (extractExprsFromMatch keyFunction path) matches
        extractExprsFromMatchGroup keyFunction path (_) = pure ()

        extractExprsFromMatch :: Text -> Text ->  LMatch GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatch keyFunction path (L _ (Match _ _ _ grhs)) = extractExprsFromGRHSs keyFunction path grhs
        extractExprsFromMatch keyFunction path _ = pure ()

        extractExprsFromGRHSs :: Text -> Text ->  GRHSs GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHSs keyFunction path (GRHSs _ grhss _) = void $ mapM (extractExprsFromGRHS keyFunction path)  grhss
        extractExprsFromGRHSs keyFunction path _ = pure ()

        extractExprsFromGRHS :: Text -> Text ->  LGRHS GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHS keyFunction path (L _ (GRHS _ _ body)) = extractExprsFromLHsCmd keyFunction path body
        extractExprsFromGRHS keyFunction path _ = pure ()

        extractExprsFromStmtLR :: Text -> Text -> StmtLR GhcTc GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromStmtLR keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body l r -> do
                processSynExpr keyFunction path l
                processSynExpr keyFunction path r
                extractExprsFromPat keyFunction path pat
                extractExprsFromLHsCmd keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                void $ mapM (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path $ unLoc binds
            ParStmt _ blocks _ bindOp -> do
                void $ mapM (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (trS_stmts)
                processExpr keyFunction path trS_using
                void $ mapM (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (noLoc trS_fmap)
            RecStmt{..} -> do
                void $ mapM (extractExprsFromStmtLR keyFunction path . unLoc) (recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            _ -> pure ()

        extractExprsFromParStmtBlock :: Text -> Text -> ParStmtBlock GhcTc GhcTc -> IO ()
        extractExprsFromParStmtBlock keyFunction path (ParStmtBlock _ stmts _ _) =
            void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) stmts
        extractExprsFromParStmtBlock keyFunction path (_) = pure ()

        processSynExpr :: Text -> Text -> SyntaxExpr GhcTc -> IO ()
        processSynExpr keyFunction path synExpr = processExpr keyFunction path (noLoc $ syn_expr synExpr)

        extractExprsFromStmtLRHsExpr :: Text -> Text -> StmtLR GhcTc GhcTc (LHsExpr GhcTc) -> IO ()
        extractExprsFromStmtLRHsExpr keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body l r -> do
                processSynExpr keyFunction path l
                processSynExpr keyFunction path r
                extractExprsFromPat keyFunction path pat
                processExpr keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                void $ mapM (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path $ unLoc binds
            ParStmt _ blocks _ bindOp -> do
                void $ mapM (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) trS_stmts
                processExpr keyFunction path trS_using
                void $ mapM (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (noLoc trS_fmap)
            RecStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            _ -> pure ()

        extractExprsFromHsCmd :: Text -> Text -> HsCmd GhcTc -> IO ()
        extractExprsFromHsCmd keyFunction path cmd = case cmd of
            HsCmdArrApp _ f arg _ _ ->
                void $ mapM (processExpr keyFunction path) [f, arg]
            HsCmdArrForm _ e _ _ cmdTops -> do
                void $ mapM (extractExprsFromLHsCmdTop keyFunction path) cmdTops
                processExpr keyFunction path e
            HsCmdApp _ cmd' e -> do
                extractExprsFromLHsCmd keyFunction path cmd'
                processExpr keyFunction path e
            HsCmdLam _ mg -> extractExprsFromMatchGroup keyFunction path mg
            HsCmdPar _ cmd' ->
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdCase _ e mg -> do
                extractExprsFromMatchGroup keyFunction path mg
                processExpr keyFunction path e
            -- HsCmdLamCase _ mg ->
            --     extractExprsFromMatchGroup keyFunction path mg
            HsCmdIf _ mSyntaxEcpr predExpr thenCmd elseCmd -> do
                when (isJust mSyntaxEcpr) (processSynExpr keyFunction path (fromJust mSyntaxEcpr)) 
                extractExprsFromLHsCmd keyFunction path elseCmd
                extractExprsFromLHsCmd keyFunction path thenCmd
                processExpr keyFunction path predExpr
            HsCmdLet _ binds cmd' -> do
                processHsLocalBinds keyFunction path $ unLoc binds
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdDo _ stmts ->
                void $ mapM (extractExprsFromCmdLStmt keyFunction path )(unLoc stmts)
            _ -> pure ()

        extractExprsFromPat :: Text -> Text -> LPat GhcTc -> IO ()
        extractExprsFromPat keyFunction path y@(L _ pat) =
            case pat of
                WildPat hsType     -> do
                    expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                VarPat _ var    -> processExpr keyFunction path (noLoc (HsVar noExtField var))
                LazyPat _ p   -> (extractExprsFromPat keyFunction path) p
                AsPat _ var p   -> do
                    processExpr keyFunction path (noLoc (HsVar noExtField var))
                    (extractExprsFromPat keyFunction path) p
                ParPat _ p    -> (extractExprsFromPat keyFunction path) p
                BangPat _ p   -> (extractExprsFromPat keyFunction path) p
                ListPat _ ps  -> void $ mapM (extractExprsFromPat keyFunction path) ps
                TuplePat _ ps _ -> void $ mapM (extractExprsFromPat keyFunction path) ps
                SumPat hsTypes p _ _ -> do 
                    void $ mapM (\hsType -> do
                                expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                            ) hsTypes
                    (extractExprsFromPat keyFunction path) p
                ConPatOut {pat_args = args} -> (extractExprsFromHsConPatDetails keyFunction path args)
                ViewPat hsType expr p -> do
                    expr' <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr')
                    processExpr keyFunction path expr
                    (extractExprsFromPat keyFunction path) p
                SplicePat _ splice -> void $ mapM (processExpr keyFunction path) $ extractExprsFromSplice splice
                LitPat _ hsLit     -> do
                    expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                NPat _ (L _ overLit) _ _ -> do
                    extractExprsFromOverLit overLit
                NPlusKPat _ _ (L _ overLit) _ _ _ ->
                    extractExprsFromOverLit overLit
                SigPat _ p _   -> (extractExprsFromPat keyFunction path) p
                _ -> pure ()
            where
            extractExprsFromOverLit :: HsOverLit GhcTc -> IO ()
            extractExprsFromOverLit (OverLit _ _ e) = processExpr keyFunction path (noLoc e)
            extractExprsFromOverLit _ = pure ()

            extractExprsFromHsConPatDetails :: Text -> Text -> HsConPatDetails GhcTc -> IO ()
            extractExprsFromHsConPatDetails keyFunction' path' (PrefixCon args) = void $ mapM (extractExprsFromPat keyFunction' path') args
            extractExprsFromHsConPatDetails keyFunction' path' z@(RecCon (HsRecFields {})) =
                void $ mapM (extractExprsFromPat keyFunction' path') $ hsConPatArgs z
            extractExprsFromHsConPatDetails keyFunction' path' (InfixCon p1 p2) = do
                (extractExprsFromPat keyFunction' path') p1
                (extractExprsFromPat keyFunction' path') p2

        extractExprFromApplicativeArg :: Text -> Text -> ApplicativeArg GhcTc -> IO ()
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgOne _ lpat expr _ _) = do 
            processExpr keyFunction path expr
            extractExprsFromPat keyFunction path lpat
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgMany _ exprLStmt _ lpat) = do
            void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path) (map (unLoc) exprLStmt)
            extractExprsFromPat keyFunction path lpat
        extractExprFromApplicativeArg keyFunction path (_) = pure ()

        -- extractExprsFromSplice :: HsSplice GhcTc -> [LHsExpr GhcTc]
        -- extractExprsFromSplice (HsTypedSplice _ _ _ e) = [e]
        -- extractExprsFromSplice (HsUntypedSplice _ _ _ e) = [e]
        -- extractExprsFromSplice (HsQuasiQuote _ _ _ _ _) = []
        -- extractExprsFromSplice (HsSpliced _ _ _) = []
        -- extractExprsFromSplice _ = []

        processXXExpr :: Text -> Text -> XXExpr GhcTc -> IO ()
        processXXExpr keyFunction path xxExpr = 
            -- In GHC 8.10.7, XXExpr GhcTc might be NoExtCon or a specific wrapper type
            -- For now, we'll just skip processing these extension points
            pure ()

#endif
        getDataTypeDetails :: Text -> Text -> LHsExpr GhcTc -> IO ()
#if __GLASGOW_HASKELL__ >= 900 
        getDataTypeDetails keyFunction path (L _ (RecordCon _ (iD) rcon_flds)) = 
            (extractRecordBinds keyFunction path (T.pack $ nameStableString $ getName (GHC.unXRec @(GhcTc) iD)) (rcon_flds))
#else
        getDataTypeDetails keyFunction path (L _ (RecordCon _ (iD) rcon_flds)) = (extractRecordBinds keyFunction path (T.pack $ nameStableString $ getName (GHC.unLoc iD)) (rcon_flds))
#endif
        getDataTypeDetails keyFunction path y@(L _ (RecordUpd x@(RecordUpdTc rupd_cons rupd_in_tys rupd_out_tys rupd_wrap) rupd_expr rupd_flds)) = do
            let names = (x ^? biplateRef :: [DataCon])
                types = (x ^? biplateRef :: [Type])
            void $ mapM (\xx -> do
                let name = T.pack $ nameStableString $ dataConName $ xx
                    _type = T.pack $ showSDocUnsafe $ ppr $ dataConRepType xx
                expr <- pure $ toJSON $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                ) names
            void $ mapM (\xx -> do
                expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr xx)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr xx), mempty)
                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                ) types
            (getFieldUpdates y keyFunction path (T.pack $ showSDocUnsafe $ ppr rupd_expr) rupd_flds)
        getDataTypeDetails keyFunction path y@(L _ (HsRecFld _ (Unambiguous id' lnrdrname))) = do
            let name = T.pack $ nameStableString $ varName id'
                _type = T.pack $ showSDocUnsafe $ ppr $ varType id'
            expr <- pure $ toJSON $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
            -- case reLocN lnrdrname of
            --     (L l rdrname) -> do
            --         print $ (handleRdrName rdrname,showSDocUnsafe $ ppr id')
        getDataTypeDetails keyFunction path y@(L _ (HsRecFld _ (Ambiguous   id'  lnrdrname))) = do
            let name = T.pack $ nameStableString $ varName id'
                _type = T.pack $ showSDocUnsafe $ ppr $ varType id'
            expr <- pure $ toJSON $ transformFromNameStableString (Just name, Just $ T.pack $ getLocTC' $ y, Just _type, mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
            -- case reLocN lnrdrname of
            --     (L l rdrname) -> do
            --         print $ (handleRdrName rdrname,showSDocUnsafe $ ppr id')
        getDataTypeDetails keyFunction path (L _ y@(HsRecFld _ _)) = pure ()
        getDataTypeDetails keyFunction path _ = pure ()

        -- inferFieldType :: Name -> String
        inferFieldTypeFieldOcc (L _ (FieldOcc _ (L _ rdrName))) = handleRdrName rdrName
        inferFieldTypeFieldOcc (L _ (XFieldOcc _)) = mempty--handleRdrName rdrName
        inferFieldTypeAFieldOcc = (handleRdrName . rdrNameAmbiguousFieldOcc . unLoc)

        handleRdrName :: RdrName -> String
        handleRdrName rdrName = case rdrName of
                Exact name -> nameStableString name  -- For exact names
                Qual mod' occ -> moduleNameString mod' ++ "." ++ occNameString occ  -- For qualified names
                Unqual occ -> occNameString occ  -- For unqualified names
                Orig mod' occ -> moduleNameString (moduleName mod') ++ "." ++ occNameString occ  -- For original names
        -- handleRdrName :: RdrName -> String
        -- handleRdrName x =
        --     case x of
        --         Unqual occName -> ("$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Qual moduleName occName -> ((moduleNameString moduleName) <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Orig module' occName -> ((moduleNameString $ moduleName module') <> "$" <> (showSDocUnsafe $ pprNameSpaceBrief $ occNameSpace occName) <> "$" <> (occNameString occName) <> "$" <> (unpackFS $ occNameFS occName))
        --         Exact name -> nameStableString name

#if __GLASGOW_HASKELL__ >= 900
        getFieldUpdates :: GenLocated (SrcSpanAnn' a) e -> Text -> Text -> Text -> Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> IO ()
        getFieldUpdates _ keyFunction path type_ fields =
            case fields of
                Left x -> void $ mapM (extractField) x
                Right x -> (void $ mapM (processRecordProj) x)
            where
            processRecordProj :: LHsRecProj GhcTc (LHsExpr GhcTc) -> IO ()
            processRecordProj (L _ (HsRecField { hsRecFieldAnn, hsRecFieldLbl=lbl , hsRecFieldArg=expr ,hsRecPun=pun })) = do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                case lbl of
                    (L _ (FieldLabelStrings ll)) -> void $ mapM (processHsFieldLabel keyFunction path) ll
                    _ -> pure ()
                processExpr keyFunction path expr

            -- extractField :: HsRecUpdField GhcTc -> IO ()
            extractField y@(L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) =do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeAFieldOcc lbl)
                processExpr keyFunction path expr
                expr' <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ getLocTC' $ y), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr')

        processHsFieldLabel :: Text -> Text -> Located (HsFieldLabel GhcTc) -> IO ()
        processHsFieldLabel keyFunction path y@(L l x@(HsFieldLabel _ (L _ hflLabel))) = do
            expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_fieldName$" <> (T.pack $ showSDocUnsafe $ ppr hflLabel)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr x), mempty)
            sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
        processHsFieldLabel keyFunction path (L _ (XHsFieldLabel _)) = pure ()
#else
        getFieldUpdates :: _ -> Text -> Text -> Text -> [LHsRecUpdField GhcTc]-> IO ()
        getFieldUpdates y keyFunction path type_ fields = void $ mapM extractField fields
            where
            extractField :: LHsRecUpdField GhcTc -> IO ()
            extractField (L l x@(HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr', hsRecPun = pun})) =do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeAFieldOcc lbl)
                processExpr keyFunction path expr'
                expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ getLocTC' y), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
#endif

        extractRecordBinds :: Text -> Text -> Text ->  HsRecFields GhcTc (LHsExpr GhcTc) -> IO ()
        extractRecordBinds keyFunction path type_ (HsRecFields{rec_flds = fields}) =
            void $ mapM extractField fields
            where
            extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> IO ()
            extractField (L l x@(HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr, hsRecPun = pun})) = do
                let fieldName = (T.pack $ showSDocUnsafe $ ppr lbl)
                    fieldType = (T.pack $ inferFieldTypeFieldOcc lbl)
                processExpr keyFunction path expr
                expr' <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_fieldName$" <> fieldName), (Just $ T.pack $ showSDocUnsafe $ ppr $ l), (Just $ fieldType), mempty)
                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr')

#if __GLASGOW_HASKELL__ > 900

        extractExprsFromLHsCmdTop :: Text -> Text -> LHsCmdTop GhcTc -> IO ()
        extractExprsFromLHsCmdTop keyFunction path (L _ cmdTop) = 
            case cmdTop of
                HsCmdTop _ cmd -> extractExprsFromLHsCmd keyFunction path cmd
                XCmdTop _ -> pure ()

        extractExprsFromLHsCmd :: Text -> Text ->  LHsCmd GhcTc -> IO ()
        extractExprsFromLHsCmd keyFunction path (L _ cmd) = extractExprsFromHsCmd keyFunction path cmd

        extractExprsFromCmdLStmt :: Text -> Text -> CmdLStmt GhcTc -> IO ()
        extractExprsFromCmdLStmt keyFunction path (L _ stmt) = extractExprsFromStmtLR keyFunction path stmt

        extractExprsFromMatchGroup :: Text -> Text -> MatchGroup GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatchGroup keyFunction path (MG _ (L _ matches) _) = void $ mapM (extractExprsFromMatch keyFunction path) matches

        extractExprsFromMatch :: Text -> Text ->  LMatch GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromMatch keyFunction path (L _ (Match _ _ _ grhs)) = extractExprsFromGRHSs keyFunction path grhs

        extractExprsFromGRHSs :: Text -> Text ->  GRHSs GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHSs keyFunction path (GRHSs _ grhss _) = void $ mapM (extractExprsFromGRHS keyFunction path)  grhss
        extractExprsFromGRHSs keyFunction path (XGRHSs _) = pure ()

        extractExprsFromGRHS :: Text -> Text ->  LGRHS GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromGRHS keyFunction path (L _ (GRHS _ _ body)) = extractExprsFromLHsCmd keyFunction path body
        extractExprsFromGRHS keyFunction path (L _ (XGRHS _)) = pure ()

        extractExprsFromStmtLR :: Text -> Text -> StmtLR GhcTc GhcTc (LHsCmd GhcTc) -> IO ()
        extractExprsFromStmtLR keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body -> do
                extractExprsFromPat keyFunction path pat
                extractExprsFromLHsCmd keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                void $ mapM (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                extractExprsFromLHsCmd keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path binds
            ParStmt _ blocks _ bindOp -> do
                void $ mapM (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (trS_stmts)
                processExpr keyFunction path trS_using
                void $ mapM (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path  trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (wrapXRec @GhcTc trS_fmap)
            RecStmt{..} -> do
                void $ mapM (extractExprsFromStmtLR keyFunction path . unLoc) (unLoc recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            XStmtLR _ -> pure ()

        extractExprsFromParStmtBlock :: Text -> Text -> ParStmtBlock GhcTc GhcTc -> IO ()
        extractExprsFromParStmtBlock keyFunction path (ParStmtBlock _ stmts _ _) =
            void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) stmts

        processSynExpr keyFunction path (SyntaxExprTc { syn_expr      = expr}) = processExpr keyFunction path (wrapXRec @GhcTc $ expr)
        processSynExpr _ _ _ = pure ()

        extractExprsFromStmtLRHsExpr :: Text -> Text -> StmtLR GhcTc GhcTc (LHsExpr GhcTc) -> IO ()
        extractExprsFromStmtLRHsExpr keyFunction path stmt = case stmt of
            LastStmt _ body _ retExpr -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path retExpr
            BindStmt _ pat body -> do
                extractExprsFromPat keyFunction path pat
                processExpr keyFunction path body
            ApplicativeStmt _ args mJoin -> do
                void $ mapM (\(op, arg) -> do
                    processSynExpr keyFunction path op
                    extractExprFromApplicativeArg keyFunction path arg) args
                case mJoin of
                    Just m -> processSynExpr keyFunction path m
                    _ -> pure ()
            BodyStmt _ body _ guardOp -> do
                processExpr keyFunction path body
                processSynExpr keyFunction path guardOp
            LetStmt _ binds ->
                processHsLocalBinds keyFunction path binds
            ParStmt _ blocks _ bindOp -> do
                void $ mapM (extractExprsFromParStmtBlock keyFunction path) blocks
                processSynExpr keyFunction path bindOp
            TransStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) trS_stmts
                processExpr keyFunction path trS_using
                void $ mapM (processExpr keyFunction path) (trS_by)
                processSynExpr keyFunction path trS_ret
                processSynExpr keyFunction path trS_bind
                processExpr keyFunction path (wrapXRec @GhcTc trS_fmap)
            RecStmt{..} -> do
                void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path . unLoc) (unXRec @GhcTc $ recS_stmts)
                processSynExpr keyFunction path recS_bind_fn
                processSynExpr keyFunction path recS_ret_fn
                processSynExpr keyFunction path recS_mfix_fn
            XStmtLR _ -> pure ()

        extractExprsFromHsCmd :: Text -> Text -> HsCmd GhcTc -> IO ()
        extractExprsFromHsCmd keyFunction path cmd = case cmd of
            HsCmdArrApp _ f arg _ _ ->
                void $ mapM (processExpr keyFunction path) [f, arg]
            HsCmdArrForm _ e _ _ cmdTops -> do
                void $ mapM (extractExprsFromLHsCmdTop keyFunction path) cmdTops
                processExpr keyFunction path e
            HsCmdApp _ cmd' e -> do
                extractExprsFromLHsCmd keyFunction path cmd'
                processExpr keyFunction path e
            HsCmdLam _ mg -> extractExprsFromMatchGroup keyFunction path mg
            HsCmdPar _ cmd' ->
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdCase _ e mg -> do
                extractExprsFromMatchGroup keyFunction path mg
                processExpr keyFunction path e
            HsCmdLamCase _ mg ->
                extractExprsFromMatchGroup keyFunction path mg
            HsCmdIf _ _ predExpr thenCmd elseCmd -> do
                extractExprsFromLHsCmd keyFunction path elseCmd
                extractExprsFromLHsCmd keyFunction path thenCmd
                processExpr keyFunction path predExpr
            HsCmdLet _ binds cmd' -> do
                processHsLocalBinds keyFunction path binds
                extractExprsFromLHsCmd keyFunction path cmd'
            HsCmdDo _ stmts ->
                void $ mapM (extractExprsFromCmdLStmt keyFunction path )(unLoc stmts)
            XCmd _ -> pure ()

        extractExprsFromPat :: Text -> Text -> LPat GhcTc -> IO ()
        extractExprsFromPat keyFunction path y@(L _ pat) =
            case pat of
                WildPat hsType     -> do
                    expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                VarPat _ var    -> processExpr keyFunction path ((wrapXRec @(GhcTc)) (HsVar noExtField (var)))
                LazyPat _ p   -> (extractExprsFromPat keyFunction path) p
                AsPat _ var p   -> do
                    processExpr keyFunction path ((wrapXRec @(GhcTc)) (HsVar noExtField (var)))
                    (extractExprsFromPat keyFunction path) p
                ParPat _ p    -> (extractExprsFromPat keyFunction path) p
                BangPat _ p   -> (extractExprsFromPat keyFunction path) p
                ListPat _ ps  -> void $ mapM (extractExprsFromPat keyFunction path) ps
                TuplePat _ ps _ -> void $ mapM (extractExprsFromPat keyFunction path) ps
                SumPat hsTypes p _ _ -> do 
                    void $ mapM (\hsType -> do
                                expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                                sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                            ) hsTypes
                    (extractExprsFromPat keyFunction path) p
                ConPat {pat_args = args} -> (extractExprsFromHsConPatDetails keyFunction path args)
                ViewPat hsType expr p -> do
                    expr' <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_type$" <> (T.pack $ showSDocUnsafe $ ppr hsType)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsType), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr')
                    processExpr keyFunction path expr
                    (extractExprsFromPat keyFunction path) p
                SplicePat _ splice -> void $ mapM (processExpr keyFunction path) $ extractExprsFromSplice splice
                LitPat _ hsLit     -> do
                    expr <- pure $ toJSON $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ getLocTC' $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
                    sendTextData' cliOptions con path (transformPayload path keyFunction EXPR expr)
                NPat _ (L _ overLit) _ _ -> do
                    extractExprsFromOverLit overLit
                NPlusKPat _ _ (L _ overLit) _ _ _ ->
                    extractExprsFromOverLit overLit
                SigPat _ p _   -> (extractExprsFromPat keyFunction path) p
                XPat _         -> pure ()
            where
            extractExprsFromOverLit :: HsOverLit GhcTc -> IO ()
            extractExprsFromOverLit (OverLit _ _ e) = processExpr keyFunction path $ wrapXRec @(GhcTc) e
            extractExprsFromOverLit _ = pure ()

            extractExprsFromHsConPatDetails :: Text -> Text -> HsConPatDetails GhcTc -> IO ()
            extractExprsFromHsConPatDetails keyFunction' path' (PrefixCon _ args) = void $ mapM (extractExprsFromPat keyFunction' path') args
            extractExprsFromHsConPatDetails keyFunction' path' z@(RecCon (HsRecFields {})) =
                void $ mapM (extractExprsFromPat keyFunction' path') $ hsConPatArgs z
            extractExprsFromHsConPatDetails keyFunction' path' (InfixCon p1 p2) = do
                (extractExprsFromPat keyFunction' path') p1
                (extractExprsFromPat keyFunction' path') p2

        extractExprFromApplicativeArg :: Text -> Text -> ApplicativeArg GhcTc -> IO ()
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgOne _ lpat expr _) = do 
            processExpr keyFunction path expr
            extractExprsFromPat keyFunction path lpat
        extractExprFromApplicativeArg keyFunction path (ApplicativeArgMany _ exprLStmt stmts lpat _) = do
            processExpr keyFunction path (wrapXRec @(GhcTc) stmts)
            void $ mapM (extractExprsFromStmtLRHsExpr keyFunction path) (map (unLoc) exprLStmt)
            extractExprsFromPat keyFunction path lpat

        processXXExpr :: Text -> Text -> XXExprGhcTc -> IO ()
        processXXExpr keyFunction path (WrapExpr (HsWrap hsWrapper hsExpr)) =
            processExpr keyFunction path (wrapXRec @(GhcTc) hsExpr)
        processXXExpr keyFunction path (ExpansionExpr (HsExpanded _ expansionExpr)) =
            void $ mapM (processExpr keyFunction path . (wrapXRec @(GhcTc))) [expansionExpr]

getLocTC' :: GenLocated (SrcSpanAnn' a) e -> String
getLocTC' = (showSDocUnsafe . ppr . la2r . getLoc)

getLoc' :: GenLocated (SrcSpanAnn' a) e -> String
getLoc'   = (showSDocUnsafe . ppr . la2r . getLoc)
#else
getLocTC' = (showSDocUnsafe . ppr . getLoc)
getLoc' = (showSDocUnsafe . ppr . getLoc)
#endif

extractExprsFromSplice :: HsSplice GhcTc -> [LHsExpr GhcTc]
extractExprsFromSplice (HsTypedSplice _ _ _ e) = [e]
extractExprsFromSplice (HsUntypedSplice _ _ _ e) = [e]
extractExprsFromSplice (HsQuasiQuote _ _ _ _ _) = []
extractExprsFromSplice (HsSpliced _ _ _) = []
extractExprsFromSplice _ = []