{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass, CPP, TypeApplications #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, RecordWildCards, PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -ddump-parsed-ast #-}

module DC.DefaultCheck where

import Control.Reference (biplateRef, (^?))
import GHC hiding (typeKind)
import Data.Aeson
import Data.Aeson as A
import Data.Generics.Uniplate.Data ()
import Data.List
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Data
import Control.Monad.Extra (anyM)
import Data.List.Extra (replace,splitOn)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Extra (filterM, ifM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Control.Exception (try,SomeException)
import System.Directory (createDirectoryIfMissing, doesFileExist )
import Data.Yaml
import qualified Data.ByteString.Lazy.Char8 as Char8
import DC.Types
import DC.Constants
#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.ConLike
import qualified GHC.Utils.Outputable as OP
import GHC.Tc.Utils.Monad (addErrs)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types
import GHC.Driver.Plugins
import GHC.Types.Var
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.Name hiding (varName)
import Control.Monad.IO.Class (liftIO)
import GHC.Utils.Misc
import GHC.Core.DataCon
import GHC.Core.PatSyn
#else
import ConLike
import qualified Outputable as OP
import TcRnMonad ( addErrs)
import Bag (bagToList)
import TcRnTypes (TcGblEnv (..), TcM)
import GhcPlugins hiding ((<>)) --(Plugin(..), defaultPlugin, purePlugin, CommandLineOption, ModSummary(..), Module (..), liftIO, moduleNameString, showSDocUnsafe, ppr, nameStableString, varName)
#endif

plugin :: Plugin
plugin =
    defaultPlugin {
      typeCheckResultAction = checkIntegrity
    , pluginRecompile = purePlugin
    }

checkIntegrity :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
checkIntegrity opts modSummary tcEnv = do
    let PluginOpts{..} = case opts of
                                    []      -> defaultPluginOpts
                                    (x : _) -> fromMaybe defaultPluginOpts $ A.decode (Char8.pack x)
    let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> ms_hspp_file modSummary
    parsedYaml :: Either ParseException CheckerConfig  <- liftIO $ parseYAMLFile domainConfigFile
    case parsedYaml of
      Right conf -> do
        let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
        liftIO $ createDirectoryIfMissing True path
        getAllUpdatesLi <- mapM (loopOverLHsBindLR prefixPath pathsTobeChecked conf moduleName') (bagToList $ tcg_binds tcEnv)
        let getAllUpdatesList = fst <$> getAllUpdatesLi
            getAllFuns = HM.unions $ snd <$> getAllUpdatesLi
        let res = foldl (\(UpdateInfo acc1 acc2 acc3 acc4 acc5 acc6) (UpdateInfo x y z z1 z2 otherFuns) -> UpdateInfo ((acc1) ++ (changeModName moduleName' <$> x)) ((acc2) ++ (changeModName moduleName' <$> y)) ((acc3) ++ (changeModName moduleName' <$> z)) ((acc4) ++ (changeModName moduleName' <$> z1)) ((acc5) ++ (changeModName moduleName' <$> z2)) ((acc6) ++ (changeModName moduleName' <$> otherFuns))) (UpdateInfo [] [] [] [] [] []) $ catMaybes getAllUpdatesList
        let combination = lookUpAndConcat getAllFuns
            allRes = getAllRes conf moduleName' combination res
        liftIO $ B.writeFile (modulePath <> ".json") (encodePretty $ (\(UpdateInfo createRec upRecords upFails cFails allFails otherFuns) -> UpdateInfoAsText (nub $ name <$> createRec) (nub $ name <$> upRecords) (nub $ name <$> upFails) (nub $ name <$> cFails) (nub $ name <$> allFails) (nub $ name <$> otherFuns)) allRes)
        !exprs <- mapM (loopOverLHsBindLRTot prefixPath pathsTobeChecked conf path allRes moduleName') (bagToList $ tcg_binds tcEnv)
        case conf of
          FieldsCheck _ -> do
            let exprsC = foldl (\acc (val) -> acc ++ getErrorrs val ) [] exprs
            addErrs $ map (mkGhcCompileError) (exprsC)
          FunctionCheck (FunctionCheckConfig{..}) -> do
            if moduleName' `elem` modulesToIgnore then do
              liftIO $ B.writeFile (modulePath <> ".err.json") "{}"
              liftIO $ B.writeFile (modulePath <> ".json") (encodePretty $ UpdateInfoAsText [] [] [] [] [] [])
            else if (not $ null pathsToConsider) && (null $ filter (\pathToConsider -> (prefixPath ++ pathToConsider) `isPrefixOf` path) pathsToConsider) then do
              liftIO $ B.writeFile (modulePath <> ".err.json") "{}"
              liftIO $ B.writeFile (modulePath <> ".json") (encodePretty $ UpdateInfoAsText [] [] [] [] [] [])
            else if (not $ null pathsToIgnore) && (not $ null $ filter (\pathToIgnore -> (prefixPath ++ pathToIgnore) `isPrefixOf` path) pathsToIgnore) then do
              liftIO $ B.writeFile (modulePath <> ".err.json") "{}"
              liftIO $ B.writeFile (modulePath <> ".json") (encodePretty $ UpdateInfoAsText [] [] [] [] [] [])
            else if moduleName' == moduleNameToCheck then do
              let exprsC = foldl (\acc (val) -> acc ++ getErrorrs val ) [] exprs
              -- liftIO $ print ("Module Name found" ++ moduleNameToCheck ++ "  " ++  show exprs )
              addErrs $ map (mkGhcCompileError) (exprsC)
            else do
              let exprsC = foldl (\acc (val) -> HM.union acc (getFuncs val) ) HM.empty exprs
              liftIO $ B.writeFile (modulePath <> ".err.json") (encodePretty exprsC)
        pure tcEnv
      Left err -> do
        liftIO $ print $ "Not using dc plugin since no config is found" ++ show err
        pure tcEnv

getErrorrs :: ErrorCase -> [CompileError]
getErrorrs (Errors val) = val
getErrorrs _ = []

getFuncs :: ErrorCase -> HM.HashMap String [CompileError]
getFuncs (Functions val) = val
getFuncs _ = HM.empty

getAllRes :: CheckerConfig -> String -> HM.HashMap String [FunctionInfo] -> UpdateInfo -> UpdateInfo
getAllRes conf  moduleName' combination res =
  case conf of
    FieldsCheck (EnumCheck{..}) -> do
      HM.foldlWithKey (\acc key val ->
          if any (\x -> name x `elem` (name <$> updatedFailurs res)) val
              then acc{updatedFailurs = nub $ updatedFailurs acc ++ ([FunctionInfo "" moduleName' key "" False])}
          else if any (\x -> name x `elem` (name <$> createdFailurs res) ) val
              then acc{createdFailurs = nub $ createdFailurs acc ++ ([FunctionInfo "" moduleName' key "" False])}
          else if any (\x -> name x `elem` (name <$> createdRecordsFun res)) val
              then if any (\x -> name x `elem` enumList) val
                  then
                  acc{createdFailurs = nub $ createdFailurs acc ++ ([FunctionInfo "" moduleName' key "" False])}
                  else
                  acc{createdRecordsFun = nub $ createdRecordsFun acc ++ ([FunctionInfo "" moduleName' key "" False])}
          else if any (\x -> name x `elem` (name <$> updatedRecordsFun res) ) val
              then if any (\x -> name x `elem` enumList) val
                  then
                  acc{updatedFailurs = nub $ updatedFailurs acc ++ ([FunctionInfo "" moduleName' key "" False])}
                  else acc{updatedRecordsFun = nub $ updatedRecordsFun acc ++ ([FunctionInfo "" moduleName' key "" False])}
          else if any (\x -> name x `elem` (name <$> allFailures res) ) val
              then acc{allFailures = nub $ allFailures acc ++ ([FunctionInfo "" moduleName' key "" False])}
              else acc) res combination
    _ -> res

lookUpAndConcat :: HM.HashMap String [FunctionInfo] -> HM.HashMap String [FunctionInfo]
lookUpAndConcat hm = HM.map (\val -> nub $ foldl (\acc x -> nub $ acc ++ lookupEachKey hm [] x) val val ) hm

changeModName :: String -> FunctionInfo -> FunctionInfo
changeModName moduleName' (FunctionInfo x y z z2 isF) = FunctionInfo x (if y == "_in" then moduleName' else y) z z2 isF

lookupEachKey :: HM.HashMap String [FunctionInfo] -> [String] -> FunctionInfo -> [FunctionInfo]
lookupEachKey hm alreadyVisited x = case HM.lookup (name x) hm of
  Just val -> [x] ++ if name x `elem` (alreadyVisited) then [] else (nub $ concat $ (lookupEachKey hm (alreadyVisited ++ ([name x])) <$> (nub (val))))
  Nothing -> [x]

processAllLetPats :: LHsBindLR GhcTc GhcTc -> (Maybe (String, [FunctionInfo]))
#if __GLASGOW_HASKELL__ >= 900
processAllLetPats (L _ (FunBind _ name matches _)) = do
#else
processAllLetPats (L _ (FunBind _ name matches _ _)) = do
#endif
    let inte = unLoc $ mg_alts matches
    if null inte then Nothing
       else Just (nameStableString $ varName $ unLoc name, concat $ map (\(GRHS _ _ val) -> processExpr val) $ map unLoc $ grhssGRHSs $ m_grhss $ unLoc $ head $ inte )
processAllLetPats (L _ _) = do
    Nothing


loopOverLHsBindLRTot :: String -> [String] -> CheckerConfig -> String -> UpdateInfo -> String -> LHsBindLR GhcTc GhcTc ->  TcM ErrorCase
loopOverLHsBindLRTot prefixPath allPaths conf path allFuns moduleName' vals@(L _ AbsBinds {abs_binds = binds}) = do
  case conf of
    FunctionCheck (FunctionCheckConfig{..}) ->
      if moduleName' == moduleNameToCheck
        then do
          let funName = (getFunctionName vals)
          -- liftIO $ print(funName)
          if ("$_in$" ++ funNameToCheck) `elem` funName then do -- getTxnStatusFromGateway
            let binds1 = ( (bagToList binds) ^? biplateRef :: [LHsExpr GhcTc])
            let allNrFuns = nub $ ((concatMap processExpr binds1))
            -- liftIO $ print("ALLFUN", allNrFuns)
            first <- catMaybes <$> (liftIO $ (mapM ((\x@(FunctionInfo _ _ _ _ _) -> do
                                  nc <- checkInOtherModsWithoutErrorFuns prefixPath allPaths conf moduleName' x
                                  -- print(nc, x)
                                  if null nc then  pure Nothing else (pure $ Just $ nc))) (allNrFuns)))
            pure $ Errors $ concat first
            else pure $ Errors []
        else do
          let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
          allFunsWithFailure <- mapM (getFunctionNameIfFailure prefixPath allPaths conf "" [] "" "" moduleName') (bagToList binds ^? biplateRef)
          let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
          let funName =  map (\y -> transformFromNameStableString y (showSDocUnsafe $ ppr $ getLoc $ vals) False )  (getFunctionName vals)
          let val = map (\(upType,listY) -> (createUpdateInfo upType $ map (\y -> transformFromNameStableString y (showSDocUnsafe $ ppr $ getLoc $ vals) False) listY)) allFunsWithFailure
          allC <- nub <$> (mapM (loopOverModBinds prefixPath allPaths conf path allLetPats allFuns moduleName' val) allVals)
          let allV = foldl (\acc (val1) -> acc ++ getErrorrs val1 ) [] allC
          -- liftIO $ print (allC, allV, funName)
          pure $ if null allV then Functions HM.empty else Functions (foldl (\acc val1 -> HM.insert val1 allV acc) HM.empty (name <$> funName))
    FieldsCheck (EnumCheck{..}) -> do
      let allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
      allFunsWithFailure <- mapM (getFunctionNameIfFailure prefixPath allPaths conf recordType enumList enumType fieldType moduleName') (bagToList binds ^? biplateRef)
      let val = map (\(upType,listY) -> (createUpdateInfo upType $ map (\y -> transformFromNameStableString y (showSDocUnsafe $ ppr $ getLoc $ vals) False) listY)) allFunsWithFailure
      let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
      allC <- nub <$> (mapM (loopOverModBinds prefixPath allPaths conf path allLetPats allFuns moduleName' val) allVals)
      case conf of
        FieldsCheck _ -> pure $ Errors $ foldl (\acc (vals1) -> acc ++ getErrorrs vals1 ) [] allC
        -- FunctionCheck _ -> do
        --   let allV = foldl (\acc (vals1) -> acc ++ getErrorrs vals1 ) [] allC
        --   -- liftIO $ print (allC, allV, funName)
        --   pure $ if null allV then Functions HM.empty else Functions (foldl (\acc vals1 -> HM.insert vals1 allV acc) HM.empty (name <$> funName))
loopOverLHsBindLRTot _ _ _ _ _ _ _ = pure $ Errors []

createUpdateInfo :: TypeOfUpdate -> [FunctionInfo] -> UpdateInfo
createUpdateInfo Update list = UpdateInfo [] list [] [] [] []
createUpdateInfo Create list = UpdateInfo list [] [] [] [] []
createUpdateInfo CreateWithFailure list = UpdateInfo [] [] [] list [] []
createUpdateInfo UpdateWithFailure list = UpdateInfo [] [] list [] [] []
createUpdateInfo Default list = UpdateInfo [] [] list [] [] []
createUpdateInfo _ _ = UpdateInfo [] [] [] [] [] []

loopOverModBinds :: String -> [String] -> CheckerConfig -> String -> HM.HashMap String [FunctionInfo] -> UpdateInfo -> String -> [UpdateInfo] -> LHsExpr GhcTc -> TcM ErrorCase
loopOverModBinds prefixPath allPaths checkerCase path allFUnsInside allFuns moduleName' allPatsList (L _ (HsCase _ _ exprLStmt)) = do
    -- liftIO $ print ("val",allFuns)
    allFunsPats <- mapM (loopOverPats prefixPath allPaths checkerCase path allFUnsInside allFuns moduleName' allPatsList) $ map unLoc $ unLoc $ mg_alts exprLStmt
    pure $ Errors $ foldl (\acc (val) -> acc ++ getErrorrs val ) [] allFunsPats
loopOverModBinds _ _ _ _ _ _ _ _ _ = do
    pure $ Errors []

getAllEnums :: LHsExpr GhcTc -> Maybe String
getAllEnums (L _ (HsConLikeOut _ liter)) = Just $ showSDocUnsafe $ ppr liter
getAllEnums (L _ _) = Nothing 


loopOverPats :: String -> [String] -> CheckerConfig -> String -> HM.HashMap String [FunctionInfo] -> UpdateInfo -> String -> [UpdateInfo] -> Match GhcTc (LHsExpr GhcTc)  -> TcM ErrorCase
loopOverPats prefixPath allPaths checkerCase path allFUnsInsid allFunsWithFailure moduleName' allPatsList match  = do
  case checkerCase of
    FieldsCheck (EnumCheck{..}) -> do
      let normalBinds = (\(GRHS _ _ stmt )-> stmt ) <$> unLoc <$> (grhssGRHSs $ m_grhss match)
          argBinds = m_pats match
          checker = any (\x -> isVarPatExprBool x) (normalBinds ^? biplateRef :: [LHsExpr GhcTc] )
      if checker then pure $ Errors [] else 
        let a = any (isVarPat ["Nothing", "Left"]) argBinds
        in if a then do
          -- liftIO $ (print (showSDocUnsafe $ ppr normalBind, showSDocUnsafe $ ppr normalBinds ))
          let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (normalBinds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
          let allFUnsInside = HM.union allLetPats allFUnsInsid
              allFuns = concat $ map processExpr (normalBinds ^? biplateRef)
          check <- mapM (\x -> case HM.lookup (mkStringFromFunctionInfo x) allFUnsInside of
                              Nothing -> throwErrorRules x prefixPath allPaths path moduleName' allFunsWithFailure allPatsList
                              Just val -> do
                                  -- liftIO $ print ("showing " ++ show val) 
                                  res <- mapM (\y -> throwErrorRules y prefixPath allPaths path  moduleName' allFunsWithFailure allPatsList) (nub val)
                                  let concatVals = concat $ catMaybes ( res)
                                  if null concatVals then pure Nothing else pure $ Just concatVals) (nub allFuns) --anyM (\x -> if module_name x ==  moduleName'
                              --    then pure $ name x `elem` (name <$> allFunsWithFailure) && module_name x `elem` (module_name <$> allFunsWithFailure)
                              --      else checkInOtherMods x) allFuns
          if ((not $ null (catMaybes check)))
              then pure $ Errors $ (\x -> CompileError "" "" x (getLocGhc $ head argBinds)) <$> (catMaybes check)
          else do
              processedPats <- mapM (\x -> do
                              allCHecks <- liftIO $ mapM (checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName') x
                              pure $ any (==True) allCHecks) allLetPats
              let allFailureNames = name <$> (updatedFailurs allFunsWithFailure)
              let allNeeded = mapMaybe getExprTypeWithName $ normalBinds ^? biplateRef
                  allVals = fst <$> allNeeded
                  allEnums = catMaybes $ snd <$> allNeeded
                  b = (any (\x -> x `elem` (splitOn " " $ showSDocUnsafe $ ppr match)) (allFailureNames) ||  any (\x -> x `elem` allEnums) ["FAILURE", "SUCCESS"]) && any (\x -> recordType `isInfixOf` (replace enumType "" x)) (allVals) && any (\x -> enumType `isInfixOf` x) allVals
              allFunsUpd <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats []) ( (match ^? biplateRef))
              let allFunsUpds = catMaybes allFunsUpd
          -- liftIO $ print $ ("TypesInfo ", allFuns) 
          -- liftIO $ print $ ("check",a, allFunsUpds, toConstr <$> unLoc <$> normalBinds, showSDocUnsafe $ ppr normalBinds, allVals, allFuns)
          -- allFunsWithFailure <- mapM getAndPut allFuns
          -- liftIO $ print ("Checker", allPatsList)
              pure $ Errors $
                  if CreateWithFailure `elem` allFunsUpds
                      then [CompileError "" "" (createError ++ show allFunsUpds) (getLocGhc $ head argBinds)]
                  else if UpdateWithFailure `elem` allFunsUpds
                      then [CompileError "" "" (updateError ++ show allFunsUpds) (getLocGhc $ head argBinds)]
                  else if b then [CompileError "" "" defaultCase (getLocGhc $ head argBinds)]
                  else []
        else pure $ Errors []
    FunctionCheck (FunctionCheckConfig{..}) -> do
      let normalBinds = (\(GRHS _ _ stmt )-> stmt ) <$> unLoc <$> (grhssGRHSs $ m_grhss match)
          argBinds = m_pats match
      let a = any (isVarPat conditionToCheck) argBinds
      if a then do
          let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (normalBinds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
          let allFUnsInside = HM.union allLetPats allFUnsInsid
              allFuns = concat $ map processExpr (normalBinds ^? biplateRef)
          check <- mapM (\x -> if  name x `elem` listOfRestrictedFuns then
                                 pure $ Just syncError else do
                            case HM.lookup (mkStringFromFunctionInfo x) allFUnsInside of
                              Nothing -> throwFunctionErrorRules x prefixPath allPaths path  moduleName' allFunsWithFailure allPatsList
                              Just val -> do
                                  -- liftIO $ print ("showing " ++ show val) 
                                  res <- mapM (\y -> throwFunctionErrorRules y prefixPath allPaths path moduleName' allFunsWithFailure allPatsList) (nub val)
                                  let concatVals = concat $ catMaybes ( res)
                                  if null concatVals then pure Nothing else pure $ Just concatVals) (nub allFuns)
          if ((not $ null (catMaybes check)))
              then pure $ Errors $ (\x -> CompileError "" "" x (getLocGhc $ head argBinds)) <$> (catMaybes check)
          else pure $ Errors []
          else pure $ Errors []

throwFunctionErrorRules 
  :: FunctionInfo
  -> String
  -> [String]
  -> String
  -> String
  -> UpdateInfo
  -> [UpdateInfo]
  -> TcM (Maybe [Char])
throwFunctionErrorRules x prefixPath allPaths path moduleName' (UpdateInfo _ _ _ _ _ otherFuns) _  = do
    -- liftIO $  print ("Checking " ++ name x ++ show x)
    if module_name x ==  moduleName' || "_in" == module_name x then
      pure $ if( name x `elem` (name <$> otherFuns) && module_name x `elem` (module_name <$> otherFuns)) then
        Just (syncError)
        else Nothing
    else checkInOtherModsFunction prefixPath allPaths path x

getLocGhc :: _ -> SrcSpan
getLocGhc val =
#if __GLASGOW_HASKELL__ >= 900
  RealSrcSpan (la2r $ getLoc val) Nothing
#else
  getLoc val
#endif

throwErrorRules :: 
  FunctionInfo
  -> String
  -> [String]
  -> String
  -> String
  -> UpdateInfo
  -> [UpdateInfo]
  -> TcM (Maybe [Char])
throwErrorRules x prefixPath allPaths path moduleName' (UpdateInfo _ _ upFails cFails _ _) _  = do
    -- liftIO $  print ("Checking " ++ name x ++ show x)
    if module_name x ==  moduleName' || "_in" == module_name x then
      pure $ if( name x `elem` (name <$> cFails) && module_name x `elem` (module_name <$> cFails)) then
            --   || (name x `elem` (concat $ map (\x -> name <$> x) $ createdRecordsFun <$> allPatsList)) then
        Just (createError ++ show (name x ++ show (name <$> cFails)))
        else if name x `elem` ((name <$> upFails)) && module_name x `elem` ((module_name <$> upFails)) then
        --   || (name x `elem` (concat $ map (\x -> name <$> x) $ updatedRecordsFun <$> allPatsList)) then
        Just (updateError ++ show (name x ++ show (name <$> upFails)))
        -- else if name x `elem` (name <$> functions) && module_name x `elem` (module_name <$> functions)
        --   || (name x `elem` (concat $ map (\x -> name <$> x) $ updatedFailurs <$> allPatsList)) then
        --   Just defaultCase
        else Nothing
    else checkInOtherMods prefixPath allPaths path x

checkInOtherModsFunction :: String -> [String] -> String -> FunctionInfo -> TcM (Maybe String)
checkInOtherModsFunction prefixPath allPaths path (FunctionInfo _ y z _ _) = do
    let newFileName = "/" ++ (intercalate "/" . splitOn "." $ y) ++ ".hs.json"
    filterNames <- liftIO $ filterM (\pos -> doesFileExist (path ++ pos ++ newFileName)) allPaths
    let orgName = if null filterNames then ("test" ++ newFileName) else prefixPath ++ head filterNames ++ newFileName
    fileContents <- liftIO $ (try $ B.readFile orgName :: IO (Either SomeException B.ByteString))
    pure $ either (\_ -> Nothing) (\contents -> 
        maybe Nothing 
            (\(UpdateInfoAsText _ _ _ _ _ otherFuns) ->
                if z `elem` otherFuns
                then Just (syncError ++ show (z,otherFuns))
                    else Nothing) (Aeson.decode contents :: Maybe UpdateInfoAsText)) fileContents

checkInOtherMods :: String -> [String] -> String -> FunctionInfo -> TcM (Maybe String)
checkInOtherMods prefixPath allPaths path (FunctionInfo _ y z _ _) = do
    let newFileName = "/" ++ (intercalate "/" . splitOn "." $ y) ++ ".hs.json"
    filterNames <- liftIO $ filterM (\pos -> doesFileExist (path ++ pos ++ newFileName)) allPaths
    let orgName = if null filterNames then ("test" ++ newFileName) else prefixPath ++ head filterNames ++ newFileName
    fileContents <- liftIO $ (try $ B.readFile orgName :: IO (Either SomeException B.ByteString))
    pure $ either (\_ -> Nothing) (\contents -> 
        maybe Nothing 
            (\(UpdateInfoAsText _ _ upFails cFails _ _) ->
                if z `elem` cFails
                then Just (createError ++ show (z,cFails))
                else if z `elem` upFails
                    then Just (updateError ++ show (z,upFails))
                    else Nothing) (Aeson.decode contents :: Maybe UpdateInfoAsText)) fileContents

checkInOtherModsWithoutError :: String -> [String] -> CheckerConfig -> String -> FunctionInfo -> IO Bool
checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName' fun@(FunctionInfo _ y z _ _) = do
  case checkerCase of
    FieldsCheck _ -> do
      if module_name fun ==  moduleName' || "_in" == module_name fun then pure False
      else do
        let newFileName = "/" ++ (intercalate "/" . splitOn "." $ y) ++ ".hs.json"
        filterNames <- liftIO $ filterM (\pos -> doesFileExist (prefixPath ++ pos ++ newFileName)) allPaths
        let orgName = if null filterNames then ("test" ++ newFileName) else prefixPath ++ head filterNames ++ newFileName
        fileContents <- liftIO $ (try $ B.readFile orgName :: IO (Either SomeException B.ByteString))
        pure $ either (\_ -> False) (\contents -> 
            maybe False 
                (\(UpdateInfoAsText creRecords upRecords upFails cFails defaultF _) ->
                    z `elem` creRecords ++ upRecords ++ upFails ++ cFails ++ defaultF) (Aeson.decode contents :: Maybe UpdateInfoAsText)) fileContents
    FunctionCheck _ ->
      if module_name fun ==  moduleName' || "_in" == module_name fun then pure False
      else do
        let newFileName = "/" ++ (intercalate "/" . splitOn "." $ y) ++ ".hs.json"
        filterNames <- liftIO $ filterM (\pos -> doesFileExist (prefixPath ++ pos ++ newFileName)) allPaths
        let orgName = if null filterNames then ("test" ++ newFileName) else prefixPath ++ head filterNames ++ newFileName
        fileContents <- liftIO $ (try $ B.readFile orgName :: IO (Either SomeException B.ByteString))
        pure $ either (\_ -> False) (\contents -> 
            maybe False 
                (\(UpdateInfoAsText _ _ _ _ _ checkerF) ->
                    z `elem` checkerF) (Aeson.decode contents :: Maybe UpdateInfoAsText)) fileContents


checkInOtherModsWithoutErrorFuns :: String -> [String] -> CheckerConfig -> String -> FunctionInfo -> IO [CompileError]
checkInOtherModsWithoutErrorFuns prefixPath allPaths checkerCase moduleName' fun@(FunctionInfo _ y z _ _) = do
  case checkerCase of
    FunctionCheck _ ->
      if module_name fun ==  moduleName' || "_in" == module_name fun then pure []
      else do
        -- print ("Dnct", fun)
        let newFileName = "/" ++ (intercalate "/" . splitOn "." $ y) ++ ".hs.err.json"
        filterNames <- liftIO $ filterM (\pos -> doesFileExist (prefixPath ++ pos ++ newFileName)) allPaths
        -- print ("Dnct", filterNames)
        let orgName = if null filterNames then (prefixPath ++ newFileName) else prefixPath ++ head filterNames ++ newFileName
        -- print ("Org", orgName)
        fileContents <- liftIO $ (try $ B.readFile orgName :: IO (Either SomeException B.ByteString))
        pure $ either (\_ -> []) (\contents -> 
            maybe []
                (\(checkerF) ->
                    case HM.lookup z checkerF of
                      Just val -> val
                      Nothing -> []) (Aeson.decode contents :: Maybe (HM.HashMap String [CompileError]))) fileContents
    _ -> pure []

#if __GLASGOW_HASKELL__ >= 900
extractLHsRecUpdField :: Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> [FunctionInfo]
extractLHsRecUpdField fields =
  case fields of
    Left fun -> concatMap (processExprCases) (fun)
    Right x ->
      let yn = (map (GHC.unXRec @(GhcTc)) x)
      in concatMap processExprUps (yn )
#else
extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [FunctionInfo]
extractLHsRecUpdField (L _ (HsRecField {hsRecFieldArg = fun})) = processExpr fun
#endif

#if __GLASGOW_HASKELL__ >= 900
processExprUps :: HsRecField' id (GenLocated SrcSpanAnnA (HsExpr GhcTc)) -> [FunctionInfo]
processExprUps (HsRecField {hsRecFieldArg = fun}) = processExpr fun

processExprCases :: GenLocated l (HsRecField' id (GenLocated SrcSpanAnnA (HsExpr GhcTc))) -> [FunctionInfo]
processExprCases (L _ (HsRecField {hsRecFieldArg = fun})) = processExpr fun
#endif


mkStringFromFunctionInfo :: FunctionInfo -> String
mkStringFromFunctionInfo (FunctionInfo pName modName name _ _) = intercalate "$" [pName, modName, name]

processExpr :: LHsExpr GhcTc -> [FunctionInfo]
processExpr x@(L _ (HsVar _ (L _ var))) =
  let name = transformFromNameStableString (nameStableString $ varName var) (showSDocUnsafe $ ppr $ getLoc $ x) False
  in [name]
processExpr (L _ (HsUnboundVar _ _)) = []
processExpr (L _ (HsApp _ funl funr)) =
  processExpr funl <> processExpr funr
processExpr (L _ (OpApp _ funl funm funr)) =
  processExpr funl <> processExpr funm <> processExpr funr
processExpr (L _ (NegApp _ funl _)) =
  processExpr funl
processExpr (L _ (HsTick _ _ fun)) =
  processExpr fun
processExpr (L _ (HsStatic _ fun)) =
  processExpr fun
processExpr (L _ (HsBinTick _ _ _ fun)) =
  processExpr fun
#if __GLASGOW_HASKELL__ < 900
processExpr (L _ (HsTickPragma _ _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsSCC _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsCoreAnn _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsWrap _ _ fun)) =
  processExpr (noLoc fun)
-- processExpr (L _ (HsWrap _ _ fun)) =
--   processExpr (noLoc fun)
processExpr (L _ (ExplicitList _ _ funList)) =
  concatMap processExpr funList
processExpr (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- processExpr (L _ (HsIf _ exprLStmt funl funm funr)) =
--   let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
--    in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
#else
processExpr (L _ (ExplicitList _ funList)) =
  concatMap processExpr funList
processExpr (L _ (HsIf exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (HsTcBracketOut _ _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> extractLHsRecUpdField rupd_flds
#endif
processExpr (L _ (ExprWithTySig _ fun _)) =
  processExpr fun
processExpr (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap processExpr stmts
processExpr (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExpr func <> nub (concatMap processExpr stmts)
processExpr (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsCase _ funl exprLStmt@(MG _ (L _ _) _))) =
   let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl] <> stmts)
processExpr (L _ (ExplicitSum _ _ _ fun)) = processExpr fun
processExpr (L _ (SectionR _ funl funr)) = processExpr funl <> processExpr funr
processExpr (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsPar _ fun)) = processExpr fun
processExpr (L _ (HsAppType _ fun _)) = processExpr fun
processExpr (L _ (HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr x@(L _ (HsLit _ liter)) =
  -- let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
  -- let literals = 
  [FunctionInfo "" (show $ toConstr liter) (showSDocUnsafe $ ppr liter) (showSDocUnsafe $ ppr $ getLoc x) False]
processExpr (L _ (HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmtsR)
processExpr (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- processExpr (L _ (RecordCon _ (L _ (iD)) rcon_flds)) = Just ((extractRecordBinds (rcon_flds)), False)
-- processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)
-- processExpr (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =

-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
processExpr x@(L _ (HsConLikeOut _ (RealDataCon liter))) =
  [FunctionInfo "" "" (showSDocUnsafe $ ppr liter) (showSDocUnsafe $ ppr $ getLoc x) False]
processExpr x@(L _ (HsConLikeOut _ (liter))) =
  [FunctionInfo "" "" (showSDocUnsafe $ ppr liter) (showSDocUnsafe $ ppr $ getLoc x) False]
processExpr (L _ _) = []

isVarPatMatch :: (LMatch GhcTc body) -> Bool
isVarPatMatch (L _ match) = 
    let argBinds = m_pats match
    in any (isVarPat ["Nothing", "Left"]) argBinds

isVarPatExprBool :: LHsExpr GhcTc -> Bool
isVarPatExprBool (L _ (HsCase _ _ (MG _ (L _ mg_alts) _))) =
    any isVarPatMatch mg_alts
    -- in L loc (HsCase m funl (MG mg_ext (L loc1 y) mg_org))
isVarPatExprBool _ = False

-- isVarPatMatch (L _ match) = 
--     let normalBinds = (\(GRHS _ _ stmt )-> stmt ) <$> unLoc <$> (grhssGRHSs $ m_grhss match)
--         argBinds = m_pats match
--     in any isVarPat argBinds

isVarPat :: [String] -> LPat GhcTc -> Bool
isVarPat matchWith (L _ pat) = case pat of
  VarPat _ (L _ _) -> True
  WildPat _ -> True
  x ->  if null matchWith
          then True
        else if any (\y -> y `isInfixOf` (showSDocUnsafe $ ppr x)) matchWith 
          then True 
          else False

getExprType :: LHsExpr GhcTc -> Maybe String
getExprType (L _ (HsVar _ idT)) = Just $ showSDocUnsafe $ ppr $ idType $ unLoc idT
getExprType (L _ (HsConLikeOut _ (RealDataCon idT))) = Just $ showSDocUnsafe $ ppr $ idType $ dataConWrapId idT
-- getExprType (L _ (HsConLikeOut _ (PatSynCon id))) = Just $ showSDocUnsafe $ ppr $ idType $ dataConWrapId id
getExprType (L _ _)=  Nothing

getExprTypeWithName :: LHsExpr GhcTc -> Maybe (String, Maybe String)
getExprTypeWithName (L _ (HsVar _ idT)) = Just $ (showSDocUnsafe $ ppr $ idType $ unLoc idT, Nothing)
getExprTypeWithName (L _ (HsConLikeOut _ (RealDataCon idT))) = Just $ (showSDocUnsafe $ ppr $ idType $ dataConWrapId idT, Just $ showSDocUnsafe $ ppr idT)
-- getExprTypeWithName (L _ (HsConLikeOut _ (PatSynCon id))) = Just $ showSDocUnsafe $ ppr $ idType $ dataConWrapId id
getExprTypeWithName (L _ _)=  Nothing

getExprTypeAsType :: LHsExpr GhcTc -> Maybe Type
getExprTypeAsType (L _ (HsVar _ idT)) = Just $ idType $ unLoc idT
getExprTypeAsType (L _ (HsConLikeOut _ (RealDataCon idT))) = Just $ idType $ dataConWrapId idT
-- getExprTypeAsType (L _ (HsConLikeOut _ (PatSynCon id))) = Just $ showSDocUnsafe $ ppr $ idType $ dataConWrapId id
getExprTypeAsType (L _ _)=  Nothing

getDataTypeDetails :: String -> [String] -> String -> String -> HM.HashMap String Bool -> [String] -> HsExpr GhcTc -> TcM (Maybe TypeOfUpdate)
#if __GLASGOW_HASKELL__ >= 900
getDataTypeDetails recordType enumList _ fieldType allLetPats allArgs (RecordCon _ iD rcon_flds) = pure $ if recordType `elem` ((splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr $ conLikeType (GHC.unXRec @(GhcTc) iD))) then Just (extractRecordBinds rcon_flds allLetPats allArgs enumList fieldType) else Nothing
#else
getDataTypeDetails recordType enumList _ fieldType allLetPats allArgs (RecordCon _ iD rcon_flds) = pure $ if recordType `elem` ((splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr $ idType $ unLoc iD)) then Just (extractRecordBinds rcon_flds allLetPats allArgs enumList fieldType) else Nothing
#endif
getDataTypeDetails recordType enumList _ fieldType allLetPats allArgs (RecordUpd _ rupd_expr rupd_flds) = 
  let allVals = mapMaybe getExprTypeAsType $ rupd_expr ^? biplateRef
  in pure $ if any (\x -> recordType `elem` ((splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr x))) allVals then Just (getFieldUpdates rupd_flds allLetPats allArgs enumList fieldType) else Nothing
getDataTypeDetails recordType enumList _ _ allLetPats allArgs x@(OpApp _ funl funm _) = do
    -- trace (show (showSDocUnsafe $ ppr x, showSDocUnsafe $ ppr funl,showSDocUnsafe $ ppr funm, showSDocUnsafe $ ppr funr)) Nothing
    if (showSDocUnsafe $ ppr funm) == "(#)"
        then do
            let allVals = mapMaybe getExprTypeAsType $ funl ^? biplateRef
            if any (\val -> recordType `elem` ((splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr val))) allVals
                then do
                    let allOps = mapMaybe (splitOnOpAp enumList allLetPats allArgs) (x ^? biplateRef)
                    -- liftIO $ print ("Proper ," ++ (show allOps)) 
                    pure $ if UpdateWithFailure `elem` allOps then Just UpdateWithFailure else if Update `elem` allOps then Just Update else Nothing
            else do
                -- liftIO $ print ("Frist ," ++ (showSDocUnsafe $ ppr allVals), showSDocUnsafe $ ppr funl)
                pure Nothing
    else do
        -- liftIO $ print ("Seconf ," ++ (showSDocUnsafe $ ppr funm))
        pure Nothing
getDataTypeDetails recordType enumList enumType fieldType _ _ pat@(HsApp _ app1 (L _ _)) = do
    if "Set" `isInfixOf` (showSDocUnsafe $ ppr app1) then do
      let allVals = mapMaybe getExprTypeAsType $ pat ^? biplateRef
      pure $ if recordType `isInfixOf` (showSDocUnsafe $ ppr allVals) && fieldType `isInfixOf` (showSDocUnsafe $ ppr pat) && enumType `isInfixOf` (showSDocUnsafe $ ppr allVals)
        then if any (\x -> x `isInfixOf` (showSDocUnsafe $ ppr pat)) enumList then
            Just UpdateWithFailure
            else Just Update
            else Nothing
    --   liftIO $ print (showSDocUnsafe $ ppr app1, showSDocUnsafe $ ppr allVals, toConstr app2, showSDocUnsafe $ ppr app2)
    else pure Nothing
    -- if showSDocUnsafe $ ppr app1 
    -- pure Nothing
getDataTypeDetails recordType enumList enumType fieldType allLetPats _ (HsPar _ (L _ pat)) = do
    -- liftIO $ print (allLetPats)
    if "setField" `isInfixOf` (showSDocUnsafe $ ppr pat) then do
      let allVals = mapMaybe getExprTypeAsType $ pat ^? biplateRef
          allInnerVals = concat $ map processExpr (pat ^? biplateRef)
      pure $ if recordType `isInfixOf` (showSDocUnsafe $ ppr allVals) && fieldType `isInfixOf` (showSDocUnsafe $ ppr pat) && enumType `isInfixOf` (showSDocUnsafe $ ppr allVals)
        then if any (\x -> x `isInfixOf` (showSDocUnsafe $ ppr pat)) enumList then
            Just UpdateWithFailure
            else do
              let allPosibleVals = 
                    map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                                Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                                Just val -> (False, val)) allInnerVals
              if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then Just UpdateWithFailure
               else Just Update
            else Nothing
    else pure Nothing
getDataTypeDetails _ _ _ _ _ _ (_) = do
    -- liftIO $ print (toConstr app1, showSDocUnsafe $ ppr app1)
    pure Nothing

splitOnOpAp :: [String] -> HM.HashMap String Bool -> [String] -> HsExpr GhcTc -> Maybe TypeOfUpdate
splitOnOpAp enumList allLetPats _ (OpApp _ left op right) =
    if (showSDocUnsafe $ ppr op) `elem` ["(%~)","(.~)"] then
      if (showSDocUnsafe $ ppr left) == "_status" then do
          let allVals = concat $ map processExpr (right ^? biplateRef)
          if any (\x -> x `isInfixOf` (showSDocUnsafe $ ppr right)) enumList then Just UpdateWithFailure
          else do
            let allPosibleVals =
                 map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                               Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                               Just val -> (False, val)) allVals
            if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then Just UpdateWithFailure
            else Just Update
      else Nothing
    else Nothing
splitOnOpAp _ _ _ _ = Nothing

  -- let allVals = mapMaybe getExprType $ x ^? biplateRef
  -- in if any (\x -> x `isInfixOf` recordType) allVals then Just (False, True) else Nothing

processRecordExpr :: HsExpr GhcTc -> String -> [FunctionInfo]
#if __GLASGOW_HASKELL__ >= 900
processRecordExpr (RecordCon _ (iD) rcon_flds) recordType = if recordType `isInfixOf` (showSDocUnsafe $ ppr $ conLikeType (GHC.unXRec @(GhcTc) iD)) then concat $ map processExpr (rcon_flds ^? biplateRef) else []
#else
processRecordExpr (RecordCon _ (L _ (iD)) rcon_flds) recordType = if recordType `isInfixOf` (showSDocUnsafe $ ppr $ idType iD) then concat $ map processExpr (rcon_flds ^? biplateRef) else []
#endif
processRecordExpr (RecordUpd _ rupd_expr rupd_flds) recordType =
  let allVals = mapMaybe getExprType $ rupd_expr ^? biplateRef
  in if any (\x -> x `isInfixOf` recordType) allVals then 
   concat $ map processExpr (rupd_flds ^? biplateRef)
   else []
processRecordExpr x recordType =
  let allVals = mapMaybe getExprType $ x ^? biplateRef
      allExprs = map processExpr (x ^? biplateRef)
  in if any (\val -> val `isInfixOf` recordType) allVals then 
   concat $ allExprs
   else []
-- inferFieldType :: Name -> String
-- inferFieldTypeFieldOcc (L _ (FieldOcc _ (L _ rdrName))) = handleRdrName rdrName
-- inferFieldTypeAFieldOcc = (handleRdrName . rdrNameAmbiguousFieldOcc . unLoc)
#if __GLASGOW_HASKELL__ >= 900
conLikeType :: ConLike -> Type
conLikeType (RealDataCon data_con) = dataConType data_con
conLikeType (PatSynCon pat_syn)    = patSynResultType pat_syn
#endif

#if __GLASGOW_HASKELL__ >= 900
getFieldUpdates :: Either [LHsRecUpdField GhcTc] [LHsRecUpdProj GhcTc] -> HM.HashMap String Bool -> [String] -> [String] -> String -> TypeOfUpdate
getFieldUpdates fields allLetPats allArgs enumList fieldType =
  case fields of
    Left x -> 
      let allUpdates = map extractField x
      in if UpdateWithFailure `elem` allUpdates then UpdateWithFailure 
        else if Update `elem` allUpdates then Update
        else NoChange
    Right x -> 
      let yn = (map (GHC.unXRec @(GhcTc)) x)
          allUpdates = map extractField' (yn)
      in if UpdateWithFailure `elem` allUpdates then UpdateWithFailure 
        else if Update `elem` allUpdates then Update
        else NoChange
    where
    extractField :: LHsRecUpdField GhcTc -> TypeOfUpdate
    extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr})) =
        let allNrFuns = nub $ ((concatMap processExpr (map noLocA $ expr ^? biplateRef)))
        in if isInfixOf fieldType (showSDocUnsafe $ ppr lbl) then
          if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) enumList then
            UpdateWithFailure
          else if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) allArgs then
            Update
          else do
            let allPosibleVals = 
                 map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                               Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                               Just val -> (False, val)) allNrFuns
            if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then UpdateWithFailure
            else Update
        else NoChange
    -- extractField' :: HsRecUpdField GhcTc -> TypeOfUpdate
    extractField' ((HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr})) =
        let allNrFuns = nub $ ((concatMap processExpr (map noLocA $ expr ^? biplateRef)))
        in if isInfixOf fieldType (showSDocUnsafe $ ppr lbl) then
          if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) enumList then
            UpdateWithFailure
          else if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) allArgs then
            Update
          else do
            let allPosibleVals = 
                 map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                               Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                               Just val -> (False, val)) allNrFuns
            if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then UpdateWithFailure
            else Update
        else NoChange
#else
getFieldUpdates :: [LHsRecUpdField GhcTc] -> HM.HashMap String Bool -> [String] -> [String] -> String -> TypeOfUpdate
getFieldUpdates fields allLetPats allArgs enumList fieldType =
    let allUpdates = map extractField fields
    in if UpdateWithFailure `elem` allUpdates then UpdateWithFailure 
       else if Update `elem` allUpdates then Update
       else NoChange
    where
    extractField :: LHsRecUpdField GhcTc -> TypeOfUpdate
    extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr})) =
        let allNrFuns = nub $ ((concatMap processExpr (map noLoc $ expr ^? biplateRef)))
        in if isInfixOf fieldType (showSDocUnsafe $ ppr lbl) then
          if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) enumList then
            UpdateWithFailure
          else if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) allArgs then
            Update
          else do
            let allPosibleVals = 
                 map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                               Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                               Just val -> (False, val)) allNrFuns
            if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then UpdateWithFailure
            else Update
        else NoChange
#endif

extractRecordBinds :: HsRecFields GhcTc (LHsExpr GhcTc) -> HM.HashMap String Bool -> [String] -> [String] -> String -> TypeOfUpdate
extractRecordBinds (HsRecFields{rec_flds = fields}) allLetPats allArgs enumList fieldType =
    let allUpdates = map extractField fields
    in if CreateWithFailure `elem` allUpdates then CreateWithFailure 
       else if Create `elem` allUpdates then Create
       else NoChange
    where
    extractField :: LHsRecField GhcTc (LHsExpr GhcTc) -> TypeOfUpdate
    extractField (L _ (HsRecField{hsRecFieldLbl = lbl, hsRecFieldArg = expr})) = do
#if __GLASGOW_HASKELL__ >= 900
        let allNrFuns = nub $ ((concatMap processExpr (map (noLocA) $ expr ^? biplateRef)))
#else
        let allNrFuns = nub $ ((concatMap processExpr (map noLoc $ expr ^? biplateRef)))
#endif
        if isInfixOf fieldType (showSDocUnsafe $ ppr lbl) then
          if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) enumList then
            CreateWithFailure
          else if any (\x -> isInfixOf x (showSDocUnsafe $ ppr expr)) allArgs then
            Create
          else do
            let allPosibleVals = 
                 map (\x -> case HM.lookup (mkStringFromFunctionInfo x) allLetPats of
                               Nothing -> maybe (True,False) (\val -> (False, val)) $ HM.lookup (name x) allLetPats 
                               Just val -> (False, val)) allNrFuns
            if any (==False) (fst <$> allPosibleVals) && any (==True) (snd <$> allPosibleVals) then CreateWithFailure
            else Create
        else NoChange
            -- then (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr lbl) (inferFieldTypeFieldOcc lbl))
            -- else (FieldRep (showSDocUnsafe $ ppr lbl) (showSDocUnsafe $ ppr $ unLoc expr) (inferFieldTypeFieldOcc lbl))

getFunctionName :: LHsBindLR GhcTc GhcTc -> [String]
#if __GLASGOW_HASKELL__ < 900
getFunctionName (L _ (FunBind _ idt _ _ _)) = [nameStableString $ getName idt]
#else
getFunctionName (L _ (FunBind _ idt _ _)) = [nameStableString $ getName idt]
#endif
getFunctionName (L _ (VarBind{var_id = var})) = [nameStableString $ varName var]
getFunctionName (L _ (PatBind{})) = [""]
getFunctionName (L _ (AbsBinds{abs_binds = binds})) = concatMap getFunctionName $ bagToList binds
getFunctionName _ = []

getFunctionNameIfFailure :: String -> [String] -> CheckerConfig -> String -> [String] -> String -> String -> String -> LHsBindLR GhcTc GhcTc -> TcM (TypeOfUpdate, [String])
#if __GLASGOW_HASKELL__ < 900
getFunctionNameIfFailure prefixPath allPaths checkerCase recordType enumList enumType fieldType moduleName' (L _ x@(FunBind _ idT _ _ _)) = do
#else
getFunctionNameIfFailure prefixPath allPaths checkerCase recordType enumList enumType fieldType moduleName' (L _ x@(FunBind _ idT _ _)) = do
#endif
  let allValsTypes = mapMaybe getExprType (x ^? biplateRef)
  let allVals = (map unLoc (x ^? biplateRef :: [LHsExpr GhcTc]))
  let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (x ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
  processedPats <- mapM (\funInfo -> do
                    if any (\val -> val  `elem` enumList) (name <$> funInfo) then pure True
                     else do
                        allCHecks <- liftIO $ mapM (checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName') funInfo
                        pure $ any (==True) allCHecks) allLetPats
  let allBinds = concat $ mapMaybe loopOverFunBind (x ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      funName = [nameStableString $ getName idT]
  (allRecordUpdsAndCrea) <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats allBinds) $ allVals
  let allRecordUpdsAndCreate = catMaybes allRecordUpdsAndCrea
  pure $ if any (\val -> val==CreateWithFailure) allRecordUpdsAndCreate
    then (CreateWithFailure, funName)
    else if any (\val -> val==UpdateWithFailure) allRecordUpdsAndCreate
    then (UpdateWithFailure, funName)
    else if any (\val -> val==Create) allRecordUpdsAndCreate
    then (Create, funName)
    else if any (\val -> val==Update) allRecordUpdsAndCreate
    then (Update, funName)
    else if any (\val -> val==Default) allRecordUpdsAndCreate
    then (Default, funName)
    else if any (\val -> isInfixOf val (showSDocUnsafe $ ppr x) ) enumList && (Just enumType) == (lastMaybe (splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr (lastMaybe allValsTypes)))
          then (Default, funName)
    else (NoChange,[])
getFunctionNameIfFailure prefixPath allPaths checkerCase recordType enumList enumType fieldType moduleName' (L _ x@(VarBind{var_id = var})) = do
  let allValsTypes = mapMaybe getExprType (x ^? biplateRef)
  let allVals = (map unLoc (x ^? biplateRef :: [LHsExpr GhcTc]))
  let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (x ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
  processedPats <- mapM (\funInfo -> do
                    if any (\val -> val  `elem` enumList) (name <$> funInfo) then pure True
                     else do
                        allCHecks <- liftIO $ mapM (checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName') funInfo
                        pure $ any (==True) allCHecks) allLetPats
  let allBinds = concat $ mapMaybe loopOverFunBind (x ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      funName = [nameStableString $ varName var]
  (allRecordUpdsAndCrea) <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats allBinds) $ allVals
  let allRecordUpdsAndCreate = catMaybes allRecordUpdsAndCrea
  pure $ if any (\val -> val==CreateWithFailure) allRecordUpdsAndCreate
    then (CreateWithFailure, funName)
    else if any (\val -> val==UpdateWithFailure) allRecordUpdsAndCreate
    then (UpdateWithFailure, funName)
    else if any (\val -> val==Create) allRecordUpdsAndCreate
    then (Create, funName)
    else if any (\val -> val==Update) allRecordUpdsAndCreate
    then (Update, funName)
    else if any (\val -> val==Default) allRecordUpdsAndCreate
    then (Default, funName)
    else if any (\val -> isInfixOf val (showSDocUnsafe $ ppr x) ) enumList && (Just enumType) == (lastMaybe (splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr (lastMaybe allValsTypes)))
          then (Default, funName)
    else (NoChange,[])
getFunctionNameIfFailure prefixPath allPaths checkerCase recordType enumList enumType fieldType moduleName' (L _ x@(AbsBinds{abs_binds = binds})) = do
  let allValsTypes = mapMaybe getExprType (x ^? biplateRef)
  let allVals = (map unLoc (bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
  let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
  processedPats <- mapM (\funInfo -> do
                    if any (\val -> val  `elem` enumList) (name <$> funInfo) then pure True
                     else do
                        allCHecks <- liftIO $ mapM (checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName') funInfo
                        pure $ any (==True) allCHecks) allLetPats
  let allBinds = concat $ mapMaybe loopOverFunBind (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      funName = concatMap getFunctionName $ bagToList binds
  (allRecordUpdsAndCrea) <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats allBinds) $ allVals
  let allRecordUpdsAndCreate = catMaybes allRecordUpdsAndCrea
  pure $ if any (\val -> val==CreateWithFailure) allRecordUpdsAndCreate
    then (CreateWithFailure, funName)
    else if any (\val -> val==UpdateWithFailure) allRecordUpdsAndCreate
    then (UpdateWithFailure, funName)
    else if any (\val -> val==Create) allRecordUpdsAndCreate
    then (Create, funName)
    else if any (\val -> val==Update) allRecordUpdsAndCreate
    then (Update, funName)
    else if any (\val -> val==Default) allRecordUpdsAndCreate
    then (Default, funName)
    else if any (\val -> isInfixOf val (showSDocUnsafe $ ppr x) ) enumList && (Just enumType) == (lastMaybe (splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr (lastMaybe allValsTypes)))
          then (Default, funName)
    else (NoChange,[])
--   pure $ if any (\(x,y) -> y==True) allRecordUpdsAndCreate then (Update, funName)
--     else if any (\(x,y) -> x==True) allRecordUpdsAndCreate then (Create, funName)
--     else if any (\val -> isInfixOf val (showSDocUnsafe $ ppr x)) enumList && any (\val -> isInfixOf enumType val ) allValsTypes then (Default, funName)  else (NoChange,[])
getFunctionNameIfFailure _ _ _ _ _ _ _hasFld _ _ = pure $ (NoChange,[])

loopOverLHsBindLR :: String -> [String] -> CheckerConfig -> String -> LHsBindLR GhcTc GhcTc  -> TcM ((Maybe UpdateInfo), (HM.HashMap String [FunctionInfo]))
loopOverLHsBindLR prefixPath allPaths checkerCase moduleName' x@(L _ AbsBinds {abs_binds = binds1}) = do
  case checkerCase of
    FieldsCheck (EnumCheck{..})   -> do
      let binds = ( bagToList binds1 ^? biplateRef)
    --   let allValsToCheck = ((bagToList binds1 ^? biplateRef :: [LHsExpr GhcTc]))
    --   liftIO (print (showSDocUnsafe $ ppr binds1, "and",  showSDocUnsafe $ ppr binds))
      let allVals = binds -- ((binds ^? biplateRef :: [LHsExpr GhcTc]))
      let allValsTypes = mapMaybe getExprTypeAsType allVals
          isF = any (\val -> isInfixOf val (showSDocUnsafe $ ppr x) ) enumList && (Just enumType) == (lastMaybe (splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr (lastMaybe allValsTypes)))
      let allLetPats = HM.fromList $ ((mapMaybe processAllLetPats (bagToList binds1 ^? biplateRef :: [LHsBindLR GhcTc GhcTc])))
      processedPats <- mapM (\(funInfo :: [FunctionInfo]) ->
                        if any (\val -> val  `elem` enumList) (name <$> funInfo) then pure True
                        else do
                            allCHecks <- liftIO $ mapM (checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName') funInfo
                            pure $ any (==True) allCHecks) allLetPats
      allBinds <- liftIO $ concat <$> catMaybes <$> mapM loopOverFunBindM (bagToList binds1 ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      let filteredAllVals = filter processHsCase allVals
      let funName = map (\y -> transformFromNameStableString y (showSDocUnsafe $ ppr $ getLoc $ x) isF ) (getFunctionName x)
      if length filteredAllVals > 0 then do
        let fname = name <$> funName
        allRecordUpdsAndCrea <- mapM (\val -> getAllNeededFunOuter recordType enumList enumType fieldType isF x  allBinds processedPats val ) filteredAllVals
        let allNrFuns = concat $ snd <$> allRecordUpdsAndCrea
        let allRecordUpdsAndCreate = concat $ fst <$> allRecordUpdsAndCrea
        -- liftIO $ print ("FInal", allRecordUpdsAndCreate)
        pure $ (if any (\val -> val==CreateWithFailure) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] [] funName [] []
            else if any (\val -> val==UpdateWithFailure) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] funName [] [] []
            else if any (\val -> val==Create) allRecordUpdsAndCreate
            then Just $ UpdateInfo funName [] [] [] [] []
            else if any (\val -> val==Update) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] funName [] [] [] []
            else if any (\val -> val==Default) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] [] [] funName []
            else if isF
                    then Just $ UpdateInfo [] [] [] [] funName []
            else Nothing, foldl (\acc val -> HM.insert (val) (nub allNrFuns) acc) HM.empty fname)

      else do
        let allNrFuns = nub $ ((concatMap processExpr allVals))
        allRecordUpdsAndCrea <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats allBinds) $ allVals ^? biplateRef
        let fname = name <$> funName
        let allRecordUpdsAndCreate = catMaybes allRecordUpdsAndCrea
        pure $ (if any (\val -> val==CreateWithFailure) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] [] funName [] []
            else if any (\val -> val==UpdateWithFailure) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] funName [] [] []
            else if any (\val -> val==Create) allRecordUpdsAndCreate
            then Just $ UpdateInfo funName [] [] [] [] []
            else if any (\val -> val==Update) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] funName [] [] [] []
            else if any (\val -> val==Default) allRecordUpdsAndCreate
            then Just $ UpdateInfo [] [] [] [] funName []
            else if isF
                    then Just $ UpdateInfo [] [] [] [] funName []
            else Nothing, foldl (\acc val -> HM.insert (val) (nub allNrFuns) acc) HM.empty fname)
    FunctionCheck (FunctionCheckConfig{..}) -> do
      let binds = ( (bagToList binds1) ^? biplateRef :: [LHsExpr GhcTc])
      let allNrFuns = nub $ ((concatMap processExpr binds))
      let funName = map (\y -> transformFromNameStableString y (showSDocUnsafe $ ppr $ getLoc $ x) False ) (getFunctionName x)
      let fname = name <$> funName
      first <- liftIO $ (ifM (anyM (\val@(FunctionInfo _ _ y _ _) -> do
                            let fc = ((y `elem` listOfRestrictedFuns)) 
                            nc <- checkInOtherModsWithoutError prefixPath allPaths checkerCase moduleName' val
                            pure $ fc || nc)  (allNrFuns))
                 (pure $ Just $ UpdateInfo [] [] [] [] [] funName)
                 (pure Nothing)
         )
      pure (first,  foldl (\acc val -> HM.insert (val) (nub allNrFuns) acc) HM.empty fname)


--   liftIO $ print (allLetPats, showSDocUnsafe $ ppr binds1)
loopOverLHsBindLR _ _ _ _ _ = pure (Nothing, HM.empty)

processHsCase :: LHsExpr GhcTc -> Bool
processHsCase (L _ (HsCase _ _ _)) = True
processHsCase _ = False

getAllNeededFunOuter :: String -> [String] -> String -> String -> Bool -> LHsBindLR GhcTc GhcTc -> [String] -> HM.HashMap String Bool -> LHsExpr GhcTc -> TcM ([TypeOfUpdate], [FunctionInfo])
getAllNeededFunOuter recordType enumList enumType fieldType isF x allBinds processedPats (L _ (HsCase _ _ exprLStmt)) = do
    allRecordUpdsAndCrea <- mapM (getAllNeededFun recordType enumList enumType fieldType isF x allBinds processedPats) $ map unLoc $ unLoc $ mg_alts exprLStmt
    -- liftIO $ print (concat allRecordUpdsAndCrea)
    pure $ (concat $ fst <$> allRecordUpdsAndCrea, concat $ snd <$> allRecordUpdsAndCrea)
getAllNeededFunOuter _ _ _ _ _ _ _ _ _ =  pure ([], [])

    -- pure $ catMaybes $ allRecordUpdsAndCrea

getAllNeededFun :: String -> [String] -> String -> String -> Bool ->  LHsBindLR GhcTc GhcTc -> [String] -> HM.HashMap String Bool -> Match GhcTc (LHsExpr GhcTc) -> TcM ([TypeOfUpdate], [FunctionInfo])
getAllNeededFun recordType enumList enumType fieldType _ _ allBinds processedPats match = do
  let normalBinds = (\(GRHS _ _ stmt )-> stmt ) <$> unLoc <$> (grhssGRHSs $ m_grhss match)
      argBinds = m_pats match
      a = any (isVarPat ["Nothing", "Left"]) argBinds
      checker = any (\val -> isVarPatExprBool val) (normalBinds ^? biplateRef :: [LHsExpr GhcTc] )
  if checker then pure ([], []) else
    if a then do
        let allNrFuns = nub $ concatMap processExpr (normalBinds ^? biplateRef :: [LHsExpr GhcTc] )
        allRecordUpdsAndCrea <- mapM (getDataTypeDetails recordType enumList enumType fieldType processedPats allBinds) $ normalBinds ^? biplateRef
        -- liftIO $ print (checker, a, showSDocUnsafe $ ppr argBinds, showSDocUnsafe $ ppr match, catMaybes allRecordUpdsAndCrea)
        pure $ (catMaybes allRecordUpdsAndCrea, allNrFuns)
    else pure ([], [])
--   liftIO $ print ("All updates ", allRecordUpdsAndCreate)
--   liftIO $ print (name, showSDocUnsafe <$> ppr <$> allValsTypes, any (\val -> isInfixOf val (showSDocUnsafe $ ppr x) ) enumList , map (\val -> (lastMaybe (splitOn " " $ replace "->" "" $ showSDocUnsafe $ ppr val))) allValsTypes)

loopAndColect :: HM.HashMap String [FunctionInfo] -> LHsBindLR GhcTc GhcTc -> IO ((HM.HashMap String [FunctionInfo]))
loopAndColect allFunsList x@(L _ AbsBinds {abs_binds = binds}) = do
  let fname = map name $ map (\y -> transformFromNameStableString (y) (showSDocUnsafe $ ppr $ getLoc $ x) False) $ (getFunctionName x)
      allVals = ((bagToList binds ^? biplateRef :: [LHsExpr GhcTc]))
--   liftIO $ print (fname, showSDocUnsafe $ ppr x)
--   let allBinds = concat $ mapMaybe loopOverFunBind (bagToList binds ^? biplateRef :: [LHsBindLR GhcTc GhcTc])
      allNrFuns = nub $ ((concatMap processExpr allVals))
  pure $ foldl (\acc val -> HM.insert (val) (nub allNrFuns) acc) allFunsList fname
loopAndColect allFunsList _ = pure allFunsList

loopOverFunBindM :: LHsBindLR GhcTc GhcTc -> (IO (Maybe [String]))
#if __GLASGOW_HASKELL__ < 900
loopOverFunBindM (L _ (FunBind _ _ matches _ _)) = do
#else
loopOverFunBindM (L _ (FunBind _ _ matches _)) = do
#endif
    let inte = unLoc $ mg_alts matches
    -- print ("iam here", showSDocUnsafe $ ppr x, length inte)
    if null inte then pure Nothing else do
        y <- mapM loopOverVarPatM $ m_pats $ unLoc $ head inte
        pure $ Just $ catMaybes y
loopOverFunBindM (L _ _) = do
    -- print ("iam not here " ++ show (toConstr x) ++ (showSDocUnsafe $ ppr x))
    pure Nothing

processAllLetPatsM :: LHsBindLR GhcTc GhcTc -> (Maybe (String, [FunctionInfo]))
#if __GLASGOW_HASKELL__ < 900
processAllLetPatsM (L _ (FunBind _ name matches _ _)) = do
#else
processAllLetPatsM (L _ (FunBind _ name matches _)) = do
#endif
    let inte = unLoc $ mg_alts matches
    if null inte then Nothing
       else Just (nameStableString $ varName $ unLoc name, concat $ map (\(GRHS _ _ val) -> processExpr val) $ map unLoc $ grhssGRHSs $ m_grhss $ unLoc $ head $ inte )
processAllLetPatsM (L _ _) = do
    Nothing


loopOverFunBind :: LHsBindLR GhcTc GhcTc -> (Maybe [String])
#if __GLASGOW_HASKELL__ < 900
loopOverFunBind (L _ (FunBind _ _ matches _ _)) = do
#else
loopOverFunBind (L _ (FunBind _ _ matches _)) = do
#endif
    let inte = unLoc $ mg_alts matches
    if null inte then Nothing else do
        let y = mapMaybe loopOverVarPat $ m_pats $ unLoc $ head inte
        Just y
loopOverFunBind (L _ _) = do
    Nothing

loopOverVarPat :: LPat GhcTc -> Maybe String
loopOverVarPat (L _ (VarPat _ (L _ name))) = Just $ nameStableString $ varName name
loopOverVarPat (L _ _) =  Nothing

loopOverVarPatM :: LPat GhcTc -> IO (Maybe String)
loopOverVarPatM (L _ (VarPat _ (L _ name))) = pure $ Just $ nameStableString $ varName name
loopOverVarPatM (L _ _) = do
    -- print ("loopOverVarPatM", show $ toConstr x)
    pure Nothing

mkGhcCompileError :: CompileError -> (SrcSpan, OP.SDoc)
mkGhcCompileError err = (src_span err, OP.text $ err_msg err)

transformFromNameStableString :: (String) -> String -> Bool -> FunctionInfo
transformFromNameStableString ( str) loc isF  =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) loc isF
     else if length parts == 3 then FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) loc isF
     else FunctionInfo "" "" "" loc isF

parseYAMLFile :: (FromJSON a) => FilePath -> IO (Either ParseException a)
parseYAMLFile file = decodeFileEither file