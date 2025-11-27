{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.ServantAPI
    ( extractServantAPITypes
    , validateAPITypesHaveFieldChecker
    ) where

import Prelude hiding (log)

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core.DataCon
import GHC.Core.InstEnv
import GHC.Core.TyCon
import qualified GHC.Core.TyCo.Rep as TyCo
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (getGblEnv, getEps)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Unit.External (eps_inst_env)
import GHC.Utils.Outputable hiding ((<>))
#else
import GHC
import DataCon
import GhcPlugins hiding ((<>))
import InstEnv
import Name
import Outputable
import SrcLoc
import TcRnMonad (getGblEnv, getEpsVar, readMutVar)
import TcRnTypes
import TyCon
import TyCoRep
import Type
#endif

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Set as Set
import UnusedFieldChecker.Types

extractServantAPITypes :: Text -> TcGblEnv -> TcM [ServantAPIType]
extractServantAPITypes modName tcEnv = do
    let tyCons = tcg_tcs tcEnv

    liftIO $ putStrLn $ "[ServantAPI] Analyzing module: " ++ T.unpack modName
    liftIO $ putStrLn $ "[ServantAPI] Found " ++ show (length tyCons) ++ " type constructors"

    apiTypesList <- forM tyCons $ \tc -> do
        if isTypeSynonymTyCon tc
            then extractTypesFromSynonym modName tc
            else return []

    let apiTypes = concat apiTypesList
    liftIO $ putStrLn $ "[ServantAPI] Extracted " ++ show (length apiTypes) ++ " Servant API types"

    return apiTypes

extractTypesFromSynonym :: Text -> TyCon -> TcM [ServantAPIType]
extractTypesFromSynonym modName tc = do
    let synName = pack $ showSDocUnsafe $ ppr $ tyConName tc
        synLoc = pack $ showSDocUnsafe $ ppr $ getSrcSpan (tyConName tc)

    case synTyConRhs_maybe tc of
        Nothing -> return []
        Just rhs -> do
            liftIO $ putStrLn $ "[ServantAPI] Analyzing type synonym: " ++ T.unpack synName
            extractedTypes <- extractTypesFromServantCombinators modName synLoc "" rhs
            return extractedTypes

extractTypesFromServantCombinators :: Text -> Text -> Text -> Type -> TcM [ServantAPIType]
extractTypesFromServantCombinators modName location endpoint ty = case ty of
#if __GLASGOW_HASKELL__ >= 900
    TyCo.TyConApp tc args -> do
        let tcName = pack $ showSDocUnsafe $ ppr $ tyConName tc
        extractFromTyConApp modName location endpoint tcName args

    TyCo.AppTy t1 t2 -> do
        types1 <- extractTypesFromServantCombinators modName location endpoint t1
        types2 <- extractTypesFromServantCombinators modName location endpoint t2
        return $ types1 ++ types2

    _ -> return []
#else
    TyConApp tc args -> do
        let tcName = pack $ showSDocUnsafe $ ppr $ tyConName tc
        extractFromTyConApp modName location endpoint tcName args

    AppTy t1 t2 -> do
        types1 <- extractTypesFromServantCombinators modName location endpoint t1
        types2 <- extractTypesFromServantCombinators modName location endpoint t2
        return $ types1 ++ types2

    _ -> return []
#endif

extractFromTyConApp :: Text -> Text -> Text -> Text -> [Type] -> TcM [ServantAPIType]
extractFromTyConApp modName location endpoint tcName args
    | ":>" `T.isInfixOf` tcName = do
        case args of
            (firstArg:rest) -> do
                let pathSegment = extractPathSegment firstArg
                    newEndpoint = if T.null endpoint
                                  then pathSegment
                                  else endpoint <> "/" <> pathSegment
                concatMapM (extractTypesFromServantCombinators modName location newEndpoint) rest
            [] -> return []

    | "ReqBody" `T.isInfixOf` tcName = do
        liftIO $ putStrLn $ "[ServantAPI] Found ReqBody at endpoint: " ++ T.unpack endpoint
        types <- extractDataTypes modName location endpoint "ReqBody '[JSON]" args
        restTypes <- concatMapM (extractTypesFromServantCombinators modName location endpoint) args
        return $ types ++ restTypes

    | "Post" `T.isInfixOf` tcName || "Get" `T.isInfixOf` tcName ||
      "Put" `T.isInfixOf` tcName || "Delete" `T.isInfixOf` tcName ||
      "Patch" `T.isInfixOf` tcName = do
        let methodName = T.takeWhile (/= ' ') tcName
        liftIO $ putStrLn $ "[ServantAPI] Found " ++ T.unpack methodName ++ " at endpoint: " ++ T.unpack endpoint
        extractDataTypes modName location endpoint (methodName <> " '[JSON]") args

    | "Capture" `T.isInfixOf` tcName = do
        liftIO $ putStrLn $ "[ServantAPI] Found Capture at endpoint: " ++ T.unpack endpoint
        types <- extractDataTypes modName location endpoint "Capture" args
        restTypes <- concatMapM (extractTypesFromServantCombinators modName location endpoint) args
        return $ types ++ restTypes

    | "QueryParam" `T.isInfixOf` tcName || "Header" `T.isInfixOf` tcName = do
        types <- extractDataTypes modName location endpoint tcName args
        restTypes <- concatMapM (extractTypesFromServantCombinators modName location endpoint) args
        return $ types ++ restTypes

    | ":<|>" `T.isInfixOf` tcName = do
        concatMapM (extractTypesFromServantCombinators modName location endpoint) args

    | otherwise = do
        concatMapM (extractTypesFromServantCombinators modName location endpoint) args

extractPathSegment :: Type -> Text
extractPathSegment ty =
    let tyStr = pack $ showSDocUnsafe $ ppr ty
    in if "\"" `T.isInfixOf` tyStr
        then case T.splitOn "\"" tyStr of
            (_:segment:_) -> segment
            _ -> ""
        else ""

extractDataTypes :: Text -> Text -> Text -> Text -> [Type] -> TcM [ServantAPIType]
extractDataTypes modName location endpoint combinator types = do
    let customTypes = mapMaybe extractCustomType types
    liftIO $ putStrLn $ "[ServantAPI] Found " ++ show (length customTypes) ++ " custom types in " ++ T.unpack combinator

    typesWithInstances <- forM customTypes $ \(typeName, typeConstructor, typeObj) -> do
        missingInstances <- checkRecursiveFieldCheckerInstance typeObj
        let hasInstance = null missingInstances
        
        liftIO $ putStrLn $ "[ServantAPI] Type " ++ T.unpack typeName ++ " recursive check passed: " ++ show hasInstance
        when (not hasInstance) $
            liftIO $ putStrLn $ "[ServantAPI] Missing instances for " ++ T.unpack typeName ++ ": " ++ show missingInstances

        return ServantAPIType
            { apiTypeName = typeName
            , apiTypeModule = modName
            , apiTypeConstructor = typeConstructor
            , apiEndpoint = endpoint
            , apiLocation = location
            , apiHasFieldChecker = hasInstance
            , apiMissingInstances = missingInstances
            , apiServantCombinator = combinator
            }

    return typesWithInstances

extractCustomType :: Type -> Maybe (Text, Text, Type)
extractCustomType t = case t of
#if __GLASGOW_HASKELL__ >= 900
    TyCo.TyConApp tc _ ->
        let tcName = pack $ showSDocUnsafe $ ppr $ tyConName tc
            tcConstructor = pack $ nameStableString $ tyConName tc
        in if isCustomType tcName
            then Just (tcName, tcConstructor, t)
            else Nothing
    _ -> Nothing
#else
    TyConApp tc _ ->
        let tcName = pack $ showSDocUnsafe $ ppr $ tyConName tc
            tcConstructor = pack $ nameStableString $ tyConName tc
        in if isCustomType tcName
            then Just (tcName, tcConstructor, t)
            else Nothing
    _ -> Nothing
#endif

isCustomType :: Text -> Bool
isCustomType t =
    not (t `elem` primitiveTypes) &&
    not ("[]" `T.isInfixOf` t) &&
    not ("Maybe" == t) &&
    not (T.null (T.strip t)) &&
    not (T.head t `elem` ['\'', '(', '[', ':'])
  where
    primitiveTypes =
        [ "Int", "Integer", "Double", "Float", "Bool", "Char"
        , "String", "Text", "ByteString", "UTCTime", "Day"
        , "JSON", "XML", "PlainText", "OctetStream", "FormUrlEncoded"
        , "UUID", "NoContent"
        ]

checkFieldCheckerInstance :: Text -> Text -> TcM Bool
checkFieldCheckerInstance typeName typeConstructor = do
    gblEnv <- getGblEnv
#if __GLASGOW_HASKELL__ >= 900
    eps <- getEps
    let extInstEnv = eps_inst_env eps
#else
    epsVar <- getEpsVar
    eps <- readMutVar epsVar
    let extInstEnv = eps_inst_env eps
#endif
    let homeInstEnv = tcg_inst_env gblEnv
        homeInsts = instEnvElts homeInstEnv
        extInsts = instEnvElts extInstEnv
        allInsts = homeInsts ++ extInsts

        hasFieldCheckerInstance = any isFieldCheckerInstanceForType allInsts

    liftIO $ putStrLn $ "[ServantAPI] Type " ++ T.unpack typeName ++ " has FieldChecker instance: " ++ show hasFieldCheckerInstance
    liftIO $ putStrLn $ "[ServantAPI] Checked " ++ show (length homeInsts) ++ " home instances and " ++ show (length extInsts) ++ " external instances"
    return hasFieldCheckerInstance
  where
    isFieldCheckerInstanceForType :: ClsInst -> Bool
    isFieldCheckerInstanceForType inst =
        let className = pack $ showSDocUnsafe $ ppr $ is_cls inst
            instTypes = is_tys inst
            instTypeStrs = map (pack . showSDocUnsafe . ppr) instTypes
        in "FieldChecker" `T.isInfixOf` className &&
           any (== typeName) instTypeStrs

validateAPITypesHaveFieldChecker :: [ServantAPIType] -> TcM [(Text, Text, SrcSpan, Text)]
validateAPITypesHaveFieldChecker apiTypes = do
    let missingInstances = filter (not . apiHasFieldChecker) apiTypes

    liftIO $ putStrLn $ "[ServantAPI] Validation: " ++ show (length missingInstances) ++
                       " types without FieldChecker instances"

    forM missingInstances $ \apiType -> do
        let srcSpan = parseLocationString (apiLocation apiType)
        return (apiTypeName apiType, apiEndpoint apiType, srcSpan, apiTypeModule apiType)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

parseLocationString :: Text -> SrcSpan
parseLocationString locStr =
    case T.splitOn ":" locStr of
        [file, line, col] ->
            case (readMaybeInt line, readMaybeInt col) of
                (Just l, Just c) ->
                    let srcLoc = mkSrcLoc (mkFastString $ T.unpack file) l c
                    in mkSrcSpan srcLoc srcLoc
                _ -> noSrcSpan
        _ -> noSrcSpan
  where
    readMaybeInt :: Text -> Maybe Int
    readMaybeInt s = case reads (T.unpack s) of
        [(x, "")] -> Just x
        _ -> Nothing

checkRecursiveFieldCheckerInstance :: Type -> TcM [Text]
checkRecursiveFieldCheckerInstance rootType = do
    visitedRef <- liftIO $ newIORef Set.empty
    missingRef <- liftIO $ newIORef []
    go visitedRef missingRef rootType
    liftIO $ readIORef missingRef
  where
    go visitedRef missingRef ty = do
        let tyStr = pack $ showSDocUnsafe $ ppr ty
        visited <- liftIO $ readIORef visitedRef
        
        if Set.member tyStr visited
            then return ()
            else do
                liftIO $ modifyIORef visitedRef (Set.insert tyStr)
                
                case ty of
#if __GLASGOW_HASKELL__ >= 900
                    TyCo.TyConApp tc args -> handleTyConApp visitedRef missingRef tc args
#else
                    TyConApp tc args -> handleTyConApp visitedRef missingRef tc args
#endif
                    _ -> return ()

    handleTyConApp visitedRef missingRef tc args = do
        let tcName = pack $ showSDocUnsafe $ ppr $ tyConName tc
            isCustom = isCustomType tcName
        
        if isCustom
            then do
                hasInstance <- checkFieldCheckerInstance tcName tcName -- Second arg is constructor, using name for now
                if not hasInstance
                    then liftIO $ modifyIORef missingRef (tcName :)
                    else do
                        -- Recurse into fields
                        let dataCons = tyConDataCons tc
                        forM_ dataCons $ \dc -> do
                            let fieldTypes = dataConRepArgTys dc
                            forM_ fieldTypes $ \ft -> do
#if __GLASGOW_HASKELL__ >= 900
                                go visitedRef missingRef (TyCo.scaledThing ft)
#else
                                go visitedRef missingRef ft
#endif
            else do
                -- Recurse into type arguments (e.g. Maybe a, [a])
                forM_ args $ \arg -> go visitedRef missingRef arg
