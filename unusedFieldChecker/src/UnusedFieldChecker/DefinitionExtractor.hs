{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnusedFieldChecker.DefinitionExtractor
    ( extractFieldDefinitions
    ) where

import Prelude hiding (log)

#if __GLASGOW_HASKELL__ >= 900
import GHC
import GHC.Core.DataCon
import GHC.Core.TyCon
import qualified GHC.Core.TyCo.Rep as TyCo
import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.InstEnv
import GHC.Core
import GHC.Types.Literal (Literal(..))
import qualified GHC.Utils.Outputable as Out
import GHC.Data.FastString
import GHC.Data.IOEnv (readMutVar)
import GHC.Tc.Types
import GHC.Tc.Utils.Env (tcLookupClass)
import GHC.Tc.Utils.Monad (getEps, getEpsVar, getGblEnv)
import GHC.Types.FieldLabel
import GHC.Types.Name (nameSrcSpan, getOccString, nameStableString, getSrcSpan, nameModule_maybe)
import GHC.Types.SrcLoc
import GHC.Types.Id (idUnfolding, idName)
import GHC.Unit.External (eps_inst_env)
import GHC.Unit.Module.ModGuts
import GHC.Unit.Types (Unit, moduleUnit, unitString)
import GHC.Utils.Outputable hiding ((<>))
#else
import DataCon
import DynFlags
import FastString
import FieldLabel
import GHC
import GhcPlugins hiding ((<>))
import Module (moduleUnitId, unitIdString)
import Name
import OccName (mkClsOcc)
import Outputable
import RdrName (mkRdrUnqual)
import SrcLoc
import TcEnv (tcLookupClass)
import TcRnMonad (getEpsVar, getGblEnv, readMutVar)
import TcRnTypes
import TyCon
import TyCoRep
import Type
import Class
import InstEnv
#endif

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List (find, stripPrefix, isInfixOf)
import Data.Maybe (catMaybes, listToMaybe)
import UnusedFieldChecker.Types
import System.IO (readFile)
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

#if __GLASGOW_HASKELL__ >= 900
extractFieldDefinitions :: CliOptions -> Text -> Unit -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions cliOpts modName currentPkg tcEnv = do
    let tyCons = extractTyCons tcEnv
        currentPkgName = extractPackageName $ pack $ unitString currentPkg
    concat <$> mapM (extractFieldsFromTyCon cliOpts modName currentPkgName) tyCons
#else
extractFieldDefinitions :: CliOptions -> Text -> UnitId -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions cliOpts modName currentPkg tcEnv = do
    let tyCons = extractTyCons tcEnv
        currentPkgName = extractPackageName $ pack $ unitIdString currentPkg
    concat <$> mapM (extractFieldsFromTyCon cliOpts modName currentPkgName) tyCons
#endif

-- Helper functions for extracting excluded fields from Core expressions
#if __GLASGOW_HASKELL__ >= 900

-- Extract a list of strings from a Core expression
-- Handles pattern: (:) "str1" ((:) "str2" [])
extractStringList :: CoreExpr -> Maybe [String]
extractStringList expr = case expr of
    -- Empty list: []
    Var v | getOccString v == "[]" -> Just []

    -- Cons: (:) head tail
    App (App (Var cons) headExpr) tailExpr
        | getOccString cons == ":" -> do
            headStr <- extractString headExpr
            tailList <- extractStringList tailExpr
            return (headStr : tailList)

    -- Lambda: \_ -> body (for excludedFields _ = ...)
    Lam _ body -> extractStringList body

    -- Type application (strip type arguments)
    App f (Type _) -> extractStringList f

    _ -> Nothing

-- Extract a string literal from a Core expression
extractString :: CoreExpr -> Maybe String
extractString expr = case expr of
    -- String literal - try to extract the string value
    Lit lit -> extractStringFromCoreExpr lit

    -- String via unpackCString#
    App (Var unpacker) (Lit lit)
        | getOccString unpacker `elem` ["unpackCString#", "unpackCStringUtf8#"] ->
            extractStringFromCoreExpr lit

    -- Nested application
    App f _ -> extractString f

    _ -> Nothing

-- Try to extract string from a literal by showing it and parsing
extractStringFromCoreExpr :: Literal -> Maybe String
extractStringFromCoreExpr lit =
    let litStr = Out.showSDocUnsafe (Out.ppr lit)
    in case litStr of
        -- Match pattern: "string_value"
        ('"':rest) -> case reverse rest of
            ('"':revStr) -> Just (reverse revStr)
            _ -> Nothing
        -- Sometimes just the string is shown without quotes
        s | not (null s) -> Just s
        _ -> Nothing

-- Extract list from Core expression and handle errors
extractListFromCoreExpr :: Bool -> CoreExpr -> Int -> TcM [String]
extractListFromCoreExpr enableLogs expr _ = do
    when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Attempting to extract list from Core: " ++ Out.showSDocUnsafe (Out.ppr expr)
    case extractStringList expr of
        Just strings -> do
            when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Successfully extracted: " ++ show strings
            return strings
        Nothing -> do
            when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Could not extract string list from Core"
            return []

-- Parse excludedFields from source code
-- Pattern: excludedFields _ = ["field1", "field2", ...]
parseExcludedFieldsFromSource :: String -> [String]
parseExcludedFieldsFromSource source =
    case findExcludedFieldsLine source of
        Nothing -> []
        Just line -> extractStringsFromList line

-- Find the line containing "excludedFields _"
findExcludedFieldsLine :: String -> Maybe String
findExcludedFieldsLine source =
    let sourceLines = lines source
        excluded = filter ("excludedFields" `isInfixOf`) sourceLines
    in listToMaybe excluded

-- Extract string list from a line like: excludedFields _ = ["a", "b"]
extractStringsFromList :: String -> [String]
extractStringsFromList line =
    case dropWhile (/= '[') line of
        "" -> []
        rest -> parseStringList rest

-- Parse strings from a list literal ["str1", "str2"]
parseStringList :: String -> [String]
parseStringList s = case s of
    ('[':rest) -> parseElements rest
    _ -> []
  where
    parseElements [] = []
    parseElements (']':_) = []
    parseElements ('"':rest) =
        let (str, afterStr) = span (/= '"') rest
        in case afterStr of
            ('"':',':afterComma) -> str : parseElements (dropWhile (\c -> c == ' ' || c == '\n') afterComma)
            ('"':']':_) -> [str]
            ('"':other) -> str : parseElements (dropWhile (not . (== '[')) other)
            _ -> []
    parseElements (',':rest) = parseElements (dropWhile (\c -> c == ' ' || c == '\n') rest)
    parseElements (' ':rest) = parseElements rest
    parseElements ('\n':rest) = parseElements rest
    parseElements (_:rest) = parseElements rest

-- Try to read source file and extract excludedFields from it
tryExtractFromSource :: Bool -> FilePath -> Int -> Int -> TcM [String]
tryExtractFromSource enableLogs filePath startLine endLine = do
    liftIO $ do
        result <- tryIOError $ readFile filePath
        case result of
            Left err -> do
                when enableLogs $ putStrLn $ "[FieldChecker] Failed to read source file " ++ filePath ++ ": " ++ show err
                return []
            Right fileContent -> do
                let sourceLines = lines fileContent
                    -- Read from startLine to at least endLine+5 to catch multi-line instances
                    numLines = min (startLine + 10) (length sourceLines)
                    relevantLines = take (numLines - startLine + 1)
                                         (drop (startLine - 1) sourceLines)
                    relevantSource = unlines relevantLines
                when enableLogs $ do
                    putStrLn $ "[FieldChecker] Extracted source lines " ++ show startLine ++ "-" ++ show numLines
                    putStrLn $ "[FieldChecker] Source:\n" ++ relevantSource
                let excluded = parseExcludedFieldsFromSource relevantSource
                when enableLogs $ putStrLn $ "[FieldChecker] Parsed excludedFields from source: " ++ show excluded
                return excluded

-- Extract excluded fields from a FieldChecker instance
extractExcludedFieldsFromInst :: Bool -> ClsInst -> TcM [String]
extractExcludedFieldsFromInst enableLogs inst = do
    let dfun = is_dfun inst            -- Dictionary function Id
        unfolding = idUnfolding dfun   -- Get the unfolding/implementation
        dfunName = getOccString dfun

    when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Attempting to extract excludedFields from instance dfun: " ++ dfunName

    case unfolding of
        -- Standard unfolding with Core expression
        CoreUnfolding { uf_tmpl = coreExpr } -> do
            when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Got CoreUnfolding"
            extractListFromCoreExpr enableLogs coreExpr 0

        -- Dictionary function unfolding (methods as arguments)
        DFunUnfolding { df_args = args } -> do
            when enableLogs $ do
                liftIO $ putStrLn $ "[FieldChecker] Got DFunUnfolding with " ++ show (length args) ++ " args"
                liftIO $ mapM_ (\(i, arg) -> putStrLn $ "[FieldChecker] Arg " ++ show i ++ ": " ++ Out.showSDocUnsafe (Out.ppr arg)) (zip [0..] args)
            -- excludedFields is the first method (index 0)
            if not (null args)
                then extractListFromCoreExpr enableLogs (head args) 0
                else return []

        _ -> do
            -- No unfolding available - try to extract from source code
            when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] No Core unfolding available, attempting source code extraction"

            -- Try to get the location of the instance definition from the dfun
            let dfunLoc = nameSrcSpan (idName dfun)
            case dfunLoc of
                RealSrcSpan rss _ -> do
                    let filePath = unpackFS (srcSpanFile rss)
                        startLine = srcSpanStartLine rss
                        endLine = srcSpanEndLine rss
                    when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Instance defined at " ++ filePath ++ ":" ++ show startLine ++ "-" ++ show endLine
                    tryExtractFromSource enableLogs filePath startLine endLine
                _ -> do
                    when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] No real source location available for instance"
                    return []

#endif

extractFieldsFromTyCon :: CliOptions -> Text -> Text -> TyCon -> TcM [FieldDefinition]
extractFieldsFromTyCon cliOpts modName currentPkgName tc
    | isAlgTyCon tc && not (isClassTyCon tc) = do
        (hasFieldChecker, excludedFieldsList) <- checkFieldCheckerInstance cliOpts tc

        if not hasFieldChecker
            then return []
            else do
                let dataCons = tyConDataCons tc
                    typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
                    typeConstructor = pack $ nameStableString $ tyConName tc
                concat <$> mapM (extractFieldsFromDataCon cliOpts modName currentPkgName typeName typeConstructor hasFieldChecker excludedFieldsList) dataCons
    | otherwise = return []

checkFieldCheckerInstance :: CliOptions -> TyCon -> TcM (Bool, [String])
checkFieldCheckerInstance cliOpts tc = do
    let enableLogs = logs cliOpts
    gblEnv <- getGblEnv
#if __GLASGOW_HASKELL__ >= 900
    eps <- getEps
    let extInstEnv = eps_inst_env eps
#else
    epsVar <- getEpsVar
    eps <- readMutVar epsVar
    let extInstEnv = eps_inst_env eps
#endif
    let tyConType = mkTyConTy tc
        typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
        homeInstEnv = tcg_inst_env gblEnv
        homeInsts = instEnvElts homeInstEnv
        extInsts = instEnvElts extInstEnv
        allInsts = homeInsts ++ extInsts

        mbMatchingInst = find (isFieldCheckerInstanceFor tyConType) allInsts

    when enableLogs $ do
        liftIO $ putStrLn $ "[FieldChecker] Type " ++ T.unpack typeName ++ " has instance: " ++ show (not $ null $ filter (isFieldCheckerInstanceFor tyConType) allInsts)
        liftIO $ putStrLn $ "[FieldChecker] Checked " ++ show (length homeInsts) ++ " home instances and " ++ show (length extInsts) ++ " external instances"

    case mbMatchingInst of
        Nothing -> return (False, [])
        Just inst -> do
#if __GLASGOW_HASKELL__ >= 900
            -- Extract excludedFields from the instance
            excluded <- extractExcludedFieldsFromInst enableLogs inst
            when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Extracted excluded fields: " ++ show excluded
            return (True, excluded)
#else
            -- Cannot extract from older GHC versions easily
            return (True, [])
#endif
  where
    isFieldCheckerInstanceFor :: Type -> ClsInst -> Bool
    isFieldCheckerInstanceFor ty inst =
        let className = pack $ showSDocUnsafe $ ppr $ is_cls inst
            instTypes = is_tys inst
        in "FieldChecker" `T.isInfixOf` className &&
           any (typeMatches ty) instTypes

    typeMatches :: Type -> Type -> Bool
    typeMatches t1 t2 =
        let t1Str = pack $ showSDocUnsafe $ ppr t1
            t2Str = pack $ showSDocUnsafe $ ppr t2
        in t1Str == t2Str

extractFieldsFromDataCon :: CliOptions -> Text -> Text -> Text -> Text -> Bool -> [String] -> DataCon -> TcM [FieldDefinition]
extractFieldsFromDataCon cliOpts modName currentPkgName typeName typeConstructor hasFieldChecker excludedFieldsList dc = do
    let enableLogs = logs cliOpts
        fieldLabels = dataConFieldLabels dc
        fieldTypes = dataConRepArgTys dc
        dcName = getName dc
        tyConName = getName $ dataConTyCon dc
        typePackage = extractPackageFromTypeConstructor typeConstructor
        -- Match if exact match OR if one package name contains the other (e.g., "gateway" matches "euler-api-gateway")
        isCurrentPackage = typePackage == currentPkgName ||
                          currentPkgName `T.isInfixOf` typePackage ||
                          typePackage `T.isInfixOf` currentPkgName

    when enableLogs $ liftIO $ putStrLn $ "[PACKAGE FILTER] Type: " ++ T.unpack typeName ++
                       ", currentPkg: " ++ T.unpack currentPkgName ++
                       ", typePackage: " ++ T.unpack typePackage ++
                       ", typeConstructor: " ++ T.unpack typeConstructor ++
                       ", isCurrentPackage: " ++ show isCurrentPackage ++
                       ", hasFieldChecker: " ++ show hasFieldChecker ++
                       ", excludedFields: " ++ show excludedFieldsList

    if not isCurrentPackage
        then do
            when enableLogs $ liftIO $ putStrLn $ "[PACKAGE FILTER] SKIPPING " ++ T.unpack typeName ++ " (typePackage: " ++ T.unpack typePackage ++ " != currentPkg: " ++ T.unpack currentPkgName ++ ")"
            return []
        else extractFieldsForCurrentPackage cliOpts modName typeName typeConstructor hasFieldChecker excludedFieldsList fieldLabels fieldTypes dcName tyConName

#if __GLASGOW_HASKELL__ >= 900
extractFieldsForCurrentPackage :: CliOptions -> Text -> Text -> Text -> Bool -> [String] -> [FieldLabel] -> [Scaled Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage cliOpts modName typeName typeConstructor hasFieldChecker excludedFieldsList fieldLabels fieldTypes dcName tyConName = do
    let enableLogs = logs cliOpts
    when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] Type: " ++ T.unpack typeName ++
                       ", fieldLabels: " ++ show (length fieldLabels) ++
                       ", fieldTypes: " ++ show (length fieldTypes) ++
                       ", excludedFields: " ++ show excludedFieldsList

    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then do
            fieldDefs <- forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
                let fieldName = pack $ unpackFS $ flLabel label
                    fieldNameStr = T.unpack fieldName

                -- Check if field is excluded
                if fieldNameStr `elem` excludedFieldsList
                    then do
                        when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Excluding field: " ++ fieldNameStr ++ " in type " ++ T.unpack typeName
                        return Nothing
                    else do
                        let fieldTypeStr = pack $ showSDocUnsafe $ ppr $ TyCo.scaledThing fieldType
                            isMaybe = isMaybeType (TyCo.scaledThing fieldType)
                            location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName
                            isSingleField = length fieldLabels == 1
                            packageName = case nameModule_maybe tyConName of
                                Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnit mod
                                Nothing -> "this"
                            fullyQualifiedType = modName <> "." <> typeName

                        return $ Just FieldDefinition
                            { fieldDefName = fieldName
                            , fieldDefType = fieldTypeStr
                            , fieldDefTypeName = typeName
                            , fieldDefIsMaybe = isMaybe
                            , fieldDefModule = modName
                            , fieldDefLocation = location
                            , fieldDefPackageName = packageName
                            , fieldDefFullyQualifiedType = fullyQualifiedType
                            , fieldDefTypeConstructor = typeConstructor
                            , fieldDefIsSingleField = isSingleField
                            , fieldDefHasFieldChecker = hasFieldChecker
                            }
            let filteredDefs = catMaybes fieldDefs
            when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] CREATED " ++ show (length filteredDefs) ++ " field definitions for " ++ T.unpack typeName
            return filteredDefs
        else do
            when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] SKIPPING " ++ T.unpack typeName ++ " - no fields or length mismatch"
            return []
#else
extractFieldsForCurrentPackage :: CliOptions -> Text -> Text -> Text -> Bool -> [String] -> [FieldLabel] -> [Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage cliOpts modName typeName typeConstructor hasFieldChecker excludedFieldsList fieldLabels fieldTypes dcName tyConName = do
    let enableLogs = logs cliOpts
    when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] Type: " ++ T.unpack typeName ++
                       ", fieldLabels: " ++ show (length fieldLabels) ++
                       ", fieldTypes: " ++ show (length fieldTypes) ++
                       ", excludedFields: " ++ show excludedFieldsList

    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then do
            fieldDefs <- forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
                let fieldName = pack $ unpackFS $ flLabel label
                    fieldNameStr = T.unpack fieldName

                -- Check if field is excluded
                if fieldNameStr `elem` excludedFieldsList
                    then do
                        when enableLogs $ liftIO $ putStrLn $ "[FieldChecker] Excluding field: " ++ fieldNameStr ++ " in type " ++ T.unpack typeName
                        return Nothing
                    else do
                        let fieldTypeStr = pack $ showSDocUnsafe $ ppr fieldType
                            isMaybe = isMaybeType fieldType
                            location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName
                            isSingleField = length fieldLabels == 1
                            packageName = case nameModule_maybe tyConName of
                                Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnitId mod
                                Nothing -> "this"
                            fullyQualifiedType = modName <> "." <> typeName

                        return $ Just FieldDefinition
                            { fieldDefName = fieldName
                            , fieldDefType = fieldTypeStr
                            , fieldDefTypeName = typeName
                            , fieldDefIsMaybe = isMaybe
                            , fieldDefModule = modName
                            , fieldDefLocation = location
                            , fieldDefPackageName = packageName
                            , fieldDefFullyQualifiedType = fullyQualifiedType
                            , fieldDefTypeConstructor = typeConstructor
                            , fieldDefIsSingleField = isSingleField
                            , fieldDefHasFieldChecker = hasFieldChecker
                            }
            let filteredDefs = catMaybes fieldDefs
            when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] CREATED " ++ show (length filteredDefs) ++ " field definitions for " ++ T.unpack typeName
            return filteredDefs
        else do
            when enableLogs $ liftIO $ putStrLn $ "[FIELD EXTRACTION] SKIPPING " ++ T.unpack typeName ++ " - no fields or length mismatch"
            return []
#endif

isMaybeType :: Type -> Bool
isMaybeType ty = case ty of
#if __GLASGOW_HASKELL__ >= 900
    TyCo.TyConApp tc _ ->
        let tcName = getOccString (getName tc)
        in tcName == "Maybe"
#else
    TyConApp tc _ ->
        let tcName = getOccString (getName tc)
        in tcName == "Maybe"
#endif
    _ -> False

extractPackageFromTypeConstructor :: Text -> Text
extractPackageFromTypeConstructor tc =
    case T.splitOn "$" tc of
        (_:pkgWithVersion:_) ->
            let parts = T.splitOn "-" pkgWithVersion
                nameParts = takeWhile (not . startsWithDigit) parts
            in T.intercalate "-" nameParts
        _ -> ""
  where
    startsWithDigit :: Text -> Bool
    startsWithDigit t = case T.uncons t of
        Just (c, _) -> c >= '0' && c <= '9'
        Nothing -> False


extractPackageName :: Text -> Text
extractPackageName unitStr =
    let parts = T.splitOn "-" unitStr
        nameParts = takeWhile (not . startsWithDigit) parts
    in T.intercalate "-" nameParts
  where
    startsWithDigit :: Text -> Bool
    startsWithDigit t = case T.uncons t of
        Just (c, _) -> c >= '0' && c <= '9'
        Nothing -> False

extractTyCons :: TcGblEnv -> [TyCon]
extractTyCons tcEnv = 
    let tcs = tcg_tcs tcEnv
    in filter isSafeTyCon tcs
  where
    isSafeTyCon tc = 
        not (isClassTyCon tc) &&
        not (isPromotedDataCon tc) &&
        not (isTcTyCon tc)
