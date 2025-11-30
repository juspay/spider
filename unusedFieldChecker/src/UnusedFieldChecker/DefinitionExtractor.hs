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
import GHC.Data.FastString
import GHC.Data.IOEnv (readMutVar)
import GHC.Tc.Types
import GHC.Tc.Utils.Env (tcLookupClass)
import GHC.Tc.Utils.Monad (getEps, getEpsVar, getGblEnv)
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.SrcLoc
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

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List (find)
import UnusedFieldChecker.Types

#if __GLASGOW_HASKELL__ >= 900
extractFieldDefinitions :: Text -> Unit -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions modName currentPkg tcEnv = do
    let tyCons = extractTyCons tcEnv
        currentPkgName = extractPackageName $ pack $ unitString currentPkg
    concat <$> mapM (extractFieldsFromTyCon modName currentPkgName) tyCons
#else
extractFieldDefinitions :: Text -> UnitId -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions modName currentPkg tcEnv = do
    let tyCons = extractTyCons tcEnv
        currentPkgName = extractPackageName $ pack $ unitIdString currentPkg
    concat <$> mapM (extractFieldsFromTyCon modName currentPkgName) tyCons
#endif

extractFieldsFromTyCon :: Text -> Text -> TyCon -> TcM [FieldDefinition]
extractFieldsFromTyCon modName currentPkgName tc
    | isAlgTyCon tc && not (isClassTyCon tc) = do
        hasFieldChecker <- checkFieldCheckerInstance tc

        if not hasFieldChecker
            then return []
            else do
                let dataCons = tyConDataCons tc
                    typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
                    typeConstructor = pack $ nameStableString $ tyConName tc
                concat <$> mapM (extractFieldsFromDataCon modName currentPkgName typeName typeConstructor hasFieldChecker) dataCons
    | otherwise = return []

checkFieldCheckerInstance :: TyCon -> TcM Bool
checkFieldCheckerInstance tc = do
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

        hasFieldCheckerInstance = any (isFieldCheckerInstanceFor tyConType) allInsts

    liftIO $ putStrLn $ "[FieldChecker] Type " ++ T.unpack typeName ++ " has instance: " ++ show hasFieldCheckerInstance
    liftIO $ putStrLn $ "[FieldChecker] Checked " ++ show (length homeInsts) ++ " home instances and " ++ show (length extInsts) ++ " external instances"
    return hasFieldCheckerInstance
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

extractFieldsFromDataCon :: Text -> Text -> Text -> Text -> Bool -> DataCon -> TcM [FieldDefinition]
extractFieldsFromDataCon modName currentPkgName typeName typeConstructor hasFieldChecker dc = do
    let fieldLabels = dataConFieldLabels dc
        fieldTypes = dataConRepArgTys dc
        dcName = getName dc
        tyConName = getName $ dataConTyCon dc
        packagePattern = "$" <> currentPkgName <> "-"
        isCurrentPackage = packagePattern `T.isPrefixOf` typeConstructor

    liftIO $ putStrLn $ "[PACKAGE FILTER] Type: " ++ T.unpack typeName ++
                       ", currentPkg: " ++ T.unpack currentPkgName ++
                       ", pattern: " ++ T.unpack packagePattern ++
                       ", typeConstructor: " ++ T.unpack typeConstructor ++
                       ", isCurrentPackage: " ++ show isCurrentPackage ++
                       ", hasFieldChecker: " ++ show hasFieldChecker

    if not isCurrentPackage
        then do
            liftIO $ putStrLn $ "[PACKAGE FILTER] SKIPPING " ++ T.unpack typeName ++ " - not from current package"
            return []
        else extractFieldsForCurrentPackage modName typeName typeConstructor hasFieldChecker fieldLabels fieldTypes dcName tyConName

#if __GLASGOW_HASKELL__ >= 900
extractFieldsForCurrentPackage :: Text -> Text -> Text -> Bool -> [FieldLabel] -> [Scaled Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage modName typeName typeConstructor hasFieldChecker fieldLabels fieldTypes dcName tyConName = do
    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
                fieldTypeStr = pack $ showSDocUnsafe $ ppr $ TyCo.scaledThing fieldType
                isMaybe = isMaybeType (TyCo.scaledThing fieldType)
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName
                isSingleField = length fieldLabels == 1
                packageName = case nameModule_maybe tyConName of
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnit mod
                    Nothing -> "this"
                fullyQualifiedType = modName <> "." <> typeName

            return FieldDefinition
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
        else return []
#else
extractFieldsForCurrentPackage :: Text -> Text -> Text -> Bool -> [FieldLabel] -> [Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage modName typeName typeConstructor hasFieldChecker fieldLabels fieldTypes dcName tyConName = do
    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
                fieldTypeStr = pack $ showSDocUnsafe $ ppr fieldType
                isMaybe = isMaybeType fieldType
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName
                isSingleField = length fieldLabels == 1
                packageName = case nameModule_maybe tyConName of
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnitId mod
                    Nothing -> "this"
                fullyQualifiedType = modName <> "." <> typeName

            return FieldDefinition
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
        else return []
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
