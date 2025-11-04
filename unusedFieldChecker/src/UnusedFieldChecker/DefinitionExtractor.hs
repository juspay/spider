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
import GHC.Data.FastString
import GHC.Tc.Types
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.SrcLoc
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
import Outputable
import SrcLoc
import TcRnTypes
import TyCon
import TyCoRep
import Type
#endif

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import UnusedFieldChecker.Types

-- | Extract field definitions from type-checked environment
-- Only extracts definitions for types from the current package
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
        let dataCons = tyConDataCons tc
            typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
            typeConstructor = pack $ nameStableString $ tyConName tc
        concat <$> mapM (extractFieldsFromDataCon modName currentPkgName typeName typeConstructor) dataCons
    | otherwise = return []

extractFieldsFromDataCon :: Text -> Text -> Text -> Text -> DataCon -> TcM [FieldDefinition]
extractFieldsFromDataCon modName currentPkgName typeName typeConstructor dc = do
    let fieldLabels = dataConFieldLabels dc
        fieldTypes = dataConRepArgTys dc
        dcName = getName dc
        tyConName = getName $ dataConTyCon dc
    
    let packagePattern = "$" <> currentPkgName <> "-"
        isCurrentPackage = packagePattern `T.isPrefixOf` typeConstructor
    
    if not isCurrentPackage
        then return []  -- Early exit for external packages
        else extractFieldsForCurrentPackage modName typeName typeConstructor fieldLabels fieldTypes dcName tyConName

#if __GLASGOW_HASKELL__ >= 900
extractFieldsForCurrentPackage :: Text -> Text -> Text -> [FieldLabel] -> [Scaled Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage modName typeName typeConstructor fieldLabels fieldTypes dcName tyConName = do
    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
                fieldTypeStr = pack $ showSDocUnsafe $ ppr $ TyCo.scaledThing fieldType
                isMaybe = isMaybeType (TyCo.scaledThing fieldType)
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName

                -- Extract package name from the type constructor
                packageName = case nameModule_maybe tyConName of
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnit mod
                    Nothing -> "this"

                -- Create fully qualified type name
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
                }
        else return []
#else
extractFieldsForCurrentPackage :: Text -> Text -> Text -> [FieldLabel] -> [Type] -> Name -> Name -> TcM [FieldDefinition]
extractFieldsForCurrentPackage modName typeName typeConstructor fieldLabels fieldTypes dcName tyConName = do
    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
                fieldTypeStr = pack $ showSDocUnsafe $ ppr fieldType
                isMaybe = isMaybeType fieldType
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName

                -- Extract package name from the type constructor
                packageName = case nameModule_maybe tyConName of
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnitId mod
                    Nothing -> "this"

                -- Create fully qualified type name
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
