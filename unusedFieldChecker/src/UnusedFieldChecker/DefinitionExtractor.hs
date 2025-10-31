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
import GHC.Utils.Outputable hiding ((<>))
#else
import DataCon
import DynFlags
import FastString
import FieldLabel
import GHC
import GhcPlugins hiding ((<>))
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
import UnusedFieldChecker.Types

-- | Extract field definitions from type-checked environment
extractFieldDefinitions :: Text -> TcGblEnv -> TcM [FieldDefinition]
extractFieldDefinitions modName tcEnv = do
    let tyCons = extractTyCons tcEnv
    concat <$> mapM (extractFieldsFromTyCon modName) tyCons

extractFieldsFromTyCon :: Text -> TyCon -> TcM [FieldDefinition]
extractFieldsFromTyCon modName tc
    | isAlgTyCon tc && not (isClassTyCon tc) = do
        let dataCons = tyConDataCons tc
            typeName = pack $ showSDocUnsafe $ ppr $ tyConName tc
            typeConstructor = pack $ nameStableString $ tyConName tc
        concat <$> mapM (extractFieldsFromDataCon modName typeName typeConstructor) dataCons
    | otherwise = return []

extractFieldsFromDataCon :: Text -> Text -> Text -> DataCon -> TcM [FieldDefinition]
extractFieldsFromDataCon modName typeName typeConstructor dc = do
    let fieldLabels = dataConFieldLabels dc
        fieldTypes = dataConRepArgTys dc
        dcName = getName dc
        tyConName = getName $ dataConTyCon dc

    if not (null fieldLabels) && length fieldLabels == length fieldTypes
        then forM (zip fieldLabels fieldTypes) $ \(label, fieldType) -> do
            let fieldName = pack $ unpackFS $ flLabel label
#if __GLASGOW_HASKELL__ >= 900
                fieldTypeStr = pack $ showSDocUnsafe $ ppr $ TyCo.scaledThing fieldType
                isMaybe = isMaybeType (TyCo.scaledThing fieldType)
#else
                fieldTypeStr = pack $ showSDocUnsafe $ ppr fieldType
                isMaybe = isMaybeType fieldType
#endif
                location = pack $ showSDocUnsafe $ ppr $ getSrcSpan dcName

                -- Extract package name from the type constructor
                packageName = case nameModule_maybe tyConName of
#if __GLASGOW_HASKELL__ >= 900
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnit mod
#else
                    Just mod -> pack $ showSDocUnsafe $ ppr $ moduleUnitId mod
#endif
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

extractTyCons :: TcGblEnv -> [TyCon]
extractTyCons tcEnv = 
    let tcs = tcg_tcs tcEnv
    in filter isSafeTyCon tcs
  where
    isSafeTyCon tc = 
        not (isClassTyCon tc) &&
        not (isPromotedDataCon tc) &&
        not (isTcTyCon tc)
