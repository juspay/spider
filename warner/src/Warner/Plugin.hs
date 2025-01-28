{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Warner.Plugin (plugin) where
#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.Errors
import GHC.Driver.Session
import GHC.Data.Bag
import GHC.Types.Error
-- import GHC.Core.TyCo.Rep
-- import GHC.Core.TyCon
-- import GHC.Core.DataCon
-- import GHC.Hs.Pat
import GHC.Unit.Types
import GHC
-- import GHC.Types.SourceText
import GHC.Driver.Plugins
import GHC.Types.SrcLoc
-- import GHC.Types.Name.Reader
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,withPprStyle,mkErrStyle,renderWithContext,defaultUserStyle)
-- import GHC.Types.Var
-- import qualified Data.Aeson.KeyMap as HM
import GHC.Unit.Module.ModGuts
import GHC.Data.FastString
import GHC.Types.SourceError
import GHC.Utils.Error
#endif

import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as DBS
import System.Directory (createDirectoryIfMissing,doesFileExist)
import Data.List (intercalate ,foldl')
import Data.List.Extra (splitOn)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import GHC.IO (unsafePerformIO)
-- import Text.Read (readMaybe)
import System.Environment
import GHC.Generics (Generic)
-- import Control.Monad


plugin :: Plugin
plugin =
    defaultPlugin
        {
        pluginRecompile = (\_ -> return NoForceRecompile)
#if __GLASGOW_HASKELL__ >= 900
        , desugarResultAction = handleWarns
#endif
        }

cacheExistingWarnings :: Bool
cacheExistingWarnings = readBool $ unsafePerformIO $ lookupEnv "CACHE_WARNINGS"
    where
        readBool :: Maybe String -> Bool
        readBool (Just "true") = True
        readBool (Just "True") = True
        readBool (Just "TRUE") = True
        readBool _ = False

-- NOT ADDING Span
data MessageFingerprint = MessageFingerprint {
    diagnostic :: String
    , reason   :: Value
    , severity :: String
    , srcSpan :: String
}
    deriving (Generic,Show,ToJSON,FromJSON)

instance Eq MessageFingerprint where
    (MessageFingerprint d1 r1 s1 _) == (MessageFingerprint d2 r2 s2 _) =
        d1 == d2 && r1 == r2 && s1 == s2

getAllWarningFlags :: [WarningFlag]
getAllWarningFlags = enumFrom Opt_WarnDuplicateExports

instance ToJSON WarningFlag where
    toJSON flag = String $ T.pack $ show flag

instance FromJSON WarningFlag where
    parseJSON = withText "WarningFlag" $ \t -> do
        let flagList = filter (\x -> (show x) == (T.unpack t)) getAllWarningFlags
        if null flagList
            then fail $ "Invalid WarningFlag: " ++ T.unpack t
            else pure $ head flagList

instance ToJSON WarnReason where
    toJSON NoReason = object [
        "type" .= ("NoReason" :: Text)
        ]
    toJSON (Reason flag) = object [
        "type" .= ("Reason" :: Text),
        "flag" .= flag
        ]
    toJSON (ErrReason mbFlag) = object [
        "type" .= ("ErrReason" :: Text),
        "flag" .= mbFlag
        ]

instance FromJSON WarnReason where
    parseJSON = withObject "WarnReason" $ \v -> do
        typ <- v .: "type"
        case typ of
            "NoReason" -> pure NoReason
            "Reason" -> Reason <$> v .: "flag"
            "ErrReason" -> ErrReason <$> v .: "flag"
            _ -> fail $ "Unknown WarnReason type: " ++ T.unpack typ

createFingerprint :: DynFlags -> MsgEnvelope DecoratedSDoc -> MessageFingerprint
createFingerprint dflags msg = MessageFingerprint 
    { diagnostic = 
        let style = mkErrStyle (errMsgContext msg)
            ctx   = initSDocContext dflags style
        in renderWithContext (initSDocContext dflags defaultUserStyle) $ withPprStyle style (formatBulleted ctx (renderDiagnostic (errMsgDiagnostic msg)))
    , reason = toJSON $ errMsgReason msg
    , severity = show $ errMsgSeverity msg
    , srcSpan = showSDocUnsafe $ ppr $ errMsgSpan msg
    }

-- getCacheWarnings :: MsgEnvelope e -> MsgCache

makeIntoError :: MsgEnvelope e -> MsgEnvelope e
makeIntoError warning = warning {
    errMsgSeverity = SevError
    , errMsgReason = case errMsgReason warning of
                        Reason flag -> ErrReason (Just flag)
                        x -> x
    }

getReason :: MsgEnvelope e -> String
getReason warning =
    case errMsgReason warning of
        Reason flag -> show flag
        _ -> mempty

mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan mod_loc
  = case ml_hs_file mod_loc of
      Just file_path -> mkGeneralSrcSpan (mkFastString file_path)
      Nothing        -> interactiveSrcSpan

#if __GLASGOW_HASKELL__ >= 900
handleWarns :: [CommandLineOption] -> (Maybe ModSummary) -> TcGblEnv -> ModGuts -> Hsc ModGuts
handleWarns opts mModSummary _tcGblEnv modGuts = do
    case mModSummary of
        Just modSummary -> do
            let prefixPath = "./.juspay/existing-errors/"
                _moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                modulePath = prefixPath <> msHsFilePath modSummary
                _moduleSrcSpan = mkFileSrcSpan $ ms_location modSummary
                path = (intercalate "/" . init . splitOn "/") modulePath
                cacheWarningsList = ["Opt_WarnIncompletePatterns","Opt_WarnIncompleteUniPatterns","Opt_WarnIncompletePatternsRecUpd"] <> (concatMap ((splitOn ",")) opts)
            liftIO $ createDirectoryIfMissing True path

            warningsBag <- getWarnings

            clearWarnings
            -- THESE WOULD BE MOVED TO ERRORS IF THEY ARE NOT IN THE CACHE
            warningsList_ <- pure $ filter isWarningMessage $ bagToList warningsBag

            (warningsList,toBeErrors) <- pure $ foldl' (\(warnings,toBeErrors) x -> if ((getReason x) `elem` cacheWarningsList) then (warnings, [x] <> toBeErrors) else ([x] <> warnings,toBeErrors)) ([],[]) warningsList_

            -- DO NOT TOUCH ERRORS LIST
            errorsList <- pure $ filter isErrorMessage $ bagToList warningsBag

            dflags <- getDynFlags
            logger <- getLogger
            liftIO $ DBS.appendFile (modulePath <> "-.json") (DBS.toStrict $ encodePretty ("invoked" :: String))
            if cacheExistingWarnings
                then do
                    let cacheList = map (createFingerprint dflags) toBeErrors
                    liftIO $ DBS.writeFile (modulePath <> ".json") (DBS.toStrict $ encodePretty cacheList)
                    updatedWarningsBag <- pure $ listToBag $ errorsList <> warningsList <> (map makeIntoError toBeErrors)
                    if (any isErrorMessage errorsList)
                        then throwErrors updatedWarningsBag
                        else liftIO $ printOrThrowWarnings logger dflags updatedWarningsBag
                else do
                    fileExists <- liftIO $ doesFileExist (modulePath <> ".json")
                    whiteListedWarns <- if fileExists then liftIO $ DBS.readFile (modulePath <> ".json") else pure $ mempty
                    let (filteredToBeErrors,isCacheButNeedToMention) =
                            case decode (DBS.fromStrict whiteListedWarns) of
                                Just (l :: [MessageFingerprint]) -> 
                                    (filter (\x -> not ((createFingerprint dflags x) `elem` l)) toBeErrors,filter (\x -> ((createFingerprint dflags x) `elem` l)) toBeErrors)
                                Nothing -> (toBeErrors,mempty)
                    updatedWarningsBag <- pure $ listToBag $ errorsList <> warningsList <> (map makeIntoError filteredToBeErrors) <> isCacheButNeedToMention
                    if (any isErrorMessage (bagToList updatedWarningsBag))
                        then throwErrors updatedWarningsBag
                        else liftIO $ printOrThrowWarnings logger dflags updatedWarningsBag
        Nothing -> pure ()
    return modGuts
    where
        getWarnings :: Hsc WarningMessages
        getWarnings = Hsc $ \_ w -> return (w, w)

        clearWarnings :: Hsc ()
        clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)
#endif