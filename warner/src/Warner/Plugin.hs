{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase,RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-error=unused-imports -Wno-error=unused-top-binds #-}

module Warner.Plugin (plugin) where
#if __GLASGOW_HASKELL__ >= 900
import Control.Monad
import Data.Text.Encoding (encodeUtf8)
import GHC
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Tc.Types
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Outputable (reallyAlwaysQualify,text,showSDocUnsafe,ppr,withPprStyle,mkErrStyle,renderWithContext,defaultUserStyle)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
#else
-- import TyCoRep
-- import DataCon
-- import Bag (bagToList)
-- import DynFlags ()
-- import GHC
-- import TcType
-- import BasicTypes
import GhcPlugins --(splitAppTys,splitFunTys,tyConName,rdrNameOcc,occNameString,RdrName (..),HsParsedModule, Hsc, Plugin (..), PluginRecompile (..), Var (..), getOccString, hpm_module, ppr, showSDocUnsafe)
-- import HscTypes (msHsFilePath)
-- import Name (nameStableString)
-- import Outputable ()
-- import Plugins (CommandLineOption, defaultPlugin)
-- import TcRnTypes (TcGblEnv (..), TcM)
#endif


import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (intercalate ,foldl')
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import qualified Data.ByteString as DBS
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing,doesFileExist)
import System.Environment

plugin :: Plugin
plugin =
    defaultPlugin
        {
        pluginRecompile = (\_ -> return NoForceRecompile)
#if __GLASGOW_HASKELL__ >= 900
        , typeCheckResultAction = fixedLengthListAction
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

#if __GLASGOW_HASKELL__ >= 900
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
            -- liftIO $ DBS.appendFile (modulePath <> "-.json") (DBS.toStrict $ encodePretty ("invoked" :: String))
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
        Nothing -> do
            liftIO $ print ("Warner : MOD summary is Nothing" :: String)
            pure ()
    return modGuts
    where
        getWarnings :: Hsc WarningMessages
        getWarnings = Hsc $ \_ w -> return (w, w)

        clearWarnings :: Hsc ()
        clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)

data CliOptions = CliOptions {
    error :: Maybe Bool
} deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

-- defaultCliOptions :: CliOptions
-- defaultCliOptions = CliOptions {error = Just False}

fixedLengthListAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fixedLengthListAction opts _ tcEnv = do
    forM_ (bagToList $ tcg_binds tcEnv) checkModule
    return tcEnv
    where
        checkModule :: LHsBindLR GhcTc GhcTc -> TcM ()
        checkModule (L _ AbsBinds{abs_binds = binds}) = do
            forM_ (bagToList binds) checkModule
        checkModule x = checkBind x

        checkValBinds :: HsValBinds GhcTc -> TcM ()
        checkValBinds = \case
            ValBinds _ binds _ -> 
                mapBagM_ checkBind binds
            XValBindsLR (NValBinds binds _) -> 
                forM_ binds $ \(_, bagBinds) -> 
                mapBagM_ checkBind bagBinds

        checkBind :: LHsBind GhcTc -> TcM ()
        checkBind (L _ bind) = case bind of
            FunBind{fun_matches = matches} -> 
                checkMatchGroup matches
            --   PatBind{pat_lhs = pat, pat_rhs = grhss} -> do
            --     checkPattern pat
            --     checkGRHSs grhss
            _ -> return ()
        -- checkBind _ = pure ()

        checkMatchGroup :: (MatchGroup GhcTc (LHsExpr GhcTc)) -> TcM ()
        checkMatchGroup ((MG _ (L _ matches) _)) = 
            forM_ matches $ \(L _ match) -> do
                forM_ (m_pats match) checkPattern
                forM_ (grhssGRHSs $ m_grhss match) checkGRHSs

        -- checkGRHSs :: (GRHS GhcTc (LHsExpr GhcTc)) -> TcM ()
        checkGRHSs (L _ (GRHS _ _ rhs)) =
            checkExpr rhs
        checkGRHSs _ = pure ()

        checkExpr :: LHsExpr GhcTc -> TcM ()
        checkExpr x@(L _ expr) = case expr of
            HsCase _ scrut matches -> do
                checkCaseScrutinee (getLocA x) scrut
                checkExpr scrut
                checkMatchGroup matches
            HsLet _ binds body -> do
                checkLocalBinds binds
                checkExpr body
            HsLam _ matches -> 
                checkMatchGroup matches
            HsApp _ e1 e2 -> do
                checkExpr e1
                checkExpr e2
            HsAppType _ e _ -> 
                checkExpr e
            OpApp _ e1 op e2 -> do
                checkExpr e1
                checkExpr op
                checkExpr e2
            NegApp _ e _ -> 
                checkExpr e
            HsPar _ e -> 
                checkExpr e
            SectionL _ e1 e2 -> do
                checkExpr e1
                checkExpr e2
            SectionR _ e1 e2 -> do
                checkExpr e1
                checkExpr e2
            HsIf _ cond then_ else_ -> do
                checkExpr cond
                checkExpr then_
                checkExpr else_
            HsMultiIf _ grhs -> 
                forM_ grhs $ \(L _ (GRHS _ _ e)) -> 
                checkExpr e
            HsDo _ _ (L _ stmts) -> 
                checkStmts stmts
            ExplicitList _ elems ->
                mapM_ checkExpr elems
            RecordCon _ _ (HsRecFields fields _) -> 
                forM_ fields $ \(L _ field) -> 
                checkExpr (hsRecFieldArg field)
            --   RecordUpd _ e fields -> do
            --     checkExpr e
            --     forM_ fields $ \(L _ field) -> 
            --       checkExpr (hsRecFieldArg field)
            ExprWithTySig _ e _ -> 
                checkExpr e
            ArithSeq _ _ info -> 
                case info of
                From e -> checkExpr e
                FromThen e1 e2 -> do
                    checkExpr e1
                    checkExpr e2
                FromTo e1 e2 -> do
                    checkExpr e1
                    checkExpr e2
                FromThenTo e1 e2 e3 -> do
                    checkExpr e1
                    checkExpr e2
                    checkExpr e3
            HsBracket _ _ -> 
                return ()
            --   HsRnBracketOut _ _ _ -> 
            --     return ()
            HsTcBracketOut _ _ _ _ -> 
                return ()
            HsSpliceE _ _ -> 
                return ()
            HsProc _ pat body -> do
                checkPattern pat
                checkCmdTop body
            HsStatic _ e -> 
                checkExpr e
            --   HsArrApp _ e1 e2 _ _ -> do
            --     checkExpr e1
            --     checkExpr e2
            --   HsArrForm _ e _ cmds -> do
            --     checkExpr e
            --     mapM_ checkCmd cmds
            HsTick _ _ e -> 
                checkExpr e
            HsBinTick _ _ _ e -> 
                checkExpr e
            --   HsTickPragma _ _ _ e -> 
            --     checkExpr e
            _ -> 
                return ()

        checkCmd :: LHsCmd GhcTc -> TcM ()
        checkCmd (L _ cmd_) = case cmd_ of
            HsCmdArrApp _ e1 e2 _ _ -> do
                checkExpr e1
                checkExpr e2
            HsCmdArrForm _ e _ _ cmdTop -> do
                checkExpr e
                forM_ cmdTop (\(L _ (HsCmdTop _ cmd)) -> checkCmd cmd)
            HsCmdApp _ c e -> do
                checkCmd c
                checkExpr e
            HsCmdLam _ matches -> 
                checkCmdMatchGroup matches
            HsCmdPar _ c -> 
                checkCmd c
            HsCmdCase _ e matches -> do
                checkExpr e
                checkCmdMatchGroup matches
            HsCmdIf _ _ e c1 c2 -> do
                checkExpr e
                checkCmd c1
                checkCmd c2
            HsCmdLet _ binds c -> do
                checkLocalBinds binds
                checkCmd c
            HsCmdDo _ (L _ stmts) -> 
                checkCmdStmts stmts
            _ -> 
                return ()

        checkCmdTop :: LHsCmdTop GhcTc -> TcM ()
        checkCmdTop (L _ (HsCmdTop _ cmd)) = checkCmd cmd

        checkCmdMatchGroup :: (MatchGroup GhcTc (LHsCmd GhcTc)) -> TcM ()
        checkCmdMatchGroup ((MG _ (L _ matches) _)) = 
            forM_ matches $ \(L _ match) -> do
                forM_ (m_pats match) checkPattern
                -- forM_ (grhssLocalBinds $ m_grhss match) checkLocalBinds
                (\(Match _ _ _ (GRHSs _ grhss _)) -> forM_ grhss (\(L _ (GRHS _ _ body)) -> checkCmd body)) (match)

        checkCmdStmts :: [LStmt GhcTc (LHsCmd GhcTc)] -> TcM ()
        checkCmdStmts = mapM_ $ \(L _ stmt) -> case stmt of
            BindStmt _ pat cmd -> do
                checkPattern pat
                checkCmd cmd
            LastStmt _ cmd _ _ -> 
                checkCmd cmd
            BodyStmt _ cmd _ _ -> 
                checkCmd cmd
            LetStmt _ binds -> 
                checkLocalBinds (binds)
            RecStmt {} -> 
                return ()
            _ -> 
                return ()

        checkCaseScrutinee :: SrcSpan -> LHsExpr GhcTc -> TcM ()
        checkCaseScrutinee loc scrut = case unLoc scrut of
            ExplicitList _ elems -> 
                when (length elems > 1) $ do
                reportError loc
            _ -> return ()  -- Only interested in explicit list literals

        checkStmts :: [LStmt GhcTc (LHsExpr GhcTc)] -> TcM ()
        checkStmts = mapM_ $ \(L _ stmt) -> case stmt of
            BindStmt _ pat expr -> do
                checkPattern pat
                checkExpr expr
            BodyStmt _ expr _ _ -> 
                checkExpr expr
            LetStmt _ binds -> 
                checkLocalBinds (binds)
            LastStmt _ expr _ _ -> 
                checkExpr expr
            ParStmt _ blocks _ _ -> 
                forM_ blocks $ \((ParStmtBlock _ stmts _ _)) -> 
                checkStmts stmts
            TransStmt {} -> 
                return ()
            RecStmt {} -> 
                return ()
            _ -> 
                return ()

        checkLocalBinds :: (HsLocalBinds GhcTc) -> TcM ()
        checkLocalBinds (binds) = case binds of
            HsValBinds _ valBinds -> 
                checkValBinds valBinds
            HsIPBinds _ _ -> 
                return ()
            EmptyLocalBinds _ -> 
                return ()

        checkPattern :: LPat GhcTc -> TcM ()
        checkPattern (L _ pat) = case pat of
            ListPat _ pats -> 
                mapM_ checkPattern pats
            --   ConPatIn _ details -> do
            --     checkConDetails details
            --   ConPatOut {} -> 
            --     return ()
            ViewPat _ expr p -> do
                checkExpr expr
                checkPattern p
            SplicePat {} -> 
                return ()
            LitPat _ _ -> 
                return ()
            NPat {} -> 
                return ()
            NPlusKPat {} -> 
                return ()
            SigPat _ p _ -> 
                checkPattern p
            AsPat _ _ p -> 
                checkPattern p
            TuplePat _ pats _ -> 
                mapM_ checkPattern pats
            SumPat _ p _ _ -> 
                checkPattern p
            BangPat _ p -> 
                checkPattern p
            LazyPat _ p -> 
                checkPattern p
            ParPat _ p -> 
                checkPattern p
            VarPat {} -> 
                return ()
            WildPat {} -> 
                return ()
            _ -> 
                return ()

        -- checkConDetails :: HsConPatDetails GhcTc -> TcM ()
        -- checkConDetails (PrefixCon _ args) = 
        --     mapM_ checkPattern args
        -- checkConDetails (RecCon (HsRecFields fields _)) = 
        --     forM_ fields $ \(L _ field) ->
        --         checkPattern (hsRecFieldArg field)
        -- checkConDetails (InfixCon p1 p2) = do
        --     checkPattern p1
        --     checkPattern p2

        reportError :: SrcSpan -> TcM ()
        reportError loc = do
            let shouldThrowError = case opts of
                    [] ->  False
                    (local : _) ->
                                case A.decode $ BL.fromStrict $ encodeUtf8 $ T.pack local of
                                    Just (CliOptions{error=error_}) -> fromMaybe False error_
                                    Nothing -> False
            let errorMessage = "Case matching on a fixed-length list literal is not allowed. Use a tuple or cons to pattern match in case scrutinee "
            -- let errorMsg = (loc, errorMessage)
            let errorMessages = listToBag [
                        if (shouldThrowError == True)
                            then mkErr loc reallyAlwaysQualify (mkDecorated [text errorMessage])
                            else mkWarnMsg loc reallyAlwaysQualify (text errorMessage)
                    ]
            if shouldThrowError
                then throwErrors errorMessages
                else do
                    dflags <- getDynFlags
                    logger <- getLogger
                    liftIO $ printOrThrowWarnings logger dflags errorMessages
#endif