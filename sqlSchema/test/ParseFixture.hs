{-# LANGUAGE CPP #-}

-- | Parses a fixture module in-process with GHC's parser.  No GHC session,
-- libdir or package database is needed, so the suite also runs in a
-- nixpkgs checkPhase, where this package is not registered yet.
module ParseFixture (parseFixture, parseFixtureHaddock) where

import           Control.Monad         (when)
import           Data.Char             (isSpace, toUpper)
import           Data.List             (isPrefixOf, stripPrefix, tails)
import qualified GHC.Data.EnumSet      as ES
import           GHC.Data.FastString   (mkFastString)
import           GHC.Data.StringBuffer (stringToStringBuffer)
import           GHC.Driver.Session    (FlagSpec (..), Language (..), impliedXFlags,
                                        languageExtensions, xFlags)
import           GHC.Hs                (GhcPs, HsModule (..), LHsDecl)
import           GHC.Parser            (parseModule)
import           GHC.Parser.Lexer      (PState (..), ParseResult (..), initParserState,
                                        mkParserOpts, unP)
import           GHC.Types.SrcLoc      (GenLocated (..), PsLoc (..), mkRealSrcLoc, srcLocCol,
                                        srcLocLine)
#if __GLASGOW_HASKELL__ >= 908
import           GHC.Parser.Lexer      (getPsErrorMessages)
import           GHC.Types.Error       (isEmptyMessages)
import           GHC.Utils.Error       (emptyDiagOpts)
#else
import           GHC.Data.Bag          (isEmptyBag)
import           GHC.Parser.Lexer      (getErrorMessages)
#endif

import           TestUtil              (failWith, isInfixOf)

-- | @parseFixture name extensions source@ parses @source@ as a Haskell2010
-- module with the named extensions (spelled as for @-X@) and returns its
-- declarations; @name@ labels locations and failures.
parseFixture :: String -> [String] -> String -> IO [LHsDecl GhcPs]
parseFixture = parseFixtureWith False

-- | 'parseFixture' keeping Haddock comments, as @-haddock@ does (nixpkgs
-- passes it to library builds): a documented type becomes an 'HsDocTy'.
parseFixtureHaddock :: String -> [String] -> String -> IO [LHsDecl GhcPs]
parseFixtureHaddock = parseFixtureWith True

parseFixtureWith :: Bool -> String -> [String] -> String -> IO [LHsDecl GhcPs]
parseFixtureWith haddock name extNames src = do
  -- The bare parser skips header pragmas, so an extension set there would
  -- silently not apply.
  when ("LANGUAGE" `isInfixOf` src || any optionPragma (tails src)) $
    failWith (name <> ": a fixture must not set extensions in a pragma; pass them to parseFixture")
  exts <- mapM extension extNames
  let enabled = closeImplied (languageExtensions (Just Haskell2010) ++ exts)
#if __GLASGOW_HASKELL__ >= 908
      opts    = mkParserOpts (ES.fromList enabled) emptyDiagOpts [] False haddock False False
#else
      opts    = mkParserOpts ES.empty (ES.fromList enabled) False haddock False False
#endif
      start   = initParserState opts (stringToStringBuffer src) (mkRealSrcLoc (mkFastString name) 1 1)
  case unP parseModule start of
    POk st (L _ m)
      | hasErrors st -> failWith (name <> ": the parser reported errors")
      | otherwise    -> pure (hsmodDecls m)
    PFailed st ->
      let at = psRealLoc (loc st)
      in failWith (name <> ": parse failed at " <> show (srcLocLine at) <> ":" <> show (srcLocCol at))
  where
    -- Case-insensitive, like GHC: {-# language … #-} and {-# OPTIONS_GHC -X… #-}.
    optionPragma s = case stripPrefix "{-#" s of
      Just rest ->
        let word = map toUpper (takeWhile (not . isSpace) (dropWhile isSpace rest))
        in word == "LANGUAGE" || "OPTIONS" `isPrefixOf` word
      Nothing -> False

    extension n = case [flagSpecFlag f | f <- xFlags, flagSpecName f == n] of
      [e] -> pure e
      _   -> failWith (name <> ": unknown extension " <> show n)

    -- DynFlags closes the set under impliedXFlags; mkParserOpts does not.
    -- The turn-off entries only clear ImplicitPrelude and CUSKs, which the
    -- parser never reads.
    closeImplied xs =
      let xs' = foldr imply xs impliedXFlags
      in if length xs' == length xs then xs else closeImplied xs'
    imply (a, on, b) acc
      | on && a `elem` acc && b `notElem` acc = b : acc
      | otherwise                             = acc

#if __GLASGOW_HASKELL__ >= 908
    hasErrors = not . isEmptyMessages . getPsErrorMessages
#else
    hasErrors = not . isEmptyBag . getErrorMessages
#endif
