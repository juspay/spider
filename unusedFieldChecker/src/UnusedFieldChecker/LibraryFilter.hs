{-# LANGUAGE OverloadedStrings #-}

module UnusedFieldChecker.LibraryFilter
    ( isLibraryTypeConstructor
    , isDerivedBinding
    ) where

import Data.Text (Text)
import qualified Data.Text as T

isLibraryTypeConstructor :: Text -> Bool
isLibraryTypeConstructor tc =
    any (`T.isPrefixOf` tc)
        [ "Data.Aeson.Types"
        , "GHC.Generics"
        , "Data.Data"
        , "GHC.Show"
        , "Text.Show"
        , "Data.Typeable"
        , "Control.Lens"
        , "GHC.Base"
        ]

isDerivedBinding :: Text -> Bool
isDerivedBinding name =
    any (`T.isPrefixOf` name)
        [ "$fToJSON"
        , "$fFromJSON"
        , "$fGeneric"
        , "$fShow"
        , "$fEq"
        , "$fOrd"
        , "$dm"
        , "$c"
        , "$W"
        , "toEncoding"
        , "toJSON"
        , "parseJSON"
        ]
    || "$" `T.isPrefixOf` name
