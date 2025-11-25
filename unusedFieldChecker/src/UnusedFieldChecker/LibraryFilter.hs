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
        , "GHC.Generics"           -- Generic deriving
        , "Data.Data"              -- Data.Data instances
        , "GHC.Show"               -- Show instances
        , "Text.Show"              -- Show instances
        , "Data.Typeable"          -- Typeable instances
        , "Control.Lens"           -- Lens library types
        , "GHC.Base"               -- Base library types
        ]

isDerivedBinding :: Text -> Bool
isDerivedBinding name =
    any (`T.isPrefixOf` name)
        [ "$fToJSON"       -- ToJSON instance
        , "$fFromJSON"     -- FromJSON instance
        , "$fGeneric"      -- Generic instance
        , "$fShow"         -- Show instance
        , "$fEq"           -- Eq instance
        , "$fOrd"          -- Ord instance
        , "$dm"            -- Default method implementations
        , "$c"             -- Constructor wrappers
        , "$W"             -- Worker wrappers
        , "toEncoding"     -- Aeson-generated (when standalone)
        , "toJSON"         -- Aeson-generated (when standalone)
        , "parseJSON"      -- Aeson-generated (when standalone)
        ]
    || "$" `T.isPrefixOf` name -- Catch-all for other compiler-generated names
