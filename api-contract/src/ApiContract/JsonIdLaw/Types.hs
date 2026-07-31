{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module ApiContract.JsonIdLaw.Types where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | A JSON object key as it appears in source (e.g. the LHS of @.=@ or the
-- argument of @.:@/@.:?@).
type JsonKey = Text

-- | The Haskell record field name a JSON key is associated with, when we can
-- infer it statically.
type FieldName = Text

-- | Which side of the round-trip a key was observed on.
data Side = EncodeSide | DecodeSide
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Information about a single observed JSON key.
data KeyInfo = KeyInfo
  { kiKey      :: !JsonKey
  -- ^ The JSON key literal as written in source.
  , kiField    :: !(Maybe FieldName)
  -- ^ The record field it maps to, if discoverable.
  , kiOptional :: !Bool
  -- ^ For decode keys: True if read via @.:?@/@.:!@ (i.e. the field is
  -- 'Maybe' and may legitimately be absent on encode). Always False for
  -- encode keys.
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | How a 'ToJSON'\/'FromJSON' instance was produced for a type.
data InstanceOrigin
  = CustomInstance
  -- ^ A hand-written @instance ToJSON T where toJSON = ...@.
  | DerivedPlain
  -- ^ @deriving (ToJSON, FromJSON)@, @GeneralizedNewtypeDeriving@, or
  -- @deriveJSON defaultOptions@. Law-abiding by construction; not checked.
  | DerivedWithOptions
  -- ^ @deriveToJSON opts@\/@deriveFromJSON opts@ with explicit (possibly
  -- non-identity) options, or @mkToJSON@\/@mkParseJSON@.
  | DerivedVia
  -- ^ @deriving (ToJSON) via V@ for some helper type @V@.
  | NotPresent
  -- ^ The instance does not exist (locally or in the instance env).
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Per-type summary of what we observed for the round-trip.
data TypeJsonInfo = TypeJsonInfo
  { tjiType        :: !Text
  -- ^ The type name (as pretty-printed).
  , tjiEncodeKeys  :: ![KeyInfo]
  -- ^ JSON keys written by @toJSON@\/@toEncoding@.
  , tjiDecodeKeys  :: ![KeyInfo]
  -- ^ JSON keys read by @parseJSON@.
  , tjiEncOrigin   :: !InstanceOrigin
  , tjiDecOrigin   :: !InstanceOrigin
  , tjiEncOpts      :: !(Maybe Text)
  -- ^ Pretty-printed @Options@ expression for the encode side, when
  -- 'tjiEncOrigin' is 'DerivedWithOptions'.
  , tjiDecOpts      :: !(Maybe Text)
  -- ^ Pretty-printed @Options@ expression for the decode side.
  , tjiEncVia       :: !(Maybe Text)
  -- ^ The @via@ type for 'DerivedVia' encode.
  , tjiDecVia       :: !(Maybe Text)
  -- ^ The @via@ type for 'DerivedVia' decode.
  , tjiDefinedHere  :: !Bool
  -- ^ Whether the type itself is defined in this module.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Errors emitted by the JSON ID-law check. These are all cases where the
-- @fromJSON . toJSON@ round-trip can statically be shown to lose data (or be
-- impossible).
data JsonIdLawError
  -- | A /required/ decode key (read via @.:@\/@.:!@) is never produced by the
  -- encoder. @fromJSON (toJSON x)@ will therefore fail to find the key.
  = KEY_ONLY_IN_DECODE
      { idlawType :: !Text
      , idlawKey  :: !JsonKey
      }
  -- | The same record field is encoded under one key but decoded from another
  -- (the classic snake_case-vs-camelCase data-loss bug).
  | KEY_CASE_MISMATCH
      { idlawType   :: !Text
      , idlawField  :: !FieldName
      , idlawEncKey :: !JsonKey
      , idlawDecKey :: !JsonKey
      }
  -- | The same JSON key maps to different record fields on encode vs decode,
  -- so the value round-trips into the wrong field.
  | FIELD_KEY_MISMATCH
      { idlawType    :: !Text
      , idlawKey      :: !JsonKey
      , idlawEncField :: !FieldName
      , idlawDecField :: !FieldName
      }
  -- | A JSON key is written by the encoder but never read by the decoder and
  -- does not correspond to any field of the type. Dead\/typo key that breaks
  -- the round-trip.
  | KEY_ONLY_IN_ENCODE
      { idlawType :: !Text
      , idlawKey  :: !JsonKey
      }
  -- | @deriveToJSON@\/@deriveFromJSON@ (or @mkToJSON@\/@mkParseJSON@) use different 'Options'
  -- (e.g. a different @fieldLabelModifier@), so encode keys != decode keys.
  | OPTIONS_MISMATCH
      { idlawType    :: !Text
      , idlawEncOpts :: !Text
      , idlawDecOpts :: !Text
      }
  -- | @deriving ... via V@ uses different via types\/options on the two sides.
  | OPTIONS_VIA_MISMATCH
      { idlawType   :: !Text
      , idlawEncVia :: !Text
      , idlawDecVia :: !Text
      }
  -- | A sum-type constructor tag value is encoded differently from what the
  -- decoder expects (e.g. encoder writes @"A"@ but decoder matches @"AA"@).
  | TAG_VALUE_MISMATCH
      { idlawType    :: !Text
      , idlawEncTag  :: !Text
      , idlawDecTag  :: !Text
      }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Pretty-print an error for the GHC diagnostic. The leading
-- @"[JsonIdLaw] "@ tag makes the check easy to identify in build logs.
generateJsonIdLawError :: JsonIdLawError -> String
generateJsonIdLawError (KEY_ONLY_IN_DECODE ty key) =
  "[JsonIdLaw] Data loss: the required JSON key '" <> T.unpack key
  <> "' is read by 'parseJSON' for type '" <> T.unpack ty
  <> "' but is never produced by 'toJSON'/'toEncoding'.\n"
  <> "\t'fromJSON (toJSON x)' will fail to find this key.\n"
  <> "\tEither add the key to the encoder or remove it from the decoder."

generateJsonIdLawError (KEY_CASE_MISMATCH ty field encKey decKey) =
  "[JsonIdLaw] Data loss: field '" <> T.unpack field
  <> "' of type '" <> T.unpack ty
  <> "' is encoded under the key '" <> T.unpack encKey
  <> "' but decoded from the key '" <> T.unpack decKey <> "'.\n"
  <> "\t'fromJSON (toJSON x)' will lose this field because the keys do not match.\n"
  <> "\tMake the encoder and decoder use the same JSON key for this field."

generateJsonIdLawError (FIELD_KEY_MISMATCH ty key encField decField) =
  "[JsonIdLaw] Data loss: JSON key '" <> T.unpack key
  <> "' for type '" <> T.unpack ty
  <> "' is bound to field '" <> T.unpack encField
  <> "' on encode but field '" <> T.unpack decField <> "' on decode.\n"
  <> "\tThe round-tripped value lands in the wrong field.\n"
  <> "\tMake both sides bind the same field for this key."

generateJsonIdLawError (KEY_ONLY_IN_ENCODE ty key) =
  "[JsonIdLaw] Data loss: JSON key '" <> T.unpack key
  <> "' is written by 'toJSON'/'toEncoding' for type '" <> T.unpack ty
  <> "' but is never read by 'parseJSON' and does not correspond to any field.\n"
  <> "\tThe decoder cannot recover this key; the round-trip is lossy.\n"
  <> "\tEither read the key in 'parseJSON' or remove it from the encoder."

generateJsonIdLawError (OPTIONS_MISMATCH ty encOpts decOpts) =
  "[JsonIdLaw] Data loss: the 'Options' used to derive 'ToJSON' and 'FromJSON' for type '"
  <> T.unpack ty <> "' differ, so the encode/decode JSON keys will not match.\n"
  <> "\tEncoder options: " <> T.unpack encOpts <> "\n"
  <> "\tDecoder options: " <> T.unpack decOpts <> "\n"
  <> "\tUse the same 'Options' (e.g. the same 'fieldLabelModifier') on both sides."

generateJsonIdLawError (OPTIONS_VIA_MISMATCH ty encVia decVia) =
  "[JsonIdLaw] Data loss: 'deriving via' uses different via types for 'ToJSON' and 'FromJSON' of type '"
  <> T.unpack ty <> "'.\n"
  <> "\tEncoder via: " <> T.unpack encVia <> "\n"
  <> "\tDecoder via: " <> T.unpack decVia <> "\n"
  <> "\tUse the same via type on both sides."

generateJsonIdLawError (TAG_VALUE_MISMATCH ty encTag decTag) =
  "[JsonIdLaw] Data loss: constructor tag '" <> T.unpack encTag
  <> "' is encoded for type '" <> T.unpack ty
  <> "' but the decoder expects '" <> T.unpack decTag <> "'.\n"
  <> "\t'fromJSON (toJSON x)' will fail to match this constructor.\n"
  <> "\tMake the encoder and decoder use the same tag value."
