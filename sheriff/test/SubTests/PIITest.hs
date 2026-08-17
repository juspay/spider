{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- Test corpus for the "PII Rule" TypeBlockedRule in .juspay/sheriffRules.yaml.
-- The rule blocks `show`/`encode`/`toJSON` on any value whose type is, or
-- transitively contains, `PII`.
--
-- As with the other sheriff test modules, this file is expected NOT to compile:
-- every line marked "Should throw error" must produce a sheriff error, and no
-- line marked "Should not throw error" may produce one.
module SubTests.PIITest where

import qualified Sheriff.Plugin ()
import Data.Aeson (ToJSON (..), Value, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import GHC.Generics (Generic)

-- Data Types Declarations

-- The marker type: a newtype over encrypted Text
newtype PII = PII Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type AliasPII = PII

-- Direct containment
data A = A {var1 :: PII, other :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Transitive containment: B -> A -> PII
data B = B {var0 :: A}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Containment through a list
data C = C {cs :: [B]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Containment through Maybe and a tuple
data D = D {md :: Maybe (Int, C)}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Positional (non record) constructor
data E = E Int PII
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Sum type where only one constructor carries PII
data F = F1 Int | F2 PII
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Parameterised container instantiated at PII
newtype Wrapper a = Wrapper a
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Recursive type carrying PII: the walker must both terminate and still detect it
data RecPII = RecPII {rself :: Maybe RecPII, rpii :: PII}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Recursive type with no PII: the walker must terminate and report nothing
data Rec = Rec {self :: Maybe Rec, txt :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype NoPII = NoPII Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Redacting wrapper, listed in the rule's `type_blocked_rule_ignore_types`
newtype MaskedPII = MaskedPII PII

instance Show MaskedPII where
  show _ = "***"

instance ToJSON MaskedPII where
  toJSON _ = toJSON ("***" :: Text)

-- A record reachable only through the redacting wrapper
data G = G {gMasked :: MaskedPII, gName :: Text}

instance ToJSON G where
  toJSON g = toJSON (gName g)

-- Values

piiVal :: PII
piiVal = PII "encrypted"

aliasPiiVal :: AliasPII
aliasPiiVal = PII "encrypted"

aVal :: A
aVal = A {var1 = piiVal, other = "plain"}

bVal :: B
bVal = B {var0 = aVal}

cVal :: C
cVal = C {cs = [bVal]}

dVal :: D
dVal = D {md = Just (1, cVal)}

eVal :: E
eVal = E 1 piiVal

fVal :: F
fVal = F2 piiVal

wrappedPii :: Wrapper PII
wrappedPii = Wrapper piiVal

wrappedInt :: Wrapper Int
wrappedInt = Wrapper 1

recPiiVal :: RecPII
recPiiVal = RecPII {rself = Nothing, rpii = piiVal}

recVal :: Rec
recVal = Rec {self = Nothing, txt = "plain"}

noPiiVal :: NoPII
noPiiVal = NoPII "plain"

maskedVal :: MaskedPII
maskedVal = MaskedPII piiVal

gVal :: G
gVal = G {gMasked = maskedVal, gName = "plain"}

-- Violations: the type IS PII

-- Should throw error
showDirect :: String
showDirect = show piiVal

-- Should throw error
encodeDirect :: BSL.ByteString
encodeDirect = encode piiVal

-- Should throw error : type synonym must be expanded before matching
encodeAlias :: BSL.ByteString
encodeAlias = encode aliasPiiVal

-- Violations: the type CONTAINS PII

-- Should throw error : A -> var1 :: PII
encodeRecord :: BSL.ByteString
encodeRecord = encode aVal

-- Should throw error : B -> var0 :: A -> var1 :: PII
encodeNestedRecord :: BSL.ByteString
encodeNestedRecord = encode bVal

-- Should throw error : C -> cs :: [B] -> B -> var0 :: A -> var1 :: PII
encodeThroughList :: BSL.ByteString
encodeThroughList = encode cVal

-- Should throw error : D -> md :: Maybe (Int, C) -> ... -> PII
encodeThroughMaybeAndTuple :: BSL.ByteString
encodeThroughMaybeAndTuple = encode dVal

-- Should throw error : positional constructor field
encodePositional :: BSL.ByteString
encodePositional = encode eVal

-- Should throw error : only one constructor of the sum type carries PII
encodeSumType :: BSL.ByteString
encodeSumType = encode fVal

-- Should throw error : PII supplied as a type argument
encodeTypeArgument :: BSL.ByteString
encodeTypeArgument = encode wrappedPii

-- Should throw error : recursive type that does carry PII
encodeRecursiveWithPii :: BSL.ByteString
encodeRecursiveWithPii = encode recPiiVal

-- Should throw error : structural nesting at the call site
encodeMaybe :: BSL.ByteString
encodeMaybe = encode (Just piiVal)

-- Should throw error : list at the call site
encodeList :: BSL.ByteString
encodeList = encode [piiVal]

-- Should throw error : tuple at the call site
encodeTuple :: BSL.ByteString
encodeTuple = encode (1 :: Int, piiVal)

-- Should throw error : toJSON is blocked as well
toJSONNested :: Value
toJSONNested = toJSON bVal

-- Should throw error : applied through `$`
encodeThroughDollar :: BSL.ByteString
encodeThroughDollar = encode $ bVal

-- Non violations

-- Should not throw error : no PII anywhere in the type
encodeNoPii :: BSL.ByteString
encodeNoPii = encode noPiiVal

-- Should not throw error : recursive type with no PII must terminate cleanly
encodeRecursiveNoPii :: BSL.ByteString
encodeRecursiveNoPii = encode recVal

-- Should not throw error : phantom-free container instantiated at a safe type
encodeSafeTypeArgument :: BSL.ByteString
encodeSafeTypeArgument = encode wrappedInt

-- Should not throw error : MaskedPII is in the rule's ignore_types
encodeMasked :: BSL.ByteString
encodeMasked = encode maskedVal

-- Should not throw error : PII is only reachable through the redacting wrapper
encodeThroughMasked :: BSL.ByteString
encodeThroughMasked = encode gVal

-- Should not throw error : this function is in the rule's ignore_functions
serialiseForVault :: PII -> BSL.ByteString
serialiseForVault p = encode p

-- Should not throw error : plain types are untouched
encodePlainInt :: BSL.ByteString
encodePlainInt = encode (1 :: Int)

main :: IO ()
main = do
  BSL.putStr encodeNoPii
  BSL.putStr encodePlainInt
