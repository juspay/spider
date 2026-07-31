{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Positive control: a law-abiding type. Must compile cleanly under the plugin.
module IdLawTest.IDLawAllTestCases where

import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Data.Aeson.Types (Parser)
import Control.Applicative ((<|>), liftA2, empty)
import Control.Category ((<<<), (>>>))
import Control.Monad (when)
import Data.Text hiding (map, toLower, zip)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

----------- Happy Case -------------
data T = T { a :: Int, b :: Maybe Int }

instance ToJSON T where
  toJSON (T a' b') = object [ "a" .= a', "b" .= b' ]

instance FromJSON T where
  parseJSON = withObject "T" $ \o -> do
    a' <- o .: "a"
    b' <- o .:? "b"
    pure (T a' b')

---------------- Camel/Snake case issue -----------------
data A = A { myField :: Int }

instance ToJSON A where
  toJSON (A f) = object [ "my_field" .= f ]

instance FromJSON A where
  parseJSON = withObject "A" $ \o -> do
    myField <- o .: "myField"
    pure (A myField)

------------------- Missing field in toJSON ----------------
data B = B { a1 :: Int, b1 :: Int }

instance ToJSON B where
  toJSON (B a' _) = object [ "a" .= a' ]

instance FromJSON B where
  parseJSON = withObject "B" $ \o -> do
    a' <- o .: "a"
    b' <- o .: "b"
    pure (B a' b')

-------------- Missing field in fromJSON --------------
data C = C { field1 :: Text, field2 :: Maybe Int }

instance ToJSON C where
  toJSON (C a b) = object [ "field1" .= a, "field2" .= b ]

instance FromJSON C where
  parseJSON = withObject "C" $ \o -> do
    a <- o .: "field1"
    pure (C a Nothing)

---------------- Optional vs Required ----------------

data D = D { dField :: Maybe Int }

instance ToJSON D where
  toJSON (D x) = object ["dField" .= x]

instance FromJSON D where
  parseJSON = withObject "D" $ \o -> do
    dField <- o .: "dField"   -- required but nullable
    pure (D dField)

---------------- Extra field written by encoder ----------------

data E = E { eField :: Int }

instance ToJSON E where
  toJSON (E x) =
    object
      [ "eField" .= x
      , "debug" .= ("test" :: Text)
      ]

instance FromJSON E where
  parseJSON = withObject "E" $ \o -> do
    eField <- o .: "eField"
    pure (E eField)

---------------- Decoder invents constant ----------------

data F = F
  { f1 :: Int
  , f2 :: Int
  }

instance ToJSON F where
  toJSON (F a b) =
    object
      [ "f1" .= a
      , "f2" .= b
      ]

instance FromJSON F where
  parseJSON = withObject "F" $ \o -> do
    f1 <- o .: "f1"
    pure (F f1 999)

---------------- Different key names ----------------

data G = G { userId :: Int }

instance ToJSON G where
  toJSON (G x) =
    object ["user_id" .= x]

instance FromJSON G where
  parseJSON = withObject "G" $ \o -> do
    userId <- o .: "userid"
    pure (G userId)

---------------- Nested record: child field lost ----------------

data ChildH = ChildH
  { childA :: Int
  , childB :: Int
  }

instance ToJSON ChildH where
  toJSON (ChildH a b) =
    object
      [ "childA" .= a
      , "childB" .= b
      ]

instance FromJSON ChildH where
  parseJSON = withObject "ChildH" $ \o -> do
    childA <- o .: "childA"
    pure (ChildH childA 0)

data H = H
  { parentField :: Int
  , childField  :: ChildH
  }

instance ToJSON H where
  toJSON (H p c) =
    object
      [ "parentField" .= p
      , "childField" .= c
      ]

instance FromJSON H where
  parseJSON = withObject "H" $ \o -> do
    parentField <- o .: "parentField"
    childField <- o .: "childField"
    pure (H parentField childField)

---------------- Newtype wrapper ----------------

newtype I = I { unI :: Int }

instance ToJSON I where
  toJSON (I x) =
    object ["value" .= x]

instance FromJSON I where
  parseJSON = withObject "I" $ \o -> do
    unI <- o .: "val"
    pure (I unI)

---------------- Same field encoded twice logically ----------------

data J = J
  { firstName :: Text
  , lastName  :: Text
  }

instance ToJSON J where
  toJSON (J f l) =
    object
      [ "name" .= f
      , "surname" .= l
      ]

instance FromJSON J where
  parseJSON = withObject "J" $ \o -> do
    firstName <- o .: "name"
    lastName <- o .: "name"
    pure (J firstName lastName)

---------------- Decoder reads wrong field ----------------

data K = K
  { k1 :: Int
  , k2 :: Int
  }

instance ToJSON K where
  toJSON (K a b) =
    object
      [ "k1" .= a
      , "k2" .= b
      ]

instance FromJSON K where
  parseJSON = withObject "K" $ \o -> do
    k1 <- o .: "k1"
    k2 <- o .: "k1"
    pure (K k1 k2)

---------------- Extra decoder key ----------------

data L = L
  { l1 :: Int
  }

instance ToJSON L where
  toJSON (L x) =
    object ["l1" .= x]

instance FromJSON L where
  parseJSON = withObject "L" $ \o -> do
    l1 <- o .: "l1"
    (_extra :: Text) <- o .: "extra"
    pure (L l1)

---------------- List field omitted ----------------

data M = M
  { mName :: Text
  , mItems :: [Int]
  }

instance ToJSON M where
  toJSON (M n _) =
    object
      [ "mName" .= n
      ]

instance FromJSON M where
  parseJSON = withObject "M" $ \o -> do
    mName <- o .: "mName"
    mItems <- o .: "mItems"
    pure (M mName mItems)

---------------- Encoder writes wrong key ----------------

data N = N
  { nValue :: Int
  }

instance ToJSON N where
  toJSON (N x) =
    object ["nvalue" .= x]

instance FromJSON N where
  parseJSON = withObject "N" $ \o -> do
    nValue <- o .: "nValue"
    pure (N nValue)

---------------- Happy nested case ----------------

data O = O
  { o1 :: Int
  , o2 :: Maybe Text
  }

instance ToJSON O where
  toJSON (O a b) =
    object
      [ "o1" .= a
      , "o2" .= b
      ]

instance FromJSON O where
  parseJSON = withObject "O" $ \o -> do
    o1 <- o .: "o1"
    o2 <- o .:? "o2"
    pure (O o1 o2)

---------------- Constructor tag mismatch ----------------

data S1
  = S1A Int
  | S1B Text

instance ToJSON S1 where
  toJSON (S1A x) =
    object
      [ "tag" .= ("A" :: Text)
      , "value" .= x
      ]

  toJSON (S1B x) =
    object
      [ "tag" .= ("B" :: Text)
      , "value" .= x
      ]

instance FromJSON S1 where
  parseJSON = withObject "S1" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "AA" -> S1A <$> o .: "value"
      "B"  -> S1B <$> o .: "value"
      _    -> fail "invalid tag"

--------------------------------------------------------------------------------

---------------- Decoder supports only one constructor ----------------

data S2
  = LeftCtor Int
  | RightCtor Text

instance ToJSON S2 where
  toJSON (LeftCtor x) =
    object
      [ "tag" .= ("LeftCtor" :: Text)
      , "value" .= x
      ]

  toJSON (RightCtor x) =
    object
      [ "tag" .= ("RightCtor" :: Text)
      , "value" .= x
      ]

instance FromJSON S2 where
  parseJSON = withObject "S2" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "LeftCtor" -> LeftCtor <$> o .: "value"
      _          -> fail "unsupported"

--------------------------------------------------------------------------------

---------------- Constructor collapse ----------------

data S3
  = Email Text
  | Phone Text

instance ToJSON S3 where
  toJSON (Email x) =
    object
      [ "tag" .= ("email" :: Text)
      , "value" .= x
      ]

  toJSON (Phone x) =
    object
      [ "tag" .= ("phone" :: Text)
      , "value" .= x
      ]

instance FromJSON S3 where
  parseJSON = withObject "S3" $ \o -> do
    value <- o .: "value"
    pure (Email value)

--------------------------------------------------------------------------------
-- NEWTYPES
--------------------------------------------------------------------------------

---------------- Key mismatch ----------------

newtype N1 = N1
  { unN1 :: Int
  }

instance ToJSON N1 where
  toJSON (N1 x) =
    object
      [ "value" .= x
      ]

instance FromJSON N1 where
  parseJSON = withObject "N1" $ \o ->
    N1 <$> o .: "val"

--------------------------------------------------------------------------------

---------------- Constant decoder ----------------

newtype N2 = N2
  { unN2 :: Int
  }

instance ToJSON N2 where
  toJSON (N2 x) =
    object
      [ "value" .= x
      ]

instance FromJSON N2 where
  parseJSON _ =
    pure (N2 42)

--------------------------------------------------------------------------------
-- NESTED TYPES
--------------------------------------------------------------------------------

---------------- Nested key mismatch ----------------

data Child1 = Child1
  { childId :: Int
  }

instance ToJSON Child1 where
  toJSON (Child1 x) =
    object
      [ "child_id" .= x
      ]

instance FromJSON Child1 where
  parseJSON = withObject "Child1" $ \o ->
    Child1 <$> o .: "childId"

--------------------------------------------------------------------------------

data Parent1 = Parent1
  { parentId :: Int
  , childObj :: Child1
  }

instance ToJSON Parent1 where
  toJSON (Parent1 p c) =
    object
      [ "parentId" .= p
      , "childObj" .= c
      ]

instance FromJSON Parent1 where
  parseJSON = withObject "Parent1" $ \o -> do
    parentId <- o .: "parentId"
    childObj <- o .: "childObj"
    pure Parent1 {..}

--------------------------------------------------------------------------------

---------------- Child field silently dropped ----------------

data Child2X = Child2X
  { childAX :: Int
  , childBX :: Int
  }

instance ToJSON Child2X where
  toJSON (Child2X a b) =
    object
      [ "childA" .= a
      , "childB" .= b
      ]

instance FromJSON Child2X where
  parseJSON = withObject "Child2X" $ \o -> do
    childAX <- o .: "childA"
    pure (Child2X childAX 0)

--------------------------------------------------------------------------------
-- LISTS
--------------------------------------------------------------------------------

---------------- List field ignored ----------------

data Arr1 = Arr1
  { values :: [Int]
  }

instance ToJSON Arr1 where
  toJSON (Arr1 xs) =
    object
      [ "values" .= xs
      ]

instance FromJSON Arr1 where
  parseJSON = withObject "Arr1" $ \_ ->
    pure (Arr1 [])

--------------------------------------------------------------------------------

---------------- Required list missing ----------------

data Arr2 = Arr2
  { ids :: [Int]
  }

instance ToJSON Arr2 where
  toJSON _ =
    object []

instance FromJSON Arr2 where
  parseJSON = withObject "Arr2" $ \o ->
    Arr2 <$> o .: "ids"

--------------------------------------------------------------------------------
-- OPTIONALITY
--------------------------------------------------------------------------------

---------------- .: on Maybe ----------------
-- Should generally roundtrip for produced JSON.
-- Good sanity check that plugin does not over-report.

data Opt1 = Opt1
  { optField :: Maybe Int
  }

instance ToJSON Opt1 where
  toJSON (Opt1 x) =
    object
      [ "optField" .= x
      ]

instance FromJSON Opt1 where
  parseJSON = withObject "Opt1" $ \o -> do
    optField <- o .: "optField"
    pure Opt1 {..}

--------------------------------------------------------------------------------
-- FIELD COLLAPSE
--------------------------------------------------------------------------------

---------------- Two encoded fields become one ----------------

data Merge1X = Merge1X
  { mergeFirstName :: Text
  , mergeLastName  :: Text
  }

instance ToJSON Merge1X where
  toJSON (Merge1X f l) =
    object
      [ "firstName" .= f
      , "lastName" .= l
      ]

instance FromJSON Merge1X where
  parseJSON = withObject "Merge1X" $ \o -> do
    name <- o .: "firstName"
    pure (Merge1X name name)

--------------------------------------------------------------------------------
-- PHANTOM TYPES
--------------------------------------------------------------------------------

---------------- Happy phantom type ----------------

newtype Phantom a = Phantom Int

instance ToJSON (Phantom a) where
  toJSON (Phantom x) =
    object
      [ "value" .= x
      ]

instance FromJSON (Phantom a) where
  parseJSON = withObject "Phantom" $ \o ->
    Phantom <$> o .: "value"

--------------------------------------------------------------------------------
-- HAPPY CASE
--------------------------------------------------------------------------------

---------------- Fully symmetric ----------------

data Good =
  Good
    { g1 :: Int
    , g2 :: Maybe Text
    , g3 :: [Int]
    }

instance ToJSON Good where
  toJSON (Good a b c) =
    object
      [ "g1" .= a
      , "g2" .= b
      , "g3" .= c
      ]

instance FromJSON Good where
  parseJSON = withObject "Good" $ \o -> do
    g1 <- o .: "g1"
    g2 <- o .:? "g2"
    g3 <- o .: "g3"
    pure Good {..}

--------------------------------------------------------------------------------
-- EXTRA DECODER KEY
--------------------------------------------------------------------------------

---------------- Decoder requires key never emitted ----------------

data L2 = L2
  { l2 :: Int
  }

instance ToJSON L2 where
  toJSON (L2 x) =
    object
      [ "l1" .= x
      ]

instance FromJSON L2 where
  parseJSON = withObject "L2" $ \o -> do
    l2 <- o .: "l1"
    (_ :: Value) <- o .: "extra"
    pure (L2 l2)

--------------------------------------------------------------------------------
-- EXTRA ENCODER KEY
--------------------------------------------------------------------------------

---------------- Encoder emits key never read ----------------

data E2 = E2
  { eField2 :: Int
  }

instance ToJSON E2 where
  toJSON (E2 x) =
    object
      [ "eField" .= x
      , "debug" .= ("test" :: Text)
      ]

instance FromJSON E2 where
  parseJSON = withObject "E2" $ \o -> do
    eField2 <- o .: "eField"
    pure (E2 eField2)

--------------------------------------------------------------------------------
-- CONSTANT FIELD INVENTION
--------------------------------------------------------------------------------

---------------- Decoder invents field ----------------

data F2 = F2
  { f1x :: Int
  , f2x :: Int
  }

instance ToJSON F2 where
  toJSON (F2 a b) =
    object
      [ "f1" .= a
      , "f2" .= b
      ]

instance FromJSON F2 where
  parseJSON = withObject "F2" $ \o -> do
    f1x <- o .: "f1"
    pure (F2 f1x 999)

--------------------------------------------------------------------------------
-- SUM TYPE VALIDATION
--------------------------------------------------------------------------------

-------------------------- Constructor Tag Mismatch + Nested Type --------------------

data UserInfo = UserInfo
  { uiUserId   :: Int
  , uiUserName :: Text
  }

instance ToJSON UserInfo where
  toJSON (UserInfo uid name) =
    object
      [ "userId" .= uid
      , "userName" .= name
      ]

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o -> do
    uiUserId   <- o .: "userId"
    uiUserName <- o .: "userName"
    pure UserInfo {..}

--------------------------------------------------------

data SumNested
  = User UserInfo
  | Admin UserInfo

instance ToJSON SumNested where
  toJSON (User u) =
    object
      [ "tag" .= ("User" :: Text)
      , "payload" .= u
      ]

  toJSON (Admin u) =
    object
      [ "tag" .= ("Admin" :: Text)
      , "payload" .= u
      ]

instance FromJSON SumNested where
  parseJSON = withObject "SumNested" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "USER"  -> User <$> o .: "payload"
      "Admin" -> Admin <$> o .: "payload"
      _       -> fail "invalid tag"

---------------------------- Constructor Collapse ---------------------------

data Address = Address
  { city :: Text
  , pinCode :: Int
  }

instance ToJSON Address where
  toJSON (Address city pinCode) =
    object
      [ "city" .= city
      , "pinCode" .= pinCode
      ]

instance FromJSON Address where
  parseJSON = withObject "Address" $ \o -> do
    city <- o .: "city"
    pinCode <- o .: "pinCode"
    pure Address {..}

--------------------------------------------------------

data CustomerType
  = Home Address
  | Office Address

instance ToJSON CustomerType where
  toJSON (Home a) =
    object
      [ "tag" .= ("Home" :: Text)
      , "payload" .= a
      ]

  toJSON (Office a) =
    object
      [ "tag" .= ("Office" :: Text)
      , "payload" .= a
      ]

instance FromJSON CustomerType where
  parseJSON = withObject "CustomerType" $ \o -> do
    payload <- o .: "payload"
    pure (Home payload)

----------------------------- Missing Constructor --------------------------

data PaymentInfo = PaymentInfo
  { amount :: Int
  }

instance ToJSON PaymentInfo where
  toJSON (PaymentInfo amount) =
    object [ "amount" .= amount ]

instance FromJSON PaymentInfo where
  parseJSON = withObject "PaymentInfo" $ \o -> do
    amount <- o .: "amount"
    pure PaymentInfo {..}

--------------------------------------------------------

data PaymentMethod
  = Card PaymentInfo
  | UPI PaymentInfo

instance ToJSON PaymentMethod where
  toJSON (Card p) =
    object
      [ "tag" .= ("Card" :: Text)
      , "payload" .= p
      ]

  toJSON (UPI p) =
    object
      [ "tag" .= ("UPI" :: Text)
      , "payload" .= p
      ]

instance FromJSON PaymentMethod where
  parseJSON = withObject "PaymentMethod" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "Card" -> Card <$> o .: "payload"
      _      -> fail "unsupported"

--------------------------- Nested Field Mismatch Inside Sum Type ---------------------

data Profile = Profile
  { emailAddress :: Text
  }

instance ToJSON Profile where
  toJSON (Profile e) =
    object
      [ "email_address" .= e
      ]

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \o -> do
    emailAddress <- o .: "emailAddress"
    pure Profile {..}

--------------------------------------------------------

data Actor
  = Customer Profile
  | Merchant Profile

instance ToJSON Actor where
  toJSON (Customer p) =
    object
      [ "tag" .= ("Customer" :: Text)
      , "payload" .= p
      ]

  toJSON (Merchant p) =
    object
      [ "tag" .= ("Merchant" :: Text)
      , "payload" .= p
      ]

instance FromJSON Actor where
  parseJSON = withObject "Actor" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "Customer" -> Customer <$> o .: "payload"
      "Merchant" -> Merchant <$> o .: "payload"
      _ -> fail "invalid"

---------------------------------------------------
-- GENERIC/CONTAINER TYPES TESTING
---------------------------------------------------

------------------------ Happy Generic Wrapper -----------------------

data MyType a = MyType
  { myValue :: a
  }

instance ToJSON a => ToJSON (MyType a) where
  toJSON (MyType a) =
    object
      [ "myValue" .= a
      ]

instance FromJSON a => FromJSON (MyType a) where
  parseJSON = withObject "MyType" $ \o -> do
    myValue <- o .: "myValue"
    pure MyType {..}

------------------------ Generic Wrapper - Key Mismatch ----------------------

data MyTypeBad a = MyTypeBad
  { myBadValue :: a
  }

instance ToJSON a => ToJSON (MyTypeBad a) where
  toJSON (MyTypeBad a) =
    object
      [ "value" .= a
      ]

instance FromJSON a => FromJSON (MyTypeBad a) where
  parseJSON = withObject "MyTypeBad" $ \o -> do
    myBadValue <- o .: "myValue"
    pure MyTypeBad {..}

-------------------------- Generic Sum Type ------------------------

data Wrapped a
  = MySuccess a
  | Failure Text

instance ToJSON a => ToJSON (Wrapped a) where
  toJSON (MySuccess a) =
    object
      [ "tag" .= ("success" :: Text)
      , "payload" .= a
      ]

  toJSON (Failure e) =
    object
      [ "tag" .= ("failure" :: Text)
      , "payload" .= e
      ]

instance FromJSON a => FromJSON (Wrapped a) where
  parseJSON = withObject "Wrapped" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "success" -> MySuccess <$> o .: "payload"
      "failure" -> Failure <$> o .: "payload"
      _         -> fail "invalid tag"

-------------------------- Generic Sum Type - Constructor Mismatch ---------------------

data WrappedBad a
  = WrappedSuccess a
  | WrappedFailure Text

instance ToJSON a => ToJSON (WrappedBad a) where
  toJSON (WrappedSuccess a) =
    object
      [ "tag" .= ("success" :: Text)
      , "payload" .= a
      ]

  toJSON (WrappedFailure e) =
    object
      [ "tag" .= ("failure" :: Text)
      , "payload" .= e
      ]

instance FromJSON a => FromJSON (WrappedBad a) where
  parseJSON = withObject "WrappedBad" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "SUCCESS" -> WrappedSuccess <$> o .: "payload"
      "failure" -> WrappedFailure <$> o .: "payload"
      _         -> fail "invalid tag"

------------------------- Nested Generic Type ---------------------

data Box a = Box
  { boxed :: a
  }

instance ToJSON a => ToJSON (Box a) where
  toJSON (Box a) =
    object
      [ "boxed" .= a
      ]

instance FromJSON a => FromJSON (Box a) where
  parseJSON = withObject "Box" $ \o -> do
    boxed <- o .: "boxed"
    pure Box {..}

------------------------------------------------

data ProfileBad = ProfileBad
  { pbEmail :: Text
  }

instance ToJSON ProfileBad where
  toJSON (ProfileBad e) =
    object
      [ "email_address" .= e
      ]

instance FromJSON ProfileBad where
  parseJSON = withObject "ProfileBad" $ \o -> do
    pbEmail <- o .: "email"
    pure ProfileBad {..}

------------------------------------------------

type NestedGeneric = Box ProfileBad

---------------------------- Higher-Kinded Container ------------------------

data Pair a = Pair
  { leftValue  :: a
  , rightValue :: a
  }

instance ToJSON a => ToJSON (Pair a) where
  toJSON (Pair l r) =
    object
      [ "left" .= l
      , "right" .= r
      ]

instance FromJSON a => FromJSON (Pair a) where
  parseJSON = withObject "Pair" $ \o -> do
    leftValue  <- o .: "left"
    rightValue <- o .: "right"
    pure Pair {..}

type PairInt = Pair Int
type PairProfile = Pair ProfileBad

--------------------------------------------------------
-- GADT TESTING
--------------------------------------------------------

------------------------------ Constructor tag mismatch -----------------------------

data GADT1 where
  GInt  :: Int  -> GADT1
  GText :: Text -> GADT1

instance ToJSON GADT1 where
  toJSON (GInt n) =
    object ["tag" .= ("int" :: Text), "value" .= n]

  toJSON (GText t) =
    object ["tag" .= ("text" :: Text), "value" .= t]

instance FromJSON GADT1 where
  parseJSON = withObject "GADT1" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "INT"  -> GInt <$> o .: "value"
      "text" -> GText <$> o .: "value"
      _      -> fail "invalid"

------------------------------ Constructor Collapse ---------------------------

data GADT2 where
  UserId  :: Int  -> GADT2
  UserName :: Text -> GADT2

instance ToJSON GADT2 where
  toJSON (UserId n) =
    object ["tag" .= ("id" :: Text), "value" .= n]

  toJSON (UserName t) =
    object ["tag" .= ("name" :: Text), "value" .= t]

instance FromJSON GADT2 where
  parseJSON = withObject "GADT2" $ \o -> do
    v <- o .: "value"
    pure (UserName v)

--------------------------- Existential Information Loss -------------------

data SomeValue where
  SomeValue :: Show a => a -> SomeValue

instance ToJSON SomeValue where
  toJSON (SomeValue a) =
    String (pack (show a))

instance FromJSON SomeValue where
  parseJSON =
    withText "SomeValue" $ \t ->
      pure (SomeValue t)

--------------------------- Type-Indexed GADT -------------------------

data Expr a where
  LitInt  :: Int  -> Expr Int
  LitText :: Text -> Expr Text

instance ToJSON (Expr a) where
  toJSON (LitInt n) =
    object ["tag" .= ("int" :: Text), "value" .= n]

  toJSON (LitText t) =
    object ["tag" .= ("text" :: Text), "value" .= t]

---------------------------------------------------------------------
-- DEFAULT FUNCTIONALITY TESTING
---------------------------------------------------------------------

data Enc1 = Enc1
  { encField :: Int
  }

instance ToJSON Enc1 where
  toJSON (Enc1 x) =
    object ["field1" .= x]

  toEncoding (Enc1 x) =
    pairs ("field2" .= x)

instance FromJSON Enc1 where
  parseJSON = withObject "Enc1" $ \o ->
    Enc1 <$> o .: "field1"

mkPayload :: Text -> Int -> Value
mkPayload k v =
  object [fromText k .= v]

data Helper1 = Helper1
  { h1 :: Int
  }

instance ToJSON Helper1 where
  toJSON (Helper1 x) =
    mkPayload "writtenField" x

instance FromJSON Helper1 where
  parseJSON = withObject "Helper1" $ \o ->
    Helper1 <$> o .: "readField"

parseField :: FromJSON a => Object -> Text -> Parser a
parseField o k =
  o .: fromText k

data Helper2 = Helper2
  { h2 :: Int
  }

instance ToJSON Helper2 where
  toJSON (Helper2 x) =
    object ["field" .= x]

instance FromJSON Helper2 where
  parseJSON = withObject "Helper2" $ \o ->
    Helper2 <$> parseField o "differentField"

data Generic1 = Generic1
  { firstField :: Int
  }
  deriving Generic

instance ToJSON Generic1 where
  toJSON =
    genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Generic1 where
  parseJSON =
    genericParseJSON defaultOptions

data Generic2 = Generic2
  { customerId :: Int
  }
  deriving Generic

instance ToJSON Generic2 where
  toJSON =
    genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Generic2 where
  parseJSON =
    genericParseJSON defaultOptions
      { fieldLabelModifier = map toLower }

data Opt2 = Opt2
  { maybeField :: Maybe Int
  }
  deriving Generic

instance ToJSON Opt2 where
  toJSON =
    genericToJSON defaultOptions
      { omitNothingFields = True }

instance FromJSON Opt2 where
  parseJSON =
    genericParseJSON defaultOptions

data WT = WT
  { wtField :: Int
  }

instance ToJSON WT where
  toJSON (WT x) =
    object ["wtField" .= x]

instance FromJSON WT where
  parseJSON =
    withText "WT" $ \_ ->
      pure (WT 0)

data NumWrapper =
  NumWrapper Int

instance ToJSON NumWrapper where
  toJSON (NumWrapper x) =
    object ["value" .= x]

instance FromJSON NumWrapper where
  parseJSON =
    withScientific "NumWrapper" $
      pure . NumWrapper . round

data Weird1 =
  Weird1 Int Text

instance ToJSON Weird1 where
  toJSON (Weird1 a b) =
    toJSON [toJSON a, toJSON b]

instance FromJSON Weird1 where
  parseJSON =
    withObject "Weird1" $ \o -> do
      a <- o .: "a"
      b <- o .: "b"
      pure (Weird1 a b)

data Weird2 =
  Weird2 Int Text

instance ToJSON Weird2 where
  toJSON (Weird2 a b) =
    object
      [ "a" .= a
      , "b" .= b
      ]

instance FromJSON Weird2 where
  parseJSON =
    withArray "Weird2" $ \v ->
      Weird2
        <$> parseJSON (v V.! 0)
        <*> parseJSON (v V.! 1)

mkUser :: Int -> Value
mkUser x =
  object ["user_id" .= x]

data HelperNested =
  HelperNested Int

instance ToJSON HelperNested where
  toJSON (HelperNested x) =
    mkUser x

instance FromJSON HelperNested where
  parseJSON = withObject "HelperNested" $ \o ->
    HelperNested <$> o .: "userId"

data Serialized =
  Serialized
  { sField :: Int
  }

instance ToJSON Serialized where
  toJSON (Serialized x) =
    String (pack (show x))

instance FromJSON Serialized where
  parseJSON =
    withText "Serialized" $
      pure . Serialized . read . unpack

mkTagged :: Text -> Value -> Value
mkTagged tag payload =
  object
    [ "tag" .= tag
    , "payload" .= payload
    ]

data HiddenTag
  = HiddenA Int
  | HiddenB Int

instance ToJSON HiddenTag where
  toJSON (HiddenA x) =
    mkTagged "A" (toJSON x)

  toJSON (HiddenB x) =
    mkTagged "B" (toJSON x)

instance FromJSON HiddenTag where
  parseJSON = withObject "HiddenTag" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "AA" -> HiddenA <$> o .: "payload"
      "B"  -> HiddenB <$> o .: "payload"
      _    -> fail "invalid"

data Tree
  = Leaf Int
  | Node Tree Tree

instance ToJSON Tree where
  toJSON (Leaf x) =
    object
      [ "tag" .= ("leaf" :: Text)
      , "value" .= x
      ]

  toJSON (Node l r) =
    object
      [ "tag" .= ("node" :: Text)
      , "left" .= l
      , "right" .= r
      ]

instance FromJSON Tree where
  parseJSON = withObject "Tree" $ \o -> do
    tag <- o .: "tag"

    case (tag :: Text) of
      "LEAF" ->
        Leaf <$> o .: "value"

      "node" ->
        Node <$> o .: "left"
             <*> o .: "right"

      _ ->
        fail "invalid"

------------------------------------------------------------
-- REAL LIFE SCENARIOS
------------------------------------------------------------

data NormalizeEmail = NormalizeEmail
  { email :: Text
  }

instance ToJSON NormalizeEmail where
  toJSON (NormalizeEmail e) =
    object ["email" .= e]

instance FromJSON NormalizeEmail where
  parseJSON = withObject "NormalizeEmail" $ \o -> do
    email <- T.toLower <$> o .: "email"
    pure NormalizeEmail {..}

data DefaultCountry = DefaultCountry
  { country :: Text
  }

instance ToJSON DefaultCountry where
  toJSON (DefaultCountry c) =
    object ["country" .= c]

instance FromJSON DefaultCountry where
  parseJSON = withObject "DefaultCountry" $ \o -> do
    country <- fromMaybe "IND" <$> o .:? "country"
    pure DefaultCountry {..}

data ProdPaymentStatus
  = ProdSuccess
  | ProdFailure
  | ProdPending

instance ToJSON ProdPaymentStatus where
  toJSON ProdSuccess = String "SUCCESS"
  toJSON ProdFailure = String "FAILURE"
  toJSON ProdPending = String "PENDING"

instance FromJSON ProdPaymentStatus where
  parseJSON = withText "ProdPaymentStatus" $ \t ->
    pure $
      case t of
        "SUCCESS" -> ProdSuccess
        "FAILURE" -> ProdFailure
        _         -> ProdPending

data ProdAmount =
  ProdAmount
    { prodAmount :: Double
    }

instance ToJSON ProdAmount where
  toJSON (ProdAmount x) =
    object ["amount" .= x]

instance FromJSON ProdAmount where
  parseJSON = withObject "Amount" $ \o -> do
    d <- (o .: "amount" :: Parser Double)
    pure $ ProdAmount (fromIntegral (round d :: Int))

data ProdCustomer = ProdCustomer
  { prodMobile :: Text
  }

instance ToJSON ProdCustomer where
  toJSON (ProdCustomer m) =
    object ["mobile" .= m]

instance FromJSON ProdCustomer where
  parseJSON = withObject "Customer" $ \o -> do
    prodMobile <- (("MASKED-" :: Text) <>) <$> (o .: "mobile" :: Parser Text)
    pure ProdCustomer {..}

data Payload = Payload
  { body :: Text
  }

instance ToJSON Payload where
  toJSON (Payload b) =
    object ["body" .= b]

decodeBase64Dummy :: Text -> Text
decodeBase64Dummy = ("decoded:" <>)

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o -> do
    body <- decodeBase64Dummy <$> o .: "body"
    pure Payload {..}

data State
  = Enabled
  | Disabled

instance ToJSON State where
  toJSON Enabled = String "ENABLED"
  toJSON Disabled = String "DISABLED"

instance FromJSON State where
  parseJSON = withText "State" $ \t ->
    pure $
      case t of
        "ENABLED" -> Enabled
        "enabled" -> Enabled
        _         -> Disabled

data ProdUser = ProdUser
  { prodFirstName :: Text
  , prodLastName :: Text
  }

instance ToJSON ProdUser where
  toJSON ProdUser{..} =
    object
      [ "full_name" .= (prodFirstName <> " " <> prodLastName)
      ]

splitName :: Text -> (Text, Text)
splitName t =
  case T.words t of
    []       -> ("", "")
    [x]      -> (x, "")
    (x : xs) -> (x, T.unwords xs)

instance FromJSON ProdUser where
  parseJSON = withObject "User" $ \o -> do
    fullName <- o .: "full_name"
    let (f,l) = splitName fullName
    pure (ProdUser f l)

data ProdMethod
  = ProdCard
  | ProdUPI
  | ProdNetBanking

instance ToJSON ProdMethod where
  toJSON ProdCard = String "CARD"
  toJSON ProdUPI = String "UPI"
  toJSON ProdNetBanking = String "NB"

instance FromJSON ProdMethod where
  parseJSON = withText "Method" $ \t ->
    pure $
      case t of
        "CARD" -> ProdCard
        "UPI" -> ProdUPI
        _ -> ProdCard

data GenericUserProd =
  GenericUserProd
    { genericCustomerId :: Int
    , genericMerchantId :: Int
    } deriving Generic

instance ToJSON GenericUserProd where
  toJSON =
    genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      }

instance FromJSON GenericUserProd where
  parseJSON =
    genericParseJSON defaultOptions

newtype Secret a = Secret a

data CustomerPIIProd =
  CustomerPIIProd
    { piiMobile :: Secret Text
    }

instance ToJSON CustomerPIIProd where
  toJSON (CustomerPIIProd (Secret m)) =
    object ["mobile" .= m]

instance FromJSON CustomerPIIProd where
  parseJSON = withObject "CustomerPII" $ \o -> do
    piiMobile <- Secret . (("encrypted:::" :: Text) <>) <$> o .: "mobile"
    pure CustomerPIIProd {..}

---------------- String field value (not a tag) ----------------

newtype Money = Money Double

instance ToJSON Money where
  toJSON (Money d) = if True then object ["version" .= String "v1", "value" .= d] else Number (fromRational (toRational d * 10000))

instance FromJSON Money where
  parseJSON v = parseNewFormat v <|> parseOldFormat v
    where
      parseNewFormat = withObject "Money" $ \obj -> do
        version <- obj .: "version"
        case (version :: Value) of
          String "v1" -> Money <$> obj .: "value"
          _           -> fail "Unsupported version"
      parseOldFormat = withScientific "Money" $ \num -> pure (Money (realToFrac num / 10000))

---------------- Direct pattern match decoder ----------------

data FlowConfigSource = GPMF

instance ToJSON FlowConfigSource where
  toJSON GPMF = String "GPMF"

instance FromJSON FlowConfigSource where
  parseJSON (String "GPMF") = pure GPMF
  parseJSON _ = fail "Invalid FlowConfigSource value"

---------------- Guard-based string matching decoder ----------------

data UpTo = UpToInt Int | UpToInfinity

instance ToJSON UpTo where
  toJSON (UpToInt n) = Number (fromIntegral n)
  toJSON UpToInfinity = String "INFINITY"

instance FromJSON UpTo where
  parseJSON (Number n) = pure (UpToInt (floor n))
  parseJSON (String s) | T.toUpper s == "INFINITY" = pure UpToInfinity
  parseJSON _ = fail "upTo must be a number or 'INFINITY'"

---------------- Fallback key via <|> ----------------

data WalletAccount = WalletAccount
  { waId :: Text
  , waVersion :: Int
  , authenticationDetails :: Maybe Text
  }

instance ToJSON WalletAccount where
  toJSON WalletAccount {..} = object
    [ "id" .= waId
    , "version" .= waVersion
    , "authDetails" .= authenticationDetails
    ]

instance FromJSON WalletAccount where
  parseJSON = withObject "walletAccount" $ \o -> do
    waId <- o .: "id"
    waVersion <- o .: "version"
    authenticationDetails <- (o .:? "authDetails") <|> (o .:? "authenticationDetails")
    pure WalletAccount {..}

---------------- Lambda in encoder (nested object keys) ----------------

data OlMonitoringData = OlMonitoringData
  { olStatic :: [Text]
  , olScoreMap :: KM.KeyMap Double
  , olPrefix :: Text
  }

instance ToJSON OlMonitoringData where
  toJSON (OlMonitoringData statics scoreMap prefix) =
    object
      [ "staticDimensions" .= statics
      , "variableDimensionScoreMap" .= map (\(key, value) -> object ["variableDimension" .= toJSON key, "score" .= value]) (KM.toList scoreMap)
      , "prefix" .= prefix
      ]

instance FromJSON OlMonitoringData where
  parseJSON = withObject "OlMonitoringData" $ \o -> do
    olStatic <- o .: "staticDimensions"
    scoreMapList <- o .: "variableDimensionScoreMap" :: Parser [Value]
    olPrefix <- o .: "prefix"
    pure (OlMonitoringData olStatic KM.empty olPrefix)

---------------- Non-standard encoder (toJSON delegation) ----------------

data CellSelectorSuccessResponse = CellSelectorSuccessResponse
  { cellStatus :: Text
  , cellSelector :: Maybe Text
  }

instance ToJSON CellSelectorSuccessResponse where
  toJSON (CellSelectorSuccessResponse s c) = object ["status" .= s, "cell_selector" .= c]

instance FromJSON CellSelectorSuccessResponse where
  parseJSON = withObject "CellSelectorSuccessResponse" $ \o -> do
    cellStatus <- o .: "status"
    cellSelector <- o .:? "cell_selector"
    pure CellSelectorSuccessResponse {..}

data GetCellSelectorResponse
  = GetCellSelectorSuccessResponse CellSelectorSuccessResponse
  | GetCellSelectorFailureResponse CellSelectorSuccessResponse

instance ToJSON GetCellSelectorResponse where
  toJSON (GetCellSelectorSuccessResponse sr) = toJSON sr
  toJSON (GetCellSelectorFailureResponse fr) = toJSON fr

instance FromJSON GetCellSelectorResponse where
  parseJSON = withObject "GetCellSelectorResponse" $ \o -> do
    status <- o .: "status"
    pure (GetCellSelectorSuccessResponse (CellSelectorSuccessResponse status Nothing))

---------------- Non-standard encoder (Object insert) ----------------

data ConfigValidationRule = ConfigValidationRule
  { ruleField :: Text
  , ruleOp :: Text
  }

instance FromJSON ConfigValidationRule where
  parseJSON = withObject "ConfigValidationRule" $ \v -> do
    ruleField <- v .: "field"
    ruleOp <- v .: "operation"
    pure ConfigValidationRule {..}

instance ToJSON ConfigValidationRule where
  toJSON (ConfigValidationRule fld op) =
    Object (KM.insert (AK.fromText "field") (toJSON fld) (KM.singleton (AK.fromText "operation") (toJSON op)))

--------------------------------------------------------------------------------
-- ADDITIONAL FALSE POSITIVE GUARD CASES (from euler-db / euler-webservice)
--------------------------------------------------------------------------------

---------------- JuspayEvent: key name mismatch (class vs className) ----------------

data JuspayEvent = JuspayEvent
  { jeClassName :: Text
  , jeEventData :: Value
  }

instance FromJSON JuspayEvent where
  parseJSON = withObject "JuspayEvent" $ \o -> do
    jeClassName <- o .: "className"
    jeEventData <- o .: "data"
    pure JuspayEvent {..}

instance ToJSON JuspayEvent where
  toJSON JuspayEvent {..} = object
    [ "class" .= jeClassName
    , "data" .= jeEventData
    ]

---------------- WalletAccount: encoder key missing from decoder (lastRefreshed) ----------------

data WalletAccountFull = WalletAccountFull
  { wafId :: Text
  , wafVersion :: Int
  , wafAuthDetails :: Maybe Text
  , wafLastRefreshed :: Maybe Text
  }

instance ToJSON WalletAccountFull where
  toJSON WalletAccountFull {..} = object
    [ "id" .= wafId
    , "version" .= wafVersion
    , "authDetails" .= wafAuthDetails
    , "lastRefreshed" .= wafLastRefreshed
    ]

instance FromJSON WalletAccountFull where
  parseJSON = withObject "walletAccount" $ \o -> do
    wafId <- o .: "id"
    wafVersion <- o .: "version"
    wafAuthDetails <- (o .:? "authDetails") <|> (o .:? "authenticationDetails")
    wafLastRefreshed <- o .:? "dateCreated"
    pure WalletAccountFull {..}

---------------- Multiple <|> fallback chains ----------------

data MultiFallback = MultiFallback
  { mfKey :: Text
  , mfValue :: Maybe Int
  }

instance ToJSON MultiFallback where
  toJSON MultiFallback {..} = object
    [ "key" .= mfKey
    , "value" .= mfValue
    ]

instance FromJSON MultiFallback where
  parseJSON = withObject "MultiFallback" $ \o -> do
    mfKey <- o .: "key"
    mfValue <- (o .:? "value") <|> (o .:? "val") <|> (o .:? "v")
    pure MultiFallback {..}

---------------- liftA2 <|> with nested object access ----------------

data LiftA2Fallback = LiftA2Fallback
  { la2Field :: Maybe Text
  }

instance ToJSON LiftA2Fallback where
  toJSON LiftA2Fallback {..} = object
    [ "field" .= la2Field
    ]

instance FromJSON LiftA2Fallback where
  parseJSON = withObject "LiftA2Fallback" $ \o -> do
    la2Field <- liftA2 (<|>) (o .:? "field") (o .:? "oldField")
    pure LiftA2Fallback {..}

---------------- String field value in if-then-else (Money variant) ----------------

newtype MoneyV2 = MoneyV2 Double

instance ToJSON MoneyV2 where
  toJSON (MoneyV2 d) =
    if d > 0
      then object ["version" .= String "v2", "amount" .= d]
      else object ["version" .= String "v1", "amount" .= d]

instance FromJSON MoneyV2 where
  parseJSON = withObject "MoneyV2" $ \obj -> do
    version <- obj .: "version"
    case (version :: Value) of
      String "v1" -> MoneyV2 <$> obj .: "amount"
      String "v2" -> MoneyV2 <$> obj .: "amount"
      _           -> fail "Unsupported version"

---------------- Guard with multiple string comparisons ----------------

data StatusV2 = StatusActive | StatusInactive | StatusPending

instance ToJSON StatusV2 where
  toJSON StatusActive   = String "ACTIVE"
  toJSON StatusInactive = String "INACTIVE"
  toJSON StatusPending  = String "PENDING"

instance FromJSON StatusV2 where
  parseJSON (String s)
    | T.toUpper s == "ACTIVE"   = pure StatusActive
    | T.toUpper s == "INACTIVE" = pure StatusInactive
    | T.toUpper s == "PENDING"  = pure StatusPending
  parseJSON _ = fail "Invalid status"

---------------- Direct pattern match with Number constructor ----------------

data NumericEnum = NumOne | NumTwo

instance ToJSON NumericEnum where
  toJSON NumOne = Number 1
  toJSON NumTwo = Number 2

instance FromJSON NumericEnum where
  parseJSON (Number 1) = pure NumOne
  parseJSON (Number 2) = pure NumTwo
  parseJSON _ = fail "Invalid NumericEnum"

---------------- toJSON delegation to nested type (sum type) ----------------

data InnerType = InnerType
  { innerField :: Text
  , innerValue :: Int
  }

instance ToJSON InnerType where
  toJSON InnerType {..} = object
    [ "field" .= innerField
    , "value" .= innerValue
    ]

instance FromJSON InnerType where
  parseJSON = withObject "InnerType" $ \o -> do
    innerField <- o .: "field"
    innerValue <- o .: "value"
    pure InnerType {..}

data WrapperType
  = WrapperA InnerType
  | WrapperB InnerType

instance ToJSON WrapperType where
  toJSON (WrapperA inner) = toJSON inner
  toJSON (WrapperB inner) = toJSON inner

instance FromJSON WrapperType where
  parseJSON = withObject "WrapperType" $ \o -> do
    f <- o .: "field"
    pure (WrapperA (InnerType f 0))

---------------- Object construction via HashMap insertion ----------------

data HashMapEncode = HashMapEncode
  { hmeKey :: Text
  , hmeVal :: Int
  }

instance FromJSON HashMapEncode where
  parseJSON = withObject "HashMapEncode" $ \o -> do
    hmeKey <- o .: "key"
    hmeVal <- o .: "val"
    pure HashMapEncode {..}

instance ToJSON HashMapEncode where
  toJSON (HashMapEncode k v) =
    Object (KM.insert (AK.fromText "key") (toJSON k) (KM.singleton (AK.fromText "val") (toJSON v)))

---------------- Conditional encoder with nested object in lambda ----------------

data ReportData = ReportData
  { rdItems :: [Text]
  , rdMetadata :: KM.KeyMap Text
  }

instance ToJSON ReportData where
  toJSON (ReportData items meta) = object
    [ "items" .= items
    , "metadata" .= map (\(k, v) -> object ["name" .= toJSON k, "value" .= v]) (KM.toList meta)
    ]

instance FromJSON ReportData where
  parseJSON = withObject "ReportData" $ \o -> do
    rdItems <- o .: "items"
    metaList <- o .: "metadata" :: Parser [Value]
    pure (ReportData rdItems KM.empty)

---------------- Encoder uses object in list comprehension ----------------

data BulkResponse = BulkResponse
  { brResults :: [Text]
  }

instance ToJSON BulkResponse where
  toJSON (BulkResponse results) = object
    [ "results" .= results
    , "meta" .= [ object ["index" .= i, "value" .= r] | (i, r) <- zip [0 :: Int ..] results ]
    ]

instance FromJSON BulkResponse where
  parseJSON = withObject "BulkResponse" $ \o -> do
    rdResults <- o .: "results"
    pure (BulkResponse rdResults)

---------------- Non-standard encoder buried in let/case ----------------
-- Mirrors real ConfigValidationRule from euler-webservice where the
-- Object construction is inside a let ... in case ... of wrapper.

data ConfigValidationRuleLet = ConfigValidationRuleLet
  { cvrlField :: Text
  , cvrlOp :: Text
  }

instance FromJSON ConfigValidationRuleLet where
  parseJSON = withObject "ConfigValidationRuleLet" $ \v -> do
    cvrlField <- v .: "field"
    cvrlOp <- v .: "operation"
    pure ConfigValidationRuleLet {..}

instance ToJSON ConfigValidationRuleLet where
  toJSON (ConfigValidationRuleLet fld op) =
    let opJson = toJSON op
    in case opJson of
      Object o -> Object (KM.insert (AK.fromText "field") (toJSON fld) o)
      _        -> error "Internal error: toJSON did not return an object"

---------------- toJSON delegation buried in let ----------------

data WrappedDelegated = WrappedDelegated
  { wdInner :: InnerType
  }

instance ToJSON WrappedDelegated where
  toJSON (WrappedDelegated inner) =
    let json = toJSON inner
    in json

instance FromJSON WrappedDelegated where
  parseJSON = withObject "WrappedDelegated" $ \o -> do
    f <- o .: "field"
    pure (WrappedDelegated (InnerType f 0))

---------------- Conditional key via maybe + lambda ----------------

data PricingTier = PricingTier
  { ptUpTo :: Text
  , ptUnitAmount :: Double
  , ptFlatFee :: Maybe Double
  }

instance ToJSON PricingTier where
  toJSON tier = object $
    [ "up_to" .= ptUpTo tier
    , "unit_amount" .= ptUnitAmount tier
    ] ++ maybe [] (\fee -> ["flat_fee" .= fee]) (ptFlatFee tier)

instance FromJSON PricingTier where
  parseJSON = withObject "PricingTier" $ \o -> do
    ptUpTo <- o .: "up_to"
    ptUnitAmount <- o .: "unit_amount"
    ptFlatFee <- o .:? "flat_fee"
    pure PricingTier {..}

---------------- Where-clause helper functions ----------------

data TokenCacheData = TokenCacheData
  { tcdResourceType :: Text
  , tcdTokenMaxUsage :: Int
  , tcdSource :: Maybe Text
  , tcdUsageCount :: Maybe Int
  }

instance ToJSON TokenCacheData where
  toJSON TokenCacheData{..} = object $
    [ "resourceType" .= tcdResourceType
    , "tokenMaxUsage" .= tcdTokenMaxUsage
    ] ++ source' tcdSource
      ++ usageCount' tcdUsageCount
    where
      source' (Just v) = ["source" .= v]
      source' Nothing  = []
      usageCount' (Just v) = ["usageCount" .= v]
      usageCount' Nothing  = []

instance FromJSON TokenCacheData where
  parseJSON = withObject "TokenCacheData" $ \o -> do
    tcdResourceType <- o .: "resourceType"
    tcdTokenMaxUsage <- o .: "tokenMaxUsage"
    tcdSource <- o .:? "source"
    tcdUsageCount <- o .:? "usageCount"
    pure TokenCacheData {..}

---------------- Decoder delegation via parseJSON ----------------

data InnerSerialized = InnerSerialized { innerJwt :: Text }

instance ToJSON InnerSerialized where
  toJSON InnerSerialized {..} = object ["JWT" .= innerJwt]

instance FromJSON InnerSerialized where
  parseJSON = withObject "InnerSerialized" $ \o -> do
    innerJwt <- o .: "jwt" <|> o .: "JWT"
    pure InnerSerialized {..}

data InnerDeserialized = InnerDeserialized
  { innerHeader :: Text
  , innerPayload :: Text
  }

instance ToJSON InnerDeserialized where
  toJSON InnerDeserialized {..} = object ["header" .= innerHeader, "payload" .= innerPayload]

instance FromJSON InnerDeserialized where
  parseJSON = withObject "InnerDeserialized" $ \o -> do
    innerHeader <- o .: "header"
    innerPayload <- o .: "payload"
    pure InnerDeserialized {..}

data DelegatedBody
  = DelegatedSerialized InnerSerialized
  | DelegatedDeserialized InnerDeserialized

instance ToJSON DelegatedBody where
  toJSON (DelegatedSerialized body) = toJSON body
  toJSON (DelegatedDeserialized body) = toJSON body

instance FromJSON DelegatedBody where
  parseJSON obj = (DelegatedDeserialized <$> parseJSON obj) <|> (DelegatedSerialized <$> parseJSON obj)

---------------- Lambda-case decoder pattern (\case) ----------------

data GatewayTypeTest = NetworkTest | GatewayTest

instance ToJSON GatewayTypeTest where
  toJSON NetworkTest = String "NETWORK"
  toJSON GatewayTest = String "GATEWAY"

instance FromJSON GatewayTypeTest where
  parseJSON = withText "GatewayTypeTest" $ \case
    "NETWORK" -> pure NetworkTest
    "GATEWAY" -> pure GatewayTest
    other     -> fail $ "Invalid GatewayTypeTest: " <> unpack other

---------------- Identical genericToJSON options with lambda [TRUE POSITIVE if keys differ] ----------------

data ApiAccountTest = ApiAccountTest
  { atMerchantId :: Text
  , atMerchantZip :: Maybe Text
  } deriving (Generic)

instance ToJSON ApiAccountTest where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = \x -> if x == "atMerchantZip" then "zip" else x}

instance FromJSON ApiAccountTest where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = \x -> if x == "atMerchantZip" then "zip" else x}

---------------- TH deriveToJSON encoder + hand-written decoder [NO ERROR: TH encoder keys unknown] ----------------

data ThEncManualDec = ThEncManualDec
  { tedField1 :: Text
  , tedField2 :: Maybe Int
  } deriving (Generic)

$(deriveToJSON A.defaultOptions { A.fieldLabelModifier = \x -> if x == "tedField1" then "field_one" else x } ''ThEncManualDec)

instance FromJSON ThEncManualDec where
  parseJSON = withObject "ThEncManualDec" $ \v -> ThEncManualDec
    <$> v .: "field_one"
    <*> v .:? "field2_mismatched"

---------------- TH deriveFromJSON decoder + hand-written encoder [NO ERROR: TH decoder keys unknown] ----------------

data ManualEncThDec = ManualEncThDec
  { medField1 :: Text
  , medField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON ManualEncThDec where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = \x -> if x == "medField1" then "field_one" else x }

$(deriveFromJSON A.defaultOptions { A.fieldLabelModifier = \x -> if x == "medField1" then "field_one" else x } ''ManualEncThDec)

---------------- AKM.lookup (AK.fromText "key") decoder pattern [NO ERROR: keys match] ----------------

data AkmLookupMatch = AkmLookupMatch
  { almPoints :: Text
  , almPockets :: Maybe [Text]
  }

instance ToJSON AkmLookupMatch where
  toJSON alm = object $
    ("points" .= almPoints alm) :
    case almPockets alm of
      Nothing -> []
      Just p  -> ["pockets" .= p]

instance FromJSON AkmLookupMatch where
  parseJSON = withObject "AkmLookupMatch" $ \o -> do
    pts <- case KM.lookup (AK.fromText "points") o of
      Just v  -> parseJSON v
      Nothing -> fail "AkmLookupMatch: missing points field"
    pkts <- case KM.lookup (AK.fromText "pockets") o of
      Nothing -> pure Nothing
      Just v  -> parseJSON v
    pure AkmLookupMatch { almPoints = pts, almPockets = pkts }

---------------- AKM.lookup decoder with key mismatch [TRUE POSITIVE: keys mismatch] ----------------

data AkmLookupMismatch = AkmLookupMismatch
  { alm2Points :: Text
  , alm2Pockets :: Maybe [Text]
  }

instance ToJSON AkmLookupMismatch where
  toJSON alm2 = object $
    ("points" .= alm2Points alm2) :
    case alm2Pockets alm2 of
      Nothing -> []
      Just p  -> ["pockets" .= p]

instance FromJSON AkmLookupMismatch where
  parseJSON = withObject "AkmLookupMismatch" $ \o -> do
    pts <- case KM.lookup (AK.fromText "points") o of
      Just v  -> parseJSON v
      Nothing -> fail "AkmLookupMismatch: missing points field"
    pkts <- case KM.lookup (AK.fromText "pockets_extra") o of
      Nothing -> pure Nothing
      Just v  -> parseJSON v
    pure AkmLookupMismatch { alm2Points = pts, alm2Pockets = pkts }

---------------- defaultEncode/defaultDecode generic with defaultOptions [NO ERROR: field labels match] ----------------

data DefaultOptsMatch = DefaultOptsMatch
  { domField1 :: Text
  , domField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON DefaultOptsMatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON DefaultOptsMatch where
  parseJSON = A.genericParseJSON A.defaultOptions

---------------- defaultEncode generic + hand-written decoder with mismatched keys [TRUE POSITIVE] ----------------

data DefaultOptsMismatch = DefaultOptsMismatch
  { dommField1 :: Text
  , dommField2 :: Maybe Int
  , dommField3 :: Maybe Text
  } deriving (Generic)

instance ToJSON DefaultOptsMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON DefaultOptsMismatch where
  parseJSON = withObject "DefaultOptsMismatch" $ \v -> DefaultOptsMismatch
    <$> v .: "field_one"
    <*> v .:? "field2_camel"
    <*> v .:? "field3_camel"

---------------- defaultEncode (no args) + hand-written decoder with mismatched keys [TRUE POSITIVE] ----------------

data DefaultEncodeMismatch = DefaultEncodeMismatch
  { demType :: Text
  , demValue :: Maybe Int
  , demBrandName :: Maybe Text
  } deriving (Generic)

instance ToJSON DefaultEncodeMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON DefaultEncodeMismatch where
  parseJSON = withObject "DefaultEncodeMismatch" $ \v -> do
    ty <- v .: "type"
    val <- v .:? "value"
    bn <- v .:? "brandName"
    pure DefaultEncodeMismatch { demType = ty, demValue = val, demBrandName = bn }

---------------- Tuple-syntax encoder + KeyMap.lookup decoder [TRUE POSITIVE: errorType written but not read] ----------------

data TupleEncMatch = ExpectedArray2 | ExpectedObject2 | CouldNotParse2 Text

instance ToJSON TupleEncMatch where
  toJSON ExpectedArray2  = object [("errorType", String "TupleEncMatch"), ("reason", String "ExpectedArray2")]
  toJSON ExpectedObject2 = object [("errorType", String "TupleEncMatch"), ("reason", String "ExpectedObject2")]
  toJSON (CouldNotParse2 r) = object [("errorType", String "TupleEncMatch"), ("reason", String $ "CouldNotParse2: " <> r)]

instance FromJSON TupleEncMatch where
  parseJSON val = pure $ case val of
    Object km -> case KM.lookup (AK.fromText "reason") km of
      Just (String "ExpectedArray2") -> ExpectedArray2
      Just (String "ExpectedObject2") -> ExpectedObject2
      Just (String res) -> CouldNotParse2 $ T.replace "CouldNotParse2: " "" res
      _ -> CouldNotParse2 "CouldNotParse2"
    _ -> CouldNotParse2 "CouldNotParse2"

---------------- Tuple-syntax encoder with mismatched decoder keys [TRUE POSITIVE] ----------------

data TupleEncMismatch = ExpectedArray3 | ExpectedObject3 | CouldNotParse3 Text

instance ToJSON TupleEncMismatch where
  toJSON ExpectedArray3  = object [("errorType", String "TupleEncMismatch"), ("reason", String "ExpectedArray3")]
  toJSON ExpectedObject3 = object [("errorType", String "TupleEncMismatch"), ("reason", String "ExpectedObject3")]
  toJSON (CouldNotParse3 r) = object [("errorType", String "TupleEncMismatch"), ("reason", String $ "CouldNotParse3: " <> r)]

instance FromJSON TupleEncMismatch where
  parseJSON val = pure $ case val of
    Object km -> case KM.lookup (AK.fromText "wrong_key") km of
      Just (String "ExpectedArray3") -> ExpectedArray3
      Just (String "ExpectedObject3") -> ExpectedObject3
      Just (String res) -> CouldNotParse3 $ T.replace "CouldNotParse3: " "" res
      _ -> CouldNotParse3 "CouldNotParse3"
    _ -> CouldNotParse3 "CouldNotParse3"

---------------- Tuple-syntax encoder + .:/.:? decoder [NO ERROR: keys match] ----------------

data TupleEncDotMatch = TupleEncDotMatch
  { tedmReason :: Text
  , tedmErrorType :: Text
  }

instance ToJSON TupleEncDotMatch where
  toJSON x = object [("reason", String (tedmReason x)), ("errorType", String (tedmErrorType x))]

instance FromJSON TupleEncDotMatch where
  parseJSON = withObject "TupleEncDotMatch" $ \v -> TupleEncDotMatch
    <$> v .: "reason"
    <*> v .: "errorType"

---------------- genericEncode with omitNothingFields + defaultDecode [NO ERROR: encoder keys unknown] ----------------

data GenericEncodeOmitNothing = GenericEncodeOmitNothing
  { geonField1 :: Text
  , geonField2 :: Maybe Int
  , geonField3 :: Maybe Text
  } deriving (Generic)

instance ToJSON GenericEncodeOmitNothing where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }

instance FromJSON GenericEncodeOmitNothing where
  parseJSON = A.genericParseJSON A.defaultOptions

---------------- defaultDecode <<< composition + defaultEncode [NO ERROR: both field labels] ----------------

convertIntToStringsTest :: A.Value -> A.Value
convertIntToStringsTest = id

data CompositionDecode = CompositionDecode
  { cdField1 :: Text
  , cdField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON CompositionDecode where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON CompositionDecode where
  parseJSON = A.genericParseJSON A.defaultOptions <<< convertIntToStringsTest

---------------- genericEncode defaultOptions + hand-written decoder mismatch [TRUE POSITIVE] ----------------

data GenericEncodeMismatch = GenericEncodeMismatch
  { gemField1 :: Text
  , gemField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON GenericEncodeMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON GenericEncodeMismatch where
  parseJSON = withObject "GenericEncodeMismatch" $ \v -> GenericEncodeMismatch
    <$> v .: "field_one"
    <*> v .:? "field2_camel"

---------------- camelCaseToSnakeCase <<< defaultEncode [NO ERROR: encoder keys unknown] ----------------

camelToSnake :: A.Value -> A.Value
camelToSnake = id

snakeToCamel :: A.Value -> A.Value
snakeToCamel = id

data ComposeEncTransform = ComposeEncTransform
  { cetField1 :: Text
  , cetField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON ComposeEncTransform where
  toJSON = camelToSnake <<< A.genericToJSON A.defaultOptions

instance FromJSON ComposeEncTransform where
  parseJSON = A.genericParseJSON A.defaultOptions

---------------- snakeCaseToCamelCase >>> defaultDecode [NO ERROR: both field labels] ----------------

data ComposeDecPreprocess = ComposeDecPreprocess
  { cdpField1 :: Text
  , cdpField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON ComposeDecPreprocess where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON ComposeDecPreprocess where
  parseJSON = snakeToCamel >>> A.genericParseJSON A.defaultOptions

---------------- camelCaseToSnakeCase <<< defaultEncode + snakeCaseToCamelCase >>> defaultDecode [NO ERROR: both unknown/match] ----------------

data ComposeBothSides = ComposeBothSides
  { cbsField1 :: Text
  , cbsField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON ComposeBothSides where
  toJSON = camelToSnake <<< A.genericToJSON A.defaultOptions

instance FromJSON ComposeBothSides where
  parseJSON = snakeToCamel >>> A.genericParseJSON A.defaultOptions

---------------- defaultDecode <<< convertIntToStrings [NO ERROR: still works] ----------------

convertIntsTest :: A.Value -> A.Value
convertIntsTest = id

data ComposeDecodeFirst = ComposeDecodeFirst
  { cdfField1 :: Text
  , cdfField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON ComposeDecodeFirst where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON ComposeDecodeFirst where
  parseJSON = A.genericParseJSON A.defaultOptions <<< convertIntsTest

---------------- defaultEncode with underscore-prefixed field [NO ERROR: _BNPL -> BNPL] ----------------

data UnderscoreUpper = UnderscoreUpper
  { _BNPL :: Text
  , _Type :: Maybe Text
  } deriving (Generic)

instance ToJSON UnderscoreUpper where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON UnderscoreUpper where
  parseJSON = withObject "UnderscoreUpper" $ \v -> UnderscoreUpper
    <$> v .: "BNPL"
    <*> v .:? "Type"

---------------- defaultEncode with special underscore fields [NO ERROR: _id -> id, _type -> type] ----------------

data UnderscoreSpecial = UnderscoreSpecial
  { _id :: Text
  , _type :: Maybe Text
  , _class :: Maybe Text
  , _data :: Maybe Text
  , _default :: Maybe Text
  } deriving (Generic)

instance ToJSON UnderscoreSpecial where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON UnderscoreSpecial where
  parseJSON = withObject "UnderscoreSpecial" $ \v -> UnderscoreSpecial
    <$> v .: "id"
    <*> v .:? "type"
    <*> v .:? "class"
    <*> v .:? "data"
    <*> v .:? "default"

---------------- defaultEncode with lowercase underscore field [NO ERROR: _myField stays _myField] ----------------

data UnderscoreLower = UnderscoreLower
  { _myField :: Text
  , normalField :: Maybe Int
  } deriving (Generic)

instance ToJSON UnderscoreLower where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON UnderscoreLower where
  parseJSON = withObject "UnderscoreLower" $ \v -> UnderscoreLower
    <$> v .: "_myField"
    <*> v .:? "normalField"

---------------- defaultEncode with underscore field + mismatched decoder [TRUE POSITIVE] ----------------

data UnderscoreMismatch = UnderscoreMismatch
  { _BNPL2 :: Text
  } deriving (Generic)

instance ToJSON UnderscoreMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON UnderscoreMismatch where
  parseJSON = withObject "UnderscoreMismatch" $ \v -> UnderscoreMismatch
    <$> v .: "_BNPL2"

---------------- defaultEncodeOmitNothingOpts encoder + defaultDecode decoder [NO ERROR: field labels match] ----------------

data OmitNothingMatch = OmitNothingMatch
  { onmField1 :: Text
  , onmField2 :: Maybe Int
  , onmField3 :: Maybe Text
  } deriving (Generic)

instance ToJSON OmitNothingMatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON OmitNothingMatch where
  parseJSON = A.genericParseJSON A.defaultOptions

---------------- defaultEncodeOmitNothingOpts encoder + hand-written decoder mismatch [TRUE POSITIVE] ----------------

data OmitNothingMismatch = OmitNothingMismatch
  { onmmField1 :: Text
  , onmmField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON OmitNothingMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON OmitNothingMismatch where
  parseJSON = withObject "OmitNothingMismatch" $ \v -> OmitNothingMismatch
    <$> v .: "field_one"
    <*> v .:? "field2_camel"

---------------- transformFn <<< defaultDecode decoder [NO ERROR: decoder keys unknown] ----------------

transformValue :: Functor f => f TransformDecMatch -> f TransformDecMatch
transformValue = fmap (\x -> x{ tdmField1 = tdmField1 x <> "_transformed" })

data TransformDecMatch = TransformDecMatch
  { tdmField1 :: Text
  , tdmField2 :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON TransformDecMatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON TransformDecMatch where
  parseJSON = transformValue <<< A.genericParseJSON A.defaultOptions

---------------- transformFn <<< defaultDecode decoder with mismatch [NO ERROR: both unknown, limitation] ----------------

data TransformDecMismatch = TransformDecMismatch
  { tdm2Field1 :: Text
  , tdm2Field2 :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON TransformDecMismatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON TransformDecMismatch where
  parseJSON = transformValue2 <<< A.genericParseJSON A.defaultOptions

transformValue2 :: Functor f => f TransformDecMismatch -> f TransformDecMismatch
transformValue2 = fmap (\x -> x{ tdm2Field1 = tdm2Field1 x <> "_transformed" })

---------------- Cross-module decoder helper functions [NO ERROR: keys match] ----------------
-- Simulates readNumberAsMoney "amount" o, readUtcTime "dateCreated" o, etc.
-- from Euler.API.Gateway.Utils.Domain.Common

crossModuleReadNumber :: Text -> A.Object -> Parser Double
crossModuleReadNumber label value = value .: AK.fromText label

crossModuleReadString :: Text -> A.Object -> Parser Text
crossModuleReadString label value = value .: AK.fromText label

data CrossModuleHelperMatch = CrossModuleHelperMatch
  { cmhAmount      :: Text
  , cmhDateCreated :: Text
  , cmhGatewayId   :: Maybe Text
  , cmhLastUpdated :: Maybe Text
  }

instance ToJSON CrossModuleHelperMatch where
  toJSON x = object
    [ "amount"        .= cmhAmount x
    , "dateCreated"   .= cmhDateCreated x
    , "gatewayId"     .= cmhGatewayId x
    , "lastUpdated"   .= cmhLastUpdated x
    ]

instance FromJSON CrossModuleHelperMatch where
  parseJSON = withObject "CrossModuleHelperMatch" $ \o -> do
    amount      <- crossModuleReadNumber "amount" o
    dateCreated <- crossModuleReadString "dateCreated" o
    gatewayId   <- (o .:? "gatewayId" :: Parser (Maybe Text))
    lastUpdated <- crossModuleReadString "lastUpdated" o
    pure CrossModuleHelperMatch {..}

---------------- Cross-module decoder helper with key mismatch [TRUE POSITIVE] ----------------

data CrossModuleHelperMismatch = CrossModuleHelperMismatch
  { cmhmAmount :: Text
  , cmhmDate   :: Text
  }

instance ToJSON CrossModuleHelperMismatch where
  toJSON x = object
    [ "amount" .= cmhmAmount x
    , "date"   .= cmhmDate x
    ]

instance FromJSON CrossModuleHelperMismatch where
  parseJSON = withObject "CrossModuleHelperMismatch" $ \o -> do
    amount <- crossModuleReadNumber "amount_wrong" o
    date   <- crossModuleReadString "date" o
    pure CrossModuleHelperMismatch {..}

---------------- withObject should NOT extract "TypeName" as a key [NO ERROR] ----------------

data WithObjectNotExtracted = WithObjectNotExtracted
  { wneField1 :: Text
  , wneField2 :: Maybe Int
  }

instance ToJSON WithObjectNotExtracted where
  toJSON x = object
    [ "field1" .= wneField1 x
    , "field2" .= wneField2 x
    ]

instance FromJSON WithObjectNotExtracted where
  parseJSON = withObject "WithObjectNotExtracted" $ \o -> WithObjectNotExtracted
    <$> o .: "field1"
    <*> o .:? "field2"

---------------- Derived ToJSON (anyclass) sum type + hand-written FromJSON [NO ERROR: encoder keys unknown] ----------------

data SumTypeDerivedEnc = SumConA Text | SumConB Int | SumConC
  deriving stock (Generic, Eq)

deriving anyclass instance ToJSON SumTypeDerivedEnc

instance FromJSON SumTypeDerivedEnc where
  parseJSON = withObject "SumTypeDerivedEnc" $ \v -> do
    tag <- v .: "tag"
    case (tag :: Text) of
      "SumConA" -> SumConA <$> v .: "contents"
      "SumConB" -> SumConB <$> v .: "contents"
      "SumConC" -> pure SumConC
      _ -> Control.Applicative.empty

---------------- Derived ToJSON (anyclass) record type + hand-written FromJSON [TRUE POSITIVE if keys mismatch] ----------------

data RecordDerivedEnc = RecordDerivedEnc
  { rdeField1 :: Text
  , rdeField2 :: Maybe Int
  } deriving stock (Generic, Eq)

deriving anyclass instance ToJSON RecordDerivedEnc

instance FromJSON RecordDerivedEnc where
  parseJSON = withObject "RecordDerivedEnc" $ \v -> RecordDerivedEnc
    <$> v .: "field_one"
    <*> v .:? "field2_camel"

---------------- <|> alternative with genericParseJSON [NO ERROR: both field labels] ----------------

data AltGenericMatch = AltGenericMatch
  { agmField1 :: Text
  , agmField2 :: Maybe Int
  } deriving (Generic)

instance ToJSON AltGenericMatch where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON AltGenericMatch where
  parseJSON val = A.genericParseJSON A.defaultOptions val <|> A.genericParseJSON A.defaultOptions val

---------------- <|> alternative with derived ToJSON + genericParseJSON [NO ERROR] ----------------

data AltGenericDerived = AltGenericDerived
  { agdField1 :: Text
  , agdField2 :: Maybe Int
  } deriving stock (Generic, Eq)

deriving anyclass instance ToJSON AltGenericDerived

instance FromJSON AltGenericDerived where
  parseJSON val = A.genericParseJSON A.defaultOptions val <|> A.genericParseJSON A.defaultOptions val

---------------- Decoder delegates to local function [NO ERROR: constructors unknown] ----------------

data LocalFuncDecDelegate = LFDD_ConA Text | LFDD_ConB Int | LFDD_ConC
  deriving stock (Eq)

instance ToJSON LocalFuncDecDelegate where
  toJSON (LFDD_ConA x) = toJSON x
  toJSON (LFDD_ConB x) = toJSON x
  toJSON LFDD_ConC = object []

instance FromJSON LocalFuncDecDelegate where
  parseJSON v = parseLFDD v

parseLFDD :: Value -> Parser LocalFuncDecDelegate
parseLFDD v =
  (LFDD_ConA <$> parseJSON v)
  <|> (LFDD_ConB <$> parseJSON v)
  <|> pure LFDD_ConC

---------------- Genuine collapse [TRUE POSITIVE: decoder always returns one constructor] ----------------

data CollapseGenuine = CG_ConA Text | CG_ConB Int
  deriving stock (Eq)

instance ToJSON CollapseGenuine where
  toJSON (CG_ConA x) = toJSON x
  toJSON (CG_ConB x) = toJSON x

instance FromJSON CollapseGenuine where
  parseJSON _ = pure (CG_ConA "")

---------------- Encoder with .= keys + decoder delegating to local function [NO ERROR: decoder keys unknown] ----------------

data LocalFuncDecKeys = LocalFuncDecKeys
  { lfdkField1 :: Text
  , lfdkField2 :: Maybe Int
  } deriving stock (Eq)

instance ToJSON LocalFuncDecKeys where
  toJSON x = object
    [ "field1" .= lfdkField1 x
    , "field2" .= lfdkField2 x
    ]

instance FromJSON LocalFuncDecKeys where
  parseJSON v = parseLocalFuncDecKeys v

parseLocalFuncDecKeys :: Value -> Parser LocalFuncDecKeys
parseLocalFuncDecKeys = withObject "LocalFuncDecKeys" $ \o -> LocalFuncDecKeys
  <$> o .: "field1"
  <*> o .:? "field2"

---------------- Encoder with .= keys + decoder delegating to local function with mismatch [TRUE POSITIVE: keys unknown on both sides via delegation, limitation] ----------------
-- This is a limitation: decoder delegates to local function so keys are unknown,
-- we can't detect the mismatch. But at least no false positive.

data LocalFuncDecMismatch = LocalFuncDecMismatch
  { lfdmField1 :: Text
  } deriving stock (Eq)

instance ToJSON LocalFuncDecMismatch where
  toJSON x = object
    [ "field1" .= lfdmField1 x
    ]

instance FromJSON LocalFuncDecMismatch where
  parseJSON v = parseLocalFuncDecMismatch v

parseLocalFuncDecMismatch :: Value -> Parser LocalFuncDecMismatch
parseLocalFuncDecMismatch = withObject "LocalFuncDecMismatch" $ \o -> LocalFuncDecMismatch
  <$> o .: "wrong_key"

---------------- Where-clause error helper should not be extracted as decoder key [NO ERROR] ----------------

data WhereClauseErrorHelper = WhereClauseErrorHelper
  { wcehField1 :: Text
  , wcehField2 :: Maybe Text
  , wcehNested :: Maybe WhereClauseNested
  } deriving stock (Generic, Eq)

data WhereClauseNested = WhereClauseNested
  { wcnInsertId :: Maybe Text
  , wcnTagId :: Maybe Text
  , wcnFlagType :: Maybe Text
  } deriving stock (Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)

instance ToJSON WhereClauseErrorHelper where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON WhereClauseErrorHelper where
  parseJSON = withObject "WhereClauseErrorHelper" $ \o -> do
    field1 <- o .: "wcehField1"
    field2 <- o .:? "wcehField2"
    nested <- o .:? "wcehNested" >>= validateNested
    pure WhereClauseErrorHelper
      { wcehField1 = field1
      , wcehField2 = field2
      , wcehNested = nested
      }
    where
      validateNested Nothing = pure Nothing
      validateNested (Just n) = do
        when (isStringAbsent (wcnInsertId n)) $
          fail $ missingErr "insertId"
        when (isStringAbsent (wcnTagId n)) $
          fail $ missingErr "tagId"
        when (isStringAbsent (wcnFlagType n)) $
          fail $ missingErr "flagType"
        pure (Just n)

      missingErr :: Text -> String
      missingErr fieldName =
        "{\"errorType\":\"MISSING_FIELD\",\"errField\":\"" <> T.unpack fieldName <> "\"}"

isStringAbsent :: Maybe Text -> Bool
isStringAbsent Nothing = True
isStringAbsent (Just "") = True
isStringAbsent _ = False

---------------- Dollar operator with genericToJSON/genericParseJSON + fieldLabelModifier [NO ERROR] ----------------

data DollarGenOpts = DollarGenOpts
  { dgoField1 :: Text
  , dgoField2 :: Maybe Int
  } deriving stock (Generic, Eq)

modifyDgo :: String -> String
modifyDgo "dgoField1" = "field_one"
modifyDgo s = s

instance ToJSON DollarGenOpts where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = modifyDgo, omitNothingFields = True}

instance FromJSON DollarGenOpts where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = modifyDgo, omitNothingFields = True}

---------------- omitNothingFields differs but fieldLabelModifier same [NO ERROR] ----------------

data OmitNothingMatch2 = OmitNothingMatch2
  { onm2Field1 :: Text
  , onm2Field2 :: Maybe Int
  , onm2Field3 :: Maybe Text
  } deriving stock (Generic, Eq)

modifyOnm2 :: String -> String
modifyOnm2 "onm2Field1" = "field_one"
modifyOnm2 s = s

instance ToJSON OmitNothingMatch2 where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = modifyOnm2, omitNothingFields = True}

instance FromJSON OmitNothingMatch2 where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = modifyOnm2}

---------------- omitNothingFields differs AND sumEncoding differs [TRUE POSITIVE: sumEncoding mismatch] ----------------

data OmitNothingSumMismatch2 = OmitNothingSumMismatch2
  { onsm2Field1 :: Text
  } deriving stock (Generic, Eq)

instance ToJSON OmitNothingSumMismatch2 where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, omitNothingFields = True}

instance FromJSON OmitNothingSumMismatch2 where
  parseJSON = genericParseJSON defaultOptions

---------------- OverloadedStrings pattern match in decoder [NO ERROR] ----------------

data SingleConstructorSum = SCS_TagA
  deriving stock (Eq, Show)

instance ToJSON SingleConstructorSum where
  toJSON SCS_TagA = String "SCS_TAG_A"

instance FromJSON SingleConstructorSum where
  parseJSON "SCS_TAG_A" = pure SCS_TagA
  parseJSON other = fail $ "Unsupported: " <> show other

---------------- $ operator with fieldLabelModifier on both sides [NO ERROR] ----------------

data DollarFieldMod = DollarFieldMod
  { dfmField1 :: Text
  , dfmField2 :: Maybe Int
  } deriving stock (Generic, Eq)

modifyDfm :: String -> String
modifyDfm "dfmField1" = "field_one"
modifyDfm s = s

instance ToJSON DollarFieldMod where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = modifyDfm, omitNothingFields = True}

instance FromJSON DollarFieldMod where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = modifyDfm, omitNothingFields = True}

---------------- Qualified omitNothingFields with multi-line ppr [NO ERROR] ----------------

data QualifiedOmitMulti = QualifiedOmitMulti
  { qomField1 :: Text
  , qomField2 :: Maybe Int
  } deriving stock (Generic, Eq)

modifyQom :: String -> String
modifyQom "qomField1" = "field_one"
modifyQom s = s

instance ToJSON QualifiedOmitMulti where
  toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = modifyQom, A.omitNothingFields = True}

instance FromJSON QualifiedOmitMulti where
  parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = modifyQom}

---------------- $ operator with mismatched fieldLabelModifier [TRUE POSITIVE] ----------------

data DollarMismatch = DollarMismatch
  { dmField1 :: Text
  } deriving stock (Generic, Eq)

modifyDmEnc :: String -> String
modifyDmEnc "dmField1" = "enc_field"
modifyDmEnc s = s

modifyDmDec :: String -> String
modifyDmDec "dmField1" = "dec_field"
modifyDmDec s = s

instance ToJSON DollarMismatch where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = modifyDmEnc}

instance FromJSON DollarMismatch where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = modifyDmDec}

---------------- omitNothingFields with long qualified names causing multi-line ppr [NO ERROR] ----------------

data QualifiedMultiLineOmit = QualifiedMultiLineOmit
  { qmloField1 :: Text
  , qmloField2 :: Maybe Int
  , qmloField3 :: Maybe Text
  } deriving stock (Generic, Eq)

modifyQmlo :: String -> String
modifyQmlo "qmloField1" = "field_one"
modifyQmlo "qmloField2" = "field_two"
modifyQmlo "qmloField3" = "field_three"
modifyQmlo s = s

instance ToJSON QualifiedMultiLineOmit where
  toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = modifyQmlo, A.omitNothingFields = True}

instance FromJSON QualifiedMultiLineOmit where
  parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = modifyQmlo}

---------------- Untagged sum type delegating to different inner types [NO ERROR] ----------------

data UntaggedSumA = UntaggedSumA
  { usaField1 :: Text
  } deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

data UntaggedSumB = UntaggedSumB
  { usbField1 :: Text
  , usbResult :: Text
  } deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

data UntaggedSum = UntaggedSumSuccess UntaggedSumA | UntaggedSumFailure UntaggedSumB
  deriving stock (Eq, Show)

instance ToJSON UntaggedSum where
  toJSON (UntaggedSumSuccess r) = toJSON r
  toJSON (UntaggedSumFailure r) = toJSON r

instance FromJSON UntaggedSum where
  parseJSON = withObject "UntaggedSum" $ \o -> do
    mbResult <- o .:? "result" :: Parser (Maybe Text)
    case mbResult of
      Just "FAIL" -> UntaggedSumFailure <$> parseJSON (Object o)
      _ -> UntaggedSumSuccess <$> parseJSON (Object o)

---------------- defaultDecode inside case expression [NO ERROR] ----------------

data CaseDefaultDecode = CaseDefaultDecode
  { cddField1 :: Text
  , cddResult :: Maybe Text
  } deriving stock (Generic, Eq)

instance ToJSON CaseDefaultDecode where
  toJSON = genericToJSON A.defaultOptions

instance FromJSON CaseDefaultDecode where
  parseJSON x@(A.Object obj) =
    case KM.lookup "result" obj of
      Just _  -> genericParseJSON A.defaultOptions (A.Object (KM.delete "result" obj))
      Nothing -> genericParseJSON A.defaultOptions x
  parseJSON x = genericParseJSON A.defaultOptions x

---------------- String-encoded sum type with top-level helper [NO ERROR] ----------------

data StrEncSumType
  = StrEncProceed
  | StrEncReject
  | StrEncReview
  deriving stock (Eq, Show)

strEncToText :: StrEncSumType -> Text
strEncToText StrEncProceed = "proceed"
strEncToText StrEncReject  = "reject"
strEncToText StrEncReview  = "review"

instance ToJSON StrEncSumType where
  toJSON = A.String . strEncToText

instance FromJSON StrEncSumType where
  parseJSON = A.withText "StrEncSumType" $ \t ->
    case T.toLower t of
      "proceed" -> pure StrEncProceed
      "reject"  -> pure StrEncReject
      "review"  -> pure StrEncReview
      _         -> fail "Invalid"

---------------- Untagged sum type with single delegation [TRUE POSITIVE: key mismatch] ----------------

data UntaggedSingle = UntaggedSingleSuccess UntaggedSingleInner | UntaggedSingleFail
  deriving stock (Eq, Show)

data UntaggedSingleInner = UntaggedSingleInner
  { usiField1 :: Text
  } deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSON UntaggedSingle where
  toJSON (UntaggedSingleSuccess r) = toJSON r
  toJSON UntaggedSingleFail = A.String "fail"

instance FromJSON UntaggedSingle where
  parseJSON (A.String "fail") = pure UntaggedSingleFail
  parseJSON v = UntaggedSingleSuccess <$> parseJSON v

---------------- T.isPrefixOf in where-clause helper [NO ERROR] ----------------

data PrefixCheckSumType
  = PrefixCheckProceed
  | PrefixCheckReject
  | PrefixCheckCall Text
  deriving stock (Eq, Show)

prefixCheckToText :: PrefixCheckSumType -> Text
prefixCheckToText PrefixCheckProceed = "proceed"
prefixCheckToText PrefixCheckReject  = "reject"
prefixCheckToText (PrefixCheckCall p) = "call_" <> p

instance ToJSON PrefixCheckSumType where
  toJSON = A.String . prefixCheckToText

instance FromJSON PrefixCheckSumType where
  parseJSON = A.withText "PrefixCheckSumType" $ \t ->
    case T.toLower t of
      "proceed" -> pure PrefixCheckProceed
      "reject"  -> pure PrefixCheckReject
      x | T.isPrefixOf "call_" x ->
        let providerText = T.drop (T.length "call_") x
        in pure (PrefixCheckCall providerText)
      _ -> fail "Invalid"

---------------- stripPrefix in inline let/case body [NO ERROR] ----------------

data StripPrefixSumType
  = StripPrefixSimple
  | StripPrefixNested Text
  deriving stock (Eq, Show)

instance ToJSON StripPrefixSumType where
  toJSON StripPrefixSimple = A.String "SIMPLE"
  toJSON (StripPrefixNested t) = A.String ("NESTED_" <> T.toUpper t)

instance FromJSON StripPrefixSumType where
  parseJSON = A.withText "StripPrefixSumType" $ \t ->
    let txt = T.toUpper t
    in case txt of
      "SIMPLE" -> pure StripPrefixSimple
      _ | Just rest <- T.stripPrefix "NESTED_" txt ->
            pure (StripPrefixNested rest)
      _ -> fail "Invalid"

