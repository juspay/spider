{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module EdgeCases where

import UnusedFieldChecker.FieldChecker (FieldChecker(..))
import GHC.Generics (Generic)

-- Edge case 1: Empty record (all Maybe fields)
data EmptyRecord = EmptyRecord
    { emptyField1 :: Maybe Int
    , emptyField2 :: Maybe String
    , emptyField3 :: Maybe Bool
    } deriving (Show, Generic)

instance FieldChecker EmptyRecord

createEmpty :: EmptyRecord
createEmpty = EmptyRecord Nothing Nothing Nothing

-- Edge case 2: Single field record
data SingleField = SingleField
    { singleValue :: String
    } deriving (Show, Generic)

instance FieldChecker SingleField

useSingle :: SingleField -> String
useSingle (SingleField val) = val

-- Edge case 3: Record with function types (should be used via pattern matching)
data CallbackRecord = CallbackRecord
    { onSuccess :: String -> IO ()
    , onError :: String -> IO ()
    , callbackName :: String
    }

instance FieldChecker CallbackRecord

useCallback :: CallbackRecord -> String -> IO ()
useCallback CallbackRecord { onSuccess, callbackName } msg =
    onSuccess (callbackName ++ ": " ++ msg)

-- Edge case 4: Recursive data type
data Tree a = Leaf
    { leafValue :: a
    , leafId :: Int
    , leafOptional :: Maybe String
    }
    | Node
    { nodeValue :: a
    , nodeLeft :: Tree a
    , nodeRight :: Tree a
    , nodeDepth :: Int
    , nodeOptional :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker (Tree Int)

treeSize :: Tree a -> Int
treeSize Leaf { leafValue } = 1
treeSize Node { nodeLeft, nodeRight } = 1 + treeSize nodeLeft + treeSize nodeRight

sumTree :: Tree Int -> Int
sumTree Leaf { leafValue } = leafValue
sumTree Node { nodeValue, nodeLeft, nodeRight } =
    nodeValue + sumTree nodeLeft + sumTree nodeRight

getDepth :: Tree a -> Int
getDepth Leaf { leafId } = leafId
getDepth Node { nodeDepth } = nodeDepth

-- Edge case 5: Records with phantom types
data Tagged tag a = Tagged
    { taggedValue :: a
    , taggedLabel :: String
    } deriving (Show, Generic)

instance FieldChecker (Tagged String Int)

useTagged :: Tagged t a -> a
useTagged Tagged { taggedValue } = taggedValue

getLabel :: Tagged t a -> String
getLabel Tagged { taggedLabel } = taggedLabel

-- Edge case 6: Record with existential types
data AnyValue = forall a. Show a => AnyValue
    { anyValue :: a
    , anyDescription :: String
    }

instance FieldChecker AnyValue

showAny :: AnyValue -> String
showAny AnyValue { anyValue, anyDescription } =
    anyDescription ++ ": " ++ show anyValue

-- Edge case 7: GADT with records
data Expr a where
    IntExpr :: { intValue :: Int, intLabel :: Maybe String } -> Expr Int
    BoolExpr :: { boolValue :: Bool, boolLabel :: Maybe String } -> Expr Bool
    StringExpr :: { stringValue :: String, stringLabel :: Maybe String } -> Expr String

deriving instance Show (Expr a)

instance FieldChecker (Expr Int)
instance FieldChecker (Expr Bool)
instance FieldChecker (Expr String)

evalExpr :: Expr a -> a
evalExpr IntExpr { intValue } = intValue
evalExpr BoolExpr { boolValue } = boolValue
evalExpr StringExpr { stringValue } = stringValue

-- Edge case 8: Record with higher-kinded types
data Container f a = Container
    { containerData :: f a
    , containerMetadata :: String
    , containerOptional :: Maybe Int
    } deriving (Generic)

instance FieldChecker (Container [] Int)

useContainer :: Container [] a -> [a]
useContainer Container { containerData } = containerData

getMetadata :: Container f a -> String
getMetadata Container { containerMetadata } = containerMetadata

-- Edge case 9: Records with constraints
data Constrained a = (Show a, Eq a) => Constrained
    { constrainedValue :: a
    , constrainedId :: Int
    , constrainedOptional :: Maybe String
    }

instance (Show a, Eq a) => FieldChecker (Constrained a)

compareConstrained :: (Show a, Eq a) => Constrained a -> Constrained a -> Bool
compareConstrained c1 c2 =
    constrainedValue c1 == constrainedValue c2

showConstrained :: (Show a, Eq a) => Constrained a -> String
showConstrained Constrained { constrainedValue, constrainedId } =
    show constrainedId ++ ": " ++ show constrainedValue

-- Edge case 10: Record with tuple fields
data TupleRecord = TupleRecord
    { tupleField1 :: (Int, String)
    , tupleField2 :: (Bool, Double, Char)
    , tupleName :: String
    , tupleOptional :: Maybe (Int, Int)
    } deriving (Show, Generic)

instance FieldChecker TupleRecord

useTupleRecord :: TupleRecord -> String
useTupleRecord TupleRecord { tupleField1 = (n, s), tupleName } =
    tupleName ++ ": " ++ show n ++ " " ++ s

extractTuple :: TupleRecord -> (Bool, Double, Char)
extractTuple TupleRecord { tupleField2 } = tupleField2

-- Edge case 11: Record with list fields
data ListRecord = ListRecord
    { listItems :: [String]
    , listInts :: [Int]
    , listName :: String
    , listOptional :: Maybe [Bool]
    } deriving (Show, Generic)

instance FieldChecker ListRecord

countItems :: ListRecord -> Int
countItems ListRecord { listItems } = length listItems

sumInts :: ListRecord -> Int
sumInts ListRecord { listInts } = sum listInts

getName :: ListRecord -> String
getName ListRecord { listName } = listName

-- Edge case 12: Nested parameterized types
data Wrapper a b = Wrapper
    { wrapperFirst :: a
    , wrapperSecond :: b
    , wrapperLabel :: String
    , wrapperOptional :: Maybe String
    } deriving (Show, Generic)

instance FieldChecker (Wrapper Int String)

getFirst :: Wrapper a b -> a
getFirst Wrapper { wrapperFirst } = wrapperFirst

getSecond :: Wrapper a b -> b
getSecond Wrapper { wrapperSecond } = wrapperSecond

getWrapperLabel :: Wrapper a b -> String
getWrapperLabel Wrapper { wrapperLabel } = wrapperLabel
