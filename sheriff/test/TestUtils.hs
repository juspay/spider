module TestUtils where

import Data.Text

newtype Number = Number Int

throwException :: Text -> ()
throwException _ = ()

throwExceptionV1 :: Text -> ()
throwExceptionV1 = throwException

throwExceptionV2 :: Text -> ()
throwExceptionV2 = throwExceptionV1

throwExceptionV3 :: Text -> ()
throwExceptionV3 = throwExceptionV2

throwExceptionV4 :: Text -> ()
throwExceptionV4 = throwException

addNumber :: Number -> Number -> Number
addNumber (Number a) (Number b) = Number (a + b)

subtractNumber :: Number -> Number -> Number
subtractNumber (Number a) (Number b) = Number (a - b)

multiplyNumber :: Number -> Number -> Number
multiplyNumber (Number a) (Number b) = Number (a * b)

(+?) :: Number -> Number -> Number
(+?) = addNumber

(-?) :: Number -> Number -> Number
(-?) = subtractNumber

(*?) :: Maybe (Either (Maybe Int) (Maybe Int)) -> Number -> Number -> Number
(*?) _ = multiplyNumber

fstArg :: a -> a -> a
fstArg a1 a2 = a1

sndArg :: a -> a -> a
sndArg a1 a2 = a2