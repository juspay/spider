module TestUtils where

throwException :: ()
throwException = ()

throwExceptionV1 :: ()
throwExceptionV1 = throwException

throwExceptionV2 :: ()
throwExceptionV2 = throwExceptionV1

throwExceptionV3 :: ()
throwExceptionV3 = throwExceptionV2

throwExceptionV4 :: ()
throwExceptionV4 = throwException