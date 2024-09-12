module TestUtils where

import Data.Text

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