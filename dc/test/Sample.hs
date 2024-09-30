module Sample where

testParentFunctionF :: String
testParentFunctionF = "this should no get caught"


proxyFunction :: String
proxyFunction = case maybeTest of
    Left _str -> temp
    Right _str -> throwErr

maybeTest :: Either String String
maybeTest = Right "test string"

temp :: String
temp = case maybeTest of
    Left _str -> throwErr
    Right _str -> throwErr

throwErr :: String
throwErr = "This should be an error"