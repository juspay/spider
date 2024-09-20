module Sample where

testParentFunctionF :: String
testParentFunctionF = "this should not get caught"

proxyFunction :: String
proxyFunction = case maybeTest of
    Left str -> temp
    Right str -> "asdfghj"

maybeTest :: Either String String
maybeTest = Right "test string"

temp :: String
temp = case maybeTest of
    Left str -> "throwErr"
    Right str -> throwErr

throwErr :: String
throwErr = "This should be an error"