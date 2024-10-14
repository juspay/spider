{-# LANGUAGE ScopedTypeVariables #-}

module DC.Constants where

createError :: String
createError = "Should not create field with status as success or failure in default case"

updateError :: String
updateError = "Should not update field with status as success or failure in default case"

defaultCase :: String
defaultCase = "Should not use status as success or failure in default case"

syncError :: String 
syncError = "Should not use exception functions for sync in default cases"

-- taking this from plugin options now
-- prefixPath :: 
-- prefixPath = "./.juspay/dc/test/"