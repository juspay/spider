module Main where

import Extra

rain :: IO ()
rain = do
    let x = getStatus
    let y = getChange 
    let r = lookupenVT math nj
    pure ()

main :: IO ()
main = pure ()

getStatus = lookupenvT "REDIS"