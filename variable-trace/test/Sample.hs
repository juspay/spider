module Sample where

sample :: String -> String -> String
sample a b = do
    let c = a
        d = b
    mappend c d

sample2 :: String -> String -> String
sample2 a b = do
    let h = a
        j = b
    mappend h j

getValueIn :: String -> IO String
getValueIn x = do
    print x
    pure $ sample2 x "l"

getval :: Int -> String
getval a = do
    let y = (a + 2)
    show y

getval2 a b = if a == b then ",a" else ",k"