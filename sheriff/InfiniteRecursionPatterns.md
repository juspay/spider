# Detectable Infinite Recursion Patterns

> For exact examples for what is covered and what is not, refer [InfiniteRecursionTest.hs](./test/SubTests/InfiniteRecursionTest.hs)

## Pattern 1 : Self call to variable / function without argument
For e.g. - 
```haskell
let x = x <> "Dummy" in x
```

## Pattern 2 : Call to complete function (non partial function) with same arguments
For e.g. - 
```haskell
fn :: Int -> Int
fn val = fn val
```
```haskell
fn :: String -> String
fn a = 
  let z = fn a
  in z
```
However, this won't throw error
```haskell
fn :: String -> String
fn a = 
  let a = "Changed value"
  in fn a
```

## Pattern 3 : Self recursive call in instance function
for e.g. - 
```haskell
toJSON a = A.toJSON a
```

## Pattern 4 : Infinite recursion call in where clause
for e.g. - 
```haskell
fn :: String -> String -> String
fn x y = fn2 x y
  where 
    fn2 a b = fn3 $ fn2 a b
```

## Pattern 5 : Infinite recursion call with specific pattern match
for e.g. - 
```haskell
fn :: Int -> Int
fn 10 = fn (10 :: Int)
fn _ = -1
```

## Pattern 6 : Infinite recursion call on partial function called as alias
for e.g. - 
```haskell
fn :: Int -> Int
fn = fn
```

## Pattern 7 : Infinite recursion call on partial function called inside function composition
for e.g. - 
```haskell
fn :: Int -> Int
fn = fn2 . fn
```

## Pattern 8 : Infinite recursion call on partial function in the last stmt 
for e.g. - 
```haskell
fn :: Int -> Int
fn = let z = "Dummy" in fn
```