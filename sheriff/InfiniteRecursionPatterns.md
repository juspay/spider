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

## Pattern 6 : Infinite recursion call on partial function
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

## Pattern 9 : Infinite recursion in instance method based on instance type being used
for e.g. -
```haskell
class TypeChanger a b where
  changeType :: a -> b

data SumType = TypeA Int | TypeB | RecType SumType

instance TypeChanger String SumType where
  changeType x = RecType $ changeType x -- Infinite recursion since types are same (TypeChanger String SumType)

instance TypeChanger Integer SumType where
  changeType = TypeA . changeType -- NOT infinite recursion since type is changed (TypeChanger Integer Int)
```

## Pattern 10 : Infinite recursion call on partial function but using lambda case (in normal function and instance methods, as first statement or let-in statement or in function composition)
for e.g. - 
```haskell
fn :: String -> String
fn = \case
  "Pattern" -> fn "Pattern" -- Infinite recursion due to same pattern
  b         -> fn b -- Infinite recursion due to same variable
```

## Pattern 11 : Infinite recursion call on partial function but using lambda functions (in normal function and instance methods, as first statement or let-in statement or in function composition)
for e.g. - 
```haskell
fn :: String -> String
fn = \x -> fn x -- Infinite recursion due to same variable
```

Following cases are detected as infinite recursion, but these might not be infinite and it can depend on how these are written:
1. Functions modifying state and preventing infinite recursion on the basis of that
2. Functions using mutable variables like IORef, MVar, etc.
3. Functions intended to be used as infinite recursion, such as loopers with threadDelay, server listeners, etc.
4. Functions generating infinite list but used lazily in context of some limiting function like zip, take, etc.
5. Some functions which are infinite only hypothetically, such as function having control flow based on some random number generated

For such cases, it is advisable to add them to `infinite_recursion_rule_ignore_functions` cases and maintain a list of such functions for visibility.
It is always advised to add ignore functions as fully qualified names.
In case of any other false positive/anamoly, let us know by raising an issue.