# Sheriff plugin
## What it does?
`sheriff` is a compiler plugin to add general code related compilation checks. It can check for the following logics :
1. ***Stringification of text/string*** - It checks if we have applied show function to any string literal or variable. We want this check to make sure that we are not having any unnecessary quotes in the code.
2. ***Logging stringified haskell objects*** - It checks if we have applied show function to any variable inside any logging function. We want this check to make sure that our application level log filtering works fine and is not impacted by stringified values.
3. ***NonIndexed DB queries check*** - It checks if we have any DB query which won't use any index. Indexed columns of a particular table are provided in *.juspay/indexedKeys.yaml* file. It works on all operations - select, update, delete. We want this check to make sure that our application doesn't make any slow sql queries.
4. ***Detecting use of non-allowed functions*** - It checks if we have used some function which should not be used anywhere in the code. The function can be specified by name or signature.
5. ***Detecting infinite recursions in code*** - It checks whether there is any recursive call for which we can say that it is infinite recursion. For more details, check [Infinite Recursion Detected Patterns](./InfiniteRecursionPatterns.md)

## How to write rules?
Any additional rules for a package can be provided as `yaml` file. The path to this rules file can be given as plugin option as follows: <br>
```cabal
-fplugin-opt=Sheriff.Plugin:{"rulesConfigPath":".juspay/sheriffRules.yaml","exceptionsConfigPath":".juspay/sheriffExceptionRules.yaml"}
```

- Both rules and exception rules follows same structure & format.
- Rules provided in exceptions file are global exceptions for all rules. It means if the code satisfies both a rule and any exceptionRule, then error won't be thrown.

> Structure of Function Rules:
>```yaml
> - fn_rule_name: "<Name of the Rule>"
>   fn_name: "<Function Name: can be Qualified as \"A.fn1\" or Unqualified as \"fn1\">"
>   arg_no: 1
>   fns_blocked_in_arg:
>     - [ModuleA.dummyFn, 0, []]
>     - [dummyFn2, 0, []]
>   types_blocked_in_arg:
>     - String
>     - Text
>     - EnumTypes
>     - Int64
>     - Person
>   types_to_check_in_arg:
>     - String
>     - Text
>     - EnumTypes
>     - Int64
>     - Person
>   fn_rule_fixes:
>     - Sample Suggested Fix 1"
>     - Sample Suggested Fix 2"
>   fn_rule_exceptions:
>     - fn_rule_name: "<Name of the Exception Rule>"
>       fn_name: dummy
>       arg_no: 0
>       fns_blocked_in_arg: []
>       types_blocked_in_arg: []
>       types_to_check_in_arg: []
>       fn_rule_fixes: []
>       fn_rule_exceptions: []
>       fn_rule_ignore_modules: 
>         - ModuleK  
>   fn_rule_ignore_modules:
>     - ModuleT
>     - ModuleP
>   fn_rule_check_modules:
>     - ModuleA
>     - ModuleB
>```

> Structure of DB Rules:
>```yaml
> - db_rule_name: "DBRuleTest"
>   table_name: "TxnRiskCheck"
>   indexed_cols_names: 
>     - partitionKey
>     - and:
>       - txnId
>       - customerId
>   db_rule_fixes:
>     - You might want to include an indexed column in the `where` clause of the query.
>   db_rule_exceptions: []
>```

> Structure of Infinite Recursion Rule:
>```yaml
> - infinite_recursion_rule_name: "Infinite Recursion Rule"
>   infinite_recursion_rule_fixes: 
>     - Fix1
>     - Fix 2
>   infinite_recursion_rule_exceptions: []
>   infinite_recursion_rule_ignore_modules:
>     - ModuleA
>   infinite_recursion_rule_check_modules:
>     - "*"
>   infinite_recursion_rule_ignore_functions:
>     - ModuleB.fn2
>```

Refer Sample Rules and exception rules in [.juspay](.juspay/sheriffRules.yaml) directory.

## How to resolve compilation errors?
> "show" on Text is not allowed
  - Remove `show` function call from the error location. If quotes are required, manually add them to the text.
  - If conversions are required, we can use `encodeUtf8`, `pack`, `unpack`, etc.
  - For e.g.,
    ```haskell
    let (txt :: Text) = "This is Text type."
        (str :: String) = "This is string type."
    
    -- It will throw error
    if gw == "\"DUMMY\""
      then pure $ txt <> show str
      else throwErr

    -- Probable Fix 1:
    if gw == "\"DUMMY\""
      then pure $ txt <> Data.Text.pack str
      else throwErr

    -- Probable Fix 2: (when code had unwanted quotes)
    if gw == "DUMMY"
      then pure $ txt <> Data.Text.pack str
      else throwErr
    ```

> Use of "show" is not allowed in "logErrorT"
  - Remove `show` function call from the error location. We can use `L.logErrorV @Text` function with tuples. To use this, we must have `ToJSON` instance on the value we are logging. We may use tuples for combining string and objects. <br>For e.g., we can fix as follows:
  - Import `Euler.WebService.Instances` module for ToJSON instances of `SomeException` and `ClientError`
  - For e.g.,
    ```haskell
      -- It will throw error
      L.logErrorT "Log_Tag" $ "Failed to fetch object: " <> show tableObj

      -- After fix, it won't throw error
      L.logErrorV @Text "Log_Tag" ("Failed to fetch object: " :: Text, tableObj)
    ```

> Querying on non-indexed column 'ColumnName' of table 'TableName' is not allowed
  - Make sure that the `where` clause in the DB query use at least one index, be it composite index or a key

> Infinite Recursion Detected
  - Remove infinite recursion by adding base case or changing the function logic. If it is genuine case that must stay in code like server loop, interval loop, etc., then add the function as ignore function.