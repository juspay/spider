### Sheriff plugin
#### What it does?
`sheriff` is a compiler plugin to add business logic compilation checks. It checks for the following logics :
1. ***Stringification of text/string*** - It checks if we have applied show function to any string literal or variable. We want this check to make sure that we are not having any unnecessary quotes in the code.
2. ***Logging stringified haskell objects*** - It checks if we have applied show function to any variable inside any logging function. We want this check to make sure that our application level log filtering works fine and is not impacted by stringified values.
3. ***NonIndexed DB queries check*** - It checks if we have any DB query which won't use any index. Indexed columns of a particular table are provided in *.juspay/indexedKeys.yaml* file. It works on all operations - select, update, delete. We want this check to make sure that our application doesn't make any slow sql queries.

#### How to resolve compilation errors?
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
