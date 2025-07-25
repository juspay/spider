
# Haskell Code Checker Plugin - Sheriff

## Overview

This Haskell plugin automatically checks function calls for given rule violations. It analyzes the source code to identify function calls and checks them against defined rule if that function call violates the rule. 
It supports the following rules as of now
1. Blocking certain type of argument to a function call :- Give a particular argument number `arg_no` and list of types to be blocked in that argument `types_blocked_in_arg`
2. Blocking use of certain function in an argument to a function call :- Give a particular argument number `arg_no` and list of functions to be blocked in argument. <br>_Note: The function presence will be checked and blocked only if the type of the argument is in list of `types_to_check_in_arg`_
3. Blocking use of a particular function in the code (specify argument number `arg_no` as `0` in the rule).
4. Blocking all or particular functions with given type signature
5. Applying rules to all or particular set of modules
6. Blocking querying in DB on non-indexed columns (indexed columns are provided in a yaml file)
7. Detecting deterministic infinite recursion in the code
8. Blocking use of functions with given signature

This tool is useful for developers to enforce better coding practices and prevent use of some specific unsafe function in the code.

## Usage

Add this to your ghc-options in cabal and mention `sheriff` in build-depends

```
-fplugin=Sheriff.Plugin
```
Also, we can provide flags to the plugin in as follows:
```
-fplugin-opt=Sheriff.Plugin:{"throwCompilationError":true,"saveToFile":true,"savePath":".juspay/tmp/sheriff/","indexedKeysPath":".juspay/indexedKeys.yaml",
"rulesConfigPath":".juspay/sheriffRules.yaml","exceptionsConfigPath":".juspay/sheriffExceptionRules.yaml","failOnFileNotFound":true,"matchAllInsideAnd":false}
```
By default, it throwsCompilationErrors and doesn't log to file. Also, it fails, if indexedKeys file is not found or is invalid.