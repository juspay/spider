
# Haskell Code Checker Plugin - Sheriff

## Overview

This Haskell plugin automatically verifies `fields` access from a `type` for rule violations. It scans the source code to identify types and evaluates them against predefined rules to detect any violations. Currently, it supports the following rules:

1. Blocking access to certain `fields` from a specified `type`.
2. Allowing exceptions to the rule for field access from the type based on field_access_whitelisted_fns or whitelisted_line_nos.

This tool is useful for developers to enforce better coding practices and prevent the use of specific fields from a type in the code.

## Usage

Add this to your ghc-options in cabal and mention `paymentFlow` in build-depends

```
-fplugin=PaymentFlow.Plugin
```
Also, we can provide flags to the plugin in as follows:
```
-fplugin=PaymentFlow.Plugin:{"throwCompilationError":true,"saveToFile":true,"savePath":".juspay/tmp/paymentFlow/","failOnFileNotFound":true}
```
By default, it throwsCompilationErrors.