
# Haskell Function Call Graph Plugin

## Overview

This Haskell plugin automatically generates function call graphs for Haskell packages. It analyzes the source code to identify function calls and relationships, then produces a visual representation of these calls as a graph. This tool is useful for developers seeking to understand the structure and dependencies of their Haskell codebase.

## Usage

Add this to your ghc-options in cabal and mention fdep it in build-depends

Use this when using through nix build
```
-fplugin=Fdep.Plugin
```

use this when using with cabal 

```
-fplugin=Fdep.Plugin -fplugin-opt Fdep.Plugin:./tmp/fdep/
```
