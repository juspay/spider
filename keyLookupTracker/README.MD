
# Haskell Code Checker Plugin - Sheriff

## Overview

The `keyLookupTracker` plugin is designed to analyze, track, and log keys accessed through lookup functions (e.g., HM.lookup) in HashMaps within Haskell codebases

## Usage

Add this to your ghc-options in cabal and mention `keyLookupTracker` in build-depends

```
-fplugin=keyLookupTracker.Plugin
```