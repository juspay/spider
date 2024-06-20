# FieldInspector Plugin

## Overview

The FieldInspector plugin is a GHC (Glasgow Haskell Compiler) plugin designed to collect and analyze field usage information within Haskell code. This plugin is useful for understanding how fields within data types are used across different modules in a Haskell project.

## Installation

To use the FieldInspector plugin, include it in your GHC configuration. Typically, this involves adding the plugin to your project's `.cabal` file or passing it as a command-line argument when invoking GHC.

## Usage

### Command-Line Options

The plugin accepts the following command-line options:

- A single optional argument specifying the output directory for the generated JSON files. If no argument is provided, the default directory is `/tmp/fieldInspector/`.

### Enabling the Plugin

To enable the plugin, add the following line to your GHC command:

```
-ghc-options "-fplugin=FieldInspector.Plugin"
```
If you want to specify an output directory, use:
```
-ghc-options "-fplugin=FieldInspector.Plugin:<output-directory>"
```

## How It Works

The plugin operates in two main phases:

1. **Core-to-Core Pass**:
   - The `buildCfgPass` function is executed during the core-to-core transformation phase of the GHC compilation process.
   - This function analyzes the Core bindings (`CoreBind`) in the current module and collects field usage information.
   - The collected information is written to a JSON file named `fieldUsage.json` in the specified or default directory.

2. **Type Collection**:
   - The `collectTypes` function is executed after the type-checking phase.
   - This function collects information about type constructors (`TyCon`) and data constructors (`DataCon`) in the module.
   - The collected type information is written to a JSON file named `types.json` in the specified or default directory.

## Output
The plugin generates two JSON files for each module:

### fieldUsage.json: 
    Contains information about field usage within the module.
### types.json: 
    Contains information about types and their constructors within the module.


## The exe onsolidates these files into two comprehensive datasets:

### fieldUsage-data.json:
    consolidated all modules data
### types-data.json
    consolidated all modules data