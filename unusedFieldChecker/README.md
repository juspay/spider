# Haskell Unused Field Checker Plugin - UnusedFieldChecker

## Overview

This Haskell plugin automatically detects unused record fields and raises compilation errors for unused non-Maybe fields. It analyzes the entire codebase to identify record fields that are never used and enforces that all required (non-Maybe) fields must be utilized.

**Key Features:**
1. Detects unused record fields across the entire repository
2. Distinguishes between Maybe (optional) and non-Maybe (required) fields
3. Raises compilation errors only for unused non-Maybe fields
4. Tracks field usage in all contexts: field access, pattern matching, record construction, and record updates
5. Supports exclusion configuration for backward compatibility with legacy code
6. Compatible with GHC 8.8.3+ and GHC 9.0+

This tool helps developers maintain cleaner code by ensuring all required data fields are actually being used.

## Usage

Add `unusedFieldChecker` to your `build-depends` and add the plugin to your `ghc-options`:

```cabal
build-depends:
    base,
    unusedFieldChecker,
    ...

ghc-options:
    -fplugin=UnusedFieldChecker.Plugin
    -fplugin-opt=UnusedFieldChecker.Plugin:'{"path":"./tmp/unusedFieldChecker/","port":4446,"host":"::1","log":false,"exclusionConfigFile":"UnusedFieldChecker.yaml"}'
```

### Integration with common-lang

Add to your common-lang stanza in both Local and production flags:

```cabal
common common-lang
  if flag(Local)
    ghc-options:
      ...
      -fplugin=UnusedFieldChecker.Plugin
      -fplugin-opt=UnusedFieldChecker.Plugin:'{"path":"./tmp/unusedFieldChecker/","port":4446,"host":"::1","log":false,"exclusionConfigFile":"UnusedFieldChecker.yaml"}'
  else
    ghc-options:
      ...
      -fplugin=UnusedFieldChecker.Plugin
      -fplugin-opt=UnusedFieldChecker.Plugin:'{"path":"./tmp/unusedFieldChecker/","port":4446,"host":"::1","log":false,"exclusionConfigFile":"UnusedFieldChecker.yaml"}'
```

**Configuration Options:**
- `path`: Directory for storing field information (default: `/tmp/unusedFieldChecker/`)
- `port`: Port for Unix socket communication (default: `4445`)
- `host`: Host for socket communication (default: `::1`)
- `log`: Enable debug logging (default: `false`)
- `exclusionConfigFile`: Path to YAML exclusion config file (default: `UnusedFieldChecker.yaml`)

## Exclusion Configuration (Backward Compatibility)

Create a `UnusedFieldChecker.yaml` file in your project root to exclude specific fields from validation:

```yaml
exclusions:
  # Exclude specific fields in a type
  - module: "MyModule.Types"
    dataType: "User"
    fields:
      - "legacyField1"
      - "legacyField2"
  
  # Exclude all fields in a type (empty list)
  - module: "Legacy.OldTypes"
    dataType: "OldConfig"
    fields: []
  
  # Use wildcards for module matching
  - module: "MyApp.Legacy.*"
    dataType: "LegacyData"
    fields:
      - "oldField"
```

**Exclusion Rules:**
- Module matching: Exact match or wildcard (e.g., `"MyApp.*"` matches all submodules)
- Type matching: Exact match (case-sensitive)
- Field matching: List specific field names, or use empty list `[]` to exclude all fields in that type
- Excluded fields will NOT trigger compilation errors even if unused and non-Maybe

See `UnusedFieldChecker.yaml.example` for more examples.

## Examples

### Compilation Error - Unused Non-Maybe Field

```haskell
data User = User
    { userName :: Text      -- Used
    , userAge :: Int        -- UNUSED - Compilation Error!
    , userEmail :: Text     -- Used
    }

displayUser :: User -> Text
displayUser user = userName user <> " - " <> userEmail user
-- Error: Field 'userAge' in type 'User' is never used and is not a Maybe type.
```

### No Error - Unused Maybe Field

```haskell
data User = User
    { userName :: Text
    , userAge :: Maybe Int     -- Unused but Maybe - OK
    , userEmail :: Text
    }

displayUser :: User -> Text
displayUser user = userName user <> " - " <> userEmail user
-- No error: userAge is Maybe and can be unused
```

### No Error - All Fields Used

```haskell
data User = User
    { userName :: Text
    , userAge :: Int
    , userEmail :: Text
    }

displayUser :: User -> Text
displayUser User{..} = 
    userName <> " (" <> show userAge <> ") - " <> userEmail
-- No error: All fields are used
```

## How It Works

1. **Field Definition Extraction**: Extracts all record fields from type declarations and identifies Maybe vs non-Maybe types
2. **Usage Tracking**: Tracks field usage in field access, pattern matching, record construction, and record updates
3. **Cross-Module Analysis**: Aggregates field information across all modules using Unix socket communication
4. **Validation**: Matches field definitions with usages and identifies unused fields
5. **Error Reporting**: Raises compilation errors for unused non-Maybe fields (excluding fields in the exclusion config)

## Benefits

- **Catch Dead Code**: Identify fields that were added but never used
- **Enforce Data Usage**: Ensure all required fields are actually utilized
- **Refactoring Safety**: Detect when field usage is accidentally removed
- **Backward Compatibility**: Gradually migrate legacy code using exclusion configuration
- **Code Quality**: Maintain cleaner, more intentional data structures

## License

MIT
