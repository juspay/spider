# Revision history for sheriff

## 0.2.1.6
* Fix DB rules not getting applied
* Add information about exceptions to be added in infinite recursion rule

## 0.2.1.5
* Resolve names to top most name to enable unique matching for name-shadowing cases of function name
* Add test cases for name shadowing of function name

## 0.2.1.4
* Fix function level exception not working for Function Rule
* Fix infinite recursion being detected for top level function being called in `where` clause
* Fix infinite recursion being detected for partial function being called inside `fmap` or any other higher order function
* Handle infinite recursion of `LambdaCase` & `Lambda` functions for partial functions
* Add/modify test cases to check above cases
* Refactor partial function infinite recursion detection code for better reusability 

## 0.2.1.3
* Fix type signature check case by avoiding constraint matching (edge case for functions with constraint cases, only in GHC 9.2.8)

## 0.2.1.2
* Add type signature check for self recursive function calls (edge cases for instance calls)

## 0.2.1.1
* Add module name check for self recursive function calls
* Add top level module name resolving for instance functions
* Add test cases for calling function with same name but from different module
* Add State based flow implementation for infinite recursion detection

## 0.2.1.0
* Add infinite recursion rule
* Add test suite for infinite recursion rule detection
* Add ignore functions in function rules
* Refactor to use implicit params for passing plugin opts
* Refactor CommonTypes, Types, Utils and TypeUtils
* Update documentation for infinite recursion rule and supported patterns

## 0.2.0.6
* Refactor test cases in cabal file to use smaller test suites
* Revert functionality to provide sheriff plugin opt in single module
* Refactor plugin to split types modules

## 0.2.0.5
* Refactor test cases to different sub test cases instead of single test file
* Add functionality to provide sheriff plugin opt in single module
* Refactor & Re-enable log rules check as a subtest
* Add -fkeep-going to keep testing individual sub-test despite compilation error

## 0.2.0.4
* Fix modules names not getting matched due to wildcard support logical bug

## 0.2.0.3
* Fix non-exhaustive pattern
* Fix wildcard character matching for asterisk operator
* Customizable asterisk matching based on use case

## 0.2.0.2
* Remove support for wilcard character "*" in function rules (to be fixed)

## 0.2.0.1
* Add derived signature from the arg types for Signature Check rules

## 0.2.0.0
* Add allowed modules list for function rule
* Add support for wilcard character "*" in function rules
* Add support for blocking functions with a particular type signature (Signature only)
* Add test rules in sheriff rules

## 0.1.0.0 -- 2024-03-20
* First version. Basic rules based compilation error.
