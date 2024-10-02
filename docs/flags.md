
# Flags you can pass to the compiler

> note this is very easy to extend, there is very little hardcoded data associated with flags, its a hashtable of strings.

## --debug
Compile the output source with debug symbols.

> note: to use `gdb` or another debugger, in your code use
`#compiler_flags "-g"` as well as the --debug flag in the command line.
This is to be reworked

## --s
Don't delete the `.hpp` and `.cpp` files used to transpile.

## --metrics
Write performance metrics (time taken for each compilation step) to stdout.

## --test
Only emit functions marked `#test` and create a test runner. The output binary will run all the tests. much like cargo test.



# '#' Directives

### Summary of Hash Directives and Their Usages

#### `#include`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Similar to C's include, it pastes a text file right above where the include is used.
  - Not a pre-processor.
- **Example**:
  ```cpp
  #include "filename"
  ```

#### `#raw`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - String literals delimited by `#raw` can span multiple lines.
- **Example**:
  ```cpp
  u8 *string = #raw
  This is a raw string literal
  #raw
  ```

#### `#read`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - Reads a file into a string at compile time.
- **Example**:
  ```cpp
  #read "filename"
  ```

#### `#test`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Declares a test function.
  - Only compiled into `--test` builds.
- **Example**:
  ```cpp
  #test test_function :: () { ... }
  ```

#### `#foreign`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Declares a foreign function, similar to C's `extern`.
- **Example**:
  ```cpp
  #foreign foreign_function :: (char *, ...);
  #foreign foreign_function :: (int) -> int;
  ```

#### `#import`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Imports from `/usr/local/lib/ela` by identifier and no file extension.
- **Example**:
  ```cpp
  #import MyModule;
  ```

#### `#type`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - Gets a `Type *` struct pointer to reflect on a given type.
- **Example**:
  ```cpp
  #type MyType
  ```

#### `#ctor` and `#dtor`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Declares constructor (`#ctor`) and destructor (`#dtor`) functions.
- **Example**:
  ```cpp
  #ctor :: () { ... }
  #dtor :: () { ... }
  ```

#### `#make`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - Serves as a casting and copy construction method, as well as normal constructors.
- **Example**:
  ```cpp
  #make(MyType, arg1, arg2)
  ```

#### `#compiler_flags`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Adds compiler flags like linker options, `-g`, etc., from within your program or header.
- **Example**:
  ```cpp
  #compiler_flags "-g -O2"
  ```

#### `#flags`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Makes an enum declaration auto-increment with a flags value.
- **Example**:
  ```cpp
  #flags MyEnum :: enum { ... }
  ```

#### `#alias`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Creates type aliases.
- **Example**:
  ```cpp
  #alias NewName :: OldName
  ```

#### `#typename`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - Gets the name of a type as a string.
- **Example**:
  ```cpp
  #typename(MyType)
  ```

#### `#self`
- **Kind**: `DIRECTIVE_KIND_EXPRESSION`
- **Usage**: 
  - Returns the type of the current declaring struct or union.
- **Example**:
  ```cpp
  #self
  ```

#### `#anon`
- **Kind**: `DIRECTIVE_KIND_STATEMENT`
- **Usage**: 
  - Declares anonymous structs. Can only be used in enums, and cannot use c style.
- **Example**:
  ```cpp
  #anon :: struct { ... }
  ```
  do NOT do:
  ```
  #anon :: struct { ... } fieldName;
  ```
  