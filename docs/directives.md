# '#' Directives

### Summary of Hash Directives and Their Usages

#### `#include`
- **Kind**: `Statement`
- **Usage**: 
  - Similar to C's include, it pastes a text file right above where the include is used.
- **Example**:
  ```cpp
  #include "filename"
  ```

#### `#raw`
- **Kind**: `Expression`
- **Usage**: 
  - String literals delimited by `#raw` can span multiple lines.
  - *Do not use this. it is currently bugged*
- **Example**:
  ```cpp
  string: u8* = #raw
  This is a raw string literal
  #raw
  ```

#### `#read`
- **Kind**: `Expression`
- **Usage**: 
  - Reads a file into a string at compile time. there is currently no option for binary output, it defaults to a char * as text. However this can and will be added easily.
- **Example**:
  ```cpp
  #read "filename"
  ```

#### `#test`
- **Kind**: `Statement`
- **Usage**: 
  - Declares a test function.
  - Only compiled into `--test` builds.
- **Example**:
  ```cpp
  #test test_function :: fn() { ... }
  ```

#### `#foreign`
- **Kind**: `Statement`
- **Usage**: 
  - Declares a foreign function, similar to C's `extern`.
- **Example**:
  ```cpp
  #foreign foreign_function :: fn(char *, ...);
  #foreign foreign_function :: fn(int) -> int;
  ```

#### `#import`
- **Kind**: `Statement`
- **Usage**: 
  - Imports from `/usr/local/lib/ela` by identifier and no file extension.
- **Example**:
  ```cpp
  #import MyModule;
  ```

#### `typeof`
- **Kind**: `Expression`
- **Usage**: 
  - Gets a `Type *` struct pointer to reflect on a given type.
- **Example**:
  ```cpp
  typeof(MyType)
  ```


#### `#c_flags`
- **Kind**: `Statement`
- **Usage**: 
  - Adds compiler flags like linker options, `-ffreestanding -fno-std-lib | -fsanitize=address | -L/my_lib/path`, etc., from within your program or header.
  - you don't need `-g` or `-O<N>` flags, just pass `--release` to the compiler invocation to build in a non-debug mode. this defaults to `-O3`
  - the most common usage of this is a library that needs to link against a certain static/dynamic C library.
- **Example**:
  ```cpp
  #c_flags "-L/lib/local/custom-c-libs -lraylib-custom"
  ```

#### `#flags`
- **Kind**: `Statement`
- **Usage**: 
  - Makes an enum declaration auto-increment with a flags value.
- **Example**:
  ```cpp
  #flags MyEnum :: enum { ... }
  ```

#### `#alias`
- **Kind**: `Statement`
- **Usage**: 
  - Creates type aliases.
- **Example**:
  ```cpp
  #alias NewName :: OldName

  ```
#### `#self`
- **Kind**: `Expression`
- **Usage**: 
  - Returns the type of the current declaring struct or union.
- **Example**:
  ```cpp
  #self
  ```

#### `#anon`
- **Kind**: `Statement`
- **Usage**: 
  - Declares anonymous structs. Can only be used in enums, and cannot use c style.
- **Example**:
  ```cpp
  #anon :: struct { ... }
  ```
  **_THIS IS NOT VALID_** :
  ```cpp
  #anon :: struct { ... } fieldName;
  ```

#### `#export`
- **Kind**: `Statement`
- **Usage**: 
  - Declares a function as being available to dynamic libraries. This is mandatory for creating a visible function within a dynamic library on both linux and windows.
- **Example**:
  ```cpp
  #export some_function_that_is_in_a_dll :: fn() -> int {
    return 0;
  }
  ```
  
  
