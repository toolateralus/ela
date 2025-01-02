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

#### `#type`
- **Kind**: `Expression`
- **Usage**: 
  - Gets a `Type *` struct pointer to reflect on a given type.
- **Example**:
  ```cpp
  #type MyType
  ```

#### `#ctor` and `#dtor`
- **Kind**: `Statement`
- **Usage**: 
  - Declares constructor (`#ctor`) and destructor (`#dtor`) functions.
- **Example**:
  ```cpp
  #ctor :: fn() { ... }
  #dtor :: fn() { ... }
  ```

#### `#make`
- **Kind**: `Expression`
- **Usage**: 
  - Serves as a casting and copy construction method, as well as normal constructors.
- **Example**:
  ```cpp
  #make(MyType, arg1, arg2)
  ```

#### `#c_flags`
- **Kind**: `Statement`
- **Usage**: 
  - Adds compiler flags like linker options, `-g`, etc., from within your program or header.
  - This is only a thing because of us using the C++ compiler stll for a backend.
- **Example**:
  ```cpp
  #c_flags "-g -O2"
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

#### `#operator`
- **Kind**: `Statement`
- **Usage**: 
  - Declare an operator overload for a given struct or union. There is not an official list of banned or non-overrideable operators, but () is one of them.
- **Example**:
  ```cpp
  
  Vec2 :: struct {
    x: float;
    y: float;
    // Note: you have to manually override both + and += if you want them both supported.
    #operator(+) :: fn(other: #self) -> #self {
      // at the time of writing this, you have to use
      // #make(#self, {..init_list..}) for return types.
      // however for parameters, arguments, and all other declarations,
      // you do not need to manually call a constructor for an initializer list.
      
      // ! This works right now though: 
      // ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- 
      //  return #make(#self {x + other.x, y + other.y});
      // ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- 
      
      // but this will be fixed soon.
      
      return {x + other.x, y + other.y};
    }
  }
  
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
  
  
