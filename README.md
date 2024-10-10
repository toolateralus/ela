
# Ela Compiler

A simple compiler that currently transpiles down to C++ code. It features a slightly stricter type system than C++, especially for numeric types, and has many features of C++ pruned out.

> **Note about the C++ backend:**  
> The end goal is to have our own backend and a bitcode interpreter for CTFE. However, the focus is on refining the type system and front end before considering this. Using Clang and LLVM provides massive optimization with little effort, which is beneficial in the early stages. The ultimate goal is to use LLVM, provided it can be made fast.

For a good idea of what is and isn't in the language, see `source/test.ela` for our end-to-end tests, or `examples/` for some example applications. Documentation is limited as the compiler is brand new, about a week or two in development.

> **Key Points:**

- Normal C-like behavior for core concepts.
- Seamless foreign function interface by compiling to C++.
- Support for Structs, Unions, Enums (Tagged Unions coming soon).
- No reference types or complex value semantics.
- Many `directive`s used with `#`, which are not exactly like C's preprocessor.  
  [Documentation on flags & directives](docs/flags)


## Very basic syntax overview

### Comments

- Single-line comments: `// This is a comment`
- Multi-line comments: `/* This is a multi-line comment */`

### Importing Modules

Modules are just .ela files located in /usr/local/lib/ela/...
This is not the final form, it's just not implemented yet.

```cpp
#import core;
#import raylib;
```

### Including files
C style include.
```cpp
#include "my_file.ela"
```

### Compiler Flags

Append a flag or a set of space seperated flags to the C++ compiler invocation.
Beware of contradictory flags.

```cpp
#compiler_flags "-g"
```

### Structs

```cpp
Person :: struct {
  name: string;
  age: int;
}

Vector2 :: struct {
  x: float32;
  y: float32;
}

Vector3 :: struct {
  vec2: Vector2;
  z: float32;
}
```

### Unions

```cpp
Value :: union {
  intValue: int;
  floatValue: float;
  charValue: char;
}
```

### Enums

```cpp
#flags Status :: enum {
  Active,
  Inactive,
  Pending,
  Completed,
}

Color :: enum {
  Red,
  Green,
  Blue = 100 * 2,
}
```

### Functions

```cpp
add :: (a: int, b: int) -> int {
  return a + b;
}

multiply :: (x: float32, y: float32) -> float32 {
  return x * y;
}

get_value :: () -> int {
  return 42;
}
```

### Methods

```cpp
Rectangle :: struct {
  width: float32;
  height: float32;

  area :: () -> float32 {
    return width * height;
  }

  scale :: (factor: float32) {
    width *= factor;
    height *= factor;
  }
}
```

### Constructors and Destructors

```cpp
Resource :: struct {
  data: int*;

  #ctor :: () {
    data = malloc(sizeof(int));
  }

  #dtor :: () {
    free(data);
  }
}
```

### Arrays


Fixed sized arrays, just like C.
```cpp
numbers: int[10];
numbers[0] = 1;
```
Dynamic arrays, much like Vec in rust or std::vector<> in C++.

Note the operators are not yet fully decided on and are subject to change.
```cpp
numbers: int[];

// push 10
numbers ~= 10;

// length info.
printf("%d\n", numbers.length);

// find and erase 10, if it exists. if it doens't do nothing.
numbers ~~ 10;

// pop the element from the end.
back := ~numbers;

```


### Pointers

```cpp
value: int = 10;
ptr: int* = &value;
```

### Memory Management

**No `->` operator is needed for accessing structs and objects behind a pointer**

**The `.` operator works for both**

- Of course if you import core you get access to normal C allocators.
```cpp
buffer: int* = malloc(10 * sizeof(int));
free(buffer);
buffer = null;
```

- new and delete are also available, with a slight improvement on c++
```cpp
// Normal c++ style new.
n := new MyStruct(0, 0);
n1 := new MyStruct(1,1);

// You can delete multiple pointers at once.
// Delete sets the pointers to null after deletion.
delete(n, n1);
```



### Tests

```cpp
#test test_addition :: () {
  result: int = add(2, 3);
  assert("result == 5", result == 5);
}

#test test_struct_initialization :: () {
  vec: Vector2;
  assert("vec.x == 0.0", vec.x == 0.0);
  assert("vec.y == 0.0", vec.y == 0.0);
}
```

### Directives
- See the documentation above for info about these. [Documentation on flags  & directives](docs/flags)


There is a lot more but this documentation needs improvement. Please, contribute!
