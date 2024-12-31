## TODO List

## Add interfaces
```cpp
IInterface :: interface {
  // needs to be implemented by the derived struct.
  to_string :: fn() -> string;
  
  // implemented by the interface and cannot be overriden.
  get_something :: fn() -> int {
    return 0;
  }
  
  // compile time constant provided by the interface.
  CONSTANT :: 100;
  
  #ctor :: fn() {
    // constructor gets called when an object that implements this interface gets constructed.
  }
  
  #dtor :: fn() {
    // just like ctor.
  }
}

Struct :: struct {
  x: int;
  y: int;
}

impl IInterface for Struct {
  // we have access to the struct memebers as if we were a method.
  to_string :: fn() -> string {
    return $"x={x}, y={y}";
  }
}

// we can take the interface itself as a parameter and anything that
// implements it can be used in its place. We may or may not have to take it
// by a pointer, depending on the internal implementation our compiler provides.
operate_on_interface :: fn(iinterface: IInterface*) {
  println(iinterface.to_string()); 
}
```

### Constants
```cpp
  // We have just totally failed to implement constants.
  // You can use enums to achieve this, with any type, but this is stll a useful thing to have.
  CONSTANT :: 100;
  CONSTANT1 :: "My Char*";
  // even non-compile time constants can still be const.
  // just like C++ const semantics, not constexpr.
  CONSTANT2 :: string{10,20,30};
```


To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### Arrays and Maps

- We should be able to do slicing with ranges.
  This shouldn't be too hard to achieve.
  
```rust
  arr : int[] = {0,1,2,3,4,5};
  slice: arr[0..3]; // {0,1,2};
```

- Right now maps have no operators other than []

```cpp
  map: int[string];
  
  // this works, we implemented this.
  map.contains("Some string")
  
  //! NYI
  map.erase("Some string")
  
  ptr : int* = map.find("Some string");
  
  // We don't have tuples in the language yet.
  // nor can you iterate over maps.
  for key, value; map {
    
  }
  
  // clear functions need to be added for maps, and [] arrays.
  map.clear();
```


- Maps are incredibly buggy and only work for very specific circumstances. Nested maps fail consistently.
- The syntax is terrible for nested maps, and is really confusing.

`int[string][string];` 
- What does this even mean? I think its `map<map<string, int>>;`

- Arrays have ~=, ~~ and ~, but those are pretty bad and should probably be replaced with either methods or some better clearer operators. Really we should just have 

```cpp

  arr: int[];
  arr.push(...);
  arr.pop();
  arr.clear();
  arr.find(...) -> int*;
  arr.erase();
  arr.resize();
  arr.reserve();
```

### Imports & FFI
- We should have a way to rename FFI functions and imported symbols.
`#foreign DrawBackground :: fn(...) as "draw_background";

> So we can have libraries conform to our own naming conventions.

- Python / JS style imports.
`#import {this, that, another} from raylib;`




Add a offsetof() or #offset() or something like that
`#offset(Vector2, x);`

Right now we can reflect on a type to get a field's offset.
However that requires some indirection, and it would be nice to get a compile time version. However we don't have a sizer.

## string interpolation: 
 Interpolating strings is *SUPER* slow, we need to find a much better waay to do it than using lambdas and sprintf.

 Right now a string interpolation is limited to rules created by the lexer. So you can't use any characters that arent recognized by the language as tokens
 I don't really want to make another lexer so we should find a way to put the lexer in a state that says we don't care about unrecognized tokens, just build them as best as you can. That probably wont work because thenw ed end up lexing `\"{something}\"` as 
  Operator: `\"{`
  Iden: `something`
  Operator: `}\"`
 I am not certain how we'd fix that.


### General Tasks
- **Fix casting bug where we can't cast an initializer list of float literals (float32) to `float[]`.**
- **Add tagged union.**
- **Add generic types.**

## new features
  - const modifier for variables, members, parameters, etc. `const s32 param, const s32 v = 1` works just like c++ const;
  - multiple return values. `v, v1 := func();`
  - Varargs, but in a more elegant way than C. probably a `params int[] values` kind of c# thing, with an optional way to do it with an `any` type.
  - slicing. `0..10, 0..len+1, arr[0..5]`;
  - constexpr stuff. `#const some_expression := 100 * 2` , or just a bitcode interpreter for ctfe.
  - Compile time reflection
  - generic types. Like C++ templates, but less obnoxious.
  
## stuff that needs work now
- `#make` should be completely elimintated. It's clunky, and we have other ways of constructing stuff.

- `#raw` concatenates all the tokens with no space. This should just be a lexer feature, not a directive.

**the entire compiler backend should get a cleanup pass all over. lot's of bloated code that's not neccesary**
  We should define a better structure and focus on certain phases at a time. If we have deferred symbol resolution later,
  this would cleanup that process a lot, same with generics or generic functions etc.
  
## Typing:
- Right now any single depth pointer can cast to any pointer of equal depth. This resembes C's weak typing and is highly undesirable.

- having stuff like `Something*[]*` proves very challenging to the type system and it's not very well equipped to deal with handling highly complex types.



