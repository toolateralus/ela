## TODO List

### Arrays and Maps

- Right now maps have no operators but []

- Maps are incredibly buggy and only work for very specific circumstances.

- Arrays have ~=, ~~ and ~, but those are pretty bad and should probably be replaced with either methods
  or some better clearer operators.

### Imports & FFI
- We should have a way to rename FFI functions and imported symbols.
`#foreign DrawBackground :: (...) as "draw_background";
> So we can have libraries conform to our own naming conventions.
- Python / JS style imports.
`#import {this, that, another} from raylib;`


To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

Add a offsetof() or #offset() or something like that
`#offset(Vector2, x);`

## string interpolation: 
 Interpolating strings is *SUPER* slow, we need to find a much better waay to do it than using lambdas and sprintf.

 Right now a string interpolation is limited to rules created by the lexer. So you can't use any characters that arent recognized by the language as tokens
 I don't really want to make another lexer so we should find a way to put the lexer in a state that says we don't care about unrecognized tokens, just build them as best as you can. That probably wont work because thenw ed end up lexing `\"{something}\"` as 
  Operator: `\"{`
  Iden: `something`
  Operator: `}\"`
 I am not certain how we'd fix that.


### General Tasks
- **Add the ability to forward declare your own structs, not just ones that exist in external libraries**
  Right now, we get a redefinition error or just a straight failure to parse a statement when you try to do
  ```
  // declaration
    Something :: struct;
  // definition
    Something :: struct {
      float32 value;
      float32 other_value;
    }
  ```
  
- **Add the ability to cast initializer lists to an appropriate struct type, implicitly.**
  - Example: `func :: (Vec2 ..) {}` and `func({0, 0});`
- **Fix casting bug where we can't cast an initializer list of float literals (float32) to `float[]`.**

- **Add union types, with an optional tagged union.**
- **Add a way to use initializer lists with fixed arrays.**
- **Add generic functions & types.**
- **Add the ability to cast initializer lists to an appropriate struct type, implicitly.**
  - Example: `func :: (Vec2 ..) {}` and `func({0, 0});`
- **Fix casting bug where we can't cast an initializer list of float literals (float32) to `float[]`.**

- **Need to be able to take references to the variables in the array.**
  ```jai
  // such as 
  for &v; vertices {
    ...
  }
  
  // or just a pointer to it, 
  for *v; vertices {
    ...
  }
  
  we should be able to easily output different C code for that.

## new features
  - const modifier for variables, members, parameters, etc. `const s32 param, const s32 v = 1` works just like c++ const;
  - multiple return values. `v, v1 := func();`
  - Varargs, but in a more elegant way than C. probably a `params int[] values` kind of c# thing, with an optional way to do it with an `any` type.
  - Ranges, slicing. `0..10, 0..len+1, arr[0..5]`;
  - constexpr stuff. `#const some_expression := 100 * 2` , or just a bitcode interpreter for ctfe.
  - Compile time reflection
  - generic types. Like C++ templates, but less obnoxious.
  
## stuff that needs work now
- `#make` needs a ton of work, and it should not be the only way to construct objects.
- better initializer lists. `Vec2 v {0, 0}`
- `#raw` concatenates all the tokens with no space. This should just be a lexer feature, not a directive.


**the entire compiler backend should get a cleanup pass all over. lot's of bloated code that's not neccesary**
  We should define a better structure and focus on certain phases at a time. If we have deferred symbol resolution later,
  this would cleanup that process a lot, same with generics or generic functions etc.
  
## Typing:
- Right now any single depth pointer can cast to any pointer of equal depth. This resembes C's weak typing and is highly undesirable.



