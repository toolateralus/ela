## TODO List

// TODO(Josh) 10/3/2024, 11:00:56 AM
// We need to fix aliasing. We can't alias functions with parameters,
// I thought i fixed this but i guess nto.
// TODO taking a function pointer to an overloaded function is a pretty complex issue and I'm not sure
// how we want to approach it. Right now I just basically ignore the issue.
// however doing som := &something; will cause errors in C++ and thats unacceptable.

To search for all info comments in the source just use vscodes regex search with
`TODO|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG`

--- Important ---
We desperately need to refactor the way we resolve symbols, 
And we need function overloading and such. This is key to enabling things like calling constructors without #make, 
It's mandatory for doing function polymorphism, and the same goes for parameterized structs.

Add a offsetof() or #offset() or something like that
`#offset(Vector2, x);`

## string interpolation: 
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
- **Add polymorphic functions & types.**
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
  - static class members. maybe not neccesary, but above is. 
  - const modifier for variables, members, parameters, etc. `const s32 param, const s32 v = 1` works just like c++ const;
  - multiple return values. `v, v1 := func();`
  - constexpr stuff. `#const some_expression := 100 * 2`
  - function overloading: right now a name can only have one value. all ctors get overwritten as we compile in the symbol table, even if theyre not callable.
  - Varargs, but in a more elegant way than C. probably a `params int[] values` kind of c# thing, with an optional way to do it with an `any` type.
  - Ranges, slicing. `0..10, 0..len+1, arr[0..5]`;
  - Iterators builtin? probably not needed, but added to stdlib.
  - Compile time reflection
  - Operator Overloads. (do we even want this? could be helpful)
  
## these depend on function overloading
  - Polymorphic functions and polymorphic types. Like C++ templates, but less obnoxious.
  
## stuff that needs work now
- we need type aliasing for `$T` type args
- `#make` needs a ton of work, and it should not be the only way to construct objects.
- better initializer lists. `Vec2 v {0, 0}`
- `#raw` concatenates all the tokens with no space. This should just be a lexer feature, not a directive.

**the entire compiler backend should get a cleanup pass all over. lot's of bloated code that's not neccesary**
  We should define a better structure and focus on certain phases at a time. If we have deferred symbol resolution later,
  this would cleanup that process a lot, same with generics or polymorphic functions etc.
  
## Typing:
- **no conversion table**, do we want one? or how do types define their ability to convert to others? This is only really relevant to structs, and most structs shouldn't convert to other structs, outside of the builtins like string and array that implicitly convert to a pointer to their data.

- Right now any single depth pointer can cast to any pointer of equal depth. This resembes C's weak typing and is highly undesirable.



