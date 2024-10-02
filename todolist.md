## TODO List

# Super Important

We desperately need to refactor the way we resolve symbols, 
And we need function overloading and such. This is key to enabling things like calling constructors without #make, 
It's mandatory for doing function polymorphism, and the same goes for parameterized structs.

Add a offsetof() or #offset() or something like that
`#offset(Vector2, x);`

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
  
  - function overloading: right now a name can only have one value. all ctors get overwritten as we compile in the symbol table,
    even if theyre not callable.
    
  - Varargs, but in a more elegant way than C. probably a `params int[] values` kind of c# thing, with an optional way to do it with an `any` type.
  - Ranges, slicing. `0..10, 0..len+1, arr[0..5]`;
  - Iterators builtin? probably not needed, but added to stdlib.
  
## these depend on function overloading
  - Polymorphic functions and polymorphic types. Like C++ templates, but less obnoxious.

  
## super easy and nice features
  - `#closure` for local functions. our scope would already allow it, but we should have a specifier that allows us to translate that to c++ code.

  
## stuff that needs work now
- types need to be in scope, not just completely global, or at least have ID's on scope where we can observe hierarchical behaviour.
- we need type aliasing for `$T` type args
- casting should not be mandatory for some of the types it is right now, especially in accordance to return types. it is way too strict.
- `#make` needs a ton of work, and it should not be the only way to construct objects.
- better initializer lists. `int[10] v {0,1,2,3,4,5,6,7,8,9};`, `Vec2 v {0, 0}`, `int[] v {0,1,2,3,4} (this would get malloc'd)` <- the last one is questionable ->
## easy : 
  - `#raw` concatenates all the tokens with no space. This should just be a lexer feature, not a directive.

**the entire compiler backend should get a cleanup pass all over. lot's of bloated code that's not neccesary** 

  
## Typing:
# This needs work. it is not where it needs to be at all.
- **explicit type conversions, implicit type conversions** 
- **no conversion table**, do we want one? or how do types define their ability to convert to others?

- **Function pointers, first class functions.** 

## Next phase
- [2] Compile time reflection
- [0] Operator Overloads. (do we even want this? could be helpful)

