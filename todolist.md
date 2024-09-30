
TODO(Josh): debug information is bad for some statements


## new features
  - switch statements
  - enum declarations. `enum {...} || enum MyEnum {...}`
  - static member access, `Type.Something`. neccesary for enums.
  - static class members. maybe not neccesary, but above is.
  - const modifier for variables, members, parameters, etc. `const s32 param, const s32 v = 1` works just like c++ const;
  - multiple return values. `v, v1 := func()`
  - constexpr stuff. `#const some_expression := 100 * 2`
  - #flags for enums so instead of 0,1,2,3, you get `1 << 0, 1 << 1, 1 << 2, 1 << 3` etc. Also these types would have a .has() and .set() etc, and not need any integer casting.
  - function overloading: right now a name can only have one value. all ctors get overwritten as we compile in the symbol table,
    even if theyre not callable.
    

    
  ### EASY: Add binary and hexadecimal numbers in the lexer.
    
```
func_a :: (s32 v) -> s32 {
  return v;
}
func_a :: (s64 v) -> s64 {
  return v;
}
```
    
  - Varargs, but in a more elegant way than C.
  - Ranges, slicing. `0..10, 0..len+1, arr[0..5]`;
  - Iterators builtin? probably not needed.
  - Type aliasing.

  
## stuff that needs work now
- types need to be in scope, not just completely global, or at least have ID's on scope where we can observe hierarchical behaviour.
- we need type aliasing for `$T` type args
- casting should not be mandatory for some of the types it is right now, especially in accordance to return types. it is way too strict.
- `#make` needs a ton of work, and it should not be the only way to construct objects.
- `#raw` concatenates all the tokens with no space. This should just be a lexer feature, not a directive.
- better initializer lists. `int[10] v {0,1,2,3,4,5,6,7,8,9};`, `Vec2 v {0, 0}`, `int[] v {0,1,2,3,4} (this would get malloc'd)` 

**the entire compiler backend should get a cleanup pass all over. lot's of bloated code that's not neccesary** 

  
## Typing:
# This needs work. it is not where it needs to be at all.
- **explicit type conversions, implicit type conversions** 
- **no conversion table**, do we want one? or how do types define their ability to convert to others?



- **Function pointers, first class functions.** 

## Next phase
- [2] Compile time reflection
- [0] Operator Overloads. (do we even want this? could be helpful)