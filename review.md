## general
  - I would've either used a std::bitset or made my own type for managing flags. the bitwise and operations everywhere seem fine until your code is absolutely unreadable.
  - Use of interned strings would vastly improve the performance of this compiler.
  - I would've avoided using any std:: types, because template instantation hurts compile times a lot, and std::vector is everywhere. also, std::string is terrible for performance and causes a million unneccesary allocations.

## type system
  - I would have made the type system scoped from day 1. module scope, file scope.
  - would've used polymorphic `Type*` instead of a polymorphic `TypeInfo*`. It adds an unneccesary layer of complexity
  - should have used a more intuitive way to search for types, and maybe pointers and arrays would just be another `Type*` that contains the pointed-to or element type.
  - maybe we should've used a hashmap instead of a flat array of `Type*`. perhaps slower for small programs but the cost of linearily searching through thousands of types likely multiple times per lookup is unacceptable.
  - 
  
## lexer
  - Interned strings instead of carrying and copying `std::string` around everywhere. at least take a view of the string.
  - otherwise the design of our lexer is pretty solid and well used in other projects.
  - Perhaps we could've streamed the file that was being compiled in the `LexerState` since we only read 8 tokens at a time in the lookahead buffer.
  - Interpolated strings should've been implemented in the lexer and not in the parser. it caused so many bugs and problems down the line, same with raw strings.
  
## parser
  - the parser is pretty solid. it's a bit messy in some spots, but the final outcome could certainly be cleaned up and made to be faster.
  
## visitors
  - we should have implement order independent compilation day 1. it is so hard to implement that retroactively.
  - would've avoided using a base visitor with `std::any` return type and instead just used either a templated return type or hand written all of the functions to not use indirection. faster and easier to manage with a heavier up front cost
  - the way the multiple stages of compilation works is nice to seperate stuff in a logical manner.
  - the type visitor should have been split up into several visitors, or phases. it is responsible for far too many jobs.
  - the emit visitor is written like absolute garbage and its horrible. its unreadable, unchangeable, and slow as all hell.
  
## symbol table
  - again, interned strings instead of std::string, would've made things a helluva lot faster.
  - using small scope optimization and promoting large scope to a hash map would be much more performant.
  - file scope and module scope should've been implemented day 1.
  - symbols would store more information than just their type, and maybe not even exist in an island like they do here.
  - ast could serve as the symbol table, and each ast could own a declaring_scope which would always be a block. then the AST would manage it's own scope on visitation. Also, this would prevent us from having to mirror flags stored in the AST nodes, in functions for example, in the symbol table as well. such as SYMBOL_IS_OVERLOADED, etc. full lexical scoping

## modules
  - we could have made modules a single static library per unit, and just have some serialized format that we can read it's symbol table from. then we wouldn't have to recompile the modules each time you import them, you could just link against them.

## lowering
  - instead of lowering to C++, we should've lowered to C. this would've lead us to be less lazy in implementing high level features, and also the compiler is muuuch much faster than the C++ compiler. methods a vtables can be implemented easily, constructors can just be global routines or macros. defers can just be goto's to blocks. in this compiler, we got lazy as hell and relied far too much on C++ being high level.

## syntax
  - `fn function_name(arg: type) return_ty` syntax kind of like go would be much preferable. it's clearer, easier to parse,
  and easier to type.
  - `*int` instead of `int*`. pointer to int instead of int pointer.
  - `[100]int` instead of  `int[100]` as well.