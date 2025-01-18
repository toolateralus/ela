
# Ela Compiler

A simple compiler that currently transpiles down to C++ code. It features a slightly stricter type system than C++, especially for numeric types, and has many features of C++ pruned out.

> **Note about the C++ backend:**  
> The end goal is to have our own backend and a bitcode interpreter for CTFE. However, the focus is on refining the type system and front end before considering this. Using Clang and LLVM provides massive optimization with little effort, which is beneficial in the early stages. The ultimate goal is to use LLVM, provided it can be made fast.

To learn about some of the basics in the language, see `docs/learning/*` for some examples, or `examples/*` for example projects, like a game, a beat sequencer, a freestanding build, and a tiny dll linked raylib hello world.

> **Key Points:**

- Normal C-like behavior for core concepts.
- Seamless foreign function interface by compiling to C++.
- Support for Structs, Unions, Enums (Tagged Unions coming soon).
- No reference types or complex value semantics.
- Many `directive`s used with `#`, which are not like C's preprocessor.  
  [Documentation on flags & directives](docs/flags)


We have very old documentation as we are in the middle of a big re-write. 
To see the syntax, you can check out `test/*.ela` or `lib/*.ela`, until we update our documentation 