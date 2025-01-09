## TODO List

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### see 'feature/*' to see some planned/proposed features that may or may not get implemented.

# features(in order)
  - modules.
  - we need mut/const semantics for variables and parameters etc.
  - quick lambdas.
  - interfaces.
  - tagged unions.
  - fully fleshed out constexpr interpreter. structs, unions, everything but syscalls and pointers basically.

## out-of-language features
- config file/ project. So we can organize submodules, add compilation commands & library paths, source ela libraries, etc.

# reworks.
- destructors are like totally broken and just destroy any struct with a destructor when the function it was created in exits.
- type inference for generics is near non existent
- A lot of libraries in /lib need to be rewritten because of impl/ no more constructor/destructor reworks

- we need to instantiate templates for generics where most appropriate, not just when theyre declared.

- we should make it so `boilerplate.hpp` doesn't exist, and use our own language, where applicable.
- if we have modules, importing C headers as modules would be nice, and allowing renaming of FFI functions.

## general
- clean up everything. the parser is a mess, a ton of ast can be simplified, and made more performant even.
- the parser does more type system interactions than it needs to.
- the emitter should emit C, not C++. the C++ compiler is omega slow, and the more we lower to C, the closer we are
  to being easily ready to use LLVM or write our own backend, and not rely on the higher level features of C++.

## ambitious
- out of order compilation.
