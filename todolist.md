## TODO List

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### see 'feature/*' to see some planned/proposed features that may or may not get implemented.

# features(in order)
  - interfaces (kind of started)
  - modules.
  - we need mut/const semantics for variables and parameters etc.
  - `for i, v in some_array {} getting an index from an iterator.
  - tagged unions.
  - quick lambdas.
  - fully fleshed out constexpr interpreter. structs, unions, everything but syscalls and pointers basically.

## out-of-language features
- config file/ project. So we can organize submodules, add compilation commands & library paths, source ela libraries, etc.

# reworks.
- rework iterators completely, no more relying on C++ iterators. they suck anyway
- defer is totally busted.
- tuples need to be completely refactored in the backend.
- interpolated strings are trash
- type inference for generics is near non existent
- A lot of libraries in /lib need to be rewritten because of impl/ no more constructor/destructor reworks

- we need to instantiate templates for generics where most appropriate, not just when theyre declared.

- if we have modules, importing C headers as modules would be nice, and allowing renaming of FFI functions. (I wrote a binding generator, it might be fine.)

## general
- clean up everything. the parser is a mess, a ton of ast can be simplified, and made more performant even.
- the parser does more type system interactions than it needs to.
- the emitter should emit C, not C++. the C++ compiler is omega slow, and the more we lower to C, the closer we are
  to being easily ready to use LLVM or write our own backend, and not rely on the higher level features of C++.

## ambitious
- out of order compilation.


## things that need to be tidy'd up before we can transpile to c


- (hard) using std::tuple<T> instead of compiling our own tuple structs per instantition

- (hard) generics need to work on types that are defined AFTER the generic is defined.

- (hard) All the lambdas we use for interpolated strings, local functions, quick lambdas, switch expressions (if/while expressions)
  need to be compiled to some kind of alternative structure.

- (easy) we need to emit stuff as typedef ... NAME {} .. NAME;
