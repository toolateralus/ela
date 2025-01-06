## TODO List

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### see 'feature/*' to see some planned/proposed features that may or may not get implemented.

# features, reworks.
- type checking for constructors is like non-existent.
- type inference for generics is near non existent as well.
- A lot of libraries in /lib could be rewritten for newer features.
- we desperately need modules, for organization. 
- at that rate, we might as well add private of some sort, with a default public.
- we need mut/const semantics for variables and parameters etc.
- constexpr evaluator needs to be an interpreter that can handle structs and stuff.
- we should add `defer`.
- we need to instantiate templates for generics where most appropriate, not just when theyre declared.
  example:

```cpp
generic :: fn![T]() {
  ...
}

my_struct :: struct {

}

// error here, because my_struct is defined after generic, and generic emits all it's instantiations at
// the root declaration, so cpp cant find my_struct
generic![my_struct]();


```

- we should make it so `boilerplate.hpp` doesn't exist, and use our own language, where applicable.
- if we have modules, importing C headers as modules would be nice, and allowing renaming of FFI functions.

## general
- clean up everything. the parser is a mess, a ton of ast can be simplified, and made more performant even.
- the parser does more type system interactions than it needs to.
- the emitter should emit C, not C++. the C++ compiler is omega slow, and the more we lower to C, the closer we are
  to being easily ready to use LLVM or write our own backend, and not rely on the higher level features of C++.

## ambitious
- out of order compilation.
