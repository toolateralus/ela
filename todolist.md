## TODO List

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### see 'feature/*' to see some planned/proposed features that may or may not get implemented.

# features
  - `for i, v in some_array` {} getting an index from an iterator.
  
  - tagged unions (half started)
  - modules.
  - fully fleshed out constexpr interpreter. structs, unions, everything but syscalls and pointers basically.

## out-of-language features
- config file/ project. So we can organize submodules, add compilation commands & library paths, source ela libraries, etc.

# reworks.
- type inference for generics is near non existent

## general
- clean up everything. the parser is a mess, a ton of ast can be simplified, and made more performant even.
- the parser does more type system interactions than it needs to.

## ambitious
- out of order compilation.
