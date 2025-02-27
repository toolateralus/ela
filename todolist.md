## TODO List

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`

### see 'feature/*' to see some planned/proposed features that may or may not get implemented.


#### Tagged Unions & Pattern Matching.

#### Drop interface and "destructors"

#### Fix any type and add ability to do implicit casting routine stuff.

#### #type needs to be it's own node, not ride on the back of ASTType

#### instead of just # stuff BEGINNING statements, we should be using them as attributes

such as
```rust
// compile time compatible struct.
Struct :: struct @const {
  x: f32,
  y: f32
}

snprintf :: fn() @foreign;
c_printf :: fn() @foreign(printf);

// we won't be constrained to 'main' as an entry point.
my_fruity_ahh_entry_point :: fn() @entry {
  ur_cappin: bool = false;
  printlnf("no cap bruh? = %", .[any::from(&ur_cappin)]);
}
```

This would allow us to stack up attributes too, and not be constrained to beginning a statement with #...

like 

```rust
  fmod :: fn() @foreign(fmod) @const;

  // we can also do auto impl's for common interfaces like Debug or Clone or whatever.
  Result :: union![_Ok, _Err] @impl[Clone, Debug] {

  }

  // we could also use this on even normal declarations / types.
  f: @dyn Interface;

```


#### Interfaces should have default or 'overrideable' functions, where the interface itself can define a method
  and the consumer of the interface can choose to provide a new one, or use the default.


#### Modules.
  A simple module system would take us a far way, avoid namespacing conflicts, provide a simple name mangling scheme etc. They would be ultimately optional.

```rust
  // or, import a module, an expose it via the :: operator, qualified.
  module main;
  import std::format;
  main :: fn() @entry {
    format::println("hello world");
  }


  // or, import targeted symbols.
  module main;
  import { println } in std::format;
  main :: fn() @entry {
    println("hello world");
  }

  // or, unload all symbols from the module into here.
  module main;
  import * in std::format;
  main :: fn() @entry {
    println("hello world");
  }


  // this wouldn't work, because we didn't actually implement std::format, we just added the std module which contains very little itself.
  module main;
  import std;
  main :: fn() @entry {
    std::format::println("hello world");
  }

```


#### change ![] to !<>. much kinder to read.
```rust
default :: fn!<T>() -> T {
  return .{}
}
```


#### allow tuple/struct destructure in for loop
```rust
  for k, v in hash_map {

  }
  for i, v in list.iter().indexed() {
    
  }
  for x, y in list_of_vec2s {

  }
```

#### add variadic generics and value generics.
```rust
  format :: fn!<T...>(fmt: str, pack: ...T) -> String {
    builder: std::String_Builder;
    builder.set_allocator(std::mem::temp_allocator.{});
    defer builder.deinit();
    #for (t, v) in pack {
      builder.rtti_append(#type(t), v);
    }
    return builder.string();
  }
```

```rust
Fixed_Array :: struct!<T, N: u32> {
  data: T[N],
  length: N
}
```

```rust
index :: fn!<T, SIZE: u32, N: u32>(array: T[SIZE]) -> T {
  return array[N];
}
```


#### improve aliases. make it so we can alias functions and bascially any symbol (besides variables and arguments)

```rust
module main;
import { println } in std::format;

alias!<_Ok, _Err> Ok  :: Result!<_Ok, _Err>::Ok;
alias!<_Ok, _Err> Err :: Result!<_Ok, _Err>::Err;

main :: fn() @entry {
  x : Result!<s32, None> = Ok(100);
}
```

#### Tagged unions. Half implemented, just need to tidy up and really flesh it out.

```rust
  Tagged :: enum(union) {
    #shared { 
      // this data would be available to all of the variants.
      // kind of like a macro style mixin.
      // struct declaration semantics.
      source_location: Source_Location,
      resolved_type: u64,
    }
    A(s32),
    B(s32, s32),
    C{x: s32, y: s32}
  }

```

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
