## A long-term, flexible roadmap for the future of the compiler.

Mainly focusing on big big ideas here. as an over-arching idea; The entire compiler can benefit endlessly from being rewritten, all over the place. redundancies, inefficiencies, just straight up bad/hacky systems. seperation of concerns. It's pretty sloppy!

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`


#### THIR
  - We should definitely have a fully resolved, desugared THIR (Typed High Level Intermediate Representation) instead of just emitting AST.
  It will make the emitter simpler, and it will make it more reliable. It will also allow us to do an MIR in the future, for even more control. It will also decouple our emitters from our AST, while still being source code dependent, it would be less of a system-shock 
  to change how the AST is represented.

  The plan at first is to just take our existing typed AST, out of the emitter, and run it through a THIR generator, and then emit that.
  Eventually, we can convert the typer to emitting the THIR, and reduce some bloat in our AST.

#### MIR
  - an MIR after the THIR stage would help us get down to a bit-code kind of level. a stack machine, SSA-esque IR that's fully desugared,
  and allows us to optimize, execute functions and code at compile time, and simplify the lowering process _even more_. It also will decouple
  the emitter from the THIR, which is still pretty source language dependent.

#### Iterative compilation
  - Having the typer run in an out-of-order fashion will be endlessly beneficial to the langauge. There are already pain points,
  where a `choice` depends on a `trait` and the `trait` depends on a `choice`, and there is literally _no way_ to resolve it. **at all**

  Also, this would allow us to do a kind of 'exclusionary hot code path dead code elimination', where we start at the `main()` function (only for non-library programs) and branch out, using the symbol table to only type what's needed/used by the program, with some exceptions (extern, exports, volatile, etc)
  
  The key would be to make it performant, and simple. I'd prefer we not use a query/pipeline/threading system, instead just a 'retry typing until it's done or a threshold of errors shows up'

  One of the largest challenges in a system like this, is error reporting. I would certainly not want to have to write _another_ visitor just
  to figure out where error(s) occured; instead, we can think of something unique.

#### CTFE
  - CTFE (Compile Time Function Execution) is an important part of a modern systems language. It allows all sorts of optimizations,
  utilities, and more. We'd want ours to be capable of performing I/O and syscalls.

  The idea is that once we have a MIR implemented, we can design it in such a way that is is executable in-place. Then, lowering to C or
  LLVM, would be identical, except the CTFE VM would have evaluated the compile time code, and resolved itself into whatever the result may be. Sort of a constant-folding, but with much more behaviour, loops, pointers, callbacks, etc.

  We can also implement a Jai-like compiler API, where you can use the main file as a build script, in language, and get callbacks on nodes that are done typing, modify code at compile time, and many other things.

---
### Below is a summary of the codebase's comments, made by AI. it may be crap, but it's better than doing it myself :P

#### Attribute system and statement attributes
  - The current `#`-prefixed directives are pretty limiting and kind of ugly. We should move to a more flexible attribute system, like `@const`, `@entry`, `@foreign`, `@impl[Clone, Debug]`, etc. Attributes should be stackable and usable on any declaration, not just at the start of statements.

  >> This is already implemented, but it's not as used as it needs to be. we should be constantly working to completely remove any `#` directives.

#### Improved alias system
  - Aliasing should work for functions and symbols, not just types. Would be nice to have something like Rust's `use` or C++'s `using`, so you can alias almost anything, and in various capacities.

#### Better error reporting and diagnostics
  - Error reporting is always a pain point, especially with iterative or out-of-order compilation. We need to make it easier for users to understand and fix type errors and dependency cycles, without writing a million visitors.

#### Project/config file support
  - At some point, we'll want a project configuration system for managing submodules, library paths, and compilation commands. This would make bigger projects and dependency management way easier.

#### Parser and AST cleanup
  - There's a lot of unnecessary complexity and inefficiency in the parser and AST. Needs a cleanup pass for performance and sanity.

#### Better macro/directive system
  - Macros (NYI) and directives should be more powerful and integrated, maybe even first-class language features instead of just parser hacks.

#### Improved type extensions and querying
  - The way type extensions (pointers, arrays, etc.) are handled is kind of clunky. Needs a refactor to make querying and manipulation more ergonomic.

#### Testing infrastructure
  - The test runner could use some love. Easier grouping/filtering, maybe even integrate testing more deeply into the language.

#### FFI improvements
  - FFI should be more robust, with better handling of calling conventions, attributes, and cross-language type mapping.

#### Better symbol/scope management
  - Scope/context management should be more lexical and less manual. Would make symbol resolution and incremental compilation easier and less error-prone.

#### Documentation and examples
  - Docs and examples are always lagging behind. Need to keep them up to date and practical, so users can actually learn and contribute.


#### add variadic generics and value generics.
```rust
  format :: fn!<T...>(fmt: str, pack: ...T) -> String {
    builder: std::String_Builder;
    builder.set_allocator(std::mem::temp_allocator.{});
    defer builder.destroy();
    #for (t, v) in pack {
      builder.rtti_append(typeof(t), v);
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



#### Method trait bounds (Reimplementing 'impl')

Right now, if I did:

```rust
impl!<T> List!<T> {
  fn idk!<T>() where T: u32 { ... }
}
```

This would ***never*** compile, because any `T` that isn't `u32` would trigger an error, and our standard library alone will break this assumption 10 times over.

Instead, we need to look at _`T`_ in that **trait bounds**, and if the condition fails, we _exclude_ that function from being added to 
the types symbol table. However, if and when that function gets called by the user, we need to _then_ throw the error for the trait bounds
not being satisified.

This raises a need to rewrite how methods are even added to types, we need them to be available for lookup by the compiler, yet we need to be able to defer errors to usage in certain cases, or consider them off limits.

> See the next section for another reason we might want to do something like this, storing methods outside of the symbol table entirely in the c++ `Type *`.

This is a very important part of having generics that are truly extremely flexible, right now everything is very rigid, and hard to use 
when doing very very generalized generics, such as a `List!<T>`

Another plain example, is somethning like `fn contains()`, for any container:

```rust
struct List!<T> { ... }

impl!<T> List!<T> {
  fn contains(self, value: T) where T: Eq {
    for item in self {
      if item == value {
        return true;
      }
    }
    return false;
  }
}
```

The above example is very simple and performant, yet we are completely unable to do this, because we have to assume that **every single type that is used in a list** would _have an equality operator implemented_ which is completely unreasonable.


#### Extremely arbitrary and imprecise impl's.

We are currently unable to do something like this:

```rust
  // marker trait.
  impl!<T> IsPointer for *mut T {}

  // trait on everything.
  impl!<T> T {
    fn my_method_on_every_type_in_the_world(self) {

    }
  }
```

As mentioned in the above section, this would be made easier if methods weren't just jammed into the type's symbol table, and if it were more so a pool of available methods.

This still wouldn't be trivial; I am not entirely sure how this would even work. We could run cleanup every time the compiler hit a block like this, retroactively do all the `impl`s, and then moving forward every type would get them?

There's certainly a better way, we can look into how rust even accomplishes such insanely fluid and generic behaviour.

#### Vtables instead of dyn objects being an aggregate of function pointers
`const static vtable_dynof_something` instead of using a struct full of pointer methods.
Harder to call, but much much cheaper to construct, and the static shared memory is much hotter in terms of cache hits.

#### remove the 'switch' node. replace it with a 'match' node
We right now have `switch` and `switch is` nodes. we should replace this with just a `match` since it's not really a switch,
it does so much more. In my mind, `switch` is just switching over enumeration values, i.e C `enum`. in our case, we will be able to
do very complex pattern matching, as shown below, and this would be much nicer to not have a `switch`, rather copy rust with just a `match`

#### Pattern matching improvements
recursive pattern stuff like this should work, to any degree:
```rust
  if x is Some(Ok(&mut v)) {

  }

  switch is (1, 2) {
    (1, 2): {

    }
    (2, 3): {

    }
  }

  switch is 50 {
    0..100: {

    }
  }

  switch is (Some(10), None) {
    (Some(v), None): {

    }
  }

```

#### Instead of variadics, we could..
  (...tuple) allow for tuple unpacking.

  ```rust

  fn printf!<T>(fmt: str, tuple: T) where T: IsTuple {
    std::c::printf(fmt, ...tuple);
  }

  ```
