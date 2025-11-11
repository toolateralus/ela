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

### Example: Advanced Attribute System

Attributes would always be executed at compile time, invoked at the end of typing whatever it's applied to.
This would be most possible once the THIR mutation API is all stable in the compiler, right now, we still 
have to finish making the THIR branch stable at runtime, then convert the compile time interpreter to use that
instead of the AST, as it does now.

#### Defining custom attributes

```rust
attribute(function: compiler::Declaration::Function) Obsolete(reason: str, alternative: str) {
  warning: compiler::diagnostics::Warning = .{
    message: reason,
    solution: Some(alternative),
  };
  compiler::diagnostics::raise(warning);
};

@[Obsolete("Old function is obsolete!", "use new_function instead!")]
fn old_function() {}
```

### Registering JSON serializer 
```
attribute(T: type) Json() {
  json::generate_and_register_serializer!<T>();
}
```

#### Attribute for variable clamping

```rust
attribute(variable: *mut compiler::Declaration::Variable) Clamp!<T>(min: T, max: T) {
  *variable.initializer = compiler::make_literal(math::clamp(*variable.initializer, min, max));
}
```

#### Attribute for auto-deriving traits

```rust
attribute(Target: type) Derive!<generics: [Trait]>() #expand {
  for T in generics {
    #insert {
      impl T for Target {}
    }
  }
}

@[Derive!<.[Clone, Copy, Blittable]()]
struct Vec2 {}
```

#### Attribute usage in main

```rust
fn main() {
  @[Clamp(0, 100)]
  value: s32 = 100;
}
```

#### Builtins for Result and Option
 
Up to this point the expectation was that we could make ADT's so strong that Result and Option could be seamlessly
implemented in the language itself, and there wouldn't be much friction, but after using V lang, I realize that having an integrated
way to propogate errors, and a simple syntax like 'or' for handling errors and none's is much more elegant and useable than
having pattern matching and unwrapping on said types. 

Having something like `!Type` as return types makes it so the user does not have to reason about how to propogate error types,
and we can pass error or non-error payloads back with ease and handling them is much more trivial.

In fact, the `or` expression could be used to handle any boolean-convertible expression not evaluating to true.

As for the result type, there's a few ways we could go about this.

One way would be to use a 'dyn Error' trait that has some methods like `fn err(*self) -> String` and `fn code(*self) -> s32`,
which is approximately how V does this. However, I think that's quite restrictive, and we could try to find some middle ground between how Zig and V does this.
I'm not sure what that would look like though.


Some ideas for one approach:

```rust
// in some File api
impl File {
  // this example would effectively compile to
  // the method shown below it.
  fn open(path: Path) -> !File {
    stat := fs::stat(path)! // propagate if error

    if !stat.exists {
      // stdlib `error` function
      return error("Error!")
    } else if stat.is_bad {
      return MyError.{..}
    }
    return File.{} // actual file opening mechanism here, return file.
  }

  fn open(path: Path) -> choice File_open_Result { error(any), File(File) } {
    stat_result := fs::stat(path);

    if stat_result.is_err() {
      return File_open_Result::error(stat.unwrap_err())
    }

    stat := stat_result.unwrap()

    // the 'any()' construction syntax is just for demonstration.
    if !stat.exists {
      return File_open_Result::error(any(new("Error!")))
    } else if stat.is_bad {
      return File_open_Result::error(any(new(MyError.{..})))
    }

    return File_open_Result::File(File.{}) // actual file opening mechanism here, return file.
  }
}

// And this would effectively compile to what's shown below as well
fn main() -> ! {
  file := File::open("my_path") or { 
    File::some_file_value()
  }
  io::println("file is valid? %", (file.is_valid(),));
}


fn main() {
  file_result := File::open("my_path");
  mut file: File;
  if file_result.is_err() {
    err: any = file.unwrap_err();
    defer {
      delete(err.ptr)
    }
    file = File::some_file_value()
  }
  io::println("file is valid? %", (file.is_valid(),));
}


```

### Fix any type, fix typeof, stop using pointer comparisons and non-determinsitic ID's for comparing types at runtime

We need some kind of deterministic hash code where two translation units that include the same code can refer to a type by the typeof()
and get a result that's valid to compare, right now we use pointer comparisons for `typeof(T) == typeof(T)` and also the `any` type,
so if a .dll or .so returned an `any`, the comparison would at worst be two pointers that obviously don't point to the same memory, and at best,
be two integer indices into the compilation unit index of the type, which if the two programs don't contain the exact same code, would never be equivalent.

We should hash the contents of each type (and also the parent scope hierarchy by name) so that two translation units that include the same types
can safely compare `typeof(T) == typeof(T)` and get a valid result, using that hashcode.

Additionally the `any` type should be more builtin to the compiler, allowing implicit conversions to the any type for scoped anys, and a simple way to hoist
a value into an any on the heap so it can be returned if we choose to use that route for the error handling mechanism. Determining when something would or wouldn't get
hoisted up onto the heap would be weird so maybe we either only do that for errors, or we just always do it. I would rather have clear explicit semantics so doing it only
when required (for errors) and allowing the user to allocate if they wanna propagate some any type up the call stack after their value may have gone out of scope seems 
to make more sense.

Also, we can add some pattern matching syntax for ease of use when handling any types, pretty much identical to the choice type pattern matching, except
instead of comparing a discriminant and accessing a field, we would compare the hashcode of the two any types, and cast the void pointer  into a useable value
within the block of whatever pattern match statement.

```rust
  a : any = 10; 
  if a is s32(int) {

  } else if a is std::ErrorCode(ec) {

  }
```


### Some attribute that can prevent zero initialization of certain types

right now anything can be zero initialized, which is great until you look at the internals of our collection types
which have to handle default lazy initialization everywhere, and in places that doesn't make sense as well.

specifically, in the allocators, we have to have some global variable representing an allocator useable at any time by anyone
and it takes away from the entire allocator system because specifying an allocator becomes much harder with nested collection types
and things passed in certain ways.

for example if you wrote this:
```rust
  mut list: List!<s32>;
  for i in ..100 {
    list.push(10)
  }
```

What's actually happening on that first push is we're checking if the `allocator` field in the list type has a pointer instance that's valid,
and if not, we're assigning it to `get_global_allocator()`. it makes the code for the container types less maintainable and more importantly
less predictable and transparent, there's just weird magical behaviour happening in the background.
Also, for nested collections, we need some kind of trait like `HasAllocator` which has a getter and setter so that when we do construct
items to store in that list, and the list says in some flag to pass allocators recursively, we can propogate our allocator to our child items


for example if you wrote this,
you would then have to iterate over the list any time it changes length
and make certain that the children all have a field that point to `my_allocator`
```rust
  my_allocator := dynof(alloc, Allocator)
  my_list: List!<List!<s32>> = List::with_allocator(my_allocator)
```

So we could have some trait like this:
```rust
trait HasAllocator {
  fn get_allocator(*self) -> dyn Allocator;
  fn set_allocator(*mut self, allocator: dyn Allocator);
}
```

or even like this, with some compile time stuff:
```rust
trait HasAllocator {
  fn get_allocator(*self) -> dyn Allocator {
    return self.allocator; // Convention would mandate that this field is just called `allocator`.
  }
  fn set_allocator(*mut self, allocator: dyn Allocator, flags: SetAllocatorFlags = ::propagate) {
    if propagate {
      // compile time for loop that just gets unrolled.
      $for field in Self::fields {
        where field.ty: HasAllocator {
          field.value.set_allocator(allocator, flags);
        }
      }
    } else {
      self.allocator = allocator;
    }
  }
}
```

```rust
  my_allocator := dynof(alloc, Allocator)
  mut my_list: List!<List!<s32>> = List::with_length_with_allocator(150, my_allocator, ::PropagateAllocatorToChildren)
  // now my_list has 150 other lists within it that all point to my_allocator.
```
