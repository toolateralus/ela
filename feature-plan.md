## A long-term, flexible roadmap for the future of the compiler.

Mainly focusing on big big ideas here. as an over-arching idea; The entire compiler can benefit endlessly from being rewritten, all over the place. redundancies, inefficiencies, just straight up bad/hacky systems. seperation of concerns. It's pretty sloppy!

To search for all info comments in the source just use vscodes regex search with
`TODO|todo|Todo|SIMPLIFY|CLEANUP|PERFORMANCE|FIX|BUG|FEATURE`


#### Iterative compilation
  - Having the typer run in an out-of-order fashion will be endlessly beneficial to the langauge. There are already pain points,
  where a `choice` depends on a `trait` and the `trait` depends on a `choice`, and there is literally _no way_ to resolve it. **at all**

  Also, this would allow us to do a kind of 'exclusionary hot code path dead code elimination', where we start at the `main()` function (only for non-library programs) and branch out, using the symbol table to only type what's needed/used by the program, with some exceptions (extern, exports, volatile, etc)
  
  The key would be to make it performant, and simple. I'd prefer we not use a query/pipeline/threading system, instead just a 'retry typing until it's done or a threshold of errors shows up'

  One of the largest challenges in a system like this, is error reporting. I would certainly not want to have to write _another_ visitor just
  to figure out where error(s) occured; instead, we can think of something unique.

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

-- 10/22/2025, 11:22:46 AM
-- We will not be adding variadic generics in the forseeable future.
   Instead, we would likely add generic slices, using the type keyword,
   slice syntax, and a compile time for loop over that slice.
  
```rust
  fn variadic!<types: [type]>() {
    for T in types {
      println(T.name);
    }
  } 

  variadic!<s32, s32>();
  // would compile to::
  fn variadic!<s32, s32> {
    println("s32");
    println("s32");
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

type!<_Ok, _Err> Ok  :: Result!<_Ok, _Err>::Ok;
type!<_Ok, _Err> Err :: Result!<_Ok, _Err>::Err;

@[entry]
fn main()  {
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

Instead, impl's would exist at the scopes level, flattened, and calling methods would trigger a resolution phase, where
impl's type arguments would effectively act as a pattern we need to match with. I think this is how rust does it,
and it makes complete sense for things like `From!<T>`, which are also unachievable right now in our current, primitive,
first-attempt level system.

This ties into the previous feature plan, with where predicates being lazily evaluated and ignored for types that don't match
their constraints, and errors only triggering on a use of a function that doesn't fit into that predicate+impl pattern.

#### Vtables instead of dyn objects being an aggregate of function pointers
`const static vtable_dynof_something` instead of using a struct full of pointer methods.
Harder to call, but much much cheaper to construct, and the static shared memory is much hotter in terms of cache hits.

#### remove the 'switch is' node. replace it with a 'match' node
We right now have `switch` and `switch is` nodes. we should replace this with just a `match` since it's not really a switch,
it does so much more. In my mind, `switch` is just switching over enumeration values, i.e C `enum`. in our case, we will be able to
do very complex pattern matching, as shown below, and this would be much nicer to not have a `switch is`, rather copy rust with just a `match`

#### Pattern matching improvements
recursive, value based, and very complex pattern stuff like this should work, to any degree. we need to get on Rust's level of pattern matching.
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

Attributes would always be executed at compile time, invoked at the end of generating MIR for whatever it's applied to.

This change depends most on the CTE working well, which right now we're in the middle of migration to an MIR -> LLVM IR pass,
from the old THIR -> Transpiled C target.

Of course, this will take a long time (the MIR & LLVM & a new VM), but once we have this
metaprogramming API even started, this should be more possible.

One gigantic glaring problem here, is that we'll have to refactor the entire the way the compiler works to even make this
kind of metaprogramming possible. Instead of making direct passes over trees of nodes in a linear fashion,
we will need to be able to re-submit AST, Type it, generate THIR, lower it to MIR, interpret it, create new MIR or THIR from it's results, (if we create THIR, put that back into MIR), and then output LLVM IR that corresponds to it.
All of those steps, excluding the last, could also re-trigger, an indeterminate amount of times.

we'd get something like this:

Text -> 
  AST -> 
    Typing -> 
      THIR -> 
        MIR -> 
          Execute Metaprogram -> (.. ? unknown, could retrigger any or none of those phases) -> 
            LLVM IR -> 
              Object File(s) -> 
                Executable.

to be clear, when I say any of those phases, i mean from AST to Metaprogram, since of course the LLVM IR
could only ever be reached once all compilation from previous pipelines has fully completed.
We could theoretically start lowering LLVM IR as soon as dependencies are ready, but we would have to be careful
because any metaprogram could touch that, and doing work in LLVM is going to always be the slowest phase of the compiler.

So we need to have a real pipelined queue system, where things can independently move through the phases of the compiler when possible.

This would only really exist for things that are truly position independent, such as type declarations, module declarations,
and function declarations.

This new pipeline setup would also enable us to do out of order typing, because we would have to design a similar system to an
order-independent typer, except it would be able to progress through not only typing, but all the way from the AST -> MIR,
in a way where things can wait on each other, and request dependencies, etc.

I hope that this is achievable within a year from Oct 2025, but it may take even longer.

We've gotten this far in a year, from nothing to where we are today, so I think it's totally doable.

#### Defining custom attributes

```rust
attribute(function: compiler::Declaration::Function) Obsolete(reason: str, alternative: *compiler::Declaration::Any) {
  warning: compiler::diagnostics::Warning = .{
    message: reason,
    solution: Some(alternative),
  };
  compiler::diagnostics::raise(warning);
};

@[Obsolete("Old function is obsolete!", new_function)]
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
  *variable.initializer = compiler::make_literal(math::clamp(variable.initializer.evaluate!<T>(), min, max));
}
```

#### Attribute for auto-deriving traits

```rust
attribute(Target: type) Derive!<generics: [type: IsTrait & TraitHasNoRequiredMethods]>() #expand {
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
  @[Clamp(0, 100)] // generic inferred.
  value: s32 = 100;
}
```




### Just a random idea here:

say we have a function func, that simply adds two numbers, and is constraint by IsNumeric

(
  or for the sake of specificity, doesn't depend on the size of type T, but simply operates on it,
  in a way where integer promotions, mantissas, etc, could be coerced safely.
)
```rust
fn func!<T: IsNumeric>(a: T, b: T) -> T {
  return a + b;
}
```

then, the user calls this with every possible type:

```rust
func(100 as u8);
func(100 as u16);
func(100 as u32);
func(100 as u64);
func(100 as s8);
func(100 as s16);
func(100 as s32);
func(100 as s64);
func(100 as f16);
func(100 as f32);
func(100 as f64);
.. etc.
```

In one single instance, this is totally negligble amounts of binary size.
But since many functions do in fact take generic numbers to be more usable and safe,
this could possibly be a massive point of binary size growth.

We could just promote T to being the largest possible numeric size of correct signedness, and a float etc,
and then just promote arguments to that size, then truncate back down on return.

of course a function like this:

```rust
fn func1!<T>(a: T, b: T) -> T {
  if (sizeof(T) > 4) {
    return a + b;
  }
  return b * a;
}
```

could never qualify from this kind of instantiation consolidation, because each instantiation has to know
exactly what constant to fill in for 'sizeof'.


This kind of optimization could also be applied to functions that take a type argument as a pointer,
but never access a specific field of that pointer. say it only passes this type to other functions (which may also recieve this
optimization), and never measures the sizeof(T), nor says v.x, etc.

```rust

fn register_some_handler_in_system!<HandlerT>(handler: *HandlerT, slot: u32, kind: Handler_Kind) -> Option!<*mut Registry> {
  registry := if registry_locator() 
    .locate(system.registries, kind)
    .expect(fn () {
      panic("unable to locate registry for kind!");
    });

  registry.insert_or_update_at_slot(slot, handler);
  return Some(registry);
}
```

This function could simply get type erased down to a void pointer, and used for every handler type, since it never needed to be
generic in the first place: if insert_or_update_at_slot takes some kind base registry, or is a list that just stores pointers,
it could be casted out of implicilty by the compiler.


This type erasure idea could ALSO extend to predictable types like `List`, where T is a pointer,
we could completely consolidate every instantiation of the struct definition itself, and keep the impl's generic and instantiated,
to save on even more code space.


This would not only reduce binary size, but DRASTICALLY reduce compile times, and we could have -03 enable complete monomorphization, for max runtime performance (promoting tiny integers to large ones takes time and space)




# Type inference for associated function calls.

We should easily be able to infer the leftmost element of a path based on an expected type.

for example:
```rust

fn a!<T>(list: List!<T>) {
  for v in list {
    println(v); /// some stuff.
  }
}


// this would be the most extreme example,
// where we not only infer that List!<T> is the leftmost element of the path,
// but also we infer that the T type argument is satisfied by the T type argument
// of the InitList!<T> that's in the initializer list.
fn main() {
  a(::init(.[0, 1, 2, 3, 4, 5, 6]));
  // fully explicit:
  a!<s32>(List!<u32>::init(u32.[0, 1, 2, 3, 4, 5, 6]));
}
```

Or, a more tame and expectable example:

```rust
struct List!<T> { ... } // implements 'Init!<T>' trait.

fn main() {
  // this would be pretty simple to implement.
  list: List!<s32> = ::init(.[0,1,2,3,4]);

  enum Abcd {
    A,B,C,D
  }

  a: Abcd = ::A;
}

```

Also, we could use a similar system to access symbols that are shadowed by the current scope, just like C++.

```rust
fn get() -> s32 {
  return 0;
}

fn main() {
  fn get() -> f32 {
    return 0.0;
  }

  x: s32 = :::get(); // begin searching for the symbol in our parent scope.
}
```


# We desperately need better switches, and matches.

This is kind of covered earlier in the file, but it's much worse.
Right now, our switch cases compile down to if-else chains.
There is no way to fall through, there is no way to match multiple conditions
in one block.

`switch is` is horrible syntax, and needs to be replaced with `match`


Note: 
  Of course, our `match` node wouldn't be the only statement who benefits from
  the extremely complex patterns as seen below:
  `if $expr is ...`
  `while $expr is ...`
  would also get these.
```rust

fn main() {
  mut value: s32 = get_value();

  // Switch against some integers, these _need_ to be compile time constants.
  switch value {
    20 => {
      io::println("It was twenty.");
    };

    // match any of these values.
    30, 40, 50, 60 => {
      io::printlnf("was a multiple of 10 between 30 and 60. {}", (value,));
      #fallthrough; // fall through to the next case, and don't check against it's pattern.
    };

    60, 70, 80, 90 => {
      io::printlnf("was a multiple of 10 between 30 and 90. {}", (value,));
    };
  }

  // Then, our pattern matching node, would do much more complex, non-numeric 
  // style switching, without fall throughs.
  // matching tuples (these do not need to be compile time constants.)
  match (0, 1) {
    (0, 1) => {
      io::println("it has matched");
    };
    (1, 0) => {
      io::println("this shouldn't happen -.-");
    }
  }

  // value defined before
  // match against ranges (these do not need to be compile time constants)
  match value {
    // match against all positive numbers (s32)
    0.. => { // We should always use =>
      io::println("Yep");
    }; // end every block with a semicolon.
    
    // match against all negative numbers (s32)
    ..0 => {
      io::println("Negative");
    };

    // else will still be our default case.
    // I think it's clear.
    else => {
      io::println("No pattern hit");
    };
  }

  // match some choice types.
  match Option!<s32>::Some(100) {
    Some(10) => { // match explicitly against a Some(10), will not work.
      io::println("Nope");
    };

    // again, match against a Some( some value between 101 and 200 ). this will not get hit in this case.
    Some(101..200) => { 
      io::println("Nope");
    };

    // simple matching against discriminant.
    None => {
      io::println("Nopers");
    };

    // extract value from a choice type, or match exactly against 100.
    // of course, in this demonstration, im just showing this,
    // both of these conditions are true, but using Some(v)
    // where we've matching against Some(value:constraint)
    // would be completely redundant in most cases.
    Some(v), Some(100) => {
      io::printlnf("Yeppers, value: {}", v);
    };

    // else will still be our default case.
    // I think it's clear.
    else => {
      io::println("No pattern hit");
    };
  }

  tuple := (Result!<s32, ()>::Ok(100), Option!<s32>::None);

  match tuple {
    // We can then extend all of these behaviours into nightmarish conditions
    // that can be incredibly specific.
    (Err(e), Some(0..10)) => {
      io::println("wababbebababe");
    };

    // _ would ignore that part of the pattern, i.e we don't care about that part.
    (_, None) => {
      io::println("Yep");
    };

    else => {
      io::println("No pattern hit");
    };
  }


  if tuple is (_, None) {
    io::println("Woop");
  } else if tuple is (Err, None) {
    io::println("Doop");
  }

  taple := (Some(10), 
    Result!<s32, ()>::Ok(Some(100)), 
    Value::Float(Ok((Some(100.0), None)))
  );
  // Just making the most nightmarish condition possible,
  // but this is great because it's compact and expressive, and eliminates so much code
  // that would have been wasted on checking conditions and pulling variables out.
  while taple is (Some(v), Ok(Some(a)), Float(Ok((100.0, Some(s))))) {
    io::printlnf("v={}, a={}, g={} s = {}", v, a, g, s);
  }
}

```

# `match where` and `where break`

Sometimes a giant where else chain is unweildy and obnoxious.
We can just match on types, and this would of couse do the exact same thing as
`where { .. }` statements, conditional compilation based on types or traits.

```rust
match where T: {
  f32: {

  };

  u32: {

  };

  Destroy: {

  };

  u128: {
    
  };

  IsBlittable: {

  };
}
```

Another thing that we could probably add to `where` statements,
is some kind of early return mechanism, again, to avoid heavily nested where statements.

The below example isn't great, because it wouldn't be horrible to just use else where.
I just can't think of a good example right now, but I have certainly thought

`I wish i could just break out of this function when this where condition passes, instead of having a huge branch chain`

```rust

fn default!<T>() {
  where !T: IsInteger | IsPointer {
    compiler::intrinsics::Error("You can't do that!", #here);
  }

  where T: IsInteger {
    return 0;
    where break;
  }

  where T: IsPointer {
    return null;
    where break;
  }

  return .{};
}

```