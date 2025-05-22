## A long-term, flexible roadmap for the future of the compiler.

Mainly focusing on big big ideas here. as an over-arching idea; The entire compiler can benefit endlessly from being rewritten, all over the place. redundancies, inefficiencies, just straight up bad/hacky systems. seperation of concerns. It's pretty sloppy!

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

#### Variadic generics and value generics
  - Supporting variadic generics (like `fn!<T...>`) and value generics (like `struct!<T, N: u32>`) would make abstractions way more expressive and reusable.

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