NOTE: The compiler does not compile on windows currently! it is primarily developed on Arch and Linux Mint.
Most linux distributions, it should work. You just need CMake, Ninja and libffi to compile it, as far as I know.

The Ela language is very immature (~1 year of daily development, with a few months of hiatus), but has a few key features:

- C-like memory simplicity.
- Strong typing, with structs, enums, unions, tagged unions (choice), and trait types.
- Powerful runtime reflection.
- 'dyn' Dynamic dispatch objects for trait types.
- Generic structs, choices, traits, impl's, and functions/methods.
- Methods implemented for any type.
- An okayish standard library. It's very immature and has a lot of inconsistencies, but once the compiler is more mature, it will get several full reworks and hashed down to 
  idiomatic, safe, and practical solutions to common problems.
- Pattern matching for choice types.
- Very convenient tuple syntax.
- Destructuring for arbitrary aggregates. tuples, structs, etc.

  Right now, the compiler is in it's third reiteration, where we've transformed a lot of features, and added a lot of features.
  We are still hashing out a really solid foundation, so users are likely to encounter many bugs for non-trivial programs.

  It's not unusable, but I think I make it clear in this readme, it is NOT reliable, but I hope that doesn't discourage you to try it out.
  I just don't want to be deceptive :)

  Please, still give the compiler a try! it's got a very ergonomic and easy to type syntax, and a lot of functionality reminsicent of C, Rust, Odin/Jai, and other influences.

  One of my primary goals right now is updating the documentation and examples, as the language has gone through incredible amounts of change over the last year of development.
  There is a ela-tutorials repository under my profile, I think, or it's in our organization. But it's not done at all, and will get merged into this repo when it's to a point
  where it's useful.

  If you would like to dig through the standard library, learn the languages idioms, and create some interactive documentation, that would be greatly appreciated.

  Feel free to leave a github issue or a github discussion with any questions or remarks about the language.
  also, contributors of any capacity are welcome, and that includes additions to the standard library, such as a http library, tcp, ui frameworks, etc, or even wrangling the 
  gnarly and messy compiler source, and fixing bugs. The compiler is slotted to get many rewrites over the next couple of years, and the state it is in is unacceptable.

  We are currently developing a full rewrite of the compiler, which would feature an expanded and polished type system with many voids the current compiler has filled,
  and there is also a branch that implements a LLVM backend rather than a transpiler, but it's nowhere near ready to be merged into main.

  If you're curious, you can check out the `feature_plan.md` document, but note that not everything in there is subject to find its way into the compiler. It's more of a scratchpad for ideas,
  but there are certainly things in there that 100% will end up in the language, sooner or later.
  
  Any level of contribution or suggestion is appreciated!

  
