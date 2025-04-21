NOTE: The compiler does not compile on windows currently! it is primarily developed on Arch and Linux Mint.
Most linux distributions, it should work. You just need CMake and Ninja to compile it, as far as I know.

The Ela language is very immature (~4-6 months of daily development), but has a few key features:


- C-like memory simplicity.
- Strong typing, with structs, enums, unions, tagged unions (choice), and trait types.
- Powerful runtime reflection.
- 'dyn' Dynamic dispatch objects for trait types.
- Generic structs, choices, traits, impl's, and functions/methods.
- Methods implemented for any type.
- A small but powerful core library. (hash map, dynamic list, full raylib library, an immature 'rayui' library, full llvm library).
- Many other features!

  Right now, the compiler is in it's third reiteration, where we've transformed a lot of features, and added a lot of features.
  We are still hashing out a really solid foundation, so users are likely to encounter bugs.
  Please, still give the compiler a try! it's got a very ergonomic and easy to type syntax, and a lot of functionality reminsicent of C, Rust, Odin/Jai, and other influences.

  Note: The documentation that exists, is likely all wrong, and there is little of it.
  To get an idea of how most features work, you can check out the compiler's testing directory,
  Where many features have a test document which display how they can be used, but usually not in depth.

  Also, for more practical examples, check out the 'lib' directory, which is essentially the core/std library.
  There are some examples in the 'examples' directory, but many of them are super old and super buggy. unfortunately.
  
  Feel free to leave a github issue, or send an email to ela-language@gmail.com with any questions or remarks about the language.
  also, contributors of any capacity are welcome, and that includes additions to the standard library, such as a http library, tcp, ui frameworks, etc.
  
  Any level of contribution or suggestion is appreciated!

  
