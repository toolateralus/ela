
# Flags you can pass to the compiler

> note this is very easy to extend, there is very little hardcoded data associated with flags, its a hashtable of strings.

## --debug
Compile the output source with debug symbols.

> note: to use `gdb` or another debugger, in your code use
`#compiler_flags "-g"` as well as the --debug flag in the command line.
This is to be reworked

## --s
Don't delete the `.hpp` and `.cpp` files used to transpile.

## --metrics
Write performance metrics (time taken for each compilation step) to stdout.

## --test
Only emit functions marked `#test` and create a test runner. The output binary will run all the tests. much like cargo test.
