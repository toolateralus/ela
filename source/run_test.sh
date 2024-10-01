ela ./test.ela --debug --s --metrics --test && ./test 

# --debug ; output line directives so the C++ debugger can use our information
# --s ; don't delete the .hpp and .cpp file used to transpile
# --metrics ; write performance metrics (time took for each compilation step) to stdout
# --test ; only emit functions marked #test and create a test runner, the output binary will run all the tests.