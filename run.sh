set -e

# use the 'c' argument to run our compiler.
if [ "$1" = "c" ]; then 
  ./bin/ela "dummy.ela" --silent > /dev/tty 2>&1
fi

# only compile when the output write date is older than the binary
if [ output.cpp -nt output ]; then
  clang++ output.cpp -o output > /dev/tty 2>&1
fi

# run the output program.
./output > /dev/tty 2>&1