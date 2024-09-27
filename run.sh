set -e


if [ -z $1 ]; then 
  ./bin/ela "dummy.ela" --silent > /dev/tty 2>&1
else 
  ./bin/ela "$1" --silent > /dev/tty 2>&1
fi

# Only compile when the output write date is older than the binary
if [ output.cpp -nt output ]; then
  clang++ output.cpp -o output > /dev/tty 2>&1
fi

# Run the output program.
./output > /dev/tty 2>&1