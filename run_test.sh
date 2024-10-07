if [ "$(basename "$PWD")" != "source" ]; then
  cd source
fi

ela ./test.ela --metrics --test --debug

if [ $? -ne 0 ]; then
  # Initial compilation failed, compile with debug symbols
  ela ./test.ela --debug --s --metrics --test 
  exit 1
fi

./test 