if [ "$(basename "$PWD")" != "source" ]; then
  cd source
fi

ela ./test.ela --metrics --s --test # --debug

if [ $? -ne 0 ]; then
  # Initial compilation failed, compile with debug symbols
  ela ./test.ela --s --metrics --test # --debug
  exit 1
fi

./test 