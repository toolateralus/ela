if [ "$(basename "$PWD")" != "source" ]; then
  cd ela_source
fi

ela ./test.ela --metrics --s --test

if [ $? -ne 0 ]; then
  ela ./test.ela --s --metrics --test 
  exit 1
fi

./test 