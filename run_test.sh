trap "echo -e '\e[1;34m --testing cancelled-- \e[0m'; exit" SIGINT SIGTERM

if [ $# -gt 0 ]; then
  test_file="tests/$1.ela"
  if [ -f "$test_file" ]; then
    echo -e "\e[1;33mRunning test: $test_file\e[0m"
    ela "$test_file" --test && ./"${test_file%.*}"
    if [ -f "${test_file%.ela}" ]; then
      rm "${test_file%.ela}"
    fi
    exit
  else
    echo -e "\e[1;31mTest file $test_file does not exist\e[0m"
    exit 1
  fi
fi

for test_file in tests/*.ela; do
  echo -e "\e[1;33mRunning test: $test_file\e[0m"
  ela "$test_file" --test && ./"${test_file%.*}"
  if [ -f "${test_file%.ela}" ]; then
    rm "${test_file%.ela}"
  fi
done
