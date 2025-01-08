for test_file in ela_source/tests/*.ela; do
  echo -e "\e[1;33mRunning test: $test_file\e[0m"
  ela "$test_file" --test && ./"${test_file%.*}"
  if [ -f "${test_file%.ela}" ]; then
    rm "${test_file%.ela}"
  fi
done