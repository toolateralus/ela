#!/bin/bash

trap "echo -e '\e[1;34m --testing cancelled-- \e[0m'; exit" SIGINT SIGTERM

total_compile_time=0
declare -a compile_times
declare -a test_files

cd tests;

if [ $# -gt 0 ] && [ "$1" != "--time" ]; then
  test_file="$1.ela"
  if [ -f "$test_file" ]; then
    echo -e "\e[1;33mRunning test: $test_file\e[0m"
    start_time=$(date +%s%N)
    ela "$test_file" --test
    end_time=$(date +%s%N)
    compile_time=$(( (end_time - start_time) / 1000000 ))
    compile_times+=($compile_time)
    test_files+=("$test_file")
    total_compile_time=$((total_compile_time + compile_time))
    ./"${test_file%.*}"
    if [ -f "${test_file%.ela}" ]; then
      rm "${test_file%.ela}"
    fi
    exit
  else
    echo -e "\e[1;31mTest file $test_file does not exist\e[0m"
    exit 1
  fi
fi

for test_file in *.ela; do
  echo -e "\e[1;33mRunning test: $test_file\e[0m"
  start_time=$(date +%s%N)
  ela "$test_file" --test
  end_time=$(date +%s%N)
  compile_time=$(( (end_time - start_time) / 1000000 ))
  compile_times+=($compile_time)
  test_files+=("$test_file")
  total_compile_time=$((total_compile_time + compile_time))
  ./"${test_file%.*}"
  if [ -f "${test_file%.ela}" ]; then
    rm "${test_file%.ela}"
  fi
done

if [ "$1" == "--time" ]; then
  for i in "${!compile_times[@]}"; do
    echo -e "\e[1;36m\e[1m${test_files[$i]}\e[0m took \e[1;32m\e[1m${compile_times[$i]}ms\e[0m"
  done
  echo -e "\e[0mran \e[1;33m\e[1m${#compile_times[@]}\e[0m in \e[1;32m${total_compile_time}ms\e[0m"
fi