#!/usr/bin/env python3

import os
import signal
import sys
import time
import subprocess

def signal_handler(sig, frame):
    print("\033[1;34m --testing cancelled-- \033[0m")
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)

total_compile_time = 0
compile_times = []
test_files = []
passed_tests = []
failed_tests = []

os.chdir('tests')

def run_test(test_file):
    print(f"\033[1;33mRunning test: {test_file}\033[0m")
    start_time = time.time_ns()
    result = subprocess.run(["ela", test_file, "--test"])
    end_time = time.time_ns()
    if result.returncode == 0:
        compile_time = (end_time - start_time) // 1000000
        compile_times.append(compile_time)
        test_files.append(test_file)
        global total_compile_time
        total_compile_time += compile_time
        run_result = subprocess.run([f"./{test_file[:-4]}"])
        if run_result.returncode == 0:
            passed_tests.append(test_file)
        else:
            failed_tests.append(test_file)
        if os.path.isfile(test_file[:-4]):
            os.remove(test_file[:-4])
    else:
        print(f"\033[1;31mCompilation failed for {test_file}\033[0m")
        failed_tests.append(test_file)

def main():
    if len(sys.argv) > 1 and sys.argv[1] != "--time":
        test_file = f"{sys.argv[1]}.ela"
        if os.path.isfile(test_file):
            run_test(test_file)
            sys.exit(0)
        else:
            print(f"\033[1;31mTest file {test_file} does not exist\033[0m")
            sys.exit(1)

    for test_file in os.listdir('.'):
        if test_file.endswith('.ela'):
            run_test(test_file)

    if len(sys.argv) > 1 and sys.argv[1] == "--time":
        for i, compile_time in enumerate(compile_times):
            print(f"\033[1;36m\033[1m{test_files[i]}\033[0m took \033[1;32m\033[1m{compile_time}ms\033[0m")
        print(f"\033[0mran \033[1;33m\033[1m{len(compile_times)}\033[0m in \033[1;32m{total_compile_time}ms\033[0m")

    # Print summary of passed and failed tests
    print("\n\033[1;34mTest Summary:\033[0m")
    print(f"\033[1;32mPassed tests ({len(passed_tests)}):\033[0m")
    for test in passed_tests:
        print(f"  - {test}")
    print(f"\033[1;31mFailed tests ({len(failed_tests)}):\033[0m")
    for test in failed_tests:
        print(f"  - {test}")

if __name__ == "__main__":
    main()