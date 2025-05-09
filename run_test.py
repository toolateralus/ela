#!/usr/bin/env python3

import os
import signal
import sys
import time
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed

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

    result = subprocess.run(["ela", test_file, "--test"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
      
    end_time = time.time_ns()
    if result.returncode == 0:
        compile_time = (end_time - start_time) // 1000000
        global total_compile_time
        total_compile_time += compile_time
        run_result = subprocess.run([f"./{test_file[:-4]}"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if run_result.returncode == 0:
            return ("passed", test_file, compile_time)
        elif run_result.returncode < 0:  # Abnormal termination (e.g., segfault)
            signal_name = signal.Signals(-run_result.returncode).name
            print(f"\033[1;31mTest {test_file} failed with signal: {signal_name}\033[0m")
            return ("failed", f"{test_file} (signal: {signal_name})", compile_time)
        else:
            print(f"\033[1;31mTest {test_file} failed with return code: {run_result.returncode}\033[0m")
            return ("failed", test_file, compile_time)
    else:
        print(f"\033[1;31mCompilation failed for {test_file}\033[0m")
        return ("failed", test_file, 0)

def main():
    global total_compile_time
    if len(sys.argv) > 1 and sys.argv[1] != "--time":
        test_file = f"{sys.argv[1]}.ela"
        if os.path.isfile(test_file):
            result, test_file, compile_time = run_test(test_file)
            if result == "passed":
                passed_tests.append(test_file)
            else:
                failed_tests.append(test_file)
            sys.exit(0)
        else:
            print(f"\033[1;31mTest file {test_file} does not exist\033[0m")
            sys.exit(1)

    # Collect all test files
    test_files = [f for f in os.listdir('.') if f.endswith('.ela')]

    # Run tests in parallel
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(run_test, test_file): test_file for test_file in test_files}
        for future in as_completed(futures):
            result, test_file, compile_time = future.result()
            if result == "passed":
                passed_tests.append(test_file)
                compile_times.append(compile_time)
            else:
                failed_tests.append(test_file)

    # Print compile times if requested
    if len(sys.argv) > 1 and sys.argv[1] == "--time":
        for i, compile_time in enumerate(compile_times):
            print(f"\033[1;36m\033[1m{test_files[i]}\033[0m took \033[1;32m\033[1m{compile_time}ms\033[0m")
        print(f"\033[0mran \033[1;33m\033[1m{len(compile_times)}\033[0m in \033[1;32m{total_compile_time}ms\033[0m")

    # Print summary of passed and failed tests
    print("\n\033[1;34mTest Summary:\033[0m")
    # Calculate dynamic column width based on the longest test name
    max_passed_length = max((len(test) for test in passed_tests), default=0)
    max_failed_length = max((len(test) for test in failed_tests), default=0)
    COLUMN_WIDTH = max(max_passed_length, max_failed_length) + 15  # Add padding for spacing
    ITEMS_PER_LINE = 3  # Adjust the number of items per line
    
    # Print passed tests
    print(f"\033[1;32mPassed tests ({len(passed_tests)}):\033[0m")
    for i in range(0, len(passed_tests), ITEMS_PER_LINE):
        row = passed_tests[i:i+ITEMS_PER_LINE]
        print("  - " + "".join(f"{test:<{COLUMN_WIDTH}}" for test in row))
    
    # Print failed tests
    print(f"\033[1;31mFailed tests ({len(failed_tests)}):\033[0m")
    for i in range(0, len(failed_tests), ITEMS_PER_LINE):
        row = failed_tests[i:i+ITEMS_PER_LINE]
        print("  - " + "".join(f"{test:<{COLUMN_WIDTH}}" for test in row))

if __name__ == "__main__":
    main()