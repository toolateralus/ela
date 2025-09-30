#!/usr/bin/env python3

import os
import sys
import time
import signal
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed

# ---------------- Signal Handling ---------------- #

def signal_handler(sig, frame):
    print("\033[1;34m --testing cancelled-- \033[0m")
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)

# ---------------- Globals ---------------- #

total_compile_time = 0
compile_times = []
passed_tests = []
failed_tests = []
failure_outputs = {}   # test_file -> captured compiler/runtime output

os.chdir("tests")

# ---------------- Test Runner ---------------- #

def run_test(test_file: str, capture_output: bool = False):
    """Compile and run a single test file. Returns (status, test_file, compile_time, output)."""
    print(f"\033[1;33mRunning test: {test_file}\033[0m")
    start_time = time.time_ns()

    compile_proc = subprocess.run(
        ["ela", test_file, "--test"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    end_time = time.time_ns()
    compile_time = (end_time - start_time) // 1_000_000

    if compile_proc.returncode != 0:
        print(f"\033[1;31mCompilation failed for {test_file}\033[0m")
        output = (compile_proc.stdout.decode() + compile_proc.stderr.decode()) if capture_output else None
        return "failed", test_file, 0, output

    global total_compile_time
    total_compile_time += compile_time

    run_proc = subprocess.run(
        [f"./{test_file[:-4]}"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    if run_proc.returncode == 0:
        return "passed", test_file, compile_time, None
    elif run_proc.returncode < 0:  # abnormal termination (signal)
        sig_name = signal.Signals(-run_proc.returncode).name
        print(f"\033[1;31mTest {test_file} failed with signal: {sig_name}\033[0m")
        msg = run_proc.stdout.decode() + run_proc.stderr.decode()
        return "failed", f"{test_file} (signal: {sig_name})", compile_time, msg
    else:
        print(f"\033[1;31mTest {test_file} failed with return code: {run_proc.returncode}\033[0m")
        msg = run_proc.stdout.decode() + run_proc.stderr.decode()
        return "failed", test_file, compile_time, msg

# ---------------- Main ---------------- #

def main():
    global total_compile_time
    args = sys.argv[1:]
    show_output = "-g" in args

    # Run a single test if specified
    if args and args[0] not in ("--time", "-g"):
        test_file = f"{args[0]}.ela"
        if not os.path.isfile(test_file):
            print(f"\033[1;31mTest file {test_file} does not exist\033[0m")
            sys.exit(1)

        status, name, ctime, output = run_test(test_file, capture_output=show_output)
        if status == "passed":
            passed_tests.append(name)
        else:
            failed_tests.append(name)
            if show_output and output:
                failure_outputs[name] = output
        sys.exit(0)

    # Collect all tests
    test_files = [f for f in os.listdir(".") if f.endswith(".ela")]

    # Run all tests in parallel
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(run_test, f, show_output): f for f in test_files}
        for future in as_completed(futures):
            status, name, ctime, output = future.result()
            if status == "passed":
                passed_tests.append(name)
                compile_times.append(ctime)
            else:
                failed_tests.append(name)
                if show_output and output:
                    failure_outputs[name] = output

    # Show compile times
    if args and args[0] == "--time":
        for fname, ctime in zip(test_files, compile_times):
            print(f"\033[1;36m{fname}\033[0m took \033[1;32m{ctime}ms\033[0m")
        print(f"\033[0mran \033[1;33m{len(compile_times)}\033[0m in \033[1;32m{total_compile_time}ms\033[0m")

    # Print summary
    print("\n\033[1;34mTest Summary:\033[0m")
    max_len = max((len(t) for t in passed_tests + failed_tests), default=0)
    col_width = max_len + 15
    per_line = 3

    print(f"\033[1;32mPassed tests ({len(passed_tests)}):\033[0m")
    for i in range(0, len(passed_tests), per_line):
        row = passed_tests[i:i+per_line]
        print("  - " + "".join(f"{t:<{col_width}}" for t in row))

    print(f"\033[1;31mFailed tests ({len(failed_tests)}):\033[0m")
    for i in range(0, len(failed_tests), per_line):
        row = failed_tests[i:i+per_line]
        print("  - " + "".join(f"{t:<{col_width}}" for t in row))

    # Print detailed failure outputs
    if show_output and failure_outputs:
        print("\n\033[1;35mDetailed Failure Outputs:\033[0m")
        for test, out in failure_outputs.items():
            print(f"\n\033[1;31m{test}:\033[0m")
            print(out.strip() or "<no output>")

    for fname in os.listdir("."):
        if not fname.endswith(".ela"):
            try:
                os.remove(fname)
            except Exception as e:
                print(f"\033[1;31mFailed to remove {fname}: {e}\033[0m")

# ---------------- Entry ---------------- #

if __name__ == "__main__":
    main()
