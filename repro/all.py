#!/usr/bin/env python3

import os
import subprocess
import shutil
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock

lock = Lock()

def process_file(file):
    base_name = os.path.splitext(file)[0]
    if subprocess.run(["ela", file], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0:
        if subprocess.run([f"./{base_name}"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0:
            return "fixed", base_name
        os.remove(base_name)
    return None, None

def process_fixed_file(file):
    base_name = os.path.splitext(os.path.basename(file))[0]
    with lock:  # Ensure thread safety
        if not os.path.exists(file):
            return None, None
        if subprocess.run(["ela", file], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode != 0:
            return "unfixed", base_name
        else:
            if subprocess.run([f"./{base_name}"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode != 0:
                return "unfixed", base_name
            os.remove(base_name)
    return None, None

def move_file(test, fixed=True):
    with lock:  # Ensure thread safety
        if fixed:
            shutil.move(f"{test}.ela", f"fixed/{test}.ela")
        else:
            shutil.move(f"fixed/{test}.ela", f"{test}.ela")

def main():
    fixed = []
    unfixed = []

    # Process *.ela files in parallel
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(process_file, file): file for file in os.listdir() if file.endswith(".ela")}
        for future in as_completed(futures):
            result, base_name = future.result()
            if result == "fixed":
                fixed.append(base_name)

    # Process fixed/*.ela files in parallel
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(process_fixed_file, os.path.join("fixed", file)): file for file in os.listdir("fixed") if file.endswith(".ela")}
        for future in as_completed(futures):
            result, base_name = future.result()
            if result == "unfixed":
                unfixed.append(base_name)

    print(f"\033[0mFixed: \033[1;32m{' '.join(fixed)}\033[0m")
    print(f"\033[0mUnfixed: \033[1;31m{' '.join(unfixed)}\033[0m")

    # Handle user input for fixed files
    for test in fixed:
        choice = input(f"Do you want to move the fixed repro '{test}'? [y/n] ").strip().lower()
        if choice == 'y':
            move_file(test, fixed=True)
            print(f"Moved {test}")
        elif choice == 'n':
            print(f"Kept {test}")
        else:
            print(f"Invalid choice, kept {test}")

    # Handle user input for unfixed files
    for test in unfixed:
        choice = input(f"Do you want to move the unfixed repro '{test}'? [y/n] ").strip().lower()
        if choice == 'y':
            move_file(test, fixed=False)
            print(f"Moved {test}")
        elif choice == 'n':
            print(f"Kept {test}")
        else:
            print(f"Invalid choice, kept {test}")

if __name__ == "__main__":
    main()