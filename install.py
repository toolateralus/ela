#!/usr/bin/env python3

import os
import subprocess
import sys
import shutil
import platform

def run_command(command):
  result = subprocess.run(command, shell=True)
  if result.returncode != 0:
    print(f"Command failed: {command}")
    sys.exit(1)

def main():
  if os.geteuid() != 0:
    print("This script requires sudo privileges. Please run with sudo.")
    sys.exit(1)

  build_dir = "build"
  os.makedirs(build_dir, exist_ok=True)
  os.chdir(build_dir)

  if len(sys.argv) > 1 and (sys.argv[1] == "Clean" or not os.path.isfile("build.ninja")):
    build_type = "Release" if len(sys.argv) > 2 and sys.argv[2] == "Release" else "Debug"
    run_command(f"cmake .. -G Ninja -DCMAKE_BUILD_TYPE={build_type}")
    run_command("ninja clean")

  run_command("ninja -j12")

  os.chdir("..")

  if platform.system() == "Windows":
    ela_lib_dir = os.path.join(os.environ["ProgramFiles"], "ela")
    ela_bin_dir = os.path.join(os.environ["ProgramFiles"], "ela", "bin")
    os.makedirs(ela_lib_dir, exist_ok=True)
    os.makedirs(ela_bin_dir, exist_ok=True)
    run_command(f"xcopy /E /I /Y lib {ela_lib_dir}")
    ela_bin_path = os.path.join(os.getcwd(), "bin", "ela.exe")
    run_command(f"copy {ela_bin_path} {ela_bin_dir}")
  else:
    ela_lib_dir = "/usr/local/lib/ela"
    if os.path.isdir(ela_lib_dir):
        run_command(f"sudo rm -r {ela_lib_dir}")
    os.makedirs(ela_lib_dir, exist_ok=True)
    run_command(f"sudo cp -r ./lib/* {ela_lib_dir}")
    ela_bin_path = os.path.join(os.getcwd(), "bin/ela")
    run_command(f"sudo ln -sf {ela_bin_path} /usr/local/bin/ela")

if __name__ == "__main__":
  main()