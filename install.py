#!/usr/bin/env python3

import os
import subprocess
import sys
import platform
import shutil

def run_command(command, use_shell=True):
  print(f"\033[1;33mrunning command: {command}\033[0m")
  result = subprocess.run(command, shell=use_shell)
  if result.returncode != 0:
    print(f"\033[1;31mCommand failed: {command}\033[0m")
    sys.exit(1)

def get_build_type():
  if len(sys.argv) > 2 and sys.argv[2].lower() == "release":
    return "Release"
  return "Debug"

def should_clean(build_dir, build_type):
  build_type_file = os.path.join(build_dir, ".last_build_type")
  if not os.path.exists(build_dir):
    return True
  if os.path.exists(build_type_file):
    with open(build_type_file) as f:
        last_type = f.read().strip()
    if last_type != build_type:
        return True
  if len(sys.argv) > 1 and sys.argv[1].lower() == "clean":
    return True
  return False

def save_build_type(build_dir, build_type):
  build_type_file = os.path.join(build_dir, ".last_build_type")
  with open(build_type_file, "w") as f:
    f.write(build_type)

def build_project(build_dir, build_type, do_clean):
  if do_clean and os.path.exists(build_dir):
    shutil.rmtree(build_dir)
  os.makedirs(build_dir, exist_ok=True)
  os.chdir(build_dir)
  if do_clean or not os.path.isfile("build.ninja"):
    run_command(f"cmake .. -G Ninja -DCMAKE_BUILD_TYPE={build_type}")
    run_command("ninja clean")
  bear_exists = shutil.which("bear") is not None
  if bear_exists and do_clean:
    run_command("bear -- ninja -j12")
  else:
    run_command("ninja -j12")
  os.chdir("..")
  save_build_type(build_dir, build_type)

def install_files():
  if platform.system() == "Windows":
    ela_lib_dir = os.path.join(os.environ["ProgramFiles"], "ela")
    ela_bin_dir = os.path.join(ela_lib_dir, "bin")
    os.makedirs(ela_lib_dir, exist_ok=True)
    os.makedirs(ela_bin_dir, exist_ok=True)
    # Copy lib directory
    if os.path.exists("lib"):
        dest_lib = os.path.join(ela_lib_dir, "lib")
        if os.path.exists(dest_lib):
            shutil.rmtree(dest_lib)
        shutil.copytree("lib", dest_lib)
    # Copy binary
    ela_bin_path = os.path.join(os.getcwd(), "bin", "ela.exe")
    if os.path.exists(ela_bin_path):
        shutil.copy2(ela_bin_path, ela_bin_dir)
    # Optionally, create a symlink in PATH (requires admin)
    # print("Add", ela_bin_dir, "to your PATH if you want to run ela globally.")
  else:
    ela_lib_dir = "/usr/local/lib/ela"
    # Remove old lib dir
    if os.path.isdir(ela_lib_dir):
        run_command(f"sudo rm -rf {ela_lib_dir}")
    run_command(f"sudo mkdir -p {ela_lib_dir}")
    run_command(f"sudo cp -r ./lib/* {ela_lib_dir}")
    ela_bin_path = os.path.join(os.getcwd(), "bin/ela")
    run_command(f"sudo ln -sf {ela_bin_path} /usr/local/bin/ela")

def main():
  build_dir = "build"
  build_type = get_build_type()
  print(f"\033[1;34mbuilding in {build_type}\033[0m")
  do_clean = should_clean(build_dir, build_type)
  build_project(build_dir, build_type, do_clean)
  install_files()

if __name__ == "__main__":
  main()