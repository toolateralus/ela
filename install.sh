#!/bin/bash

mkdir -p build
cd build

if [ "$1" == "Clean" ] || [ ! -f build.ninja ]; then
  if [ "$2" == "Release" ]; then
    cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Release
  else
    cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Debug
  fi
  ninja clean
fi

ninja -j12

if [ $? -ne 0 ]; then
  echo "Ninja build failed"
  exit 1
fi

cd ..

if [ -d /usr/local/lib/ela ]; then
  sudo rm -r /usr/local/lib/ela
fi
# Create the destination directory if it doesn't exist
sudo mkdir -p /usr/local/lib/ela
# Move the contents of ./lib to /usr/local/lib/ela
sudo cp -r ./lib/* /usr/local/lib/ela/

sudo ln -sf $(pwd)/bin/ela /usr/local/bin/ela
