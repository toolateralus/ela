#!/bin/bash

mkdir -p build
cd build

if [ "$1" == "Release" ]; then
    cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Debug
fi

ninja clean
ninja -j12
cd ..

# Create the destination directory if it doesn't exist
mkdir -p /usr/local/lib/ela

# Move the contents of ./lib to /usr/local/lib/ela
sudo cp ./lib/* /usr/local/lib/ela/

sudo cp ./ela_source/boilerplate.hpp /usr/local/lib/ela/boilerplate.hpp

sudo ln -sf $(pwd)/bin/ela /usr/local/bin/ela
