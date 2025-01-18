#!/bin/bash

clang++ bindings-generator.cpp -std=c++26 -lclang -g -o ela-bindings-generator

if [ "$1" ]; then
  sudo ln -sf "$(pwd)/ela-bindings-generator" /usr/local/bin/ela-bindings-generator
fi