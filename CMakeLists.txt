cmake_minimum_required(VERSION 3.10)

project(ela LANGUAGES CXX)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(include)

# Source files
file(GLOB SRCS "src/*.cpp")

# Output directories
set(OBJ_DIR ${CMAKE_BINARY_DIR}/objs)
set(BIN_DIR ${CMAKE_BINARY_DIR}/bin)

# Create directories
file(MAKE_DIRECTORY ${OBJ_DIR})
file(MAKE_DIRECTORY ${BIN_DIR})

# Executable
add_executable(ela ${SRCS})
target_compile_definitions(ela PRIVATE MAX_NUM_TYPES=10000)
