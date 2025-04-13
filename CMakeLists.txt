cmake_minimum_required(VERSION 3.20.0)

project(ela LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Default to Debug build type if not specified
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()

# Include directories
include_directories(include)

# Source files
file(GLOB SRCS "src/*.cpp")

# Output directories
set(OBJ_DIR ${CMAKE_BINARY_DIR}/../objs)
set(BIN_DIR ${CMAKE_BINARY_DIR}/../bin)

# Create directories
file(MAKE_DIRECTORY ${OBJ_DIR})
file(MAKE_DIRECTORY ${BIN_DIR})

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Print LLVM information
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "LLVM Include Dir: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM Libraries: ${LLVM_LIBRARIES}")

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Add executable
add_executable(ela ${SRCS})


#! my system has a monolithic LLVM build (arch//pacman//wayland)
#! this may not work depending on your package manager.

# Link against the monolithic LLVM library
target_link_libraries(ela PRIVATE LLVM)

# Set output directory for the executable
set_target_properties(ela PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR}
)