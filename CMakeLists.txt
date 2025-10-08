cmake_minimum_required(VERSION 3.10)

project(ela LANGUAGES CXX)

# -----------------------------
# Build Options
# -----------------------------
option(BUILD_SHARED "Build ela as a shared library instead of an executable" OFF)

# -----------------------------
# Compiler Settings
# -----------------------------
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_COMPILER clang++)

# Default build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()

# -----------------------------
# Include Directories
# -----------------------------
include_directories(include)

# -----------------------------
# Source Files
# -----------------------------
file(GLOB SRCS "src/*.cpp")

if(BUILD_SHARED)
  # Exclude main.cpp when building as a shared library
  list(FILTER SRCS EXCLUDE REGEX ".*/main\\.cpp$")
endif()

# -----------------------------
# Output Directories
# -----------------------------
set(OBJ_DIR ${CMAKE_BINARY_DIR}/../objs)
set(BIN_DIR ${CMAKE_BINARY_DIR}/../bin)

file(MAKE_DIRECTORY ${OBJ_DIR})
file(MAKE_DIRECTORY ${BIN_DIR})

# -----------------------------
# Target Definition
# -----------------------------
if(BUILD_SHARED)
  add_library(ela SHARED ${SRCS})
  target_compile_options(ela PRIVATE -fPIC)
  set_target_properties(ela PROPERTIES POSITION_INDEPENDENT_CODE ON)
else()
  add_executable(ela ${SRCS})
endif()

# -----------------------------
# Link Libraries
# -----------------------------
find_library(FFI_LIBRARY ffi)
if(FFI_LIBRARY)
  target_link_libraries(ela PRIVATE ${FFI_LIBRARY})
else()
  message(FATAL_ERROR "libffi not found")
endif()

# -----------------------------
# Output Configuration
# -----------------------------
set_target_properties(ela PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_DEBUG ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_RELEASE ${BIN_DIR}
)

# -----------------------------
# Compiler Warnings
# -----------------------------
target_compile_options(ela PRIVATE
  -Wall
  -Wextra
  -Wunused-function
  -Wno-missing-designated-field-initializers
  -Wno-missing-field-initializers
  -Wno-c99-designator
)

# -----------------------------
# Compiler Flags
# -----------------------------
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -flto")

# -----------------------------
# Debug Definitions
# -----------------------------
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_definitions(ela PRIVATE DEBUG DEBUG=1)
endif()

message(STATUS "Configuration complete. Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Shared library build: ${BUILD_SHARED}")
