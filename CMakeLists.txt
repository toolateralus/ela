cmake_minimum_required(VERSION 3.10)

project(ela LANGUAGES CXX)

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
set(SRCS_EXE ${SRCS})
set(SRCS_LIB ${SRCS})
list(FILTER SRCS_LIB EXCLUDE REGEX ".*/main\\.cpp$")  # library doesn't get main.cpp

# -----------------------------
# Output Directories
# -----------------------------
set(OBJ_DIR ${CMAKE_BINARY_DIR}/../objs)
set(BIN_DIR ${CMAKE_BINARY_DIR}/../bin)

file(MAKE_DIRECTORY ${OBJ_DIR})
file(MAKE_DIRECTORY ${BIN_DIR})

# -----------------------------
# Targets
# -----------------------------
# Executable (default)
add_executable(ela ${SRCS_EXE})

# Shared library (excluded from default 'all')
add_library(ela_shared SHARED ${SRCS_LIB})
# Make the .so have the same base name as the executable (libela.so)
set_target_properties(ela_shared PROPERTIES
  OUTPUT_NAME ela
  POSITION_INDEPENDENT_CODE ON
  EXCLUDE_FROM_ALL TRUE
)

# Provide a convenient top-level target so you can run: ninja shared
add_custom_target(shared DEPENDS ela_shared)

# -----------------------------
# Output locations
# -----------------------------
# Executable runtime output
set_target_properties(ela PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_DEBUG ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_RELEASE ${BIN_DIR}
)

# Shared library output
set_target_properties(ela_shared PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ${BIN_DIR}
  LIBRARY_OUTPUT_DIRECTORY_DEBUG ${BIN_DIR}
  LIBRARY_OUTPUT_DIRECTORY_RELEASE ${BIN_DIR}
  ARCHIVE_OUTPUT_DIRECTORY ${OBJ_DIR}
)

# -----------------------------
# Compiler Warnings & Flags
# -----------------------------
foreach(tgt IN ITEMS ela ela_shared)
  target_compile_options(${tgt} PRIVATE
    -Wall
    -Wextra
    -Wunused-function
    -Wno-missing-designated-field-initializers
    -Wno-missing-field-initializers
    -Wno-c99-designator
  )
endforeach()

# PIC only needed for the shared lib; ensure it's set
target_compile_options(ela_shared PRIVATE -fPIC)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -flto")

# -----------------------------
# Link Libraries
# -----------------------------
find_library(FFI_LIBRARY ffi)
if(FFI_LIBRARY)
  target_link_libraries(ela PRIVATE ${FFI_LIBRARY})
  target_link_libraries(ela_shared PRIVATE ${FFI_LIBRARY})
else()
  message(FATAL_ERROR "libffi not found")
endif()

# -----------------------------
# Debug Definitions
# -----------------------------
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_definitions(ela PRIVATE DEBUG DEBUG=1)
  target_compile_definitions(ela_shared PRIVATE DEBUG DEBUG=1)
endif()

message(STATUS "Configuration complete. Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Default target: ela (use 'ninja' to build). To build shared library run 'ninja shared'.")
