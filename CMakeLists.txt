cmake_minimum_required(VERSION 3.10)

project(ela LANGUAGES CXX)

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

# Executable
add_executable(ela ${SRCS})

# Link against libffi
find_library(FFI_LIBRARY ffi)
if(FFI_LIBRARY)
    target_link_libraries(ela PRIVATE ${FFI_LIBRARY})
else()
    message(FATAL_ERROR "libffi not found")
endif()

set_target_properties(ela PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_DEBUG ${BIN_DIR}
  RUNTIME_OUTPUT_DIRECTORY_RELEASE ${BIN_DIR}
)

target_compile_options(ela PRIVATE
  -Wall -Wextra -Wunused-function
  -Wno-missing-designated-field-initializers
  -Wno-missing-field-initializers
  -Wno-c99-designator
)

# Set compiler flags based on the compiler and platform
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    # -fsanitize=address,leak
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -flto")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W3 /EHsc")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Zi /Ob0 /Od /RTC1")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /Ob2 /DNDEBUG")
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_definitions(ela PRIVATE DEBUG DEBUG=1)
endif()