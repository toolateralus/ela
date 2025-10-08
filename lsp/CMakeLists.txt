cmake_minimum_required(VERSION 3.10)

project(ela_lsp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_COMPILER clang++ CACHE STRING "C++ compiler" FORCE)
# Default build type to Debug for single-config generators
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type specified; defaulting to Debug")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build type" FORCE)
endif()

# Expose common build type choices in the cache
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug;Release;RelWithDebInfo;MinSizeRel")

# Include directories
include_directories(
  "${CMAKE_CURRENT_LIST_DIR}/../include"
  "${CMAKE_CURRENT_LIST_DIR}/include"
)

# Only build the local LSP sources
file(GLOB_RECURSE SRCS
  "${CMAKE_CURRENT_LIST_DIR}/src/*.cpp"
)

# Output directories
set(OBJ_DIR "${CMAKE_BINARY_DIR}/objs")
set(BIN_DIR "${CMAKE_BINARY_DIR}/bin")
file(MAKE_DIRECTORY "${OBJ_DIR}")
file(MAKE_DIRECTORY "${BIN_DIR}")

# Executable
add_executable(ela_lsp ${SRCS})

# Link against the prebuilt libela.so
# Assuming libela.so is at project_root/deps/libela.so
target_link_libraries(ela_lsp PRIVATE "${CMAKE_CURRENT_LIST_DIR}/deps/libela.so")

find_package(nlohmann_json REQUIRED)
target_link_libraries(ela_lsp PRIVATE nlohmann_json::nlohmann_json)

# Runtime output location
set_target_properties(ela_lsp PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${BIN_DIR}"
  RUNTIME_OUTPUT_DIRECTORY_DEBUG "${BIN_DIR}"
  RUNTIME_OUTPUT_DIRECTORY_RELEASE "${BIN_DIR}"
)

# Compiler warnings & flags
target_compile_options(ela_lsp PRIVATE
  -Wall -Wextra -Wunused-function
  -Wno-missing-designated-field-initializers
  -Wno-missing-field-initializers
  -Wno-c99-designator
)

# Debug/release flags
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -flto")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_definitions(ela_lsp PRIVATE DEBUG=1)
endif()

# Optional: helper target to generate compile_commands.json using bear
find_program(BEAR_EXECUTABLE bear)
if(BEAR_EXECUTABLE)
  add_custom_target(bear
    COMMAND ${BEAR_EXECUTABLE} --append -- cmake --build ${CMAKE_BINARY_DIR} --target ${PROJECT_NAME}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Running bear to produce compile_commands.json"
  )
else()
  add_custom_target(bear
    COMMENT "bear not found: install 'bear' to generate compile_commands.json (or use CMAKE_EXPORT_COMPILE_COMMANDS)"
  )
endif()
