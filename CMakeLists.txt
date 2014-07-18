cmake_minimum_required(VERSION 2.6)

PROJECT(picrin)

# load extra cmake modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

# ----

# git submodule update --init
find_package(Git REQUIRED)
execute_process(
  COMMAND ${GIT_EXECUTABLE} submodule update --init
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_C_FLAGS "-Wall -Wextra")
set(CMAKE_C_FLAGS_DEBUG "-g -DDEBUG=1")

option(USE_C11_FEATURE "Enable c11 feature" OFF)
if(USE_C11_FEATURE)
  add_definitions(-std=c11)
else()
  add_definitions(-std=c99)     # at least c99 is required
endif()

include_directories(include extlib)

# build picrin
include(piclib/CMakeLists.txt)
include(contrib/CMakeLists.txt)
include(src/CMakeLists.txt)
include(tools/CMakeLists.txt)

# ----

# $ make run
add_custom_target(run bin/picrin DEPENDS repl)

# $ make test
add_custom_target(test DEPENDS test-r7rs)

# $ make test-r7rs
add_custom_target(test-r7rs bin/picrin ${PROJECT_SOURCE_DIR}/t/r7rs-tests.scm DEPENDS repl)

# $ make tak
add_custom_target(tak bin/picrin ${PROJECT_SOURCE_DIR}/etc/tak.scm DEPENDS repl)

# $ make lines
add_custom_target(lines find . -name "*.[chyl]" | xargs wc -l WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
