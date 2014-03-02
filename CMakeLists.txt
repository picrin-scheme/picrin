cmake_minimum_required(VERSION 2.8)

project(picrin)

# git submodule update --init
execute_process(
  COMMAND ${GIT_EXECUTABLE} submodule update --init
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_C_FLAGS "-Wall -Wextra -std=c99")
set(CMAKE_C_FLAGS_DEBUG "-DDEBUG=1")

# external libraries
add_library(xfile SHARED extlib/xfile/xfile.c)

# build picrin
include_directories(include extlib)
include(src/CMakeLists.txt)
include(tools/CMakeLists.txt)

# copy runtime files
file(COPY piclib DESTINATION .)
file(COPY etc DESTINATION .)

# $ make run
add_custom_target(run bin/picrin DEPENDS repl)

# $ make no-act
add_custom_target(no-act bin/picrin -e '' > /dev/null DEPENDS repl)

# $ make tak
add_custom_target(tak bin/picrin etc/tak.scm DEPENDS repl)

# $ make lines
add_custom_target(lines find . -name "*.[chyl]" | xargs wc -l WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
