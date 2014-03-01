cmake_minimum_required(VERSION 2.8)

project(picrin)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_C_FLAGS "-std=c99")

# external libraries
add_library(xfile SHARED extlib/xfile/xfile.c)

# build picrin
include_directories(include extlib)
link_directories(${PROJECT_SOURCE_DIR}/lib)
include(src/CMakeLists.txt)
include(tools/CMakeLists.txt)

# copy runtime files
file(COPY piclib DESTINATION .)
