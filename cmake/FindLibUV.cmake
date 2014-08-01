# - Try to find Libuv
# Once done this will define
#  LIBUV_FOUND - System has Libuv
#  LIBUV_INCLUDE_DIRS - The Libuv include directories
#  LIBUV_LIBRARIES - The libraries needed to use Libuv
#  LIBUV_DEFINITIONS - Compiler switches required for using Libuv

find_package(PkgConfig)
pkg_check_modules(PC_LIBUV QUIET libuv)
set(LIBUV_DEFINITIONS ${PC_LIBUV_CFLAGS_OTHER})

find_path(LIBUV_INCLUDE_DIR uv.h
          HINTS ${PC_LIBUV_INCLUDEDIR} ${PC_LIBUV_INCLUDE_DIRS})

find_library(LIBUV_LIBRARY NAMES uv
             HINTS ${PC_LIBUV_LIBDIR} ${PC_LIBUV_LIBRARY_DIRS})

set(LIBUV_LIBRARIES ${LIBUV_LIBRARY})
set(LIBUV_INCLUDE_DIRS ${LIBUV_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Libuv  DEFAULT_MSG
                                  LIBUV_LIBRARY LIBUV_INCLUDE_DIR)

mark_as_advanced(LIBUV_INCLUDE_DIR LIBUV_LIBRARY)
