# - Try to find libedit
# Once done this will define
#
#  Libedit_FOUND - system has libedit
#  Libedit_INCLUDE_DIRS - the libedit include directory
#  Libedit_LIBRARIES - Link these to use libedit
#  Libedit_DEFINITIONS - Compiler switches required for using libedit
#
#  Copyright (c) 2014 Yuichi Nishiwaki <yuichi@idylls.jp>
#  Copyright (c) 2008 Andreas Schneider <mail@cynapses.org>
#  Modified for other libraries by Lasse Kärkkäinen <tronic>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#


if (Libedit_LIBRARIES AND Libedit_INCLUDE_DIRS)
  # in cache already
  set(Libedit_FOUND TRUE)
else (Libedit_LIBRARIES AND Libedit_INCLUDE_DIRS)
  # use pkg-config to get the directories and then use these values
  # in the FIND_PATH() and FIND_LIBRARY() calls
  if (${CMAKE_MAJOR_VERSION} EQUAL 2 AND ${CMAKE_MINOR_VERSION} EQUAL 4)
    include(UsePkgConfig)
    pkgconfig(libedit _Libedit_INCLUDEDIR _Libedit_LIBDIR _Libedit_LDFLAGS _Libedit_CFLAGS)
  else (${CMAKE_MAJOR_VERSION} EQUAL 2 AND ${CMAKE_MINOR_VERSION} EQUAL 4)
    find_package(PkgConfig)
    if (PKG_CONFIG_FOUND)
      pkg_check_modules(_LIBEDIT libedit)
    endif (PKG_CONFIG_FOUND)
  endif (${CMAKE_MAJOR_VERSION} EQUAL 2 AND ${CMAKE_MINOR_VERSION} EQUAL 4)
  find_path(Libedit_EDITLINE_INCLUDE_DIR
    NAMES
      editline/readline.h
      editline/history.h
    PATHS
      ${_Libedit_INCLUDEDIR}
      /usr/include
      /usr/local/include
      /opt/local/include
      /sw/include
  )
  if (Libedit_EDITLINE_INCLUDE_DIR)
    set(Libedit_INCLUDE_DIR_SUFFIX editline)
    set(Libedit_INCLUDE_DIR ${Libedit_EDITLINE_INCLUDE_DIR})
  else (Libedit_EDITLINE_INCLUDE_DIR)
    find_path(Libedit_READLINE_INCLUDE_DIR
      NAMES
        readline/readline.h
        readline/history.h
      PATHS
        /usr/include/edit
        /usr/local/include/edit
        /opt/local/include/edit
        /sw/include/edit
        )
    if (Libedit_READLINE_INCLUDE_DIR)
      set(Libedit_INCLUDE_DIR_SUFFIX readline)
      set(Libedit_INCLUDE_DIR ${Libedit_READLINE_INCLUDE_DIR})
    endif (Libedit_READLINE_INCLUDE_DIR)
  endif (Libedit_EDITLINE_INCLUDE_DIR)

  find_library(Libedit_LIBRARY
    NAMES
      edit
    PATHS
      ${_Libedit_LIBDIR}
      /usr/lib
      /usr/local/lib
      /opt/local/lib
      /sw/lib
  )

  if (Libedit_LIBRARY)
    set(Libedit_FOUND TRUE)
  endif (Libedit_LIBRARY)

  set(Libedit_INCLUDE_DIRS
    ${Libedit_INCLUDE_DIR}
  )

  if (Libedit_FOUND)
    set(Libedit_LIBRARIES
      ${Libedit_LIBRARIES}
      ${Libedit_LIBRARY}
    )
  endif (Libedit_FOUND)

  if (Libedit_INCLUDE_DIRS AND Libedit_LIBRARIES)
     set(Libedit_FOUND TRUE)
  endif (Libedit_INCLUDE_DIRS AND Libedit_LIBRARIES)

  if (Libedit_FOUND)
    if (NOT Libedit_FIND_QUIETLY)
      message(STATUS "Found libedit: ${Libedit_LIBRARY}, ${Libedit_INCLUDE_DIR}")
    endif (NOT Libedit_FIND_QUIETLY)
  else (Libedit_FOUND)
    if (Libedit_FIND_REQUIRED)
      message(FATAL_ERROR "Could not find libedit")
    endif (Libedit_FIND_REQUIRED)
  endif (Libedit_FOUND)

  # show the Libedit_INCLUDE_DIRS and Libedit_LIBRARIES variables only in the advanced view
  mark_as_advanced(Libedit_INCLUDE_DIRS Libedit_LIBRARIES)

endif (Libedit_LIBRARIES AND Libedit_INCLUDE_DIRS)
