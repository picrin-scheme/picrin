# regex

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/contrib/10.regexp/cmake/")

find_package(REGEX)

if (REGEX_FOUND)
  add_definitions(${REGEX_DEFINITIONS})
  include_directories(${REGEX_INCLUDE_DIR})

  file(GLOB PICRIN_REGEX_SOURCES ${PROJECT_SOURCE_DIR}/contrib/10.regexp/src/*.c)

  list(APPEND PICRIN_CONTRIB_INITS regexp)
  list(APPEND PICRIN_CONTRIB_LIBRARIES ${REGEX_LIBRARIES})
  list(APPEND PICRIN_CONTRIB_SOURCES ${PICRIN_REGEX_SOURCES})
  add_custom_target(test-regexp for test in ${PROJECT_SOURCE_DIR}/contrib/10.regexp/t/*.scm \; do bin/picrin "$$test" \; done DEPENDS repl)
  set(CONTRIB_TESTS ${CONTRIB_TESTS} test-regexp)
endif()
