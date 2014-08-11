# readline

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/contrib/10.readline/cmake/")

find_package(Libedit)
if (Libedit_FOUND)
  add_definitions(${Libedit_DEFINITIONS} -DPIC_READLINE_FOUND=1)
  add_definitions(${Libedit_DEFINITIONS} -DPIC_READLINE_INCLUDE_DIR_SUFFIX=${Libedit_INCLUDE_DIR_SUFFIX})
  include_directories(${Libedit_INCLUDE_DIR})

  file(GLOB PICRIN_READLINE_SOURCES ${PROJECT_SOURCE_DIR}/contrib/10.readline/src/*.c)

  list(APPEND PICRIN_CONTRIB_INITS readline)
  list(APPEND PICRIN_CONTRIB_LIBRARIES ${Libedit_LIBRARIES})
  list(APPEND PICRIN_CONTRIB_SOURCES ${PICRIN_READLINE_SOURCES})
  add_custom_target(test-readline for test in ${PROJECT_SOURCE_DIR}/contrib/10.readline/t/*.scm \; do bin/picrin "$$test" \; done DEPENDS repl)
  set(CONTRIB_TESTS ${CONTRIB_TESTS} test-readline)
endif()
