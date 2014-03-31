# regex
find_package(REGEX)

if (REGEX_FOUND)
  add_definitions(${REGEX_DEFINITIONS})
  include_directories(${REGEX_INCLUDE_DIR})

  file(GLOB PICRIN_REGEX_SOURCES ${PROJECT_SOURCE_DIR}/contrib/regexp/src/*.c)

  list(APPEND PICRIN_CONTRIB_INITS "void pic_init_regexp(pic_state *)\; pic_init_regexp(pic)\;")
  list(APPEND PICRIN_CONTRIB_LIBRARIES ${REGEX_LIBRARIES})
  list(APPEND PICRIN_CONTRIB_SOURCES ${PICRIN_REGEX_SOURCES})
endif()
