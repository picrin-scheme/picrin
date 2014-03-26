list(APPEND REPL_LIBRARIES picrin)

find_package(READLINE)
if (READLINE_FOUND)
  include_directories(${READLINE_INCLUDE_DIRS})
  add_definitions(${READLINE_DEFINITIONS} -DPIC_READLINE_FOUND=1)
  list(APPEND REPL_LIBRARIES ${READLINE_LIBRARIES})
endif()

add_executable(repl tools/main.c)
set_target_properties(repl PROPERTIES OUTPUT_NAME picrin)
target_link_libraries(repl ${REPL_LIBRARIES})
