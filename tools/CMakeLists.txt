find_package(READLINE)
if (READLINE_FOUND)
  include_directories(${READLINE_INCLUDE_DIRS})
  add_definitions(${READLINE_DEFINITIONS} -DPIC_READLINE_FOUND=1)
endif()

add_executable(repl tools/main.c)
set_target_properties(repl PROPERTIES OUTPUT_NAME picrin)
target_link_libraries(repl picrin ${READLINE_LIBRARIES})
