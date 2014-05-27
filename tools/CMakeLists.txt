list(APPEND REPL_LIBRARIES picrin)

find_package(LIBEDIT)
if (Libedit_FOUND)
  include_directories(${Libedit_INCLUDE_DIRS})
  add_definitions(${Libedit_DEFINITIONS} -DPIC_READLINE_FOUND=1)
  list(APPEND REPL_LIBRARIES ${Libedit_LIBRARIES})
endif()

# build
add_executable(repl tools/main.c)
set_target_properties(repl PROPERTIES OUTPUT_NAME picrin)
target_link_libraries(repl ${REPL_LIBRARIES})

# install
install(TARGETS repl RUNTIME DESTINATION bin)
