list(APPEND REPL_LIBRARIES picrin)

# build
add_executable(repl tools/main.c)
set_target_properties(repl PROPERTIES OUTPUT_NAME picrin)
target_link_libraries(repl ${REPL_LIBRARIES})

# install
install(TARGETS repl RUNTIME DESTINATION bin)
