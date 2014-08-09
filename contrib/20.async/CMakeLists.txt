file(GLOB ASYNC_FILES ${PROJECT_SOURCE_DIR}/contrib/20.async/piclib/*.scm)
list(APPEND PICLIB_CONTRIB_LIBS ${ASYNC_FILES})

# add_custom_target(test-async async test in ${PROJECT_SOURCE_DIR}/contrib/20.async/t/*.scm \; do bin/picrin "$$test" \; done DEPENDS repl)
# set(CONTRIB_TESTS ${CONTRIB_TESTS} test-async)
