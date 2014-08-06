file(GLOB FOR_FILES ${PROJECT_SOURCE_DIR}/contrib/20.for/piclib/*.scm)
list(APPEND PICLIB_CONTRIB_LIBS ${FOR_FILES})
add_custom_target(test-for for test in ${PROJECT_SOURCE_DIR}/contrib/20.for/t/*.scm \; do bin/picrin "$$test" \; done DEPENDS repl)
set(CONTRIB_TESTS ${CONTRIB_TESTS} test-for)
