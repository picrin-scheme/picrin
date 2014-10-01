file(GLOB OPTIONAL_FILES ${PROJECT_SOURCE_DIR}/contrib/10.optional/piclib/*.scm)
list(APPEND PICLIB_CONTRIB_LIBS ${OPTIONAL_FILES})
add_custom_target(test-optional
  for test in ${PROJECT_SOURCE_DIR}/contrib/10.optional/t/*.scm \;
  do
    bin/picrin "$$test" \;
  done
  DEPENDS repl)
set(CONTRIB_TESTS ${CONTRIB_TESTS} test-optional)
