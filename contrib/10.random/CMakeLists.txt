file(GLOB PICRIN_RANDOM_SOURCES ${PROJECT_SOURCE_DIR}/contrib/10.random/src/*.c)

list(APPEND PICRIN_CONTRIB_INITS random)
list(APPEND PICRIN_CONTRIB_SOURCES ${PICRIN_RANDOM_SOURCES})
add_custom_target(test-random
  for test in ${PROJECT_SOURCE_DIR}/contrib/10.random/t/*.scm \;
  do
    bin/picrin "$$test" \;
  done
  DEPENDS repl)
set(CONTRIB_TESTS ${CONTRIB_TESTS} test-random)
