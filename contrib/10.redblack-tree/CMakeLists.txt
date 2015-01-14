file(GLOB RB_TREE_FILES ${PROJECT_SOURCE_DIR}/contrib/10.redblack-tree/piclib/*.scm)
list(APPEND PICLIB_CONTRIB_LIBS ${RB_TREE_FILES})
add_custom_target(test-rb-tree for test in ${PROJECT_SOURCE_DIR}/contrib/10.redblack-tree/t/*.scm \; do bin/picrin "$$test" \; done DEPENDS repl)
set(CONTRIB_TESTS ${CONTRIB_TESTS} test-rb-tree)
