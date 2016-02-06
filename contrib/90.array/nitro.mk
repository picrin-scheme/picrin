CONTRIB_LIBS += $(wildcard contrib/90.array/*.scm)

CONTRIB_TESTS += test-array

test-array: bin/picrin
	$(TEST_RUNNER) contrib/90.array/t/array.scm
