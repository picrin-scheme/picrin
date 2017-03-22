CONTRIB_LIBS += $(wildcard contrib/90.array/*.scm)

CONTRIB_TESTS += test-array

test-array: $(TEST_RUNNER)
	./$(TEST_RUNNER) contrib/90.array/t/array.scm
