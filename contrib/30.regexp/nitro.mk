CONTRIB_SRCS += contrib/30.regexp/src/regexp.c
CONTRIB_INITS += regexp
CONTRIB_TESTS += test-regexp

test-regexp: $(TEST_RUNNER)
	for test in `ls contrib/30.regexp/t/*.scm`; do \
	  ./$(TEST_RUNNER) $$test; \
	done
