CONTRIB_INITS += random
CONTRIB_SRCS += $(wildcard contrib/30.random/src/*.c)
CONTRIB_TESTS += test-random

test-random: $(TEST_RUNNER)
	for test in `ls contrib/30.random/t/*.scm`; do \
	  ./$(TEST_RUNNER) $$test; \
	done
