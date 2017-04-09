CONTRIB_INITS += roundtrip

CONTRIB_SRCS += contrib/10.roundtrip/emyg_dtoa.c \
                contrib/10.roundtrip/emyg_atod.c \
		contrib/10.roundtrip/emyg.c

CONTRIB_TESTS += test-roundtrip

test-roundtrip: $(TEST_RUNNER)
	./$(TEST_RUNNER) contrib/10.roundtrip/t/roundtrip.scm
