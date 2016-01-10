CONTRIB_DEFS += -DPIC_CSTRING_TO_DOUBLE=emyg_atod -DPIC_DOUBLE_TO_CSTRING=emyg_dtoa

CONTRIB_SRCS += contrib/10.roundtrip/emyg_dtoa.c \
                contrib/10.roundtrip/emyg_atod.c

CONTRIB_TESTS += test-roundtrip

test-roundtrip: bin/picrin
	$(TEST_RUNNER) contrib/10.roundtrip/t/roundtrip.scm
