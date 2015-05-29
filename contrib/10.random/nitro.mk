CONTRIB_INITS += random
CONTRIB_SRCS += $(wildcard contrib/10.random/src/*.c)
CONTRIB_TESTS += test-random

test-random: bin/picrin
	for test in `ls contrib/10.random/t/*.scm`; do \
	  bin/picrin $$test; \
	done
