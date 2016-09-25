CONTRIB_INITS += \
	big_number
CONTRIB_LIBS += $(wildcard contrib/50.big-number/big-number/*.scm)
CONTRIB_SRCS += contrib/50.big-number/src/big_number.c
CONTRIB_TESTS += test-big-number

test-big-number: bin/picrin
	for test in `ls contrib/50.big-number/t/*.scm`; do \
	  bin/picrin $$test; \
	done
