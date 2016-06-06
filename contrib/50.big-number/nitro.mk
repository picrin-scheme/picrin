CONTRIB_INITS += \
	big_number
CONTRIB_LIBS += contrib/50.big-number/lib/lib.scm
CONTRIB_SRCS += $(wildcard contrib/50.big-number/src/*.c)
CONTRIB_TESTS += test-big-number

test-big-number: bin/picrin
	for test in `ls contrib/50.big-number/t/*.scm`; do \
	  bin/picrin $$test; \
	done
