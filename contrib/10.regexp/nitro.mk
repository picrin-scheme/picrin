CONTRIB_SRCS += contrib/10.regexp/src/regexp.c
CONTRIB_INITS += regexp
CONTRIB_TESTS += test-regexp

test-regexp: bin/picrin
	for test in `ls contrib/10.regexp/t/*.scm`; do \
	  bin/picrin $$test; \
	done
