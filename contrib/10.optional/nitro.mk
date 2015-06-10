CONTRIB_LIBS += $(wildcard contrib/10.optional/piclib/*.scm)
CONTRIB_TESTS += test-optional

test-optional: bin/picrin
	for test in `ls contrib/10.optional/t/*.scm`; do \
	  bin/picrin $$test; \
	done
