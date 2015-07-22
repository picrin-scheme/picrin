CONTRIB_LIBS += $(wildcard contrib/30.optional/piclib/*.scm)
CONTRIB_TESTS += test-optional

test-optional: bin/picrin
	for test in `ls contrib/30.optional/t/*.scm`; do \
	  $(TEST_RUNNER) $$test; \
	done
