CONTRIB_LIBS += $(wildcard contrib/50.for/piclib/*.scm)
CONTRIB_TESTS += test-for

test-for: bin/picrin
	for test in `ls contrib/50.for/t/*.scm`; do \
	  $(TEST_RUNNER) "$$test"; \
	done
