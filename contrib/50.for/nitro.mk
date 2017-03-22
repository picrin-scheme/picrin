CONTRIB_LIBS += $(wildcard contrib/50.for/piclib/*.scm)
CONTRIB_TESTS += test-for

test-for: $(TEST_RUNNER)
	for test in `ls contrib/50.for/t/*.scm`; do \
	  ./$(TEST_RUNNER) "$$test"; \
	done
