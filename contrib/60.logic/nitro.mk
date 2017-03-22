CONTRIB_LIBS += $(wildcard contrib/60.logic/*.scm)
CONTRIB_TESTS += test-logic

test-logic: $(TEST_RUNNER)
	for test in `ls contrib/60.logic/t/*.scm`; do \
	  ./$(TEST_RUNNER) "$$test"; \
	done
