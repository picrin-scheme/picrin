CONTRIB_LIBS += $(wildcard contrib/20.for/piclib/*.scm)
CONTRIB_TESTS += test-for

test-for: bin/picrin
	for test in `ls contrib/20.for/t/*.scm`; do \
	  bin/picrin "$$test"; \
	done
