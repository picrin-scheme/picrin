CONTRIB_LIBS += contrib/60.peg/picrin/parser.scm contrib/60.peg/picrin/parser/string.scm

CONTRIB_TESTS += test-peg

test-peg: bin/picrin
	for test in `ls contrib/60.peg/t/*.scm`; do \
	  $(TEST_RUNNER) "$$test"; \
	done
