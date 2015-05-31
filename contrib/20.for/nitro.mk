for: r7rs partcont
	echo $(wildcard contrib/20.for/piclib/*.scm) >> $(CONTRIB_LIBS_LIST)
	echo test-for >> $(CONTRIB_TESTS_LIST)

test-for: bin/picrin
	for test in `ls contrib/20.for/t/*.scm`; do \
	  bin/picrin "$$test"; \
	done
