optional: r7rs
	echo $(wildcard contrib/10.optional/piclib/*.scm) >> $(CONTRIB_LIBS_LIST)
	echo test-optional >> $(CONTRIB_TESTS_LIST)

test-optional: bin/picrin
	for test in `ls contrib/10.optional/t/*.scm`; do \
	  bin/picrin $$test; \
	done
