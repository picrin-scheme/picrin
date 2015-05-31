regexp:
	echo contrib/10.regexp/src/regexp.c >> $(CONTRIB_SRCS_LIST)
	echo regexp >> $(CONTRIB_INITS_LIST)
	echo test-regexp >>$(CONTRIB_TESTS_LIST)

test-regexp: bin/picrin
	for test in `ls contrib/10.regexp/t/*.scm`; do \
	  bin/picrin $$test; \
	done
