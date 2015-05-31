random:
	echo random >> $(CONTRIB_INITS_LIST)
	echo contrib/10.random/src/random.c >> $(CONTRIB_SRCS_LIST)
	echo contrib/10.random/src/mt19937ar.c >> $(CONTRIB_SRCS_LIST)
	echo test-random >> $(CONTRIB_TESTS_LIST)

test-random: bin/picrin
	for test in `ls contrib/10.random/t/*.scm`; do \
	  bin/picrin $$test; \
	done
