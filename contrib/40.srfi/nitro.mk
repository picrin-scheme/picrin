CONTRIB_INITS += socket
CONTRIB_LIBS += \
	contrib/40.srfi/srfi/1.scm\
	contrib/40.srfi/srfi/8.scm\
	contrib/40.srfi/srfi/17.scm\
	contrib/40.srfi/srfi/26.scm\
	contrib/40.srfi/srfi/43.scm\
	contrib/40.srfi/srfi/60.scm\
	contrib/40.srfi/srfi/95.scm\
	contrib/40.srfi/srfi/106.scm\
	contrib/40.srfi/srfi/111.scm
CONTRIB_SRCS += contrib/40.srfi/src/106.c
CONTRIB_TESTS += test-srfi

test-srfi: bin/picrin
	for test in `ls contrib/40.srfi/t/*.scm`; do \
	  bin/picrin "$$test"; \
	done
