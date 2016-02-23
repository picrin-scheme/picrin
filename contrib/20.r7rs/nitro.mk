CONTRIB_INITS += r7rs

CONTRIB_SRCS += \
	contrib/20.r7rs/src/r7rs.c\
	contrib/20.r7rs/src/file.c\
	contrib/20.r7rs/src/load.c\
	contrib/20.r7rs/src/system.c\
	contrib/20.r7rs/src/time.c

CONTRIB_LIBS += \
	contrib/20.r7rs/scheme/base.scm\
	contrib/20.r7rs/scheme/cxr.scm\
	contrib/20.r7rs/scheme/read.scm\
	contrib/20.r7rs/scheme/write.scm\
	contrib/20.r7rs/scheme/file.scm\
	contrib/20.r7rs/scheme/case-lambda.scm\
	contrib/20.r7rs/scheme/lazy.scm\
	contrib/20.r7rs/scheme/eval.scm\
	contrib/20.r7rs/scheme/inexact.scm\
	contrib/20.r7rs/scheme/load.scm\
	contrib/20.r7rs/scheme/process-context.scm\
	contrib/20.r7rs/scheme/time.scm\
	contrib/20.r7rs/scheme/r5rs.scm

CONTRIB_TESTS += test-r7rs

test-r7rs: bin/picrin
	for test in `ls contrib/20.r7rs/t/*.scm`; do \
	  $(TEST_RUNNER) "$$test"; \
	done
