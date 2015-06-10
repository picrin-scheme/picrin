CONTRIB_INITS += r7rs

CONTRIB_SRCS += \
	contrib/05.r7rs/src/r7rs.c\
	contrib/05.r7rs/src/file.c\
	contrib/05.r7rs/src/load.c\
	contrib/05.r7rs/src/mutable-string.c\
	contrib/05.r7rs/src/system.c\
	contrib/05.r7rs/src/time.c

CONTRIB_LIBS += \
	contrib/05.r7rs/scheme/base.scm\
	contrib/05.r7rs/scheme/cxr.scm\
	contrib/05.r7rs/scheme/read.scm\
	contrib/05.r7rs/scheme/write.scm\
	contrib/05.r7rs/scheme/file.scm\
	contrib/05.r7rs/scheme/case-lambda.scm\
	contrib/05.r7rs/scheme/lazy.scm\
	contrib/05.r7rs/scheme/eval.scm\
	contrib/05.r7rs/scheme/inexact.scm\
	contrib/05.r7rs/scheme/load.scm\
	contrib/05.r7rs/scheme/process-context.scm\
	contrib/05.r7rs/scheme/time.scm\
	contrib/05.r7rs/scheme/r5rs.scm
