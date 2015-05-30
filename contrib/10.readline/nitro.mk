libedit_exists := $(shell pkg-config libedit --exists; echo $$?)

ifeq ($(libedit_exists),0)
  CONTRIB_SRCS += contrib/10.readline/src/readline.c
  CONTRIB_INITS += readline
  CONTRIB_TESTS += test-readline
  LDFLAGS += `pkg-config libedit --libs`
endif

contrib/src/readline.o: contrib/src/readline.c
	$(CC) $(CFLAGS) -o $@ $< `pkg-config libedit --cflags`

test-readline: bin/picrin
	for test in `ls contrib/10.readline/t/*.scm`; do \
	  bin/picrin $$test; \
	done
