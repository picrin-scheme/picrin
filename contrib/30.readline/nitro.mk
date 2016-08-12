libedit_exists := $(shell pkg-config libedit --exists; echo $$?)

ifeq ($(libedit_exists),0)
  CONTRIB_SRCS += contrib/30.readline/src/readline.c
  CONTRIB_INITS += readline
  CONTRIB_TESTS += test-readline
  LDFLAGS += `pkg-config libedit --libs`
endif

contrib/30.readline/src/readline.o: contrib/30.readline/src/readline.c
	$(CC) $(CFLAGS) -c -o $@ $< `pkg-config libedit --cflags`

test-readline: bin/picrin
	for test in `ls contrib/30.readline/t/*.scm`; do \
	  $(TEST_RUNNER) $$test; \
	done
