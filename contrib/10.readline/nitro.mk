libedit_exists := $(shell pkg-config libedit --exists; echo $$?)

ifeq ($(libedit_exists),0)
  LDFLAGS += `pkg-config libedit --libs`
endif


readline:
ifeq ($(libedit_exists),0)
	echo contrib/10.readline/src/readline.c >> $(CONTRIB_SRCS_LIST)
	echo readline >> $(CONTRIB_INITS_LIST)
	echo test-readline >> $(CONTRIB_TESTS_LIST)
endif

contrib/src/readline.o: contrib/src/readline.c
	$(CC) $(CFLAGS) -o $@ $< `pkg-config libedit --cflags`

test-readline: bin/picrin
	for test in `ls contrib/10.readline/t/*.scm`; do \
	  bin/picrin $$test; \
	done
