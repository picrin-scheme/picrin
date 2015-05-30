libedit_exists := $(shell pkg-config libedit --exists; echo $$?)

ifeq ($(libedit_exists),0)
  CONTRIB_SRCS += contrib/10.readline/src/readline.c
  CONTRIB_INITS += readline
  LDFLAGS += `pkg-config libedit --libs`
endif

contrib/src/readline.o: contrib/src/readline.c
	$(CC) $(CFLAGS) -o $@ $< `pkg-config libedit --cflags`
