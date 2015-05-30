CONTRIB_SRCS += contrib/10.readline/src/readline.c
CONTRIB_INITS += readline

LDFLAGS += `pkg-config libedit --libs`

contrib/src/readline.o: contrib/src/readline.c
	$(CC) $(CFLAGS) -o $@ $< `pkg-config libedit --cflags`
