LIBPICRIN_SRCS = \
	blob.c\
	bool.c\
	char.c\
	cont.c\
	data.c\
	debug.c\
	dict.c\
	error.c\
	gc.c\
	load.c\
	number.c\
	pair.c\
	port.c\
	proc.c\
	record.c\
	state.c\
	string.c\
	symbol.c\
	var.c\
	vector.c\
	weak.c\
	ext/eval.c\
	ext/read.c\
	ext/write.c\
	ext/file.c
LIBPICRIN_OBJS = \
	$(LIBPICRIN_SRCS:.c=.o)

LIBPICRIN_HEADERS = \
	include/picrin.h\
	include/picconf.h\
	include/picrin/extra.h\
	include/picrin/setup.h\
	include/picrin/value.h\
	khash.h\
	object.h\
	state.h

CFLAGS += -I./include -Wall -Wextra -O0 -g

mini-picrin: libpicrin.so ext/main.o
	$(CC) $(CFLAGS) -o $@ libpicrin.so ext/main.o

libpicrin.so: $(LIBPICRIN_OBJS)
	$(CC) $(CFLAGS) -shared -o $@ $(LIBPICRIN_OBJS) $(LDFLAGS)

$(LIBPICRIN_OBJS): $(LIBPICRIN_HEADERS)

clean:
	$(RM) $(LIBPICRIN_OBJS) libpicrin.so

.PHONY: clean
