LIBPICRIN_SRCS = \
	attr.c\
	blob.c\
	bool.c\
	char.c\
	data.c\
	dict.c\
	error.c\
	gc.c\
	number.c\
	pair.c\
	port.c\
	proc.c\
	record.c\
	serialize.c\
	state.c\
	string.c\
	symbol.c\
	var.c\
	vector.c\
	ext/cont.c\
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

override CFLAGS += -I./include -Wall -Wextra -g

mini-picrin: ext/main.o libpicrin.a
	$(CC) $(CFLAGS) -o $@ ext/main.o libpicrin.a

libpicrin.a: $(LIBPICRIN_OBJS)
	$(AR) $(ARFLAGS) $@ $(LIBPICRIN_OBJS)

$(LIBPICRIN_OBJS): $(LIBPICRIN_HEADERS)

clean:
	$(RM) $(LIBPICRIN_OBJS) ext/main.o mini-picrin libpicrin.a

.PHONY: clean
