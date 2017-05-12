LIBPICRIN_SRCS = \
	attr.c\
	blob.c\
	bool.c\
	char.c\
	data.c\
	dict.c\
	gc.c\
	number.c\
	pair.c\
	proc.c\
	record.c\
	serialize.c\
	state.c\
	string.c\
	symbol.c\
	value.c\
	var.c\
	vector.c\
	ext/cont.c\
	ext/eval.c\
	ext/port.c\
	ext/read.c\
	ext/write.c\
	ext/file.c\
	ext/error.c
LIBPICRIN_OBJS = \
	$(LIBPICRIN_SRCS:.c=.o)

LIBPICRIN_HEADERS = \
	include/picrin.h\
	include/picconf.h\
	include/picrin/extra.h\
	include/picrin/setup.h\
	khash.h\
	object.h\
	value.h\
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
