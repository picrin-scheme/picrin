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

CFLAGS += -I./include -Wall -Wextra -g

mini-picrin: $(LIBPICRIN_OBJS) ext/main.o
	$(CC) $(CFLAGS) -o $@ ext/main.o $(LIBPICRIN_OBJS)

libpicrin.so: CFLAGS += -fPIC
libpicrin.so: $(LIBPICRIN_OBJS)
	$(CC) $(CFLAGS) -shared -o $@ $(LIBPICRIN_OBJS) $(LDFLAGS)

libpicrin.so.minimal: $(LIBPICRIN_SRCS)
	$(CC) -I./include -Os -DPIC_USE_LIBC=0 -DPIC_USE_CALLCC=0 -DPIC_USE_FILE=0 -DPIC_USE_READ=0 -DPIC_USE_WRITE=0 -DPIC_USE_EVAL=0 -nostdlib -ffreestanding -fno-stack-protector -shared -o $@ $(LIBPICRIN_SRCS) $(LDFLAGS)
	strip $@

$(LIBPICRIN_OBJS): $(LIBPICRIN_HEADERS)

clean:
	$(RM) $(LIBPICRIN_OBJS) ext/main.o mini-picrin libpicrin.so libpicrin.so.minimal

.PHONY: clean
