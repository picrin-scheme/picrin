LIBPICRIN_SRCS = $(wildcard lib/*.c)
LIBPICRIN_OBJS = $(LIBPICRIN_SRCS:.c=.o)

PICRIN_SRCS = \
	src/main.c\
	src/load_piclib.c\
	src/init_contrib.c
PICRIN_OBJS = \
	$(PICRIN_SRCS:.c=.o)

CONTRIB_SRCS =
CONTRIB_OBJS = $(CONTRIB_SRCS:.c=.o)
CONTRIB_LIBS =
CONTRIB_DEFS =
CONTRIB_INITS =
CONTRIB_TESTS =
CONTRIB_DOCS = $(wildcard contrib/*/docs/*.rst)
PICRIN_ISSUE_TESTS = $(wildcard t/issue/*.scm)
REPL_ISSUE_TESTS = $(wildcard t/issue/*.sh)

TEST_RUNNER = picrin

CFLAGS += -I./lib/include -Wall -Wextra
LDFLAGS += -lm

prefix ?= /usr/local

all: CFLAGS += -O2 -DNDEBUG=1
all: picrin

debug: CFLAGS += -O0 -g
debug: picrin

include $(sort $(wildcard contrib/*/nitro.mk))

picrin: CFLAGS += $(CONTRIB_DEFS)
picrin: $(PICRIN_OBJS) $(CONTRIB_OBJS) $(LIBPICRIN_OBJS)
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(CONTRIB_OBJS) $(LIBPICRIN_OBJS) $(LDFLAGS)

src/load_piclib.c: $(CONTRIB_LIBS)
	perl etc/mkloader.pl $(CONTRIB_LIBS) > $@

src/init_contrib.c:
	perl etc/mkinit.pl $(CONTRIB_INITS) > $@

# FIXME: Undefined symbols error for _emyg_atod and _emyg_dtoa
# libpicrin.so: $(LIBPICRIN_OBJS)
# 	$(CC) -shared $(CFLAGS) -o $@ $(LIBPICRIN_OBJS) $(LDFLAGS)

lib/boot.o: lib/boot.c
	cd lib; perl boot.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(LIBPICRIN_OBJS) $(PICRIN_OBJS) $(CONTRIB_OBJS): lib/include/picrin.h lib/include/picrin/*.h

doc: docs/*.rst docs/contrib.rst
	$(MAKE) -C docs html
	mkdir -p doc
	cp -uR docs/_build/* -t doc/

docs/contrib.rst: $(CONTRIB_DOCS)
	echo "Contrib Libraries \\\(a.k.a nitros\\\)" > $@
	echo "================================" >> $@
	echo "" >> $@
	cat $(CONTRIB_DOCS) >> $@

run: picrin
	./picrin

test: test-contribs test-nostdlib test-issue

test-contribs: picrin $(CONTRIB_TESTS)

test-nostdlib:
	$(CC) -I./lib/include -D'PIC_USE_LIBC=0' -D'PIC_USE_STDIO=0' -D'PIC_USE_WRITE=0' -ffreestanding -nostdlib -Os -fPIC -shared -std=c89 -pedantic -Wall -Wextra -Werror -o libpicrin-tiny.so $(LIBPICRIN_SRCS) etc/libc_polyfill.c -fno-stack-protector
	strip libpicrin-tiny.so
	ls -lh libpicrin-tiny.so
	rm -f libpicrin-tiny.so

test-issue: test-picrin-issue test-repl-issue

test-picrin-issue: $(TEST_RUNNER) $(PICRIN_ISSUE_TESTS)
	for test in $(PICRIN_ISSUE_TESTS); do \
	  ./$(TEST_RUNNER) "$$test"; \
	done

test-repl-issue: $(REPL_ISSUE_TESTS)

$(REPL_ISSUE_TESTS):
	PICRIN=./$(TEST_RUNNER) ./$@

install: all
	install -c picrin $(prefix)/bin/picrin

clean:
	rm -f src/load_piclib.c src/init_contrib.c
	rm -f libpicrin.so libpicrin-tiny.so
	rm -f $(LIBPICRIN_OBJS)
	rm -f $(PICRIN_OBJS)
	rm -f $(CONTRIB_OBJS)

.PHONY: all install clean run test test-r7rs test-contribs test-issue test-picrin-issue test-repl-issue doc $(CONTRIB_TESTS) $(REPL_ISSUE_TESTS)
