LIBPICRIN_SRCS = \
	lib/blob.c\
	lib/bool.c\
	lib/char.c\
	lib/cont.c\
	lib/data.c\
	lib/debug.c\
	lib/dict.c\
	lib/error.c\
	lib/gc.c\
	lib/number.c\
	lib/pair.c\
	lib/port.c\
	lib/proc.c\
	lib/record.c\
	lib/state.c\
	lib/string.c\
	lib/symbol.c\
	lib/var.c\
	lib/vector.c\
	lib/weak.c\
	lib/ext/boot.c\
	lib/ext/lib.c\
	lib/ext/file.c\
	lib/ext/load.c\
	lib/ext/read.c\
	lib/ext/write.c
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
CONTRIB_INITS =
CONTRIB_TESTS =
CONTRIB_DOCS = $(wildcard contrib/*/docs/*.rst)
PICRIN_ISSUE_TESTS = $(wildcard t/issue/*.scm)
REPL_ISSUE_TESTS = $(wildcard t/issue/*.sh)

TEST_RUNNER = picrin

CFLAGS += -I./lib/include -Wall -Wextra
LDFLAGS += -lm

prefix ?= /usr/local

all: CFLAGS += -O2 -g -DNDEBUG=1
all: picrin

debug: CFLAGS += -O0 -g
debug: picrin

tiny-picrin: CFLAGS += -O0 -g -DPIC_USE_LIBRARY=0
tiny-picrin: $(LIBPICRIN_OBJS) src/tiny-main.o
	$(CC) $(CFLAGS) -o $@ $(LIBPICRIN_OBJS) src/tiny-main.o $(LDFLAGS)

include $(sort $(wildcard contrib/*/nitro.mk))

picrin: $(PICRIN_OBJS) $(CONTRIB_OBJS) $(LIBPICRIN_OBJS)
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(CONTRIB_OBJS) $(LIBPICRIN_OBJS) $(LDFLAGS)

src/load_piclib.c: $(CONTRIB_LIBS)
	perl tools/mkloader.pl $(CONTRIB_LIBS) > $@

src/init_contrib.c:
	perl tools/mkinit.pl $(CONTRIB_INITS) > $@

# FIXME: Undefined symbols error for _emyg_atod and _emyg_dtoa
# libpicrin.so: $(LIBPICRIN_OBJS)
# 	$(CC) -shared $(CFLAGS) -o $@ $(LIBPICRIN_OBJS) $(LDFLAGS)

lib/ext/boot.c: piclib/compile.scm piclib/library.scm
	cat piclib/compile.scm piclib/library.scm | bin/picrin-bootstrap tools/mkboot.scm > lib/ext/boot.c

$(LIBPICRIN_OBJS) $(PICRIN_OBJS) $(CONTRIB_OBJS): lib/include/picrin.h lib/include/picrin/*.h lib/khash.h lib/object.h lib/state.h lib/vm.h

doc: docs/*.rst docs/contrib.rst
	$(MAKE) -C docs html
	mkdir -p doc
	cp -uR docs/_build/* -t doc/

docs/contrib.rst: $(CONTRIB_DOCS)
	echo "Contrib Libraries \\\(a.k.a nitros\\\)" > $@
	echo "================================" >> $@
	echo "" >> $@
	cat $(CONTRIB_DOCS) >> $@

test: test-contribs test-nostdlib test-issue

test-contribs: picrin $(CONTRIB_TESTS)

test-nostdlib: lib/ext/boot.c
	$(CC) -I./lib -I./lib/include -D'PIC_USE_LIBC=0' -D'PIC_USE_STDIO=0' -D'PIC_USE_WRITE=0' -D'PIC_USE_LIBRARY=0' -D'PIC_USE_EVAL=0' -ffreestanding -nostdlib -Os -fPIC -shared -std=c89 -pedantic -Wall -Wextra -Werror -o libpicrin-tiny.so $(LIBPICRIN_SRCS) etc/libc_polyfill.c -fno-stack-protector
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

push:
	git subtree push --prefix=lib git@github.com:picrin-scheme/libpicrin.git master

install: all
	install -c picrin $(prefix)/bin/picrin

clean:
	$(RM) picrin
	$(RM) src/load_piclib.c src/init_contrib.c lib/ext/boot.c
	$(RM) libpicrin.so libpicrin-tiny.so
	$(RM) $(LIBPICRIN_OBJS)
	$(RM) $(PICRIN_OBJS)
	$(RM) $(CONTRIB_OBJS)

.PHONY: all install clean push test test-r7rs test-contribs test-issue test-picrin-issue test-repl-issue doc $(CONTRIB_TESTS) $(REPL_ISSUE_TESTS)
