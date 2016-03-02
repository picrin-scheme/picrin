BENZ_SRCS = $(wildcard extlib/benz/*.c)
BENZ_OBJS = $(BENZ_SRCS:.c=.o)

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

TEST_RUNNER = bin/picrin

CFLAGS += -I./extlib/benz/include -Wall -Wextra
LDFLAGS += -lm

prefix ?= /usr/local

all: CFLAGS += -O2 -flto -DNDEBUG=1
all: bin/picrin

debug: CFLAGS += -O0 -g
debug: bin/picrin

include $(sort $(wildcard contrib/*/nitro.mk))

bin/picrin: CFLAGS += $(CONTRIB_DEFS)
bin/picrin: $(PICRIN_OBJS) $(CONTRIB_OBJS) $(BENZ_OBJS)
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(CONTRIB_OBJS) $(BENZ_OBJS) $(LDFLAGS)

src/load_piclib.c: $(CONTRIB_LIBS)
	perl etc/mkloader.pl $(CONTRIB_LIBS) > $@

src/init_contrib.c:
	perl etc/mkinit.pl $(CONTRIB_INITS) > $@

lib/libbenz.so: $(BENZ_OBJS)
	$(CC) -shared $(CFLAGS) -o $@ $(BENZ_OBJS) $(LDFLAGS)

extlib/benz/boot.c: tools/built-in.scm
	$(MAKE) -C tools

$(BENZ_OBJS) $(PICRIN_OBJS) $(CONTRIB_OBJS): extlib/benz/include/picrin.h extlib/benz/include/picrin/*.h

doc: docs/*.rst docs/contrib.rst
	$(MAKE) -C docs html
	mkdir -p doc
	cp -uR docs/_build/* -t doc/

docs/contrib.rst: $(CONTRIB_DOCS)
	echo "Contrib Libraries \\\(a.k.a nitros\\\)" > $@
	echo "================================" >> $@
	echo "" >> $@
	cat $(CONTRIB_DOCS) >> $@

run: bin/picrin
	bin/picrin

test: test-contribs test-nostdlib test-issue

test-contribs: bin/picrin $(CONTRIB_TESTS)

test-nostdlib:
	$(CC) -I extlib/benz/include -D'PIC_USE_LIBC=0' -D'PIC_USE_STDIO=0' -D'PIC_USE_WRITE=0' -ffreestanding -nostdlib -Os -fPIC -shared -std=c89 -pedantic -Wall -Wextra -Werror -o lib/libbenz-tiny.so $(BENZ_SRCS) etc/libc_polyfill.c -fno-stack-protector
	strip lib/libbenz-tiny.so
	ls -lh lib/libbenz-tiny.so
	rm -f lib/libbenz-tiny.so

test-issue: test-picrin-issue test-repl-issue

test-picrin-issue: $(TEST_RUNNER) $(PICRIN_ISSUE_TESTS)
	for test in $(PICRIN_ISSUE_TESTS); do \
	  $(TEST_RUNNER) "$$test"; \
	done

test-repl-issue: $(REPL_ISSUE_TESTS)

$(REPL_ISSUE_TESTS):
	PICRIN=$(TEST_RUNNER) ./$@

install: all
	install -c bin/picrin $(prefix)/bin/picrin

clean:
	rm -f src/load_piclib.c src/init_contrib.c
	rm -f lib/libbenz.so
	rm -f $(BENZ_OBJS)
	rm -f $(PICRIN_OBJS)
	rm -f $(CONTRIB_OBJS)
	$(MAKE) -C tools clean

.PHONY: all install clean run test test-r7rs test-contribs test-issue test-picrin-issue test-repl-issue doc $(CONTRIB_TESTS) $(REPL_ISSUE_TESTS)
