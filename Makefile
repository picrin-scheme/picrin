PICRIN_SRCS = \
	src/main.c\
	src/init_lib.c\
	src/lib.c\
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

CFLAGS += -I./lib/include -I./include -Wall -Wextra
LDFLAGS += -lm

prefix ?= /usr/local

all: CFLAGS += -O2 -g -DNDEBUG=1
all: bootstrap picrin

debug: CFLAGS += -O0 -g
debug: bootstrap picrin

include $(sort $(wildcard contrib/*/nitro.mk))

bootstrap: bin/picrin-bootstrap

bin/picrin-bootstrap:
	test -f bin/picrin-bootstrap || { $(MAKE) lib/mini-picrin && mv lib/mini-picrin bin/picrin-bootstrap; }

lib/mini-picrin: FORCE
	$(MAKE) -C lib mini-picrin

lib/libpicrin.a: FORCE
	$(MAKE) -C lib libpicrin.a

ext: lib/ext/eval.c

lib/ext/eval.c: piclib/eval.scm
	bin/picrin-bootstrap -c piclib/eval.scm | bin/picrin-bootstrap tools/mkeval.scm > lib/ext/eval.c

picrin: $(PICRIN_OBJS) $(CONTRIB_OBJS) ext lib/libpicrin.a
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(CONTRIB_OBJS) lib/libpicrin.a $(LDFLAGS)

src/init_lib.c: piclib/library.scm
	cat piclib/library.scm | bin/picrin-bootstrap tools/mklib.scm > src/init_lib.c

src/load_piclib.c: $(CONTRIB_LIBS)
	perl tools/mkloader.pl $(CONTRIB_LIBS) > $@

src/init_contrib.c:
	perl tools/mkinit.pl $(CONTRIB_INITS) > $@

$(PICRIN_OBJS) $(CONTRIB_OBJS): lib/include/*.h lib/include/picrin/*.h lib/*.h include/picrin/*.h

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

test-nostdlib: ext
	$(CC) -I./lib/include -Os -DPIC_USE_LIBC=0 -DPIC_USE_CALLCC=0 -DPIC_USE_FILE=0 -DPIC_USE_READ=0 -DPIC_USE_WRITE=0 -DPIC_USE_EVAL=0 -nostdlib -ffreestanding -fno-stack-protector -shared -pedantic -std=c89 -Wall -Wextra -Werror -o libpicrin-tiny.so $(wildcard lib/*.c)
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
	$(MAKE) -C lib clean
	$(RM) picrin
	$(RM) src/load_piclib.c src/init_contrib.c src/init_lib.c
	$(RM) libpicrin-tiny.so
	$(RM) $(PICRIN_OBJS)
	$(RM) $(CONTRIB_OBJS)

FORCE:

.PHONY: all bootstrap ext install clean push test test-r7rs test-contribs test-issue test-picrin-issue test-repl-issue doc $(CONTRIB_TESTS) $(REPL_ISSUE_TESTS)
