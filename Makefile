BENZ_SRCS = $(wildcard extlib/benz/*.c)
BENZ_OBJS = $(BENZ_SRCS:.c=.o)

PICRIN_SRCS = \
	src/main.c\
	src/load_piclib.c\
	src/init_contrib.c
PICRIN_OBJS = \
	$(PICRIN_SRCS:.c=.o)
PICRIN_LIBS = \
	piclib/picrin/base.scm\
	piclib/picrin/macro.scm\
	piclib/picrin/experimental/lambda.scm\
	piclib/picrin/syntax-rules.scm\
	piclib/picrin/test.scm

CONTRIB_SRCS =
CONTRIB_OBJS = $(CONTRIB_SRCS:.c=.o)
CONTRIB_LIBS =
CONTRIB_INITS =
CONTRIB_TESTS =
CONTRIB_DOCS = $(wildcard contrib/*/docs/*.rst)

CFLAGS += -I./extlib/benz/include -Wall -Wextra
LDFLAGS += -lm

prefix = /usr/local

all: CFLAGS += -O2 -DNDEBUG=1
all: bin/picrin

include $(sort $(wildcard contrib/*/nitro.mk))

debug: CFLAGS += -O0 -g
debug: bin/picrin

bin/picrin: $(PICRIN_OBJS) $(CONTRIB_OBJS) lib/libbenz.a
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(CONTRIB_OBJS) lib/libbenz.a $(LDFLAGS)

src/load_piclib.c: $(PICRIN_LIBS) $(CONTRIB_LIBS)
	perl etc/mkloader.pl $(PICRIN_LIBS) $(CONTRIB_LIBS) > $@

src/init_contrib.c:
	perl etc/mkinit.pl $(CONTRIB_INITS) > $@

lib/libbenz.a: $(BENZ_OBJS)
	$(AR) $(ARFLAGS) $@ $(BENZ_OBJS)

extlib/benz/boot.o: extlib/benz/boot.c
	cd extlib/benz; perl boot.c
	$(CC) $(CFLAGS) -c -o $@ $<

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

test: test-contribs test-nostdlib

test-contribs: bin/picrin $(CONTRIB_TESTS)

test-nostdlib:
	$(CC) -I extlib/benz/include -D'PIC_ENABLE_LIBC=0' -D'PIC_ENABLE_FLOAT=0' -D'PIC_ENABLE_STDIO=0' -nostdlib -fPIC -shared -std=c89 -ansi -pedantic -Wall -Wextra -o lib/libbenz.so $(BENZ_SRCS) etc/libc_polyfill.c -fno-stack-protector
	rm -f lib/libbenz.so

install: all
	install -c bin/picrin $(prefix)/bin/picrin

clean:
	rm -f src/load_piclib.c src/init_contrib.c
	rm -f lib/libbenz.a
	rm -f $(BENZ_OBJS)
	rm -f $(PICRIN_OBJS)
	rm -f $(CONTRIB_OBJS)

.PHONY: all install clean run test test-r7rs test-contribs doc $(CONTRIB_TESTS)
