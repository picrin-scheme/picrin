ROOT_DIR := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))

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
	piclib/picrin/record.scm\
	piclib/picrin/array.scm\
	piclib/picrin/dictionary.scm\
	piclib/picrin/experimental/lambda.scm\
	piclib/picrin/syntax-rules.scm\
	piclib/picrin/test.scm

CONTRIB_SRCS_LIST := $(ROOT_DIR)/contrib/srcs.txt
CONTRIB_LIBS_LIST := $(ROOT_DIR)/contrib/libs.txt
CONTRIB_INITS_LIST := $(ROOT_DIR)/contrib/inits.txt
CONTRIB_TESTS_LIST := $(ROOT_DIR)/contrib/tests.txt
CONTRIB_DOCS = $(wildcard contrib/*/docs/*.rst)

CFLAGS += -I./extlib/benz/include -Wall -Wextra
LDFLAGS += -lm

prefix = /usr/local

all: CFLAGS += -O0 -ggdb
all: bin/picrin

include contrib/Makefile

debug: CFLAGS += -O0 -g -DDEBUG=1
debug: bin/picrin

bin/picrin: $(PICRIN_OBJS) $(shell sed 's/.c$$/.o/g' $(CONTRIB_SRCS_LIST)) lib/libbenz.a
	$(CC) $(CFLAGS) -o $@ $(PICRIN_OBJS) $(shell sed 's/.c$$/.o/g' $(CONTRIB_SRCS_LIST)) lib/libbenz.a $(LDFLAGS)

src/load_piclib.c: $(PICRIN_LIBS) $(CONTRIB_LIBS_LIST)
	perl etc/mkloader.pl $(PICRIN_LIBS) $(shell cat $(CONTRIB_LIBS_LIST)) > $@

src/init_contrib.c: $(CONTRIB_INITS_LIST)
	perl etc/mkinit.pl $(shell cat $(CONTRIB_INITS_LIST)) > $@

lib/libbenz.a: $(BENZ_OBJS)
	$(AR) $(ARFLAGS) $@ $(BENZ_OBJS)

$(PICRIN_OBJS) $(shell sed 's/.c$$/.o/g' $(CONTRIB_SRCS_LIST)): extlib/benz/include/picrin.h extlib/benz/include/picrin/*.h

$(CONTRIB_SRCS_LIST) $(CONTRIB_LIBS_LIST) $(CONTRIB_INITS_LIST) $(CONTRIB_TESTS_LIST): contrib

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

test: test-r7rs test-contribs test-nostdlib

test-r7rs: bin/picrin t/r7rs-tests.scm
	bin/picrin t/r7rs-tests.scm

test-contribs: bin/picrin 

test-nostdlib:
	$(CC) -I extlib/benz/include -D'PIC_ENABLE_LIBC=0' -D'PIC_ENABLE_FLOAT=0'-nostdlib -fPIC -shared -std=c89 -ansi -pedantic -Wall -Wextra -o lib/libbenz.so $(BENZ_SRCS)
	rm -f lib/libbenz.so

install: all
	install -c bin/picrin $(prefix)/bin/picrin

clean:
	rm -f src/load_piclib.c src/init_contrib.c
	rm -f lib/libbenz.a
	rm -f $(BENZ_OBJS)
	rm -f $(PICRIN_OBJS)
	rm -f $(shell sed 's/.c$$/.o/g' $(CONTRIB_SRCS_LIST))

.PHONY: all insall clean run test test-r7rs test-contribs doc $(shell cat $(CONTRIB_TESTS_LIST))
