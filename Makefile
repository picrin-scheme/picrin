CC=gcc
CFLAGS=-Wall -O3
UNAME_S := $(shell uname -s)
SO_PREFIX=lib
SO_EXT=.so

ifeq ($(findstring CYGWIN,$(UNAME_S)), CYGWIN)
  SO_PREFIX=cyg
  SO_EXT=.dll
endif

PICRIN_LIB=$(SO_PREFIX)picrin$(SO_EXT)

all: build-debug run

release: CFLAGS += -DDEBUG=0
release: build

build-debug: CFLAGS += -g -DDEBUG=1
build-debug: build

build: build-lib build-main

build-main:
	$(CC) $(CFLAGS) tools/main.c -o bin/picrin -I./include -L./lib -lpicrin -lreadline
	cp lib/$(PICRIN_LIB) bin/

build-lib:
	cd src; \
	  yacc -d parse.y; \
	  lex scan.l
	$(CC) $(CFLAGS) -shared src/*.c -o lib/$(PICRIN_LIB) -I./include -I./extlib

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/$(PICRIN_LIB)
	rm -f bin/picrin

run:
	bin/picrin

tak: build
	bin/picrin etc/tak.scm
