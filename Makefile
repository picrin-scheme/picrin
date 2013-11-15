CC=gcc
CFLAGS=-Wall -O3
PICRIN_LIB=libpicrin.so

ifeq ($(findstring CYGWIN,$(shell uname -s)), CYGWIN)
  PICRIN_LIB =cygpicrin.dll
endif

all: debug-build run

release: CFLAGS += -DDEBUG=0
release: build

debug-build: CFLAGS += -g -DDEBUG=1
debug-build: build

build: build-lib build-main

build-main:
	$(CC) $(CFLAGS) tools/main.c -o bin/picrin -I./include -L./lib -lpicrin -lreadline
	cp lib/$(PICRIN_LIB) bin/

build-lib:
	cd src; \
	  yacc -d parse.y; \
	  lex scan.l
	$(CC) $(CFLAGS) -shared src/*.c -o lib/$(PICRIN_LIB) -I./include -I./extlib -lm

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/$(PICRIN_LIB) bin/$(PICRIN_LIB)
	rm -f bin/picrin

run:
	bin/picrin

tak: build
	bin/picrin etc/tak.scm
