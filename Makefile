CC=gcc
CFLAGS=-Wall -O3

all: build-debug run

release: build

build-debug: CFLAGS += -g -DDEBUG=1
build-debug: build

build: build-lib build-main

build-main:
	$(CC) $(CFLAGS) tools/main.c -o bin/picrin -I./include -L./lib -lpicrin -lreadline

build-lib:
	cd src; \
	  yacc -d parse.y; \
	  lex scan.l
	$(CC) $(CFLAGS) -shared src/*.c -o lib/libpicrin.so -I./include -I./extlib -lm

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/libpicrin.so
	rm -f bin/picrin

run:
	bin/picrin

tak: build
	bin/picrin etc/tak.scm
