CC=gcc

all: build run

build: build-lib build-main

build-main:
	$(CC) -Wall -g tools/main.c -o bin/picrin -I./include -L./lib -lpicrin -lreadline

build-lib:
	cd src; \
	yacc -d parse.y; \
	lex scan.l
	 $(CC) -Wall -g -shared src/*.c -o lib/libpicrin.so -I./include -I./extlib

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/libpicrin.so
	rm -f bin/picrin

run:
	bin/picrin
