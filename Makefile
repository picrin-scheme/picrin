CC=gcc

all: build run

build: build-lib build-main

build-main:
	$(CC) -Wall -o bin/picrin -I./include -L./lib -lpicrin -lreadline tools/main.c

build-lib:
	cd src; \
	yacc -d parse.y; \
	lex scan.l
	 $(CC) -Wall -shared -o lib/libpicrin.so -I./include -lreadline src/*.c

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/libpicrin.so
	rm -f bin/picrin

run:
	bin/picrin
