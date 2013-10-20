CC=gcc

all: build run

build: build-lib build-main

build-main:
	$(CC) -Wall -o bin/picrin -I./include -L./lib -lpicrin -lreadline etc/main.c

build-lib:
	cd src; \
	yacc -d parse.y; \
	lex scan.l
	 $(CC) -Wall -shared -o lib/libpicrin.so -I./include -lreadline src/state.c src/gc.c src/pair.c src/port.c src/symbol.c src/value.c src/y.tab.c src/lex.yy.c src/bool.c src/vm.c src/init.c src/number.c src/time.c src/codegen.c src/error.c

clean:
	rm -f src/y.tab.c src/y.tab.h src/lex.yy.c
	rm -f lib/libpicrin.so
	rm -f bin/picrin

run:
	bin/picrin
