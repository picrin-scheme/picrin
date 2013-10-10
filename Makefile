all: build run

build:
	gcc -o bin/picrin -I./include src/main.c src/state.c

run:
	bin/picrin
