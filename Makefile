all: build run

build:
	gcc -o bin/picrin src/main.c

run:
	bin/picrin
