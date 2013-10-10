all: build run

build:
	gcc -o bin/picrin -I./include src/main.c src/state.c

clean:
	rm bin/picrin

run:
	bin/picrin
