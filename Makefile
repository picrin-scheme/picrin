all: build run

build:
	gcc -o bin/picrin -I./include src/main.c src/state.c src/gc.c src/pair.c src/write.c src/symbol.c src/value.c

clean:
	rm bin/picrin

run:
	bin/picrin
