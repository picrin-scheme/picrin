
	This product is developed at the second-grade course, Informatic Science Basic Experiment class at the University of Tokyo.

# Picrin - a lightweight scheme interpreter

## Freatures

- R7RS compatibility (but partial support)
- reentrant design (all VM states are stored in single global state object)
- bytecode interpreter (based on Stack VM technology)
- direct threading VM
- exact GC (simple mark and sweep strategy)
- advanced REPL support (multi-line input, etc)

## How to use it

- build

		$ make build

	built executable binary will be under bin/ directory.

- run

	Simply directly run the binary `bin/picrin` from terminal, or you can use `make` to execute it like this.

		$ make run
	
In the default option, when `make` command is called without arguments, it builds the binary and right after that dropped into the picrin interactive shell (REPL).

## Requirement

picrin scheme depends on some external libraries to build the binary:

- bison
- yacc
- make
- gcc
- readline

## Authors

Yuichi Nishiwaki (yuichi.nishiwaki at gmail.com)
