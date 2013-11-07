
	This product is developed at the second-grade course, Informatic Science
	Basic Experiment class at the University of Tokyo.

# Picrin - a lightweight scheme interpreter

## Features

- R7RS compatibility (but partial support)
- reentrant design (all VM states are stored in single global state object)
- bytecode interpreter (based on Stack VM technology)
- direct threaded VM
- Internal representation by Nan-Boxing
- exact GC (simple mark and sweep strategy)
- advanced REPL support (multi-line input, etc)
- tiny & portable library (all functions will be in `libpicrin.so`)

## Homepage

Currently picrin is hosted on Github. You can freely send a bug report or pull-request, and fork the repository.

https://github.com/wasabiz/picrin

## How to use it

- build

		$ make build

	built executable binary will be under bin/ directory and shared library `libpicrin.so` under lib/.

- run

	Simply directly run the binary `bin/picrin` from terminal, or you can use `make` to execute it like this.

		$ make run
	
In the default option, when `make` command is called without arguments, it builds the binary and right after that dropped into the picrin interactive shell (REPL).

- install

	As of now picrin does not provide a command automatically installs the binary. If you want to place picrin library and binary in a parmanent directory, please do it by hand.

## Requirement

picrin scheme depends on some external libraries to build the binary:

- bison
- yacc
- make
- gcc
- readline

The compilation is tested only on Mac OSX. I think (or hope) it'll be ok to compile and run on other operating systems such as Linux or Windows, but there's no guarantee :(

## Authors

Yuichi Nishiwaki (yuichi.nishiwaki at gmail.com)
