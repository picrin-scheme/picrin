# Picrin [![Build Status](https://travis-ci.org/wasabiz/picrin.png)](https://travis-ci.org/wasabiz/picrin)

Picrin is a lightweight scheme implementation intended to comply with full R7RS specification. Its code is written in pure C99 and does not requires any special external libraries installed on the platform.

## Features

- R7RS compatibility (but partial support)
- reentrant design (all VM states are stored in single global state object)
- bytecode interpreter (based on Stack VM)
- direct threaded VM
- Internal representation by Nan-Boxing
- conservative call/cc implementation (users can freely interleave native stack with VM stack)
- exact GC (simple mark and sweep, partially reference count is used as well)
- support full set hygienic macro transformers, including implicit renaming macros
- extended library syntax
- advanced REPL support (multi-line input, etc)
- tiny & portable library (all functions will be in `libpicrin.so`)

## Compliance with R7RS

| section                     | status     | comments |
| --- | --- | --- |
| 2.2 Whitespace and comments | incomplete | block comments are not implemented |
| 2.3 Other notations         | incomplete | #e #i #b #o #d #x |
| 2.4 Datum labels            | no         | unsupported |
| 3.1 Variables, syntactic keywords, and regions | | |
| 3.2 Disjointness of types   | yes | |
| 3.3 External representations | | |
| 3.4 Storage model | yes | |
| 3.5 Proper tail recursion | incomplete | TODO: apply, call/cc |
| 4.1.1 Variable references | yes | |
| 4.1.2 Literal expressions | yes | |
| 4.1.3 Procedure calls | yes | In picrin `()` is self-evaluating |
| 4.1.4 Procedures | yes | |
| 4.1.5 Conditionals | yes | In picrin `(if #f #f)` returns `#f` |
| 4.1.6 Assignments | yes | |
| 4.1.7 Inclusion | no | `include` and `include-ci` |
| 4.2.1 Conditionals | incomplete | TODO: `cond-expand` |
| 4.2.2 Binding constructs | yes | |
| 4.2.3 Sequencing | yes | |
| 4.2.4 Iteration | yes | |
| 4.2.5 Delayed evaluation | N/A | |
| 4.2.6 Dynamic bindings | yes | |
| 4.2.7 Exception handling | no | `guard` syntax. |
| 4.2.8 Quasiquotation | incomplete | nested is unsupported |
| 4.2.9 Case-lambda | N/A | |
| 4.3.1 Bindings constructs for syntactic keywords | incomplete | (*1) |
| 4.3.2 Pattern language | no | `syntax-rules` |
| 4.3.3 Signaling errors in macro transformers | yes | |
| 5.1 Programs | yes | |
| 5.2 Import declarations | incomplete | only simple import declarations, no support for import with renaming. |
| 5.3.1 Top level definitions | yes | |
| 5.3.2 Internal definitions | yes | TODO: interreferential definitions |
| 5.3.3 Multiple-value definitions | yes | |
| 5.4 Syntax definitions | yes | TODO: internal macro definition is not supported. |
| 5.5 Recored-type definitions | no | |
| 5.6.1 Library Syntax | incomplete | In picrin, libraries can be reopend and can be nested. |
| 5.6.2 Library example | N/A | |
| 5.7 The REPL | yes | |
| 6.1 Equivalence predicates | yes | |
| 6.2.1 Numerical types | yes | picrin has only two types of internal representation of numbers: fixnum and double float. It still comforms the R7RS spec. |
| 6.2.2 Exactness | yes | |
| 6.2.3 Implementation restrictions | yes | |
| 6.2.4 Implementation extensions | yes | |
| 6.2.5 Syntax of numerical constants | yes | |
| 6.2.6 Numerical operations | yes | `denominator`, `numerator`, and `rationalize` are not supported for now. Also, picrin does not provide complex library procedures. |
| 6.2.7 Numerical input and output | no | |
| 6.3 Booleans | yes | |
| 6.4 Pairs and lists | yes | |
| 6.5 Symbols | yes | |
| 6.6 Characters | yes | |
| 6.7 Strings | yes | `substring` is not provided |
| 6.8 Vectors | yes | |
| 6.9 Bytevectors | yes | |
| 6.10  Control features | yes | |
| 6.11 Exceptions | yes | TODO: native error handling |
| 6.12 Environments and evaluation | N/A | |
| 6.13 Ports | incomplete | |
| 6.14 System interface | yes | |

1. Picrin provides hygienic macros in addition to so-called legacy macro (`define-macro`), such as syntactic closure, explicit renaming macro, and implicit renaming macro. As of now let-syntax and letrec-syntax are not provided.

## Homepage

Currently picrin is hosted on Github. You can freely send a bug report or pull-request, and fork the repository.

https://github.com/wasabiz/picrin

## How to use it

- build

	A built executable binary will be under bin/ directory and a shared library `libpicrin.so` under lib/.
	
		$ make

	If you want to build picrin on other systems than x86_64, make sure PIC_NAN_BOXING flag is turned off (see include/config.h for detail).

- run

	Simply directly run the binary `bin/picrin` from terminal, or you can use `make` to execute it like this.

		$ make run

- debug-run

	When `make` command is called with an argument `debug`, it builds the binary with all debug flags enabled (PIC_GC_STRESS, VM_DEBUG, DEBUG).

		$ make debug
	
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

See `AUTHORS`
