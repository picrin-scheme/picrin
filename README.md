
	This product is developed at the second-grade course, Informatic Science
	Basic Experiment class at the University of Tokyo.

# Picrin - a lightweight scheme interpreter

## Features

- R7RS compatibility (but partial support)
- reentrant design (all VM states are stored in single global state object)
- bytecode interpreter (based on Stack VM technology)
- direct threaded VM
- Internal representation by Nan-Boxing
- conservative call/cc implementation (users can freely interleave native stack with VM stack)
- exact GC (simple mark and sweep strategy)
- advanced REPL support (multi-line input, etc)
- tiny & portable library (all functions will be in `libpicrin.so`)

## Supported Range

| section                     | status     | comments |
|-|-|-|
| 2.2 Whitespace and comments | incomplete | block comments are not implemented |
| 2.3 Other notations         | incomplete | #e #i #b #o #d #x |
| 2.4 Datum labels            | no         | unsupported |
| 3.1 Variables, syntactic keywords, and regions | | |
| 3.2 Disjointness of types   | yes | |
| 3.3 External representations | | |
| 3.4 Storage model | yes | |
| 3.5 Proper tail recursion | incomplete | apply, call/cc, call/values, eval are not yet |
| 4.1.1 Variable references | yes | |
| 4.1.2 Literal expressions | yes | `(quote <datum> )` |
| 4.1.3 Procedure calls | yes | In picrin `()` is self-evaluating |
| 4.1.4 Procedures | yes | `(lambda <fomals> <body>)` |
| 4.1.5 Conditionals | yes | `(if <test> <consequent> [ <alternate> ])`. In picrin `(if #f #f)` returns `#f` |
| 4.1.6 Assignments | yes | `(set! <var> <expr>)` |
| 4.1.7 Inclusion | no | `include` and `include-ci` |
| 4.2.1 Conditionals | incomplete | TODO: `cond-expand` |
| 4.2.2 Binding constructs | incomplete | TODO: `let-values`, `let*-values` |
| 4.2.3 Sequencing | yes | |
| 4.2.4 Iteration | no | |
| 4.2.5 Delayed evaluation | N/A | |
| 4.2.6 Dynamic bindings | no | TODO: `make-parameter`, `parameterize` in C level |
| 4.2.7 Exception handling | no | `guard` syntax. |
| 4.2.8 Quasiquotation | incomplete | nested is unsupported |
| 4.2.9  Case-lambda | N/A | |
| 4.3.1 Bindings constructs for syntactic keywords | no | Instead, picrin provides so-called legacy macro facility (`define-macro`). |
| 4.3.2 Pattern language | no | see above. |
| 4.3.3 Signaling errors in macro transformers | no | see above. |
| 5.1 Programs | yes | |
| 5.2 Import declarations | no | |
| 5.3.1 Top level definitions | yes | |
| 5.3.2 Internal definitions | no | |
| 5.3.3 Multiple-value definitions | no | |
| 5.4 Syntax definitions | no | see notes on section 4.3.1. |
| 5.5 Recored-type definitions | no | |
| 5.6.1 Library Syntax | no | |
| 5.6.2 Library example | N/A | |
| 5.7 The REPL | yes | |


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

- debug-run

	By default make command runs REPL with all debug flags enabled.

		$ make
	
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
