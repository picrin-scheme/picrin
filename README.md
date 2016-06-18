<img width="500" src="https://raw.githubusercontent.com/picrin-scheme/picrin/master/etc/picrin-logo-fin01-02.png"></img>

[![Build Status](https://travis-ci.org/picrin-scheme/picrin.png?branch=master)](https://travis-ci.org/picrin-scheme/picrin)
[![Docs Status](https://readthedocs.org/projects/picrin/badge/?version=latest)](https://picrin.readthedocs.org/)

Picrin is a lightweight R7RS scheme implementation written in pure C89. It contains a reasonably fast VM, an improved hygienic macro system, useful contribution libraries, and simple but powerful C interface.

- R7RS compatible
- Reentrant design (all VM states are stored in single global state object)
- Bytecode interpreter
- Direct threaded VM
- Internal representation by nan-boxing (available only on x64)
- Conservative call/cc implementation (VM stack and native c stack can interleave)
- Exact GC (simple mark and sweep, partially reference count)
- String representation by rope
- Hygienic macro transformers (syntactic closures, explicit and implicit renaming macros)
- Extended library syntax

## Documentation

See http://picrin.readthedocs.org/

## Homepage

Currently picrin is hosted on Github. You can freely send a bug report or pull-request, and fork the repository.

https://github.com/picrin-scheme/picrin

## IRC

Our chat room is at #picrin channel, chat.freenode.org. IRC logs here: https://botbot.me/freenode/picrin/

## Build

Just type `make` in the project root directory. You will find an executable binary newly created at bin/ directory.

    $ make

When you are building picrin on x86_64 system, PIC_NAN_BOXING flag is automatically turned on (see include/picrin/config.h for detail).

## Install

`make install` target is provided. By default it installs picrin binary into `/usr/local/bin/`.

	$ make install

Since picrin does not use autoconf, if you want to specify the install directory, pass the custom path to `make` via command line argument.

	$ make install prefix=/path/to/dir

## Requirement

To build Picrin Scheme from source code, some external libraries are required:

- perl
- regex.h of POSIX.1
- libedit (optional)

Make command automatically turns on optional libraries if available.
Picrin is mainly developed on Mac OS X and only tested on OS X or Ubuntu 14.04+. When you tried to run picrin on other platforms and found something was wrong with it, please send us an issue.

## Authors

See `AUTHORS`
