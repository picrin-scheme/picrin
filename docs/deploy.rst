Installation
============

Installation instructions below.


Build and Install
-----------------

- make `Makefile`

Change directory to `build` then run `ccmake` to create Makefile. Once `Makefile` is generated you can run `make` command to build picrin::

  $ cd build

  $ ccmake ..

Actually you don't necessarily need to move to `build` directory before running `ccmake` (in that case `$ ccmake .`), but I strongly recommend to follow above instruction.

- build

A built executable binary will be under bin/ directory and shared libraries under lib/::

  $ make

If you are building picrin on other systems than x86_64, PIC_NAN_BOXING flag is automatically turned on (see include/picrin/config.h for detail).

- install

Just running `make install`, picrin library, headers, and runtime binary are install on your system, by default into `/usr/local` directory. You can change this value via ccmake::

  $ make install

- run

Before installing picrin, you can try picrin without breaking any of your system. Simply directly run the binary `bin/picrin` from terminal, or you can use `make` to execute it like this::

  $ make run

- debug run

If you execute `cmake` with debug flag `-DCMAKE_BUILD_TYPE=Debug`, it builds the binary with all debug flags enabled (PIC_GC_STRESS, VM_DEBUG, DEBUG)::

  $ cmake -DCMAKE_BUILD_TYPE=Debug ..


Requirement
-----------

Picrin scheme depends on some external libraries to build the binary:

- perl
- lex (preferably, flex)
- getopt
- readline (optional)
- regex.h of POSIX.1 (optional)

Optional libraries are, if cmake detected them, automatically enabled.
The compilation is tested only on Mac OSX and Ubuntu. I think (or hope) it'll be ok to compile and run on other operating systems such as Arch or Windows, but I don't guarantee :(
