Installation
============

Installation instructions below.


Build and Install
-----------------

To build picrin, you need some build tools installed on your platform.

- cmake (>= 2.6)
- git

Because of submodule dependencies, it is necessary to get picrin's source code via git clone command. Basically our git dependencies are only due to submodules, so in fact, If you have no git on your machine, it is possible to build it by downloading a tarball from github page as well. But in such case, you are assumed to modify CMakeLists.txt by yourself to get it work completely. We just strongly recommend you to use git-clone.

Generate Makefile
^^^^^^^^^^^^^^^^^

Change directory to `build` then run `ccmake` to create Makefile. Once `Makefile` is generated you can run `make` command to build picrin::

  $ cd build

  $ ccmake ..

Actually you don't necessarily need to move to `build` directory before running `ccmake` (in that case `$ ccmake .`), but I strongly recommend to follow above instruction.

Build
^^^^^

A built executable binary will be under bin/ directory and shared libraries under lib/::

  $ make

If you are building picrin on other systems than x86_64, PIC_NAN_BOXING flag is automatically turned on (see include/picrin/config.h for detail).

Install
^^^^^^^

Just running `make install`, picrin library, headers, and runtime binary are install on your system, by default into `/usr/local` directory. You can change this value via ccmake::

  $ make install

Run
^^^

Before installing picrin, you can try picrin without breaking any of your system. Simply directly run the binary `bin/picrin` from terminal, or you can use `make` to execute it like this::

  $ make run

Debug run
^^^^^^^^^

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
