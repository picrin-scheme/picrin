Installation
============

Installation instructions below.


Build
-----

Just type `make` in the project root directory. You will find an executable binary newly created at bin/ directory.

    $ make

When you are building picrin on x86_64 system, PIC_NAN_BOXING flag is automatically turned on (see include/picrin/config.h for detail).

Install
-------

`make install` target is provided. By default it installs picrin binary into `/usr/local/bin/`.

	$ make install

Since picrin does not use autoconf, if you want to specify the install directory, pass the custom path to `make` via command line argument.

	$ make install prefix=/path/to/dir

Requirement
-----------

To build Picrin Scheme from source code, some external libraries are required:

- perl
- regex.h of POSIX.1
- libedit (optional)

Make command automatically turns on optional libraries if available.
Picrin is mainly developed on Mac OS X and only tested on OS X or Ubuntu 14.04+. When you tried to run picrin on other platforms and found something was wrong with it, please send us an issue.
