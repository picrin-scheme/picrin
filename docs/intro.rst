Introduction
============

Picrin is a lightweight scheme implementation intended to comply with full R7RS specification. Its code is written in pure C99 and does not require any special external libraries installed on the platform.

- R7RS compatibility
- reentrant design (all VM states are stored in single global state object)
- bytecode interpreter (based on stack VM)
- direct threaded VM
- internal representation by nan-boxing
- conservative call/cc implementation (users can freely interleave native stack with VM stack)
- exact GC (simple mark and sweep, partially reference count is used as well)
- string representation by rope data structure
- support full set hygienic macro transformers, including implicit renaming macros
- extended library syntax
- advanced REPL support (multi-line input, etc)
- tiny & portable library (all functions will be in `libpicrin.so`)

Homepage
--------

Currently picrin is hosted on Github. You can freely send a bug report or pull-request, and fork the repository.

https://github.com/picrin-scheme/picrin

Documentation
-------------

See http://picrin.readthedocs.org/

IRC
---

There is a chat room on chat.freenode.org, channel #picrin. IRC logs here: https://botbot.me/freenode/picrin/

LICENSE
-------

Copyright (c) 2013-2014 Yuichi Nishiwaki and other picrin contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
