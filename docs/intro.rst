Introduction
============

Picrin is a lightweight R7RS scheme implementation written in pure C89. It contains a reasonably fast VM, an improved hygienic macro system, usuful contribution libraries, and simple but powerful C interface.

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
