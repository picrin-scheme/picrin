# libpicrin

libpicrin is a super tiny scheme interpreter intended to be embedded in other applications such as game engine and network server. It provides a subset language of R7RS with several useful extensions. By default, libpicrin only contains some C files and headers and this README file. To embed, you only need to copy the files into the project and add `include` dir to the include path.

Originally, libpicrin used to be the core component of [Picrin Scheme](https://github.com/picrin-scheme/picrin). They are currently maintained at separate repositories.

## Example

```c
#include <stdio.h>

#include "picrin.h"
#include "picrin/extra.h"

/* Simple REPL program */

int
main(int argc, char *argv[])
{
  pic_state *pic;
  pic_value expr;

  pic = pic_open(pic_default_allocf, NULL);

  while (1) {
    printf("> ");

    expr = pic_read(pic, pic_stdin(pic));

    if (pic_eof_p(pic, expr)) {
      break;
    }

    pic_printf(pic, "~s\n", pic_eval(pic, expr, "picrin.user"));
  }

  pic_close(pic);

  return 0;
}
```

## More Example

Function binding is also easy. `pic_defun` defines a scheme procedure converting from a C function. In the native function, callee arguments can be taken with `pic_get_args`. `pic_get_args` gets arguments according to the format string. If actual arguments does not match a number or incompatible types, it will raise an exception.

```c
#include "picrin.h"
#include "picrin/extra.h"

int fact(int i) {
  return i == 1 ? 1 : i * fact(i - 1);
}

pic_value factorial(pic_state *pic) {
  int i;

  pic_get_args(pic, "i", &i);

  return pic_int_value(pic, fact(i));
}

int
main(int argc, char *argv[])
{
  pic_state *pic = pic_open(pic_default_allocf, NULL);

  pic_defun(pic, "fact", factorial); /* define fact procedure */

  pic_load_cstr(pic, "(display (fact 10))");

  pic_close(pic);

  return 0;
}
```

## Language

All procedures and syntaces are exported from a single library named `(picrin base)`. The complete list is found at https://gist.github.com/wasabiz/344d802a2340d1f734b7 .

### call/cc

Full continuation has many problems in embbeding into applications. By default, libpicrin's call/cc operator does not support continuation that can handle re-entering (it only supports escape continuations). To remove this restriction, please use an add-on provided from [Picrin Scheme's repository](https://github.com/picrin-scheme/picrin/tree/master/contrib/03.callcc).

### Strings

libpicrin utilize rope data structure to implement string type. Thanks to the implementation, string-append is guaranteed to be done in a constant time (so do string-copy, when ascii-only mode is enabled). In return for that, strings in libpicrin are immutable by default. It does not provide mutation API (string-set!, string-copy! and string-fill! in R7RS). This restriction can be also removed with an add-on in [Picrin Scheme's repository](https://github.com/picrin-scheme/picrin/tree/master/contrib/03.mutable-string).

### Dictionaries

Dictionary is a hash table from symbol to object.

## Authors

See https://github.com/picrin-scheme/picrin for details.

## LICENSE

Copyright (c) 2013-2017 Yuichi Nishiwaki and other picrin contributors

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

