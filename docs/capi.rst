C API
=====

You can write Picrin's extension by yourself from both sides of C and Scheme. This page describes the way to control the interpreter from the C world.

Extension Library
-----------------

If you want to create a contribution library with C, the only thing you need to do is make a directory under contrib/. Below is a sample code of extension library.

* contrib/add/nitro.mk

.. sourcecode:: cmake

  CONTRIB_INITS += add
  CONTRIB_SRCS  += contrib/add/add.c

* contrib/add/add.c

.. sourcecode:: c

  #include "picrin.h"

  static pic_value
  pic_add(pic_state *pic)
  {
    double a, b;

    pic_get_args(pic, "ff", &a, &b);

    return pic_float_value(pic, a + b);
  }

  void
  pic_init_add(pic_state *pic)
  {
    pic_deflibrary (pic, "(picrin add)") {
      pic_defun(pic, "add", pic_add);
    }
  }

After recompiling the interpreter, the library "(picrin add)" is available in the REPL, which library provides a funciton "add".

User-data vs GC
^^^^^^^^^^^^^^^

When you use dynamic memory allocation inside C APIs, you must be caseful about Picrin's GC. Fortunately, we provides a set of wrapper functions for complete abstraction of GC. In the case below, the memory (de)allocators *create_foo* and *finalize_foo* are wrapped in pic_data object, so that when an instance of foo losts all references from others to it picrin can automatically finalize the orphan object.

.. sourcecode:: c

  /** foo.c **/
  #include <stdlib.h>
  #include "picrin.h"

  /*
   * C-side API
   */

  struct foo {
    // blah blah blah
  };

  struct foo *
  create_foo ()
  {
    return malloc(sizeof(struct foo));
  }

  void
  finalize_foo (void *foo) {
    struct foo *f = foo;
    free(f);
  }


  /*
   * picrin-side FFI interface
   */

  static const pic_data_type foo_type = { "foo", finalize_foo };

  static pic_value
  pic_create_foo(pic_state *pic)
  {
    struct foo *f;

    pic_get_args(pic, ""); // no args here

    f = create_foo();

    return pic_data_value(pic, md, &foo_type);
  }

  void
  pic_init_foo(pic_state *pic)
  {
    pic_defun(pic, "create-foo", pic_create_foo); // (create-foo)
  }

