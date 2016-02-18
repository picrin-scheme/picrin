/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void
pic_load(pic_state *pic, struct pic_port *port)
{
  pic_value form;
  size_t ai = pic_gc_arena_preserve(pic);

  while (! pic_eof_p(pic, form = pic_read(pic, port))) {
    pic_eval(pic, form, pic_current_library(pic));

    pic_gc_arena_restore(pic, ai);
  }
}

void
pic_load_cstr(pic_state *pic, const char *str)
{
  struct pic_port *port = pic_make_port(pic, xfopen_buf(pic, str, strlen(str), "r"));

  pic_try {
    pic_load(pic, port);
  }
  pic_catch {
    pic_close_port(pic, port);
    pic_raise(pic, pic->err);
  }

  pic_close_port(pic, port);
}
