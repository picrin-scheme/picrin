/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void
pic_load_port(pic_state *pic, struct pic_port *port)
{
  pic_value form;
  size_t ai = pic_gc_arena_preserve(pic);

  while (! pic_eof_p(form = pic_read(pic, port))) {
    pic_eval(pic, form, pic->lib->env);

    pic_gc_arena_restore(pic, ai);
  }
}

void
pic_load_cstr(pic_state *pic, const char *src)
{
  struct pic_port *port = pic_open_input_string(pic, src);

  pic_try {
    pic_load_port(pic, port);
  }
  pic_catch {
    pic_close_port(pic, port);
    pic_raise(pic, pic->err);
  }

  pic_close_port(pic, port);
}
