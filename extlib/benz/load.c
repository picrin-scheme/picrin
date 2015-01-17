/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/port.h"
#include "picrin/error.h"

void
pic_load_port(pic_state *pic, struct pic_port *port)
{
  pic_value form;

  pic_try {
    size_t ai = pic_gc_arena_preserve(pic);

    while (! pic_eof_p(form = pic_read(pic, port))) {
      pic_eval(pic, form, pic->lib);

      pic_gc_arena_restore(pic, ai);
    }
  }
  pic_catch {
    pic_errorf(pic, "load error: %s", pic_errmsg(pic));
  }
}

void
pic_load_cstr(pic_state *pic, const char *src)
{
  struct pic_port *port = pic_open_input_string(pic, src);

  pic_load_port(pic, port);

  pic_close_port(pic, port);
}
