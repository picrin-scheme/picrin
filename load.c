/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/port.h"
#include "picrin/error.h"

static void
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

void
pic_load(pic_state *pic, const char *filename)
{
  struct pic_port *port;
  xFILE *file;

  file = xfopen(filename, "r");
  if (file == NULL) {
    pic_errorf(pic, "could not open file: %s", filename);
  }

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = file;
  port->flags = PIC_PORT_IN | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;

  pic_load_port(pic, port);

  pic_close_port(pic, port);
}

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid;
  char *fn;

  pic_get_args(pic, "z|o", &fn, &envid);

  pic_load(pic, fn);

  return pic_none_value();
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary (pic, "(scheme load)") {
    pic_defun(pic, "load", pic_load_load);
  }
}
