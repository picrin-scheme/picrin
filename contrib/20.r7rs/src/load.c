/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void
pic_load(pic_state *pic, const char *filename)
{
  struct pic_port *port;

  port = pic_open_file(pic, filename, PIC_PORT_IN | PIC_PORT_TEXT);

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

  return pic_undef_value();
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary (pic, "(scheme load)") {
    pic_defun(pic, "load", pic_load_load);
  }
}
