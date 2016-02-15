/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid;
  char *fn;
  struct pic_port *port;

  pic_get_args(pic, "z|o", &fn, &envid);

  port = pic_open_file(pic, fn, PIC_PORT_IN | PIC_PORT_TEXT);

  pic_load(pic, port);

  pic_close_port(pic, port);

  return pic_undef_value();
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary(pic, "(scheme load)");

  pic_defun(pic, "load", pic_load_load);
}
