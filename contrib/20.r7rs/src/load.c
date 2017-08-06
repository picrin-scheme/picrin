/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

#include <stdio.h>

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid, port, e;
  char *fn;
  FILE *fp;

  pic_get_args(pic, "z|o", &fn, &envid);

  fp = fopen(fn, "r");
  if (fp == NULL) {
    pic_error(pic, "load: could not open file", 1, pic_cstr_value(pic, fn));
  }

  port = pic_fopen(pic, fp, "r");
  pic_try {
    size_t ai = pic_enter(pic);

    while (1) {
      pic_value form = pic_funcall(pic, "read", 1, port);
      if (pic_eof_p(pic, form))
        break;
      pic_funcall(pic, "eval", 1, form);
      pic_leave(pic, ai);
    }
  }
  pic_catch (e) {
    pic_fclose(pic, port);
    pic_funcall(pic, "raise", 1, e);
  }
  pic_fclose(pic, port);

  return pic_undef_value(pic);
}

void
pic_nitro_init_load(pic_state *pic)
{
  pic_defun(pic, "scheme.load:load", pic_load_load);
}
