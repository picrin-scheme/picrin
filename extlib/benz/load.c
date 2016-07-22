/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

void
pic_load(pic_state *pic, pic_value port)
{
  pic_value form;
  size_t ai = pic_enter(pic);

  while (! pic_eof_p(pic, form = pic_read(pic, port))) {
    pic_eval(pic, form, pic_current_library(pic));

    pic_leave(pic, ai);
  }
}

void
pic_load_cstr(pic_state *pic, const char *str)
{
  pic_value e, port = pic_fmemopen(pic, str, strlen(str), "r");

  pic_try {
    pic_load(pic, port);
  }
  pic_catch(e) {
    pic_fclose(pic, port);
    pic_raise(pic, e);
  }

  pic_fclose(pic, port);
}
