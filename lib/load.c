/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "../object.h"

pic_value
pic_load(pic_state *pic, pic_value expr)
{
  return pic_execute(pic, pic_assemble(pic, expr));
}

void
pic_load_native(pic_state *pic, const char *str)
{
  pic_value e, port = pic_fmemopen(pic, str, strlen(str), "r");

  pic_try {
    size_t ai = pic_enter(pic);
    pic_load(pic, pic_funcall(pic, "read", 1, port));
    pic_leave(pic, ai);
  }
  pic_catch(e) {
    pic_fclose(pic, port);
    pic_raise(pic, e);
  }
  pic_fclose(pic, port);
}

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value program;

  pic_get_args(pic, "o", &program);

  return pic_load(pic, program);
}

void
pic_init_load(pic_state *pic)
{
  pic_defun(pic, "load", pic_load_load);
}
