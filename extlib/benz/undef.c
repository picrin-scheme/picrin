/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
pic_undef_undefined_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_undef_p(v) ? pic_true_value() : pic_false_value();
}

void
pic_init_undef(pic_state *pic)
{
  pic_defun(pic, "undefined?", pic_undef_undefined_p);
}
