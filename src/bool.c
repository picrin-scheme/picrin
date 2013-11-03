#include <string.h>

#include "picrin.h"

bool
pic_eq_p(pic_state *pic, pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_sym(x) == pic_sym(y);
  default:
    return false;
  }
}

static pic_value
pic_bool_eq_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_eq_p(pic, x, y));
}

/* TODO: replace it with native opcode */
static pic_value
pic_bool_not(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_false_p(v) ? pic_true_value() : pic_false_value();
}

static pic_value
pic_bool_boolean_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return (pic_true_p(v) || pic_false_p(v)) ? pic_true_value() : pic_false_value();
}

void
pic_init_bool(pic_state *pic)
{
  pic_defun(pic, "eq?", pic_bool_eq_p);

  pic_defun(pic, "not", pic_bool_not);
  pic_defun(pic, "boolean?", pic_bool_boolean_p);
}
