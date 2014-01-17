/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"

bool
pic_equal_p(pic_state *pic, pic_value x, pic_value y)
{
  enum pic_tt type;

  if (pic_eqv_p(x, y))
    return true;

  type = pic_type(x);
  if (type != pic_type(y))
    return false;
  switch (type) {
  case PIC_TT_PAIR:
    return pic_equal_p(pic, pic_car(pic, x), pic_car(pic, y))
      && pic_equal_p(pic, pic_cdr(pic, x), pic_cdr(pic, y));
  default:
    return false;
  }
}

static pic_value
pic_bool_eq_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_eq_p(x, y));
}

static pic_value
pic_bool_eqv_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_eqv_p(x, y));
}

static pic_value
pic_bool_equal_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_equal_p(pic, x, y));
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
  pic_defun(pic, "eqv?", pic_bool_eqv_p);
  pic_defun(pic, "equal?", pic_bool_equal_p);

  pic_defun(pic, "not", pic_bool_not);
  pic_defun(pic, "boolean?", pic_bool_boolean_p);
}
