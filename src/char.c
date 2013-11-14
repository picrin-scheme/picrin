#include "picrin.h"

static pic_value
pic_char_char_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_char_p(v) ? pic_true_value() : pic_false_value();
}

static pic_value
pic_char_char_to_integer(pic_state *pic)
{
  char c;

  pic_get_args(pic, "c", &c);

  return pic_int_value(c);
}

static pic_value
pic_char_integer_to_char(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_char_value(i);
}

void
pic_init_char(pic_state *pic)
{
  pic_defun(pic, "char?", pic_char_char_p);
  pic_defun(pic, "char->integer", pic_char_char_to_integer);
  pic_defun(pic, "integer->char", pic_char_integer_to_char);
}
