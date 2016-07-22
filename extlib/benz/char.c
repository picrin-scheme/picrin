/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"

static pic_value
pic_char_char_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_char_p(pic, v) ? pic_true_value(pic) : pic_false_value(pic);
}

static pic_value
pic_char_char_to_integer(pic_state *pic)
{
  char c;

  pic_get_args(pic, "c", &c);
  assert((c & 0x80) == 0);

  return pic_int_value(pic, c);
}

static pic_value
pic_char_integer_to_char(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  if (i < 0 || i > 127) {
    pic_error(pic, "integer->char: integer out of char range", 1, pic_int_value(pic, i));
  }

  return pic_char_value(pic, (char)i);
}

#define DEFINE_CHAR_CMP(op, name)			\
  static pic_value					\
  pic_char_##name##_p(pic_state *pic)			\
  {							\
    int argc, i;                                        \
    pic_value *argv;					\
    char c, d;						\
    							\
    pic_get_args(pic, "cc*", &c, &d, &argc, &argv);	\
    							\
    if (! (c op d))					\
      return pic_false_value(pic);                      \
                                                        \
    for (i = 0; i < argc; ++i) {                        \
      c = d;                                            \
      TYPE_CHECK(pic, argv[i], char);                   \
      d = pic_char(pic, argv[i]);                       \
                                                        \
      if (! (c op d))					\
	return pic_false_value(pic);			\
    }							\
    							\
    return pic_true_value(pic);				\
  }

DEFINE_CHAR_CMP(==, eq)
DEFINE_CHAR_CMP(<, lt)
DEFINE_CHAR_CMP(>, gt)
DEFINE_CHAR_CMP(<=, le)
DEFINE_CHAR_CMP(>=, ge)

void
pic_init_char(pic_state *pic)
{
  pic_defun(pic, "char?", pic_char_char_p);
  pic_defun(pic, "char->integer", pic_char_char_to_integer);
  pic_defun(pic, "integer->char", pic_char_integer_to_char);
  pic_defun(pic, "char=?", pic_char_eq_p);
  pic_defun(pic, "char<?", pic_char_lt_p);
  pic_defun(pic, "char>?", pic_char_gt_p);
  pic_defun(pic, "char<=?", pic_char_le_p);
  pic_defun(pic, "char>=?", pic_char_ge_p);
}
