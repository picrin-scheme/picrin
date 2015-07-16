#include "picrin.h"

/**
 * Big integer is represented as a vector of digits.
 * A digit is 0 ~ 255.
 */

#define BIG_NUMBER_MAX 255

/*
 * Creates a big integer by the given int value.
 */
static pic_value
big_integer_init_int(pic_state *pic, int value)
{
  int i;
  pic_vec *bn = pic_make_vec(pic, 4);

  for (i = 0; i < 4; ++i) {
    bn->data[i] = pic_int_value((value >> (8 * i)) & 0xff);
  }

  return pic_obj_value(bn);
}

static pic_value
pic_big_number_make_big_integer(pic_state *pic)
{
  int value;

  pic_get_args(pic, "i", &value);

  return big_integer_init_int(pic, value);
}

void
pic_init_big_number(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin big-number)") {
    pic_defun(pic, "make-big-integer", pic_big_number_make_big_integer);
  }
}
