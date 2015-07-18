#include "picrin.h"

/**
 * Big integer is represented as a vector of digits.
 * A digit is 0 ~ 255.
 */

struct pic_bigint_t {
  int signum;
  pic_vec *digits;
};

static void
bigint_dtor(pic_state *pic, void *data)
{
  // TODO empty, memory-leaking
}

static const pic_data_type bigint_type = { "big-integer", bigint_dtor, NULL };
#define pic_bigint_p(o) (pic_data_type_p((o), &bigint_type))
#define pic_bigint_data_ptr(o) ((struct pic_bigint_t *)pic_data_ptr(o)->data)

#define BIG_NUMBER_MAX 255

/*
 * Creates a big integer by the given int value.
 */
static struct pic_bigint_t *
big_integer_init_int(pic_state *pic, int value)
{
  int i;
  pic_vec *bn = pic_make_vec(pic, 4);
  struct pic_bigint_t *bi;

  bi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  bi->signum = value < 0;
  if (value < 0) {
    value = -value;
  }

  for (i = 0; i < 4; ++i) {
    bn->data[i] = pic_int_value((value >> (8 * i)) & 0xff);
  }
  bi->digits = bn;

  return bi;
}

static struct pic_bigint_t *
big_integer_add(pic_state *pic, struct pic_bigint_t *bn1, struct pic_bigint_t *bn2)
{
  size_t i, len;
  int msb1, msb2, carry;
  pic_vec *ret;
  struct pic_bigint_t *retbi;

  len = bn1->digits->len;
  if (len < bn2->digits->len) {
    len = bn2->digits->len;
  }
  msb1 = pic_int(bn1->digits->data[bn1->digits->len - 1]);
  msb2 = pic_int(bn2->digits->data[bn2->digits->len - 1]);
  if (msb1 + msb2 >= 255 || msb1 + msb2 <= -256) {
    ++len;
  }
  carry = 0;
  ret = pic_make_vec(pic, len);
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi->digits = ret;

  for (i = 0; i < len; ++i) {
    int d1 = i >= bn1->digits->len ? 0 : pic_int(bn1->digits->data[i]);
    int d2 = i >= bn2->digits->len ? 0 : pic_int(bn2->digits->data[i]);
    carry += d1 + d2;
    if (i == len - 1) {
      ret->data[i] = pic_int_value(carry);
    } else {
      ret->data[i] = pic_int_value(carry & 0xff);
      carry >>= 8;
    }
  }
  return retbi;
}


static pic_value
pic_big_number_make_big_integer(pic_state *pic)
{
  int value;

  pic_get_args(pic, "i", &value);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, big_integer_init_int(pic, value)));
}

static pic_value
pic_big_number_big_integer_add(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = pic_bigint_data_ptr(value2);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, big_integer_add(pic, bi1, bi2)));
}

void
pic_init_big_number(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin big-number)") {
    pic_defun(pic, "make-big-integer", pic_big_number_make_big_integer);
    pic_defun(pic, "big-integer-add", pic_big_number_big_integer_add);
  }
}
