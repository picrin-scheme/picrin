#include "picrin.h"
#include <math.h>

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
  (void) pic;
  (void) data;
  // TODO empty, memory-leaking
}

static const pic_data_type bigint_type = { "bigint", bigint_dtor, NULL };
#define pic_bigint_p(o) (pic_data_type_p((o), &bigint_type))
#define pic_bigint_data_ptr(o) ((struct pic_bigint_t *)pic_data_ptr(o)->data)

#define BIG_NUMBER_MAX 255


/*
 * Eliminates all leading zeroes.
 */
static pic_vec *
bigint_vec_compact(pic_state *pic, const pic_vec *v)
{
  int i;
  int l = v->len - 1;
  pic_vec *ret;

  if (pic_int(v->data[l]) != 0) {
    return (pic_vec *)v;
  }
  --l;
  while (l >= 0 && pic_int(v->data[l]) == 0) {
    --l;
  }
  ret = pic_make_vec(pic, l + 1);
  for (i = 0; i <= l; ++i) {
    ret->data[i] = v->data[i];
  }

  return ret;
}

/*
 * Checks whether v1 and v2 represents the same value.
 */
static bool
bigint_vec_eq(const pic_vec *v1, const pic_vec *v2)
{
  int i;
  if (v1->len != v2->len) {
    return false;
  }
  for (i = v1->len - 1; i >= 0; --i) {
    if (pic_int(v1->data[i]) != pic_int(v2->data[i])) {
      return false;
    }
  }

  return true;
}

static bool
bigint_vec_lt(const pic_vec *v1, const pic_vec *v2)
{
  int i;
  if (v1->len != v2->len) {
    return v1->len < v2->len;
  }
  for (i = v1->len - 1; i >= 0; --i) {
    int d1 = pic_int(v1->data[i]);
    int d2 = pic_int(v2->data[i]);
    if (d1 != d2) {
      return d1 < d2;
    }
  }

  return false;
}

static pic_vec *
bigint_vec_add(pic_state *pic, const pic_vec *v1, const pic_vec *v2)
{
  int carry, msb1, msb2;
  size_t i, len;
  pic_vec *ret;

  if (v1->len == 0) {
    return (pic_vec *)v2;
  }
  if (v2->len == 0) {
    return (pic_vec *)v1;
  }
  // v1 > 0, v2 > 0
  len = v1->len;
  if (len < v2->len) {
    len = v2->len;
  }
  msb1 = pic_int(v1->data[v1->len - 1]);
  msb2 = pic_int(v2->data[v2->len - 1]);
  if (msb1 + msb2 >= 255) {
    ++len;
  }
  carry = 0;
  ret = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    int d1 = i >= v1->len ? 0 : pic_int(v1->data[i]);
    int d2 = i >= v2->len ? 0 : pic_int(v2->data[i]);
    carry += d1 + d2;
    if (i == len - 1) {
      ret->data[i] = pic_int_value(carry);
    } else {
      ret->data[i] = pic_int_value(carry & 0xff);
      carry >>= 8;
    }
  }

  return bigint_vec_compact(pic, ret);
}

/*
 * Precondition: v1 >= v2
 */
static pic_vec *
bigint_vec_sub(pic_state *pic, const pic_vec *v1, const pic_vec *v2)
{
  int carry;
  size_t i, len;
  pic_vec *ret;

  len = v1->len; // v1 must be larger than v2
  carry = 0;
  ret = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    int d1 = pic_int(v1->data[i]);
    int d2 = i >= v2->len ? 0 : pic_int(v2->data[i]);
    carry += d1 - d2;
    ret->data[i] = pic_int_value(carry & 0xff);
    carry >>= 8;
  }

  return bigint_vec_compact(pic, ret);
}

static pic_vec *
bigint_vec_asl(pic_state *pic, const pic_vec *val, int sh);


/*
 * Classical algorithm. O(n^2)
 */
static pic_vec *
bigint_vec_mul(pic_state *pic, const pic_vec *v1, const pic_vec *v2)
{
  int len1, len2, i, j;
  pic_vec *ret;

  len1 = v1->len;
  len2 = v2->len;
  ret = pic_make_vec(pic, 0);

  for (i = 0; i < len1; ++i) {
    int d1 = pic_int(v1->data[i]);
    for (j = 0; j < 8; ++j) {
      if (d1 & (1 << j)) {
	ret = bigint_vec_add(pic, ret, bigint_vec_asl(pic, v2, i * 8 + j));
      }
    }
  }

  return ret;
}

static pic_vec *
bigint_vec_asl(pic_state *pic, const pic_vec *val, int sh)
{
  pic_vec *ret;
  int bitsh, bytesh, carry;
  int i, len;

  bitsh = sh % 8;
  bytesh = sh / 8;
  carry = 0;

  len = val->len;
  ret = pic_make_vec(pic, len + bytesh + 1);
  for (i = 0; i < bytesh; ++i) {
    ret->data[i] = pic_int_value(0);
  }
  for (i = 0; i < len; ++i) {
    carry |= pic_int(val->data[i]) << bitsh;
    ret->data[i + bytesh] = pic_int_value(carry & 0xff);
    carry >>= 8;
  }
  ret->data[bytesh + len] = pic_int_value(carry);

  return bigint_vec_compact(pic, ret);
}


/*
 * Creates a big integer by the given int value.
 */
static struct pic_bigint_t *
bigint_init_int(pic_state *pic, int value)
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
  bi->digits = bigint_vec_compact(pic, bn);

  return bi;
}

static struct pic_bigint_t *
bigint_add(pic_state *pic, struct pic_bigint_t *bn1, struct pic_bigint_t *bn2)
{
  struct pic_bigint_t *retbi;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));

  if (bn1->signum != bn2->signum) {
    if (bigint_vec_lt(bn1->digits, bn2->digits)) { // bn2 wins
      retbi->signum = bn2->signum;
      retbi->digits = bigint_vec_sub(pic, bn2->digits, bn1->digits);
      return retbi;
    }
    retbi->signum = bn1->signum;
    retbi->digits = bigint_vec_sub(pic, bn1->digits, bn2->digits);
    if (retbi->digits->len == 0) { // bn1 + bn2 == 0
      retbi->signum = 0;
    }
    return retbi;
  }
  // two signums are equal
  retbi->signum = bn1->signum;
  retbi->digits = bigint_vec_add(pic, bn1->digits, bn2->digits);

  return retbi;
}

static struct pic_bigint_t *
bigint_mul(pic_state *pic, struct pic_bigint_t *v1, struct pic_bigint_t *v2)
{
  struct pic_bigint_t *retbi;
  pic_vec *ret;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  ret = bigint_vec_mul(pic, v1->digits, v2->digits);

  retbi->signum = v1->signum ^ v2->signum;
  if (ret->len == 0) {
    retbi->signum = 0;
  }
  retbi->digits = ret;
  return retbi;
}

static struct pic_bigint_t *
bigint_asl(pic_state *pic, struct pic_bigint_t *val, int sh)
{
  struct pic_bigint_t *retbi;
  pic_vec *ret;
  int bitsh, bytesh, carry;
  int i, len;

  ret = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  if (sh <= 0) {
    retbi->signum = val->signum;
    retbi->digits = val->digits; // copy
    return retbi;
  }
  bitsh = sh % 8;
  bytesh = sh / 8;
  carry = 0;

  len = val->digits->len;
  ret = pic_make_vec(pic, len + bytesh + 1);
  for (i = 0; i < bytesh; ++i) {
    ret->data[i] = pic_int_value(0);
  }
  for (i = 0; i < len; ++i) {
    carry |= pic_int(val->digits->data[i]) << bitsh;
    ret->data[i + bytesh] = pic_int_value(carry & 0xff);
    carry >>= 8;
  }
  ret->data[bytesh + len] = pic_int_value(carry);

  retbi->signum = val->signum;
  retbi->digits = bigint_vec_compact(pic, ret);
  return retbi;
}

static double
bigint_to_double(struct pic_bigint_t *bi)
{
  double ret = 0, p = 1.0;
  int i;
  int len, lim;

  if (bi->digits->len >= 129) { // max double value < 2^1024
    return bi->signum ? -1.0 / 0.0 : 1.0 / 0.0;
  }

  len = bi->digits->len;
  lim = 8;
  if (lim > len) {
    lim = len;
  }
  for (i = 0; i < lim; ++i) {
    ret += pic_int(bi->digits->data[len - i - 1]) * p;
    p /= 256;
  }
  if (bi->signum) {
    ret = -ret;
  }

  return ret * pow(256.0, len - 1);
}


static pic_value
pic_big_number_make_bigint(pic_state *pic)
{
  int value;

  pic_get_args(pic, "i", &value);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bigint_init_int(pic, value)));
}

static pic_value
pic_big_number_bigint_add(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = pic_bigint_data_ptr(value2);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bigint_add(pic, bi1, bi2)));
}

static pic_value
pic_big_number_bigint_sub(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2, *result;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = pic_bigint_data_ptr(value2);

  bi2->signum = 1 - bi2->signum;
  result = bigint_add(pic, bi1, bi2);
  bi2->signum = 1 - bi2->signum;

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, result));
}

static pic_value
pic_big_number_bigint_mul(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = pic_bigint_data_ptr(value2);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bigint_mul(pic, bi1, bi2)));
}

/*
 * Returns underlying vector of given biginteger.
 */
static pic_value
pic_big_number_bigint_underlying(pic_state *pic)
{
  pic_value value;
  struct pic_bigint_t *bi;

  pic_get_args(pic, "o", &value);
  bi = pic_bigint_data_ptr(value);

  return pic_obj_value(bi->digits);
}

static pic_value
pic_big_number_bigint_equal_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = pic_bigint_data_ptr(v1);
  bi2 = pic_bigint_data_ptr(v2);

  return pic_bool_value(bi1->signum == bi2->signum && bigint_vec_eq(bi1->digits, bi2->digits));
}

static pic_value
pic_big_number_bigint_asl(pic_state *pic)
{
  pic_value val;
  int sh;
  struct pic_bigint_t *result;

  pic_get_args(pic, "oi", &val, &sh);
  result = bigint_asl(pic, pic_bigint_data_ptr(val), sh);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, result));
}
static pic_value
pic_big_number_bigint_to_number(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *bi;
  double result;

  pic_get_args(pic, "o", &val);
  bi = pic_bigint_data_ptr(val);
  result = bigint_to_double(bi);

  return pic_float_value(result);
}

void
pic_init_big_number(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin big-number)") {
    pic_defun(pic, "make-bigint", pic_big_number_make_bigint);
    pic_defun(pic, "bigint-add", pic_big_number_bigint_add);
    pic_defun(pic, "bigint-sub", pic_big_number_bigint_sub);
    pic_defun(pic, "bigint-mul", pic_big_number_bigint_mul);
    pic_defun(pic, "bigint-underlying", pic_big_number_bigint_underlying);
    pic_defun(pic, "bigint-equal?", pic_big_number_bigint_equal_p);
    pic_defun(pic, "bigint-asl", pic_big_number_bigint_asl);
    pic_defun(pic, "bigint->number", pic_big_number_bigint_to_number);
  }
}
