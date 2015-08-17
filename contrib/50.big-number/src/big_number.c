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

typedef unsigned bigint_digit;
typedef unsigned long long bigint_2digits;
typedef long long bigint_diff;
#define bigint_shift 32
#define bigint_digit_max 0xffffffffULL // : bigint_2digits

static pic_vec *
bigint_vec_clone(pic_state *pic, const pic_vec *v) {
  size_t i;
  size_t len = v->len;
  pic_vec *ret = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    ret->data[i] = v->data[i];
  }

  return ret;
}

/*
 * Eliminates all leading zeroes.
 */
static pic_vec *
bigint_vec_compact(pic_state *pic, const pic_vec *v)
{
  int i;
  int l = v->len - 1;
  pic_vec *ret;

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
    bigint_digit d1 = pic_int(v1->data[i]);
    bigint_digit d2 = pic_int(v2->data[i]);
    if (d1 != d2) {
      return d1 < d2;
    }
  }

  return false;
}

static pic_vec *
bigint_vec_add(pic_state *pic, const pic_vec *v1, const pic_vec *v2)
{
  bigint_2digits carry;
  bigint_digit msb1, msb2;
  size_t i, len;
  pic_vec *ret;

  if (v1->len == 0) {
    return bigint_vec_clone(pic, v2);
  }
  if (v2->len == 0) {
    return bigint_vec_clone(pic, v1);
  }
  // v1 > 0, v2 > 0
  len = v1->len;
  if (len < v2->len) {
    len = v2->len;
  }
  msb1 = pic_int(v1->data[v1->len - 1]);
  msb2 = pic_int(v2->data[v2->len - 1]);
  if ((bigint_2digits)msb1 + msb2 >= bigint_digit_max) {
    ++len;
  }
  carry = 0;
  ret = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = i >= v1->len ? 0 : pic_int(v1->data[i]);
    bigint_digit d2 = i >= v2->len ? 0 : pic_int(v2->data[i]);
    carry += d1;
    carry += d2;
    if (i == len - 1) {
      ret->data[i] = pic_int_value(carry);
    } else {
      ret->data[i] = pic_int_value(carry & bigint_digit_max);
      carry >>= bigint_shift;
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
  bigint_diff carry;
  size_t i, len;
  pic_vec *ret;

  len = v1->len; // v1 must be larger than v2
  carry = 0;
  ret = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = pic_int(v1->data[i]);
    bigint_digit d2 = i >= v2->len ? 0 : pic_int(v2->data[i]);
    carry += d1;
    carry -= d2;
    ret->data[i] = pic_int_value(carry & bigint_digit_max);
    carry >>= bigint_shift;
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
  bigint_digit *tmp;
  bigint_2digits carry;

  len1 = v1->len;
  len2 = v2->len;
  tmp = (bigint_digit *) malloc((len1 + len2) * sizeof(bigint_digit));
  carry = 0;

  for (i = 0; i < len1 + len2; ++i) {
    tmp[i] = 0;
  }

  for (i = 0; i < len1; ++i) {
    bigint_digit d1 = pic_int(v1->data[i]);
    carry = 0;
    for (j = 0; j < len2; ++j) {
      carry += tmp[i + j];
      bigint_digit d2 = pic_int(v2->data[j]);
      carry += (bigint_2digits) d1 * d2;
      tmp[i + j] = carry & bigint_digit_max;
      carry >>= bigint_shift;
    }
    tmp[i + len2] = carry;
  }

  ret = pic_make_vec(pic, len1 + len2);
  for (i = 0; i < len1 + len2; ++i) {
    ret->data[i] = pic_int_value(tmp[i]);
  }

  free(tmp);
  return bigint_vec_compact(pic, ret);
}

static void
bigint_vec_div(pic_state *pic, const pic_vec *v1, const pic_vec *v2,
	       pic_vec **quo, pic_vec **rem)
{
  pic_vec *quov, *remv, *one;
  int i;
  assert (v2->len >= 1);
 
  // Very slow, but still in polynomial time. :)
  quov = pic_make_vec(pic, 0);
  remv = bigint_vec_clone(pic, v1);
  one = pic_make_vec(pic, 1);
  one->data[0] = pic_int_value(1);

  for (i = bigint_shift * (v1->len - v2->len + 1) - 1; i >= 0; --i) {
    pic_vec *sh = bigint_vec_asl(pic, v2, i);
    if (! bigint_vec_lt(remv, sh)) { // 2^i * v2 <= rem
      remv = bigint_vec_sub(pic, remv, sh);
      quov = bigint_vec_add(pic, quov, bigint_vec_asl(pic, one, i));
    }
  }

  *quo = quov;
  *rem = remv;
}

static pic_vec *
bigint_vec_asl(pic_state *pic, const pic_vec *val, int sh)
{
  pic_vec *ret;
  int bitsh, bytesh;
  bigint_2digits carry;
  int i, len;

  bitsh = sh % bigint_shift;
  bytesh = sh / bigint_shift;
  carry = 0;

  len = val->len;
  ret = pic_make_vec(pic, len + bytesh + 1);
  for (i = 0; i < bytesh; ++i) {
    ret->data[i] = pic_int_value(0);
  }
  for (i = 0; i < len; ++i) {
    carry |= (bigint_2digits) pic_int(val->data[i]) << bitsh;
    ret->data[i + bytesh] = pic_int_value(carry & bigint_digit_max);
    carry >>= bigint_shift;
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
  int s = 32 / bigint_shift; // if bigint_shift == 8, s == 4
  pic_vec *bn = pic_make_vec(pic, s);
  struct pic_bigint_t *bi;

  bi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  bi->signum = value < 0;
  if (value < 0) {
    value = -value;
  }

  for (i = 0; i < s; ++i) {
    bn->data[i] = pic_int_value((value >> (bigint_shift * i)) & bigint_digit_max);
  }
  bi->digits = bigint_vec_compact(pic, bn);

  return bi;
}

static struct pic_bigint_t *
bigint_init_str(pic_state *pic, pic_str *str)
{
  const char *cstr;
  size_t pos, len;
  pic_vec *ret, *digit, *base;
  struct pic_bigint_t *retbi;

  cstr = pic_str_cstr(pic, str);
  pos = 0;
  len = pic_str_len(str);
  ret = pic_make_vec(pic, 0);
  base = pic_make_vec(pic, 1);
  base->data[0] = pic_int_value(10);
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi->signum = 0;

  if (cstr[0] == '-') {
    retbi->signum = 1;
    pos = 1;
  }
  if (pos == len) { // no digits
    pic_errorf(pic, "bigint-make: there are no digits");
  }
  for (; pos < len; ++pos) {
    char ch = cstr[pos];
    if (ch >= '0' && ch <= '9') {
      ret = bigint_vec_mul(pic, ret, base);
      digit = pic_make_vec(pic, 1);
      digit->data[0] = pic_int_value(ch - '0');
      if (ch != '0') {
	ret = bigint_vec_add(pic, ret, digit);
      }
    } else {
      //error
      pic_errorf(pic, "bigint-make: not a digit: %c", ch);
    }
  }

  if (ret->len == 0) {
    retbi->signum = 0;
  }
  retbi->digits = ret;
  return retbi;
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

/*
 * Calculates the quotient and the remainder and assign them to quo and rem.
 * If some error occurred, returned value is 0.
 * Otherwise, returned value is positive.
 * The sign of remainder is the same as that of v1 (the numerator).
 */
static int
bigint_div(pic_state *pic, struct pic_bigint_t *v1, struct pic_bigint_t *v2,
	   struct pic_bigint_t **quo, struct pic_bigint_t **rem)
{
  struct pic_bigint_t *quobi, *rembi;
  pic_vec *qv, *rv;

  if (v2->digits->len == 0) { // Division by zero
    pic_errorf(pic, "bigint_div: Division by zero");
  }

  bigint_vec_div(pic, v1->digits, v2->digits, &qv, &rv);

  quobi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  rembi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  
  quobi->signum = qv->len == 0 ? 0 : v1->signum ^ v2->signum;
  quobi->digits = qv;
  rembi->signum = rv->len == 0 ? 0 : v1->signum;
  rembi->digits = rv;

  *quo = quobi;
  *rem = rembi;
  return 0;
}

/*
 * assign bn1 + bn2 to bn1.
 */
static struct pic_bigint_t *
bigint_add_i(pic_state *pic, struct pic_bigint_t *bn1, struct pic_bigint_t *bn2)
{
  struct pic_bigint_t *retbi;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));

  if (bn1->signum != bn2->signum) {
    if (bigint_vec_lt(bn1->digits, bn2->digits)) { // bn2 wins
      retbi->signum = bn2->signum;
      retbi->digits = bigint_vec_sub(pic, bn2->digits, bn1->digits);
      goto end;
    }
    retbi->signum = bn1->signum;
    retbi->digits = bigint_vec_sub(pic, bn1->digits, bn2->digits);
    if (retbi->digits->len == 0) { // bn1 + bn2 == 0
      retbi->signum = 0;
    }
    goto end;
  }
  // two signums are equal
  retbi->signum = bn1->signum;
  retbi->digits = bigint_vec_add(pic, bn1->digits, bn2->digits);
  
 end:
  bn1->signum = retbi->signum;
  bn1->digits = retbi->digits;
  return bn1;
}
static struct pic_bigint_t *
bigint_mul_i(pic_state *pic, struct pic_bigint_t *v1, struct pic_bigint_t *v2)
{
  pic_vec *ret;

  ret = bigint_vec_mul(pic, v1->digits, v2->digits);

  v1->signum ^= v2->signum;
  if (ret->len == 0) {
    v1->signum = 0;
  }
  v1->digits = ret;
  return v1;
}

static bool
bigint_less(struct pic_bigint_t *val1, struct pic_bigint_t *val2) {
  if (val1->signum != val2->signum) { // signums differ
    return val2->signum;
  }
  return val1->signum ^ bigint_vec_lt(val1->digits, val2->digits);
}

static struct pic_bigint_t *
bigint_asl(pic_state *pic, struct pic_bigint_t *val, int sh)
{
  struct pic_bigint_t *retbi;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  if (sh <= 0) {
    retbi->signum = val->signum;
    retbi->digits = val->digits; // copy
    return retbi;
  }

  retbi->signum = val->signum;
  retbi->digits = bigint_vec_asl(pic, val->digits, sh);
  return retbi;
}

static double
bigint_to_double(struct pic_bigint_t *bi)
{
  double ret = 0, p = 1.0;
  int i;
  int len, lim;
  double base = (bigint_2digits) 1 << bigint_shift;

  if (bi->digits->len >= 1024 / bigint_shift + 1) { // max double value < 2^1024
    return bi->signum ? -1.0 / 0.0 : 1.0 / 0.0;
  }

  len = bi->digits->len;
  lim = 53 / bigint_shift + 1;
  if (lim > len) {
    lim = len;
  }
  for (i = 0; i < lim; ++i) {
    ret += (bigint_digit)pic_int(bi->digits->data[len - i - 1]) * p;
    p /= base;
  }
  if (bi->signum) {
    ret = -ret;
  }

  return ret * pow(base, len - 1);
}

/*
 * Take a value that contains a bigint or an int, and convert it to a bigint.
 */
static struct pic_bigint_t *
take_bigint_or_int(pic_state *pic, pic_value val)
{
  struct pic_bigint_t *bi;

  if (pic_int_p(val)) {
    int v = pic_int(val);
    bi = bigint_init_int(pic, v);
  } else {
    bi = pic_bigint_data_ptr(val);
  }

  return bi;
}

/*
 * make-bigint can take int or string as its argument.
 */
static pic_value
pic_big_number_make_bigint(pic_state *pic)
{
  pic_value value;
  struct pic_bigint_t *bi;

  pic_get_args(pic, "o", &value);

  if (pic_int_p(value)) {
    bi = bigint_init_int(pic, pic_int(value));
  } else if (pic_float_p(value)) {
    bi = bigint_init_int(pic, pic_float(value));
  } else if (pic_str_p(value)) {
    bi = bigint_init_str(pic, pic_str_ptr(value));
  } else {
    //error
    pic_errorf(pic, "make-bigint can take only int/string as its argument, but got: ~s", value);
  }
  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bi));
}

static pic_value
pic_big_number_bigint_add(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bigint_add(pic, bi1, bi2)));
}

static pic_value
pic_big_number_bigint_sub(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2, *result;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

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
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, bigint_mul(pic, bi1, bi2)));
}

static pic_value
pic_big_number_bigint_div(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2, *rem, *quo;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_div(pic, bi1, bi2, &quo, &rem);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, quo));
}
/*
 * The sign of remainder is the same as that of value1.
 */
static pic_value
pic_big_number_bigint_rem(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2, *rem, *quo;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_div(pic, bi1, bi2, &quo, &rem);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, rem));
}

static pic_value
pic_big_number_bigint_add_i(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  // Since bigint-add! modifies the first argument, it must be a bigint.
  if (! pic_bigint_p(value1)) {
    pic_errorf(pic, "The first argument of bigint-add! must be a bigint.");
  }
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_add_i(pic, bi1, bi2);

  return value1;
}

static pic_value
pic_big_number_bigint_sub_i(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  // Since bigint-sub! modifies the first argument, it must be a bigint.
  if (! pic_bigint_p(value1)) {
    pic_errorf(pic, "The first argument of bigint-sub! must be a bigint.");
  }
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = take_bigint_or_int(pic, value2);

  bi2->signum = 1 - bi2->signum;
  bigint_add_i(pic, bi1, bi2);
  bi2->signum = 1 - bi2->signum;

  return value1;
}

static pic_value
pic_big_number_bigint_mul_i(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  // Since bigint-mul! modifies the first argument, it must be a bigint.
  if (! pic_bigint_p(value1)) {
    pic_errorf(pic, "The first argument of bigint-mul! must be a bigint.");
  }
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_mul_i(pic, bi1, bi2);

  return value1;
}

/*
 * Returns a copy of underlying vector of given biginteger.
 */
static pic_value
pic_big_number_bigint_underlying(pic_state *pic)
{
  pic_value value;
  struct pic_bigint_t *bi;

  pic_get_args(pic, "o", &value);
  bi = take_bigint_or_int(pic, value);

  return pic_obj_value(bigint_vec_clone(pic, bi->digits));
}

static pic_value
pic_big_number_bigint_equal_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = take_bigint_or_int(pic, v1);
  bi2 = take_bigint_or_int(pic, v2);

  return pic_bool_value(bi1->signum == bi2->signum && bigint_vec_eq(bi1->digits, bi2->digits));
}

static pic_value
pic_big_number_bigint_less_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = take_bigint_or_int(pic, v1);
  bi2 = take_bigint_or_int(pic, v2);

  return pic_bool_value(bigint_less(bi1, bi2));
}

static pic_value
pic_big_number_bigint_asl(pic_state *pic)
{
  pic_value val;
  int sh;
  struct pic_bigint_t *result;

  pic_get_args(pic, "oi", &val, &sh);
  result = bigint_asl(pic, take_bigint_or_int(pic, val), sh);

  return pic_obj_value(pic_data_alloc(pic, &bigint_type, result));
}
static pic_value
pic_big_number_bigint_to_number(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *bi;
  double result;

  pic_get_args(pic, "o", &val);
  bi = take_bigint_or_int(pic, val);
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
    pic_defun(pic, "bigint-div", pic_big_number_bigint_div);
    pic_defun(pic, "bigint-rem", pic_big_number_bigint_rem);
    pic_defun(pic, "bigint-add!", pic_big_number_bigint_add_i);
    pic_defun(pic, "bigint-sub!", pic_big_number_bigint_sub_i);
    pic_defun(pic, "bigint-mul!", pic_big_number_bigint_mul_i);
    pic_defun(pic, "bigint-underlying", pic_big_number_bigint_underlying);
    pic_defun(pic, "bigint-equal?", pic_big_number_bigint_equal_p);
    pic_defun(pic, "bigint-less?", pic_big_number_bigint_less_p);
    pic_defun(pic, "bigint-asl", pic_big_number_bigint_asl);
    pic_defun(pic, "bigint->number", pic_big_number_bigint_to_number);
  }
}
