#include "picrin.h"
#include "picrin/extra.h"
#include <math.h>

/**
 * Big integer is represented as a vector of digits.
 * A digit is 32-bit long, 0 ~ 2^32 - 1.
 */

struct pic_bigint_t {
  int signum;
  pic_value digits;
};

static void
bigint_dtor(pic_state *pic, void *data)
{
  pic_value v = ((struct pic_bigint_t *)data)->digits;
  for (int i = 0; i < pic_vec_len(pic, v); ++i) {
    pic_vec_set(pic, v, i, pic_undef_value(pic));
  }
}

static void
bigint_mark(pic_state *pic, void *data, void (*gc_mark)(pic_state *, pic_value))
{
  struct pic_bigint_t *bi = (struct pic_bigint_t *)data;
  gc_mark(pic, bi->digits);
}

static const pic_data_type bigint_type = { "bigint", bigint_dtor, bigint_mark };
#define pic_bigint_p(o) (pic_data_p(pic, (o), &bigint_type))
#define pic_bigint_data_ptr(o) ((struct pic_bigint_t *)pic_data(pic, o))

typedef unsigned bigint_digit;
typedef unsigned long long bigint_2digits;
typedef long long bigint_diff;
#define bigint_shift 32
#define bigint_digit_max 0xffffffffULL // : bigint_2digits

static pic_value
bigint_vec_clone(pic_state *pic, const pic_value v) {
  size_t i;
  size_t len = pic_vec_len(pic, v);
  pic_value ret = pic_make_vec(pic, len, NULL);

  for (i = 0; i < len; ++i) {
    pic_vec_set(pic, ret, i, pic_vec_ref(pic, v, i));
  }

  return ret;
}

/*
 * Eliminates all leading zeroes.
 */
static pic_value
bigint_vec_compact(pic_state *pic, const pic_value v)
{
  int i;
  int l = pic_vec_len(pic, v) - 1;
  pic_value ret;

  while (l >= 0 && pic_int(pic, pic_vec_ref(pic, v, l)) == 0) {
    --l;
  }
  ret = pic_make_vec(pic, l + 1, NULL);
  for (i = 0; i <= l; ++i) {
    pic_vec_set(pic, ret, i, pic_vec_ref(pic, v, i));
  }

  return ret;
}


/*
 * Checks whether v1 and v2 represents the same value.
 */
static bool
bigint_vec_eq(pic_state *pic, const pic_value v1, const pic_value v2)
{
  int i;
  if (pic_vec_len(pic, v1) != pic_vec_len(pic, v2)) {
    return false;
  }
  for (i = pic_vec_len(pic, v1) - 1; i >= 0; --i) {
    if (pic_int(pic, pic_vec_ref(pic, v1, i)) != pic_int(pic, pic_vec_ref(pic, v2, i))) {
      return false;
    }
  }

  return true;
}

static bool
bigint_vec_lt(pic_state *pic, const pic_value v1, const pic_value v2)
{
  int i;
  if (pic_vec_len(pic, v1) != pic_vec_len(pic, v2)) {
    return pic_vec_len(pic, v1) < pic_vec_len(pic, v2);
  }
  for (i = pic_vec_len(pic, v1) - 1; i >= 0; --i) {
    bigint_digit d1 = pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = pic_int(pic, pic_vec_ref(pic, v2, i));
    if (d1 != d2) {
      return d1 < d2;
    }
  }

  return false;
}

static pic_value
bigint_vec_add(pic_state *pic, const pic_value v1, const pic_value v2)
{
  bigint_2digits carry;
  bigint_digit msb1, msb2;
  int i, len;
  pic_value ret;

  if (pic_vec_len(pic, v1) == 0) {
    return bigint_vec_clone(pic, v2);
  }
  if (pic_vec_len(pic, v2) == 0) {
    return bigint_vec_clone(pic, v1);
  }
  // v1 > 0, v2 > 0
  len = pic_vec_len(pic, v1);
  if (len < pic_vec_len(pic, v2)) {
    len = pic_vec_len(pic, v2);
  }
  msb1 = pic_int(pic, pic_vec_ref(pic, v1, pic_vec_len(pic, v1) - 1));
  msb2 = pic_int(pic, pic_vec_ref(pic, v2, pic_vec_len(pic, v2) - 1));
  if ((bigint_2digits)msb1 + msb2 >= bigint_digit_max) {
    ++len;
  }
  carry = 0;
  ret = pic_make_vec(pic, len, NULL);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = i >= pic_vec_len(pic, v1) ? 0 : pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = i >= pic_vec_len(pic, v2) ? 0 : pic_int(pic, pic_vec_ref(pic, v2, i));
    carry += d1;
    carry += d2;
    pic_vec_set(pic, ret, i, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }

  assert (carry == 0);
  
  return bigint_vec_compact(pic, ret);
}

/*
 * Precondition: v1 >= v2
 */
static pic_value 
bigint_vec_sub(pic_state *pic, const pic_value v1, const pic_value v2)
{
  bigint_diff carry;
  int i, len;
  pic_value ret;

  assert (! bigint_vec_lt(pic, v1, v2)); // v1 >= v2
  len = pic_vec_len(pic, v1); // v1 must be larger than v2
  carry = 0;
  ret = pic_make_vec(pic, len, NULL);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = i >= pic_vec_len(pic, v2) ? 0 : pic_int(pic, pic_vec_ref(pic, v2, i));
    carry += d1;
    carry -= d2;
    pic_vec_set(pic, ret, i, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }

  assert (carry == 0);
  return bigint_vec_compact(pic, ret);
}

static pic_value 
bigint_vec_asl(pic_state *pic, const pic_value val, int sh);


/*
 * Classical algorithm. O(n^2)
 */
static pic_value 
bigint_vec_mul(pic_state *pic, const pic_value v1, const pic_value v2)
{
  int len1, len2, i, j;
  pic_value ret;
  bigint_digit *tmp;
  bigint_2digits carry;

  len1 = pic_vec_len(pic, v1);
  len2 = pic_vec_len(pic, v2);
  tmp = (bigint_digit *) malloc((len1 + len2) * sizeof(bigint_digit));
  carry = 0;

  for (i = 0; i < len1 + len2; ++i) {
    tmp[i] = 0;
  }

  for (i = 0; i < len1; ++i) {
    bigint_digit d1 = pic_int(pic, pic_vec_ref(pic, v1, i));
    carry = 0;
    for (j = 0; j < len2; ++j) {
      carry += tmp[i + j];
      bigint_digit d2 = pic_int(pic, pic_vec_ref(pic, v2, j));
      carry += (bigint_2digits) d1 * d2;
      tmp[i + j] = carry & bigint_digit_max;
      carry >>= bigint_shift;
    }
    tmp[i + len2] = carry;
  }

  ret = pic_make_vec(pic, len1 + len2, NULL);
  for (i = 0; i < len1 + len2; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, tmp[i]));
  }

  free(tmp);
  return bigint_vec_compact(pic, ret);
}

static void
bigint_vec_div(pic_state *pic, const pic_value v1, const pic_value v2,
	       pic_value *quo, pic_value *rem)
{
  pic_value quov, remv, one;
  int i;
  assert (pic_vec_len(pic, v2) >= 1);
 
  // Very slow, but still in polynomial time. :)
  quov = pic_make_vec(pic, 0, NULL);
  remv = v1;
  one = pic_make_vec(pic, 1, NULL);
  pic_vec_set(pic, one, 0, pic_int_value(pic, 1));

  int init = bigint_shift * (pic_vec_len(pic, v1) - pic_vec_len(pic, v2) + 1);
  assert (bigint_vec_lt(pic, remv, bigint_vec_asl(pic, v2, init)));
  for (i = init - 1; i >= 0; --i) {
    pic_value sh = bigint_vec_asl(pic, v2, i);
    if (! bigint_vec_lt(pic, remv, sh)) { // 2^i * v2 <= rem
      remv = bigint_vec_sub(pic, remv, sh);
      quov = bigint_vec_add(pic, quov, bigint_vec_asl(pic, one, i)); // [suspect]!!
    }
    assert (bigint_vec_lt(pic, remv, sh));
  }

  *quo = quov;
  *rem = remv;
}

static pic_value 
bigint_vec_asl(pic_state *pic, const pic_value val, int sh)
{
  pic_value ret;
  int bitsh, bytesh;
  bigint_2digits carry;
  int i, len;

  assert (sh >= 0);
  bitsh = sh % bigint_shift;
  bytesh = sh / bigint_shift;
  carry = 0;

  len = pic_vec_len(pic, val);
  ret = pic_make_vec(pic, len + bytesh + 1, NULL);
  for (i = 0; i < bytesh; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, 0));
  }
  for (i = 0; i < len; ++i) {
    carry |= ((bigint_2digits) (bigint_digit) pic_int(pic, pic_vec_ref(pic, val, i))) << bitsh;
    pic_vec_set(pic, ret, i + bytesh, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }
  pic_vec_set(pic, ret, bytesh + len, pic_int_value(pic, carry));

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
  pic_value bn = pic_make_vec(pic, s, NULL);
  struct pic_bigint_t *bi;

  bi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  bi->signum = value < 0;
  if (value < 0) {
    value = -value;
  }

  for (i = 0; i < s; ++i) {
    pic_vec_set(pic, bn, i, pic_int_value(pic, (value >> (bigint_shift * i)) & bigint_digit_max));
  }
  bi->digits = bigint_vec_compact(pic, bn);

  return bi;
}

/* radix is in 2 ... 36 */
static struct pic_bigint_t *
bigint_init_str(pic_state *pic, pic_value str, int radix)
{
  size_t pos, len;
  pic_value ret, digit, base;
  struct pic_bigint_t *retbi;

  pos = 0;
  len = pic_str_len(pic, str);
  ret = pic_make_vec(pic, 0, NULL);
  base = pic_make_vec(pic, 1, NULL);
  pic_vec_set(pic, base, 0, pic_int_value(pic, radix)); // radix is a one-digit number
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi->signum = 0;

  if (pic_str_ref(pic, str, 0) == '-') {
    retbi->signum = 1;
    pos = 1;
  }
  if (pos == len) { // no digits
    pic_error(pic, "make-bigint: there are no digits", 0);
  }
  for (; pos < len; ++pos) {
    char ch = pic_str_ref(pic, str, pos);
    if ((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' || ch <= 'Z')) {
      int dig;

      if (ch >= '0' && ch <= '9') {
	dig = ch - '0';
      } else if (ch >= 'a' && ch <= 'z') {
	dig = ch - 'a' + 10;
      } else {
	dig = ch - 'A' + 10;
      }

      if (dig >= radix) {
	pic_error(pic, "bigint-make: digit out of range", 0);
      }

      ret = bigint_vec_mul(pic, ret, base);
      digit = pic_make_vec(pic, 1, NULL);
      pic_vec_set(pic, digit, 0, pic_int_value(pic, dig));
      if (ch != '0') {
	ret = bigint_vec_add(pic, ret, digit);
      }
    } else {
      //error
      pic_error(pic, "bigint-make: not a digit", 1, ch);
    }
  }

  if (pic_vec_len(pic, ret) == 0) {
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
    if (bigint_vec_lt(pic, bn1->digits, bn2->digits)) { // bn2 wins
      retbi->signum = bn2->signum;
      retbi->digits = bigint_vec_sub(pic, bn2->digits, bn1->digits);
      return retbi;
    }
    retbi->signum = bn1->signum;
    retbi->digits = bigint_vec_sub(pic, bn1->digits, bn2->digits);
    if (pic_vec_len(pic, retbi->digits) == 0) { // bn1 + bn2 == 0
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
  pic_value ret;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  ret = bigint_vec_mul(pic, v1->digits, v2->digits);

  retbi->signum = v1->signum ^ v2->signum;
  if (pic_vec_len(pic, ret) == 0) {
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
  pic_value qv, rv;

  if (pic_vec_len(pic, v2->digits) == 0) { // Division by zero
    pic_error(pic, "bigint_div: Division by zero", 0);
  }

  bigint_vec_div(pic, v1->digits, v2->digits, &qv, &rv);

  quobi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  rembi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  
  quobi->signum = pic_vec_len(pic, qv) == 0 ? 0 : v1->signum ^ v2->signum;
  quobi->digits = qv;
  rembi->signum = pic_vec_len(pic, rv) == 0 ? 0 : v1->signum;
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
    if (bigint_vec_lt(pic, bn1->digits, bn2->digits)) { // bn2 wins
      retbi->signum = bn2->signum;
      retbi->digits = bigint_vec_sub(pic, bn2->digits, bn1->digits);
      goto end;
    }
    retbi->signum = bn1->signum;
    retbi->digits = bigint_vec_sub(pic, bn1->digits, bn2->digits);
    if (pic_vec_len(pic, retbi->digits) == 0) { // bn1 + bn2 == 0
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
  pic_value ret;

  ret = bigint_vec_mul(pic, v1->digits, v2->digits);

  v1->signum ^= v2->signum;
  if (pic_vec_len(pic, ret) == 0) {
    v1->signum = 0;
  }
  v1->digits = ret;
  return v1;
}

static bool
bigint_less(pic_state *pic, struct pic_bigint_t *val1, struct pic_bigint_t *val2) {
  if (val1->signum != val2->signum) { // signums differ
    return val1->signum; // - < +, not + < -
  }
  return val1->signum ^ bigint_vec_lt(pic, val1->digits, val2->digits);
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
bigint_to_double(pic_state *pic, struct pic_bigint_t *bi)
{
  double ret = 0, p = 1.0;
  int i;
  int len, lim;
  double base = (bigint_2digits) 1 << bigint_shift;

  if (pic_vec_len(pic, bi->digits) >= 1024 / bigint_shift + 1) { // max double value < 2^1024
    return bi->signum ? -1.0 / 0.0 : 1.0 / 0.0;
  }

  len = pic_vec_len(pic, bi->digits);
  lim = 53 / bigint_shift + 1;
  if (lim > len) {
    lim = len;
  }
  for (i = 0; i < lim; ++i) {
    ret += (bigint_digit)pic_int(pic, pic_vec_ref(pic, bi->digits, len - i - 1)) * p;
    p /= base;
  }
  if (bi->signum) {
    ret = -ret;
  }

  return ret * pow(base, len - 1);
}

static pic_value
bigint_to_string(pic_state *pic, const struct pic_bigint_t *value, int radix)
{
  const char digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
  char *buf;
  int len;
  int i, dstart, j;
  pic_value cur, rad;
  pic_value result;

  if (pic_vec_len(pic, value->digits) == 0) { // value == 0.
    buf = malloc(1);
    buf[0] = '0';
    result = pic_str_value(pic, buf, 1);
    free(buf);
    return result;
  }

  len = ceil(bigint_shift * log(2) / log(radix)) * pic_vec_len(pic, value->digits) + 1;
  dstart = 0;
  buf = malloc(len);

  if (value->signum) {
    buf[0] = '-';
    dstart = 1;
  }

  cur = bigint_vec_clone(pic, value->digits);
  rad = pic_make_vec(pic, 1, NULL);
  pic_vec_set(pic, rad, 0, pic_int_value(pic, radix)); // radix is 10 .. 36

  i = dstart;
  while (pic_vec_len(pic, cur) > 0) { // put digits in the reverse order
    pic_value quo, rem;
    int d;
    bigint_vec_div(pic, cur, rad, &quo, &rem);
    if (pic_vec_len(pic, rem) == 0) {
      d = 0;
    } else {
      d = pic_int(pic, pic_vec_ref(pic, rem, 0));
    }
    assert (0 <= d && d < radix);
    buf[i++] = digits[d];
    cur = quo;
  }
  assert (i <= len);

  // reverse digits
  for (j = 0; j < (i - dstart) / 2; ++j) {
    char tmp = buf[dstart + j];
    buf[dstart + j] = buf[i - 1 - j];
    buf[i - 1 - j] = tmp;
  }

  result = pic_str_value(pic, buf, i);
  free(buf);

  return result;
}


/*
 * Take a value that contains a bigint or an int, and convert it to a bigint.
 */
static struct pic_bigint_t *
take_bigint_or_int(pic_state *pic, pic_value val)
{
  struct pic_bigint_t *bi;

  if (pic_int_p(pic, val)) {
    int v = pic_int(pic, val);
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

  if (pic_int_p(pic, value)) {
    bi = bigint_init_int(pic, pic_int(pic, value));
  } else if (pic_float_p(pic, value)) {
    bi = bigint_init_int(pic, pic_float(pic, value));
  } else if (pic_str_p(pic, value)) {
    bi = bigint_init_str(pic, value, 10);
  } else {
    //error
    pic_error(pic, "make-bigint can take only int/string as its argument, but got", 1, value);
  }
  return pic_data_value(pic, bi, &bigint_type);
}

/*
 * make-bigint-radix takes a string and radix and returns a bigint.
 */
static pic_value
pic_big_number_make_bigint_radix(pic_state *pic) {
  pic_value value;
  struct pic_bigint_t *bi;
  int radix;

  pic_get_args(pic, "oi", &value, &radix);

  if (radix <= 1 || radix >= 37) {
    pic_error(pic, "make-bigint-radix: radix out of range", 0);
  }

  if (pic_str_p(pic, value)) {
    bi = bigint_init_str(pic, value, radix);
  } else {
    //error
    pic_error(pic, "make-bigint-radix can take only string as its argument, but got:", 1, value);
  }

  return pic_data_value(pic, bi, &bigint_type);
}

static pic_value
pic_big_number_bigint_add(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  return pic_data_value(pic, bigint_add(pic, bi1, bi2), &bigint_type);
}

static pic_value
pic_big_number_bigint_sub(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2, *result;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  if (bi1 != bi2) { // destructive change of bi2 will not cause problems
    bi2->signum = 1 - bi2->signum;
    result = bigint_add(pic, bi1, bi2);
    bi2->signum = 1 - bi2->signum;
  } else {
    // 0
    return pic_data_value(pic, bigint_init_int(pic, 0), &bigint_type);
  }
  return pic_data_value(pic, result, &bigint_type);
}

static pic_value
pic_big_number_bigint_mul(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  return pic_data_value(pic, bigint_mul(pic, bi1, bi2), &bigint_type);
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

  return pic_data_value(pic, quo, &bigint_type);
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

  return pic_data_value(pic, rem, &bigint_type);
}

static pic_value
pic_big_number_bigint_add_i(pic_state *pic)
{
  pic_value value1, value2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &value1, &value2);

  // Since bigint-add! modifies the first argument, it must be a bigint.
  if (! pic_bigint_p(value1)) {
    pic_error(pic, "The first argument of bigint-add! must be a bigint.", 0);
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
    pic_error(pic, "The first argument of bigint-sub! must be a bigint.", 0);
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
    pic_error(pic, "The first argument of bigint-mul! must be a bigint.", 0);
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

  return bigint_vec_clone(pic, bi->digits);
}

static pic_value
pic_big_number_bigint_equal_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = take_bigint_or_int(pic, v1);
  bi2 = take_bigint_or_int(pic, v2);

  return pic_bool_value(pic, bi1->signum == bi2->signum && bigint_vec_eq(pic, bi1->digits, bi2->digits));
}

static pic_value
pic_big_number_bigint_less_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = take_bigint_or_int(pic, v1);
  bi2 = take_bigint_or_int(pic, v2);

  return pic_bool_value(pic, bigint_less(pic, bi1, bi2));
}

static pic_value
pic_big_number_bigint_asl(pic_state *pic)
{
  pic_value val;
  int sh;
  struct pic_bigint_t *result;

  pic_get_args(pic, "oi", &val, &sh);
  result = bigint_asl(pic, take_bigint_or_int(pic, val), sh);

  return pic_data_value(pic, result, &bigint_type);
}
static pic_value
pic_big_number_bigint_to_number(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *bi;
  double result;

  pic_get_args(pic, "o", &val);
  bi = take_bigint_or_int(pic, val);
  result = bigint_to_double(pic, bi);

  return pic_float_value(pic, result);
}
static pic_value
pic_big_number_bigint_to_string(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *bi;
  pic_value result;
  int radix;
  int num_args;

  num_args = pic_get_args(pic, "o|i", &val, &radix);

  switch (num_args) {
  case 1:
    radix = 10;
    break;
  case 2:
    break;
  }

  if (radix <= 1 || radix >= 37) {
    pic_error(pic, "bigint->string: radix out of range", 0);
  }

  bi = take_bigint_or_int(pic, val);
  result = bigint_to_string(pic, bi, radix);

  return result;
}

void
pic_init_big_number(pic_state *pic)
{
  pic_deflibrary(pic, "picrin.big-number");
  pic_defun(pic, "make-bigint", pic_big_number_make_bigint);
  pic_defun(pic, "make-bigint-radix", pic_big_number_make_bigint_radix);
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
  pic_defun(pic, "bigint->string", pic_big_number_bigint_to_string);
}
