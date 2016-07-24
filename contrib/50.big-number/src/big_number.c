#include "picrin.h"
#include "picrin/extra.h"
#include <math.h>
void chk_vc(pic_value v) {
  int i;
  int len = pic_vec_len(0, v);
  printf("[debug] len=%d ", len);
  for (i = 0; i < len; ++i) {
    printf("%8x ", pic_int(0, pic_vec_ref(0, v, i)));
  }
  printf("\n");
}

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
  (void) pic;
  (void) data;
  // TODO empty, memory-leaking
}

static const pic_data_type bigint_type = { "bigint", bigint_dtor, NULL };
#define pic_bigint_p(o) (pic_data_p(pic, (o), &bigint_type))
#define pic_bigint_data_ptr(o) ((struct pic_bigint_t *)pic_data(pic, o))

typedef unsigned bigint_digit;
typedef unsigned long long bigint_2digits;
typedef long long bigint_diff;
#define bigint_shift 32
#define bigint_digit_max 0xffffffffULL // : bigint_2digits

static pic_value
bigint_vec_from_int(pic_state *pic, int v)
{
  pic_value ret;

  assert (v >= 0);
  assert ((bigint_2digits) v <= bigint_digit_max);

  if (v == 0) {
    ret = pic_make_vec(pic, 0, NULL);
  } else {
    ret = pic_make_vec(pic, 1, NULL);
    pic_vec_set(pic, ret, 0, v);
  }
  pic_protect(pic, ret);
  return ret;
}

#define COMPACT(ret, ai) (ret = bigint_vec_compact(pic, ret), pic_leave(pic, ai), pic_protect(pic, ret))

static pic_value
bigint_vec_clone(pic_state *pic, const pic_value v) {
  size_t i;
  size_t len = pic_vec_len(pic, v);
  size_t ai;
  ai = pic_enter(pic);
  pic_value ret = pic_make_vec(pic, len, NULL);
  pic_protect(pic, ret);

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
  size_t ai;

  while (l >= 0 && pic_int(pic, pic_vec_ref(pic, v, l)) == 0) {
    --l;
  }
  ai = pic_enter(pic);
  pic_protect(pic, v);
  ret = pic_make_vec(pic, l + 1, NULL);
  for (i = 0; i <= l; ++i) {
    pic_vec_set(pic, ret, i, pic_vec_ref(pic, v, i));
  }

  pic_leave(pic, ai);
  pic_protect(pic, ret);
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
  size_t ai;

  ai = pic_enter(pic); 

  if (pic_vec_len(pic, v1) == 0) {
    ret = bigint_vec_clone(pic, v2);
    pic_protect(pic, ret);
    return ret;
  }
  if (pic_vec_len(pic, v2) == 0) {
    ret = bigint_vec_clone(pic, v1);
    pic_protect(pic, ret);
    return ret;
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
  pic_protect(pic, ret);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = i >= pic_vec_len(pic, v1) ? 0 : pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = i >= pic_vec_len(pic, v2) ? 0 : pic_int(pic, pic_vec_ref(pic, v2, i));
    carry += d1;
    carry += d2;
    pic_vec_set(pic, ret, i, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }

  assert (carry == 0);
  
  ret = bigint_vec_compact(pic, ret);
  pic_leave(pic, ai);
  pic_protect(pic, ret);

  return ret;
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
  size_t ai;

  assert (! bigint_vec_lt(pic, v1, v2)); // v1 >= v2
  ai = pic_enter(pic);
  len = pic_vec_len(pic, v1); // v1 must be larger than v2
  carry = 0;
  ret = pic_make_vec(pic, len, NULL);
  pic_protect(pic, ret);

  for (i = 0; i < len; ++i) {
    bigint_digit d1 = pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = i >= pic_vec_len(pic, v2) ? 0 : pic_int(pic, pic_vec_ref(pic, v2, i));
    carry += d1;
    carry -= d2;
    pic_vec_set(pic, ret, i, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }

  if (carry) {
    chk_vc(v1);
    chk_vc(v2);
    printf("carry=%d\n", carry);
  }
  assert (carry == 0);
  ret = bigint_vec_compact(pic, ret);
  pic_leave(pic, ai);
  pic_protect(pic, ret);

  return ret;
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
  size_t ai;

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
  ai = pic_enter(pic);
  pic_protect(pic, ret);
  for (i = 0; i < len1 + len2; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, tmp[i]));
  }

  free(tmp);
  COMPACT(ret, ai);

  return ret;
}

static void
bigint_vec_div(pic_state *pic, const pic_value v1, const pic_value v2,
	       pic_value *quo, pic_value *rem)
{
  pic_value quov, remv, one;
  size_t ai, ai2;
  int i;
  assert (pic_vec_len(pic, v2) >= 1);
 
  // Very slow, but still in polynomial time. :)
  ai = pic_enter(pic);
  quov = bigint_vec_from_int(pic, 0);
  remv = v1;
  one = bigint_vec_from_int(pic, 1);
  ai2 = pic_enter(pic);

  int init = bigint_shift * (pic_vec_len(pic, v1) - pic_vec_len(pic, v2) + 1);
  assert (bigint_vec_lt(pic, remv, bigint_vec_asl(pic, v2, init)));
  for (i = init - 1; i >= 0; --i) {
    pic_value sh = bigint_vec_asl(pic, v2, i);
    if (! bigint_vec_lt(pic, remv, sh)) { // 2^i * v2 <= rem
      remv = bigint_vec_sub(pic, remv, sh);
      quov = bigint_vec_add(pic, quov, bigint_vec_asl(pic, one, i));
    }
    assert (bigint_vec_lt(pic, remv, sh));
    pic_leave(pic, ai2);
    pic_protect(pic, quov);
    pic_protect(pic, remv);
  }

  pic_leave(pic, ai);
  pic_protect(pic, quov);
  pic_protect(pic, remv);
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
  size_t ai;

  assert (sh >= 0);
  bitsh = sh % bigint_shift;
  bytesh = sh / bigint_shift;
  carry = 0;

  ai = pic_enter(pic);
  len = pic_vec_len(pic, val);
  ret = pic_make_vec(pic, len + bytesh + 1, NULL);
  pic_protect(pic, ret);
  for (i = 0; i < bytesh; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, 0));
  }
  for (i = 0; i < len; ++i) {
    carry |= ((bigint_2digits) (bigint_digit) pic_int(pic, pic_vec_ref(pic, val, i))) << bitsh;
    pic_vec_set(pic, ret, i + bytesh, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }
  pic_vec_set(pic, ret, bytesh + len, pic_int_value(pic, carry));

  COMPACT(ret, ai);

  return ret;
}


/*
 * Creates a big integer by the given int value.
 */
static pic_value
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

  return pic_data_value(pic, bi, &bigint_type);
}

/* radix is in 2 ... 36 */
static pic_value
bigint_init_str(pic_state *pic, pic_value str, int radix)
{
  size_t pos, len;
  pic_value ret, digit, base;
  struct pic_bigint_t *retbi;
  size_t ai, ai2;

  pos = 0;
  ai = pic_enter(pic);
  len = pic_str_len(pic, str);
  ret = bigint_vec_from_int(pic, 0);
  base = bigint_vec_from_int(pic, radix);
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi->signum = 0;
  ai2 = pic_enter(pic);

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
      digit = bigint_vec_from_int(pic, dig);
      ret = bigint_vec_add(pic, ret, digit);
      pic_leave(pic, ai2);
      pic_protect(pic, ret);
    } else {
      //error
      pic_error(pic, "bigint-make: not a digit", 1, ch);
    }
  }

  if (pic_vec_len(pic, ret) == 0) {
    retbi->signum = 0;
  }
  retbi->digits = ret;
  pic_leave(pic, ai);
  pic_protect(pic, ret);
  return pic_data_value(pic, retbi, &bigint_type);
}

static pic_value
bigint_add(pic_state *pic, pic_value v1, pic_value v2)
{
  struct pic_bigint_t *retbi, *bn1, *bn2;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  bn1 = pic_bigint_data_ptr(v1);
  bn2 = pic_bigint_data_ptr(v2);

  if (bn1->signum != bn2->signum) {
    if (bigint_vec_lt(pic, bn1->digits, bn2->digits)) { // bn2 wins
      retbi->signum = bn2->signum;
      retbi->digits = bigint_vec_sub(pic, bn2->digits, bn1->digits);
      return pic_data_value(pic, retbi, &bigint_type);
    }
    retbi->signum = bn1->signum;
    retbi->digits = bigint_vec_sub(pic, bn1->digits, bn2->digits);
    if (pic_vec_len(pic, retbi->digits) == 0) { // bn1 + bn2 == 0
      retbi->signum = 0;
    }
    return pic_data_value(pic, retbi, &bigint_type);
  }
  // two signums are equal
  retbi->signum = bn1->signum;
  retbi->digits = bigint_vec_add(pic, bn1->digits, bn2->digits);

  return pic_data_value(pic, retbi, &bigint_type);
}

static pic_value 
bigint_mul(pic_state *pic, pic_value v1, pic_value v2)
{
  struct pic_bigint_t *retbi, *v1bi, *v2bi;
  pic_value ret;
  v1bi = pic_bigint_data_ptr(v1);
  v2bi = pic_bigint_data_ptr(v2);

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  ret = bigint_vec_mul(pic, v1bi->digits, v2bi->digits);

  retbi->signum = v1bi->signum ^ v2bi->signum;
  if (pic_vec_len(pic, ret) == 0) {
    retbi->signum = 0;
  }
  retbi->digits = ret;
  return pic_data_value(pic, retbi, &bigint_type);
}

/*
 * Calculates the quotient and the remainder and assign them to quo and rem.
 * If some error occurred, returned value is 0.
 * Otherwise, returned value is positive.
 * The sign of remainder is the same as that of v1 (the numerator).
 */
static int
bigint_div(pic_state *pic, pic_value v1, pic_value v2,
	   pic_value *quo, pic_value *rem)
{
  struct pic_bigint_t *v1bi, *v2bi;
  struct pic_bigint_t *quobi, *rembi;
  pic_value qv, rv;

  v1bi = pic_bigint_data_ptr(v1);
  v2bi = pic_bigint_data_ptr(v2);
  if (pic_vec_len(pic, v2bi->digits) == 0) { // Division by zero
    pic_error(pic, "bigint_div: Division by zero", 0);
  }

  bigint_vec_div(pic, v1bi->digits, v2bi->digits, &qv, &rv);

  quobi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  rembi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  
  quobi->signum = pic_vec_len(pic, qv) == 0 ? 0 : v1bi->signum ^ v2bi->signum;
  quobi->digits = qv;
  rembi->signum = pic_vec_len(pic, rv) == 0 ? 0 : v1bi->signum;
  rembi->digits = rv;

  *quo = pic_data_value(pic, quobi, &bigint_type);
  *rem = pic_data_value(pic, rembi, &bigint_type);
  return 0;
}

#if 0 // mutable unsupported now

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

#endif

static bool
bigint_less(pic_state *pic, pic_value v1, pic_value v2) {
  struct pic_bigint_t *val1 = pic_bigint_data_ptr(v1);
  struct pic_bigint_t *val2 = pic_bigint_data_ptr(v2);
  if (val1->signum != val2->signum) { // signums differ
    return val1->signum; // - < +, not + < -
  }
  return val1->signum ^ bigint_vec_lt(pic, val1->digits, val2->digits);
}

static pic_value
bigint_asl(pic_state *pic, pic_value v, int sh)
{
  struct pic_bigint_t *retbi;
  struct pic_bigint_t *val = pic_bigint_data_ptr(v);
  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  if (sh <= 0) {
    retbi->signum = val->signum;
    retbi->digits = val->digits; // copy
    return pic_data_value(pic, retbi, &bigint_type);
  }

  retbi->signum = val->signum;
  retbi->digits = bigint_vec_asl(pic, val->digits, sh);
  return pic_data_value(pic, retbi, &bigint_type);
}

static double
bigint_to_double(pic_state *pic, pic_value val)
{
  double ret = 0, p = 1.0;
  int i;
  int len, lim;
  double base = (bigint_2digits) 1 << bigint_shift;
  struct pic_bigint_t *bi = pic_bigint_data_ptr(val);

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
bigint_to_string(pic_state *pic, const pic_value val, int radix)
{
  const char digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
  size_t ai, ai2;
  char *buf;
  int len;
  int i, dstart, j;
  pic_value cur, rad;
  pic_value result;
  const struct pic_bigint_t *value = pic_bigint_data_ptr(val);

  if (pic_vec_len(pic, value->digits) == 0) { // value == 0.
    buf = malloc(1);
    buf[0] = '0';
    result = pic_str_value(pic, buf, 1);
    free(buf);
    return result;
  }

  ai = pic_enter(pic);
  len = ceil(bigint_shift * log(2) / log(radix)) * pic_vec_len(pic, value->digits) + 1;
  dstart = 0;
  buf = malloc(len);

  if (value->signum) {
    buf[0] = '-';
    dstart = 1;
  }

  cur = bigint_vec_clone(pic, value->digits);
  rad = bigint_vec_from_int(pic, radix);
  ai2 = pic_enter(pic);

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
    pic_leave(pic, ai2);
    pic_protect(pic, cur);
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
  pic_leave(pic, ai);
  pic_protect(pic, result);

  return result;
}

/* alternative of bigint_to_string. radix is forced to be 16. */
static pic_value
bigint_to_string_16(pic_state *pic, const pic_value val, int radix)
{
  (void) radix;
  const char digits[17] = "0123456789abcdef";
  char *buf;
  int len;
  int i, dstart, j, k;
  pic_value result;
  struct pic_bigint_t *value = pic_bigint_data_ptr(val);

  if (pic_vec_len(pic, value->digits) == 0) { // value == 0.
    buf = malloc(1);
    buf[0] = '0';
    result = pic_str_value(pic, buf, 1);
    free(buf);
    return result;
  }

  assert (bigint_shift == 32);
  len = 8 * pic_vec_len(pic, value->digits) + 1;
  dstart = 0;
  buf = malloc(len);

  if (value->signum) {
    buf[0] = '-';
    dstart = 1;
  }

  i = dstart;
  for (j = 0; j < len / 8; ++j) {
    bigint_digit d = pic_int_value(pic, pic_vec_ref(pic, value->digits, j));
    for (k = 0; k < 8; ++k) {
      buf[dstart + 8 * j + k] = digits[d % 16];
      d /= 16;
    }
  }
  i = dstart + 8 * (len / 8);
  while (i >= dstart && buf[i - 1] == '0') {
    i--;
  }
  assert (dstart <= i);
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
static pic_value
take_bigint_or_int(pic_state *pic, pic_value val)
{
  pic_value bi;

  if (pic_int_p(pic, val)) {
    int v = pic_int(pic, val);
    bi = bigint_init_int(pic, v);
    pic_protect(pic, bi);
  } else {
    bi = val;
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
  pic_value bi;

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
  return bi;
}

/*
 * make-bigint-radix takes a string and radix and returns a bigint.
 */
static pic_value
pic_big_number_make_bigint_radix(pic_state *pic) {
  pic_value value;
  pic_value bi;
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

  return bi;
}

static pic_value
pic_big_number_bigint_add(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2, result;
  size_t ai;

  pic_get_args(pic, "oo", &value1, &value2);

  ai = pic_enter(pic);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  result = bigint_add(pic, bi1, bi2);
  pic_leave(pic, ai);
  pic_protect(pic, result);

  return result;
}

static pic_value
pic_big_number_bigint_sub(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2, result;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  if (bi1 != bi2) { // destructive change of bi2 will not cause problems
    struct pic_bigint_t *bi2_ptr = pic_bigint_data_ptr(bi2);
    bi2_ptr->signum = 1 - bi2_ptr->signum;
    result = bigint_add(pic, bi1, bi2);
    bi2_ptr->signum = 1 - bi2_ptr->signum;
  } else {
    // 0
    return bigint_init_int(pic, 0);
  }
  return result;
}

static pic_value
pic_big_number_bigint_mul(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  return bigint_mul(pic, bi1, bi2);
}

static pic_value
pic_big_number_bigint_div(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2, rem, quo;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_div(pic, bi1, bi2, &quo, &rem);

  return quo;
}
/*
 * The sign of remainder is the same as that of value1.
 */
static pic_value
pic_big_number_bigint_rem(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2, rem, quo;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_div(pic, bi1, bi2, &quo, &rem);

  return rem;
}

#if 0 // i
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
  assert (!"add! is suppressed");

  /*
  bi1 = pic_bigint_data_ptr(value1);
  bi2 = take_bigint_or_int(pic, value2);

  bigint_add_i(pic, bi1, bi2);
  */

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

  assert (!"sub! is suppressed");

  bi2->signum = 1 - bi2->signum;
  bigint_add_i(pic, bi1, bi2);
  bi2->signum = 1 - bi2->signum;

  return value1;
}

static pic_value
pic_big_number_bigint_mul_i(pic_state *pic)
{
  pic_value value1, value2;
  pic_value bi1, bi2;

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
#endif // i


/*
 * Returns a copy of underlying vector of given biginteger.
 */
static pic_value
pic_big_number_bigint_underlying(pic_state *pic)
{
  pic_value value;
  pic_value bi;

  pic_get_args(pic, "o", &value);
  bi = take_bigint_or_int(pic, value);

  return bigint_vec_clone(pic, pic_bigint_data_ptr(bi)->digits);
}

static pic_value
pic_big_number_bigint_equal_p(pic_state *pic)
{
  pic_value v1, v2;
  struct pic_bigint_t *bi1, *bi2;

  pic_get_args(pic, "oo", &v1, &v2);
  bi1 = pic_bigint_data_ptr(take_bigint_or_int(pic, v1));
  bi2 = pic_bigint_data_ptr(take_bigint_or_int(pic, v2));

  return pic_bool_value(pic, bi1->signum == bi2->signum && bigint_vec_eq(pic, bi1->digits, bi2->digits));
}

static pic_value
pic_big_number_bigint_less_p(pic_state *pic)
{
  pic_value v1, v2;
  pic_value bi1, bi2;

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
  pic_value result;

  pic_get_args(pic, "oi", &val, &sh);
  result = bigint_asl(pic, take_bigint_or_int(pic, val), sh);

  return result;
}
static pic_value
pic_big_number_bigint_to_number(pic_state *pic)
{
  pic_value val;
  pic_value bi;
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
  pic_value bi;
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
  /*
  pic_defun(pic, "bigint-add!", pic_big_number_bigint_add_i);
  pic_defun(pic, "bigint-sub!", pic_big_number_bigint_sub_i);
  pic_defun(pic, "bigint-mul!", pic_big_number_bigint_mul_i);
  */
  pic_defun(pic, "bigint-underlying", pic_big_number_bigint_underlying);
  pic_defun(pic, "bigint-equal?", pic_big_number_bigint_equal_p);
  pic_defun(pic, "bigint-less?", pic_big_number_bigint_less_p);
  pic_defun(pic, "bigint-asl", pic_big_number_bigint_asl);
  pic_defun(pic, "bigint->number", pic_big_number_bigint_to_number);
  pic_defun(pic, "bigint->string", pic_big_number_bigint_to_string);
}
