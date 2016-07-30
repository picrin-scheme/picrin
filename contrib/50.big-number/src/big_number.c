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
bigint_mark(pic_state *pic, void *data, void (*gc_mark)(pic_state *, pic_value))
{
  struct pic_bigint_t *bi = (struct pic_bigint_t *)data;
  gc_mark(pic, bi->digits);
}

static const pic_data_type bigint_type = { "bigint", NULL, bigint_mark };
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
  bigint_diff pcarry; // for prediction
  int i, len;
  pic_value ret;

  if (pic_vec_len(pic, v1) == 0) {
    return v2;
  }
  if (pic_vec_len(pic, v2) == 0) {
    return v1;
  }
  // v1 > 0, v2 > 0
  len = pic_vec_len(pic, v1);
  if (len < pic_vec_len(pic, v2)) {
    len = pic_vec_len(pic, v2);
  }

  // exact prediction: checks if an extra digit is needed
  pcarry = (bigint_diff) 1 << bigint_shift;
  for (i = len - 1; i >= 0; --i) {
    bigint_digit d1 = i >= pic_vec_len(pic, v1) ? 0 : pic_int(pic, pic_vec_ref(pic, v1, i));
    bigint_digit d2 = i >= pic_vec_len(pic, v2) ? 0 : pic_int(pic, pic_vec_ref(pic, v2, i));
    pcarry -= d1;
    pcarry -= d2;
    if (pcarry < 0 || pcarry >= 2) {
      break;
    }
    pcarry <<= bigint_shift;
  }
  if (pcarry <= 0) {
    len++;
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
  int trim;

  len1 = pic_vec_len(pic, v1);
  len2 = pic_vec_len(pic, v2);
  if (len1 == 0) {
    return v1;
  }
  if (len2 == 0) {
    return v2;
  }
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

  trim = tmp[len1 + len2 - 1] ? 0 : 1;
  ret = pic_make_vec(pic, len1 + len2 - trim, NULL);
  for (i = 0; i < len1 + len2 - trim; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, tmp[i]));
  }

  free(tmp);
  return ret;
}

static int
bigint_vec_bit_length(pic_state *pic, const pic_value val);

/**
 * This function assumes buf1[0, len1) and out[0, len1 + 1) are available
 */
static void
bigint_buf_mul(int len1, const bigint_digit *buf1, bigint_digit v2,
	       bigint_digit *out)
{
  int j;
  bigint_2digits carry = 0;

  for (j = 0; j < len1 + 1; ++j) {
    out[j] = 0;
  }
  for (j = 0; j < len1; ++j) {
    carry += (bigint_2digits)buf1[j] * v2;
    out[j] = (bigint_digit) carry;
    carry >>= bigint_shift;
  }
  out[len1] = carry;
}

static void
bigint_buf_sub_ip(int len1, bigint_digit *buf1, int len2, const bigint_digit *buf2) {
  bigint_diff pcarry;
  int j;

  assert (len1 >= len2);
  pcarry = 0;
  for (j = 0; j < len1; ++j) {
    pcarry += buf1[j];
    if (j < len2) {
      pcarry -= buf2[j];
    }
    buf1[j] = (bigint_digit)pcarry;
    pcarry >>= bigint_shift;
  }

  assert (pcarry == 0);
}

/**
 * Computes v1 / v2 and stores the result to quo and rem.
 * quo and rem are nullable. If so, the corresponding result will be simply discarded.
 */
static void
bigint_vec_div(pic_state *pic, pic_value v1, pic_value v2,
	       pic_value *quo, pic_value *rem)
{
  pic_value quov, remv;
  int i, j, len1, len2, k, init;
  bigint_digit msb2;
  bigint_digit *buf1, *buf2, *quobuf = NULL, *mulbuf;
  assert (pic_vec_len(pic, v2) >= 1);

  {
    int bitlen1, bitlen2;
    bitlen1 = bigint_vec_bit_length(pic, v1);
    bitlen2 = bigint_vec_bit_length(pic, v2);

    if (bitlen1 == 0) {
      if (quo) {
	*quo = v1;
      }
      if (rem) {
	*rem = v1;
      }
      return;
    }

    if (bitlen1 < bitlen2) {
      if (quo) {
	*quo = pic_make_vec(pic, 0, NULL);
      }
      if (rem) {
	*rem = v1;
      }
      return;
    }

    // shift by k bits so that v2's msb is in [base / 2, base)
    // http://www.yamatyuu.net/computer/program/long/div/index.html
    k = (bigint_shift - bitlen2 % bigint_shift) % bigint_shift;
    v1 = bigint_vec_asl(pic, v1, k);
    v2 = bigint_vec_asl(pic, v2, k);
  }
  
  len1 = pic_vec_len(pic, v1);
  len2 = pic_vec_len(pic, v2);
  msb2 = pic_int(pic, pic_vec_ref(pic, v2, pic_vec_len(pic, v2) - 1));
  assert (msb2 > bigint_digit_max / 2);

  buf1 = (bigint_digit *) malloc((len1 + 1) * sizeof(bigint_digit));
  mulbuf = (bigint_digit *) malloc((len1 + 1) * sizeof(bigint_digit));
  buf2 = (bigint_digit *) malloc(len2 * sizeof(bigint_digit));
  init = len1 - len2 + 1;
  if (quo) {
    quobuf = (bigint_digit *) malloc(init * sizeof(bigint_digit));
  }
  for (i = 0; i < len1; ++i) {
    buf1[i] = pic_int(pic, pic_vec_ref(pic, v1, i));
  }
  buf1[len1] = 0;
  for (i = 0; i < len2; ++i) {
    buf2[i] = pic_int(pic, pic_vec_ref(pic, v2, i));
  }

  for (i = init - 1; i >= 0; --i) {
    bigint_2digits qq, q;
    bigint_2digits msb1 = buf1[i + len2 - 1];
    int buf1_avail = len2 + (i == init - 1 ? 0 : 1);
    // [buf1_avail, buf1_avail + i) is a subset of [0, len1 + 1)
    msb1 += i + len2 < len1 ? (bigint_2digits) buf1[i + len2] << bigint_shift : 0;

    q = qq = msb1 / ((bigint_2digits)msb2 + 1);
    for (j = 0; j < len1 + 1; ++j) {
      mulbuf[j] = 0;
    }
    bigint_buf_mul(len2, buf2, qq, mulbuf + i);
    bigint_buf_sub_ip(len1 + 1, buf1, len1 + 1, mulbuf);
    while (1) {
      bool lt = false;
      for (j = buf1_avail - 1; j >= 0; --j) {
	if (buf1[i + j] != (j == len2 ? 0 : buf2[j])) {
	  lt = buf1[i + j] < (j == len2 ? 0 : buf2[j]);
	  break;
	}
      }
      if (lt) { // if buf1 < buf2 * 2^(bigint_shift * i)
	break;
      }
      bigint_buf_sub_ip(buf1_avail, buf1 + i, len2, buf2);
      q++;
    }
    assert (q - qq < 3);
    if (quo) {
      quobuf[i] = q;
    }
  }
  if (quo) {
    quov = pic_make_vec(pic, init, NULL);
    for (i = 0; i < init; ++i) {
      pic_vec_set(pic, quov, i, pic_int_value(pic, quobuf[i]));
    }
    *quo = bigint_vec_compact(pic, quov);
  }
  if (rem) {
    // buf1 (remainder) shift
    if (k > 0) {
      for (i = 0; i < len1; ++i) {
	if (i >= 1) {
	  buf1[i - 1] |= buf1[i] << (bigint_shift - k);
	}
	buf1[i] >>= k;
      }
    }
    remv = pic_make_vec(pic, len1, NULL);
    for (i = 0; i < len1; ++i) {
      pic_vec_set(pic, remv, i, pic_int_value(pic, buf1[i]));
    }
    *rem = bigint_vec_compact(pic, remv);
  }
  free(buf1);
  free(buf2);
  free(mulbuf);
  free(quobuf);
}
static pic_value
bigint_vec_rem(pic_state *pic, pic_value v1, pic_value v2)
{
  pic_value remv;
  bigint_vec_div(pic, v1, v2, NULL, &remv);
  return remv;
}

static pic_value 
bigint_vec_mul_mod(pic_state *pic, const pic_value v1, const pic_value v2,
		   const pic_value mod)
{
  return bigint_vec_rem(pic, bigint_vec_mul(pic, v1, v2), mod);
  /*
    int len1, i, j;
    pic_value ret, cur, base;

    len1 = pic_vec_len(pic, v1);
    ret = pic_make_vec(pic, 0, NULL);
    cur = v2;
    base = pic_make_vec(pic, 2, NULL);
    pic_vec_set(pic, base, 0, pic_int_value(pic, 0));
    pic_vec_set(pic, base, 1, pic_int_value(pic, 1));

    for (i = 0; i < len1; ++i) {
    bigint_digit d1 = pic_int(pic, pic_vec_ref(pic, v1, i));
    pic_value digit = pic_make_vec(pic, 1, NULL);
    pic_vec_set(pic, digit, 0, pic_int_value(pic, d1));
    ret = bigint_vec_add(pic, ret, bigint_vec_mul(pic, cur, digit));
    ret = bigint_vec_rem(pic, ret, mod);
    cur = bigint_vec_mul(pic, cur, base);
    cur = bigint_vec_rem(pic, cur, mod);
    }

    return ret;
  */
}

static pic_value
bigint_vec_asl(pic_state *pic, const pic_value val, int sh)
{
  pic_value ret;
  int bitsh, bytesh;
  bigint_digit msb;
  bigint_2digits carry;
  int i, len, append;

  assert (sh >= 0);
  bitsh = sh % bigint_shift;
  bytesh = sh / bigint_shift;
  len = pic_vec_len(pic, val);
  if (len == 0) {
    return val;
  }
  msb = pic_int(pic, pic_vec_ref(pic, val, len - 1));
  carry = 0;

  if (bitsh == 0 || (msb >> (bigint_shift - bitsh)) == 0) {
    append = 0;
  } else {
    append = 1; // an extra digit is needed
  }
  ret = pic_make_vec(pic, len + bytesh + append, NULL);
  for (i = 0; i < bytesh; ++i) {
    pic_vec_set(pic, ret, i, pic_int_value(pic, 0));
  }
  for (i = 0; i < len + append; ++i) {
    if (i < len) {
      carry |= ((bigint_2digits) (bigint_digit) pic_int(pic, pic_vec_ref(pic, val, i))) << bitsh;
    }
    pic_vec_set(pic, ret, i + bytesh, pic_int_value(pic, carry & bigint_digit_max));
    carry >>= bigint_shift;
  }
  assert (carry == 0);

  return ret;
}

static bool
bigint_vec_bit_test(pic_state *pic, const pic_value val, int index)
{
  int bit, byte;

  assert (index >= 0);
  byte = index / bigint_shift;
  bit = index % bigint_shift;

  if (byte >= pic_vec_len(pic, val)) {
    return false;
  }

  bigint_digit d = pic_int(pic, pic_vec_ref(pic, val, byte));
  return (d & ((bigint_digit) 1 << bit)) != 0;
}
static int
bigint_vec_bit_length(pic_state *pic, pic_value val)
{
  int i;
  int len = pic_vec_len(pic, val);
  bigint_digit msb;

  if (len == 0) {
    return 0;
  }

  msb = pic_int(pic, pic_vec_ref(pic, val, len - 1));

  for (i = bigint_shift - 1; i >= 0; --i) {
    if (msb & ((bigint_digit) 1 << i)) {
      return bigint_shift * (len - 1) + i + 1;
    }
  }
  PIC_UNREACHABLE();
}

unsigned long genrand_int32(void); // in 30.random/src/mt19937ar.c


static pic_value
bigint_vec_rand(pic_state *pic, pic_value max)
{
  int i, len;
  pic_value ret;
  bigint_digit msb;
  
  len = pic_vec_len(pic, max);
  if (len == 0) {
    return max; // 0
  }
  msb = pic_int(pic, pic_vec_ref(pic, max, len - 1));

  while (1) {
    ret = pic_make_vec(pic, len, NULL);
    // TODO ugly random number generation!!
    for (i = 0; i < len - 1; ++i) {
      pic_vec_set(pic, ret, i, pic_int_value(pic, genrand_int32()));
    }
    pic_vec_set(pic, ret, len - 1, pic_int_value(pic, msb == bigint_digit_max ? genrand_int32() : (genrand_int32() % (msb + 1))));
    ret = bigint_vec_compact(pic, ret);
    if (bigint_vec_lt(pic, ret, max)) {
      return ret;
    }
  }
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
static struct pic_bigint_t *
bigint_rem(pic_state *pic, struct pic_bigint_t *v1, struct pic_bigint_t *v2)
{
  struct pic_bigint_t *rembi;
  pic_value rv;

  if (pic_vec_len(pic, v2->digits) == 0) { // Division by zero
    pic_error(pic, "bigint_rem: Division by zero", 0);
  }

  rv = bigint_vec_rem(pic, v1->digits, v2->digits);

  rembi = pic_malloc(pic, sizeof(struct pic_bigint_t));

  rembi->signum = pic_vec_len(pic, rv) == 0 ? 0 : v1->signum;
  rembi->digits = rv;

  return rembi;
}

static struct pic_bigint_t *
bigint_mul_mod(pic_state *pic, struct pic_bigint_t *v1, struct pic_bigint_t *v2,
	       struct pic_bigint_t *mod)
{
  struct pic_bigint_t *rembi;
  pic_value rv;

  if (pic_vec_len(pic, mod->digits) == 0) { // Division by zero
    pic_error(pic, "bigint_mul_mod: Division by zero", 0);
  }

  if (mod->signum) {
    pic_error(pic, "bigint_mul_mod: Divisor not positive", 0);
  }

  rv = bigint_vec_mul_mod(pic, v1->digits, v2->digits, mod->digits);

  rembi = pic_malloc(pic, sizeof(struct pic_bigint_t));

  rembi->signum = pic_vec_len(pic, rv) == 0 ? 0 : (v1->signum ^ v2->signum);
  rembi->digits = rv;

  return rembi;
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

static bool
bigint_bit_test(pic_state *pic, struct pic_bigint_t *val, int index)
{
  if (index < 0) {
    pic_error(pic, "bigint-bit-test: index must be >= 0", 1, pic_int_value(pic, index));
  }
  if (val->signum) {
    pic_error(pic, "bigint-bit-test does not support negative numbers", 0);
  }

  return bigint_vec_bit_test(pic, val->digits, index);
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

  cur = value->digits;
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

static struct pic_bigint_t *
bigint_rand(pic_state *pic, struct pic_bigint_t *bi)
{
  struct pic_bigint_t *retbi;

  retbi = pic_malloc(pic, sizeof(struct pic_bigint_t));
  retbi->signum = 0;
  retbi->digits = bigint_vec_rand(pic, bi->digits);

  return retbi;
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
  struct pic_bigint_t *bi1, *bi2, *rem;

  pic_get_args(pic, "oo", &value1, &value2);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);

  rem = bigint_rem(pic, bi1, bi2);

  return pic_data_value(pic, rem, &bigint_type);
}

/*
 * mod must be positive
 */
static pic_value
pic_big_number_bigint_mul_mod(pic_state *pic)
{
  pic_value value1, value2, mod;
  struct pic_bigint_t *bi1, *bi2, *modbi, *rem;

  pic_get_args(pic, "ooo", &value1, &value2, &mod);
  bi1 = take_bigint_or_int(pic, value1);
  bi2 = take_bigint_or_int(pic, value2);
  modbi = take_bigint_or_int(pic, mod);

  rem = bigint_mul_mod(pic, bi1, bi2, modbi);

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
pic_big_number_bigint_bit_test(pic_state *pic)
{
  pic_value val;
  int index;

  pic_get_args(pic, "oi", &val, &index);
  return pic_bool_value(pic, bigint_bit_test(pic, take_bigint_or_int(pic, val), index));
}

/**
 * Returns the minimum non-negative integer i s.t. |val| < 2^i.
 */
static pic_value
pic_big_number_bigint_bit_length(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *valbi;

  pic_get_args(pic, "o", &val);
  valbi = take_bigint_or_int(pic, val);
  return pic_int_value(pic, bigint_vec_bit_length(pic, valbi->digits));
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

static pic_value
pic_big_number_bigint_rand(pic_state *pic)
{
  pic_value val;
  struct pic_bigint_t *bi;
  struct pic_bigint_t *result;

  pic_get_args(pic, "o", &val);
  bi = take_bigint_or_int(pic, val);
  result = bigint_rand(pic, bi);

  return pic_data_value(pic, result, &bigint_type);
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
  pic_defun(pic, "bigint-mul-mod", pic_big_number_bigint_mul_mod);
  pic_defun(pic, "bigint-add!", pic_big_number_bigint_add_i);
  pic_defun(pic, "bigint-sub!", pic_big_number_bigint_sub_i);
  pic_defun(pic, "bigint-mul!", pic_big_number_bigint_mul_i);
  pic_defun(pic, "bigint-underlying", pic_big_number_bigint_underlying);
  pic_defun(pic, "bigint-equal?", pic_big_number_bigint_equal_p);
  pic_defun(pic, "bigint-less?", pic_big_number_bigint_less_p);
  pic_defun(pic, "bigint-asl", pic_big_number_bigint_asl);
  pic_defun(pic, "bigint-bit-test", pic_big_number_bigint_bit_test);
  pic_defun(pic, "bigint-bit-length", pic_big_number_bigint_bit_length);
  pic_defun(pic, "bigint->number", pic_big_number_bigint_to_number);
  pic_defun(pic, "bigint->string", pic_big_number_bigint_to_string);
  pic_defun(pic, "bigint-rand", pic_big_number_bigint_rand);
}
