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
static bool
bigint_buf_lt(int len1, const bigint_digit *buf1, int len2, const bigint_digit *buf2) {
  int j, len;

  len = len1;
  if (len < len2) {
    len = len2;
  }
  for (j = len - 1; j >= 0; --j) {
    bigint_digit d1 = j >= len1 ? 0 : buf1[j];
    bigint_digit d2 = j >= len2 ? 0 : buf2[j];
    if (d1 != d2) {
      return d1 < d2;
    }
  }

  return false;
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
  int i, j, len1, len2, k, quolen;
  bigint_digit msb2;
  bigint_digit *buf1, *buf2, *mulbuf;
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
  for (i = 0; i < len1; ++i) {
    buf1[i] = pic_int(pic, pic_vec_ref(pic, v1, i));
  }
  buf1[len1] = 0;
  for (i = 0; i < len2; ++i) {
    buf2[i] = pic_int(pic, pic_vec_ref(pic, v2, i));
  }
  quolen = len1 - len2 + 1;
  if (bigint_buf_lt(len2, buf1 + quolen - 1, len2, buf2)) {
    quolen--; // for exact estimation of pic_vec_len(quov)
  }
  if (quo) {
    quov = pic_make_vec(pic, quolen, NULL);
  }

  for (i = quolen - 1; i >= 0; --i) {
    bigint_2digits qq, q;
    bigint_2digits msb1 = buf1[i + len2 - 1];
    int buf1_avail = len2 + (i + len2 < len1 ? 1 : 0);
    // [buf1_avail, buf1_avail + i) is a subset of [0, len1 + 1)
    msb1 += i + len2 < len1 ? (bigint_2digits) buf1[i + len2] << bigint_shift : 0;

    q = qq = msb1 / ((bigint_2digits)msb2 + 1);
    for (j = 0; j < len1 + 1; ++j) {
      mulbuf[j] = 0;
    }
    bigint_buf_mul(len2, buf2, qq, mulbuf + i);
    bigint_buf_sub_ip(len1 + 1, buf1, len1 + 1, mulbuf);
    while (1) {
      bool lt = bigint_buf_lt(buf1_avail, buf1 + i, len2, buf2);
      if (lt) { // if buf1 < buf2 * 2^(bigint_shift * i)
	break;
      }
      bigint_buf_sub_ip(buf1_avail, buf1 + i, len2, buf2);
      q++;
    }
    assert (q - qq < 3);
    if (quo) {
      pic_vec_set(pic, quov, i, pic_int_value(pic, q));
    }
  }
  if (quo) {
    *quo = quov;
  }
  if (rem) {
    int remlen; // exact estimation of pic_vec_len(pic, remv)
    // buf1 (remainder) shift
    if (k > 0) {
      for (i = 0; i < len1; ++i) {
	if (i >= 1) {
	  buf1[i - 1] |= buf1[i] << (bigint_shift - k);
	}
	buf1[i] >>= k;
      }
    }
    remlen = len2;
    while (remlen >= 1 && buf1[remlen - 1] == 0) {
      remlen--;
    }
    remv = pic_make_vec(pic, remlen, NULL);
    for (i = 0; i < remlen; ++i) {
      pic_vec_set(pic, remv, i, pic_int_value(pic, buf1[i]));
    }
    *rem = remv;
  }
  free(buf1);
  free(buf2);
  free(mulbuf);
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
