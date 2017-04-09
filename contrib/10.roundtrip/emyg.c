#include "picrin.h"

#include "emyg_dtoa.h"
#include "emyg_atod.h"

static int
int2str(long x, int base, char *buf)
{
  static const char digits[36] = "0123456789abcdefghijklmnopqrstuvwxyz";
  int i, neg, len;

  neg = 0;
  if (x < 0) {
    neg = 1;
    x = -x;
  }

  i = 0;
  do {
    buf[i++] = digits[x % base];
  } while ((x /= base) != 0);

  if (neg) {
    buf[i++] = '-';
  }
  buf[i] = '\0';
  len = i;

  for (i = 0; i < len / 2; ++i) {
    char tmp = buf[i];
    buf[i] = buf[len - i - 1];
    buf[len - i - 1] = tmp;
  }
  return len;
}

static pic_value
emyg_number_to_string(pic_state *pic)
{
  double f;
  bool e;
  int radix = 10;

  pic_get_args(pic, "F|i", &f, &e, &radix);

  if (radix < 2 || radix > 36) {
    pic_error(pic, "invalid radix (between 2 and 36, inclusive)", 1, pic_int_value(pic, radix));
  }

  if (e) {
    char buf[sizeof(int) * CHAR_BIT + 3];
    int len = int2str((int) f, radix, buf);
    return pic_str_value(pic, buf, len);
  }
  else {
    char buf[64];
    emyg_dtoa(f, buf);
    return pic_cstr_value(pic, buf);
  }
}

static bool
strcaseeq(const char *s1, const char *s2)
{
  char a, b;

  while ((a = *s1++) * (b = *s2++)) {
    if (tolower(a) != tolower(b))
      return false;
  }
  return a == b;
}

static pic_value
string_to_number(pic_state *pic, const char *str)
{
  double flt;
  const char *c = str;
  bool isint = 1;

  if (*c == '+' || *c == '-')
    c++;

  if (! isdigit(*c++)) {
    return pic_false_value(pic);
  }
  while (isdigit(*c)) c++;

  if (*c == '.') {
    isint = false;
    c++;
    while (isdigit(*c)) c++;
  }
  if (*c == 'e' || *c == 'E') {
    isint = false;
    c++;
    if (*c == '+' || *c == '-')
      c++;
    if (! isdigit(*c++)) {
      return pic_false_value(pic);
    }
    while (isdigit(*c)) c++;
  }

  if (*c != '\0') {
    return pic_false_value(pic);
  }

  flt = emyg_atod(str);

  if (isint && INT_MIN <= flt && flt <= INT_MAX) {
    return pic_int_value(pic, flt);
  } else {
    return pic_float_value(pic, flt);
  }
}

static pic_value
emyg_string_to_number(pic_state *pic)
{
  const char *str;
  int radix = 10;
  long num;
  char *eptr;

  pic_get_args(pic, "z|i", &str, &radix);

  if (strcaseeq(str, "+inf.0"))
    return pic_float_value(pic, 1.0 / 0.0);
  if (strcaseeq(str, "-inf.0"))
    return pic_float_value(pic, -1.0 / 0.0);
  if (strcaseeq(str, "+nan.0"))
    return pic_float_value(pic, 0.0 / 0.0);
  if (strcaseeq(str, "-nan.0"))
    return pic_float_value(pic, -0.0 / 0.0);

  num = strtol(str, &eptr, radix);
  if (*eptr == '\0') {
    return INT_MIN <= num && num <= INT_MAX ? pic_int_value(pic, num) : pic_float_value(pic, num);
  }

  return string_to_number(pic, str);
}

void
pic_nitro_init_roundtrip(pic_state *PIC_UNUSED(pic))
{
  pic_set(pic, "number->string", pic_lambda(pic, emyg_number_to_string, 0));
  pic_set(pic, "string->number", pic_lambda(pic, emyg_string_to_number, 0));
}
