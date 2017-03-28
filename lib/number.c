/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
pic_number_number_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_float_p(pic, v) || pic_int_p(pic, v));
}

static pic_value
pic_number_exact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_int_p(pic, v));
}

static pic_value
pic_number_inexact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_float_p(pic, v));
}

static pic_value
pic_number_inexact(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(pic, f);
}

static pic_value
pic_number_exact(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_int_value(pic, (int)f);
}

#define pic_define_aop(name, op, guard)                                 \
  pic_value                                                             \
  name(pic_state *pic, pic_value a, pic_value b)                        \
  {                                                                     \
    double f;                                                           \
    if (pic_int_p(pic, a) && pic_int_p(pic, b)) {                       \
      f = (double)pic_int(pic, a) op (double)pic_int(pic, b);           \
      return (INT_MIN <= f && f <= INT_MAX && guard)                    \
        ? pic_int_value(pic, (int)f)                                    \
        : pic_float_value(pic, f);                                      \
    } else if (pic_float_p(pic, a) && pic_float_p(pic, b)) {            \
      return pic_float_value(pic, pic_float(pic, a) op pic_float(pic, b)); \
    } else if (pic_int_p(pic, a) && pic_float_p(pic, b)) {              \
      return pic_float_value(pic, pic_int(pic, a) op pic_float(pic, b)); \
    } else if (pic_float_p(pic, a) && pic_int_p(pic, b)) {              \
      return pic_float_value(pic, pic_float(pic, a) op pic_int(pic, b)); \
    } else {                                                            \
      pic_error(pic, #name ": non-number operand given", 0);              \
    }                                                                   \
    PIC_UNREACHABLE();                                                  \
  }

pic_define_aop(pic_add, +, true)
pic_define_aop(pic_sub, -, true)
pic_define_aop(pic_mul, *, true)
pic_define_aop(pic_div, /, f == (int)f)

#define pic_define_cmp(name, op)                                        \
  bool                                                                  \
  name(pic_state *pic, pic_value a, pic_value b)                        \
  {                                                                     \
    if (pic_int_p(pic, a) && pic_int_p(pic, b)) {                       \
      return pic_int(pic, a) op pic_int(pic, b);                        \
    } else if (pic_float_p(pic, a) && pic_float_p(pic, b)) {            \
      return pic_float(pic, a) op pic_float(pic, b);                    \
    } else if (pic_int_p(pic, a) && pic_float_p(pic, b)) {              \
      return pic_int(pic, a) op pic_float(pic, b);                      \
    } else if (pic_float_p(pic, a) && pic_int_p(pic, b)) {              \
      return pic_float(pic, a) op pic_int(pic, b);                      \
    } else {                                                            \
      pic_error(pic, #name ": non-number operand given", 0);              \
    }                                                                   \
    PIC_UNREACHABLE();                                                  \
  }

pic_define_cmp(pic_eq, ==)
pic_define_cmp(pic_lt, <)
pic_define_cmp(pic_le, <=)
pic_define_cmp(pic_gt, >)
pic_define_cmp(pic_ge, >=)

#define DEFINE_CMP(op)                                  \
  static pic_value                                      \
  pic_number_##op(pic_state *pic)                       \
  {                                                     \
    int argc, i;                                        \
    pic_value *argv;                                    \
                                                        \
    pic_get_args(pic, "*", &argc, &argv);               \
                                                        \
    if (argc < 2) {                                     \
      return pic_true_value(pic);                          \
    }                                                   \
                                                        \
    for (i = 1; i < argc; ++i) {                        \
      if (! pic_##op(pic, argv[i - 1], argv[i])) {      \
        return pic_false_value(pic);                       \
      }                                                 \
    }                                                   \
    return pic_true_value(pic);                            \
  }

DEFINE_CMP(eq)
DEFINE_CMP(lt)
DEFINE_CMP(le)
DEFINE_CMP(gt)
DEFINE_CMP(ge)

#define DEFINE_AOP(op, v1, c0)                  \
  static pic_value                              \
  pic_number_##op(pic_state *pic)               \
  {                                             \
    int argc, i;                                \
    pic_value *argv, tmp;                       \
                                                \
    pic_get_args(pic, "*", &argc, &argv);       \
                                                \
    if (argc == 0) {                            \
      c0;                                       \
    }                                           \
    else if (argc == 1) {                       \
      return v1;                                \
    }                                           \
                                                \
    tmp = argv[0];                              \
    for (i = 1; i < argc; ++i) {                \
      tmp = pic_##op(pic, tmp, argv[i]);        \
    }                                           \
    return tmp;                                 \
  }

DEFINE_AOP(add, argv[0], do {
    return pic_int_value(pic, 0);
  } while (0))
DEFINE_AOP(mul, argv[0], do {
    return pic_int_value(pic, 1);
  } while (0))
DEFINE_AOP(sub, pic_sub(pic, pic_int_value(pic, 0), argv[0]), do {
    pic_error(pic, "-: at least one argument required", 0);
  } while (0))
DEFINE_AOP(div, pic_div(pic, pic_int_value(pic, 1), argv[0]), do {
    pic_error(pic, "/: at least one argument required", 0);
  } while (0))

static int
number_string_length(int val, int radix)
{
  unsigned long v = val; /* in case val == INT_MIN */
  int count = 0;
  if (val == 0) {
    return 1;
  }
  if (val < 0) {
    v = -val;
    count = 1;
  }
  while (v > 0) {
    ++count;
    v /= radix;
  }
  return count;
}

static void
number_string(int val, int radix, int length, char *buffer) {
  const char digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
  unsigned long v = val;
  int i;
  if (val == 0) {
    buffer[0] = '0';
    buffer[1] = '\0';
    return;
  }
  if (val < 0) {
    buffer[0] = '-';
    v = -val;
  }

  for(i = length - 1; v > 0; --i) {
    buffer[i] = digits[v % radix];
    v /= radix;
  }
  buffer[length] = '\0';
  return;
}

static pic_value
pic_number_number_to_string(pic_state *pic)
{
  double f;
  bool e;
  int radix = 10;
  pic_value str;

  pic_get_args(pic, "F|i", &f, &e, &radix);

  if (radix < 2 || radix > 36) {
    pic_error(pic, "number->string: invalid radix (between 2 and 36, inclusive)", 1, pic_int_value(pic, radix));
  }

  if (e) {
    int ival = (int) f;
    int ilen = number_string_length(ival, radix);
    char *buf = pic_alloca(pic, ilen + 1);

    number_string(ival, radix, ilen, buf);

    str = pic_str_value(pic, buf, ilen);
  }
  else {
    pic_value port = pic_fmemopen(pic, NULL, 0, "w");
    const char *buf;
    int len;

    pic_fprintf(pic, port, "%f", f);
    pic_fgetbuf(pic, port, &buf, &len);
    str = pic_str_value(pic, buf, len);
    pic_fclose(pic, port);
  }

  return str;
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

  flt = PIC_CSTRING_TO_DOUBLE(str);

  if (isint && INT_MIN <= flt && flt <= INT_MAX) {
    return pic_int_value(pic, flt);
  } else {
    return pic_float_value(pic, flt);
  }
}

static pic_value
pic_number_string_to_number(pic_state *pic)
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
pic_init_number(pic_state *pic)
{
  pic_defun(pic, "number?", pic_number_number_p);
  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_defun(pic, "=", pic_number_eq);
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, ">", pic_number_gt);
  pic_defun(pic, "<=", pic_number_le);
  pic_defun(pic, ">=", pic_number_ge);
  pic_defun(pic, "+", pic_number_add);
  pic_defun(pic, "-", pic_number_sub);
  pic_defun(pic, "*", pic_number_mul);
  pic_defun(pic, "/", pic_number_div);
  pic_defun(pic, "number->string", pic_number_number_to_string);
  pic_defun(pic, "string->number", pic_number_string_to_number);
}
