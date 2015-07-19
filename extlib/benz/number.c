/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

/**
 * Returns the length of string representing val.
 * radix is between 2 and 36 (inclusive).
 * No error checks are performed in this function.
 */
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

/**
 * Returns the string representing val.
 * radix is between 2 and 36 (inclusive).
 * This function overwrites buffer and stores the result.
 * No error checks are performed in this function. It is caller's responsibility to avoid buffer-overrun.
 */
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
pic_number_number_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

#if PIC_ENABLE_FLOAT
  return pic_bool_value(pic_float_p(v) || pic_int_p(v));
#else
  return pic_bool_value(pic_int_p(v));
#endif
}

static pic_value
pic_number_exact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_int_p(v));
}

static pic_value
pic_number_inexact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

#if PIC_ENABLE_FLOAT
  return pic_bool_value(pic_float_p(v));
#else
  return pic_false_value();
#endif
}

static pic_value
pic_number_inexact(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(f);
}

static pic_value
pic_number_exact(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_int_value((int)(round(f)));
}

#define DEFINE_ARITH_CMP(op, name)			\
  static pic_value					\
  pic_number_##name(pic_state *pic)			\
  {							\
    size_t argc, i;                                     \
    pic_value *argv;					\
    double f,g;						\
    							\
    pic_get_args(pic, "ff*", &f, &g, &argc, &argv);	\
    							\
    if (! (f op g))					\
      return pic_false_value();				\
    							\
    for (i = 0; i < argc; ++i) {			\
      f = g;                                            \
      if (pic_float_p(argv[i]))				\
	g = pic_float(argv[i]);				\
      else if (pic_int_p(argv[i]))			\
	g = pic_int(argv[i]);				\
      else						\
	pic_errorf(pic, #op ": number required");	\
      							\
      if (! (f op g))					\
	return pic_false_value();			\
    }							\
    							\
    return pic_true_value();				\
  }

#define DEFINE_ARITH_CMP2(op, name)			\
  static pic_value					\
  pic_number_##name(pic_state *pic)			\
  {							\
    size_t argc, i;                                     \
    pic_value *argv;					\
    int f,g;						\
    							\
    pic_get_args(pic, "ii*", &f, &g, &argc, &argv);	\
    							\
    if (! (f op g))					\
      return pic_false_value();				\
    							\
    for (i = 0; i < argc; ++i) {			\
      f = g;                                            \
      if (pic_int_p(argv[i]))                           \
	g = pic_int(argv[i]);				\
      else						\
	pic_errorf(pic, #op ": number required");	\
      							\
      if (! (f op g))					\
	return pic_false_value();			\
    }							\
    							\
    return pic_true_value();				\
  }

#if PIC_ENABLE_FLOAT
DEFINE_ARITH_CMP(==, eq)
DEFINE_ARITH_CMP(<, lt)
DEFINE_ARITH_CMP(>, gt)
DEFINE_ARITH_CMP(<=, le)
DEFINE_ARITH_CMP(>=, ge)
#else
DEFINE_ARITH_CMP2(==, eq)
DEFINE_ARITH_CMP2(<, lt)
DEFINE_ARITH_CMP2(>, gt)
DEFINE_ARITH_CMP2(<=, le)
DEFINE_ARITH_CMP2(>=, ge)
#endif

#define DEFINE_ARITH_OP(op, name, unit)                         \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc, i;                                             \
    pic_value *argv;                                            \
    double f;                                                   \
    bool e = true;                                              \
                                                                \
    pic_get_args(pic, "*", &argc, &argv);                       \
                                                                \
    f = unit;                                                   \
    for (i = 0; i < argc; ++i) {                                \
      if (pic_int_p(argv[i])) {                                 \
        f op##= pic_int(argv[i]);                               \
      }                                                         \
      else if (pic_float_p(argv[i])) {                          \
        e = false;                                              \
        f op##= pic_float(argv[i]);                             \
      }                                                         \
      else {                                                    \
        pic_errorf(pic, #op ": number required");               \
      }                                                         \
    }                                                           \
                                                                \
    return e ? pic_int_value((int)f) : pic_float_value(f);      \
  }

#define DEFINE_ARITH_OP2(op, name, unit)                        \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc, i;                                             \
    pic_value *argv;                                            \
    int f;                                                      \
                                                                \
    pic_get_args(pic, "*", &argc, &argv);                       \
                                                                \
    f = unit;                                                   \
    for (i = 0; i < argc; ++i) {                                \
      if (pic_int_p(argv[i])) {                                 \
        f op##= pic_int(argv[i]);                               \
      }                                                         \
      else {                                                    \
        pic_errorf(pic, #op ": number required");               \
      }                                                         \
    }                                                           \
                                                                \
    return pic_int_value(f);                                    \
  }

#if PIC_ENABLE_FLOAT
DEFINE_ARITH_OP(+, add, 0)
DEFINE_ARITH_OP(*, mul, 1)
#else
DEFINE_ARITH_OP2(+, add, 0)
DEFINE_ARITH_OP2(*, mul, 1)
#endif

#define DEFINE_ARITH_INV_OP(op, name, unit, exact)                      \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc, i;                                                     \
    pic_value *argv;                                                    \
    double f;                                                           \
    bool e = true;                                                      \
                                                                        \
    pic_get_args(pic, "F*", &f, &e, &argc, &argv);                      \
                                                                        \
    e = e && exact;                                                     \
                                                                        \
    if (argc == 0) {                                                    \
      f = unit op f;                                                    \
    }                                                                   \
    for (i = 0; i < argc; ++i) {                                        \
      if (pic_int_p(argv[i])) {                                         \
        f op##= pic_int(argv[i]);                                       \
      }                                                                 \
      else if (pic_float_p(argv[i])) {                                  \
        e = false;                                                      \
        f op##= pic_float(argv[i]);                                     \
      }                                                                 \
      else {                                                            \
        pic_errorf(pic, #op ": number required");                       \
      }                                                                 \
    }                                                                   \
                                                                        \
    return e ? pic_int_value((int)f) : pic_float_value(f);              \
  }

#define DEFINE_ARITH_INV_OP2(op, name, unit)                            \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc, i;                                                     \
    pic_value *argv;                                                    \
    int f;                                                              \
                                                                        \
    pic_get_args(pic, "i*", &f, &argc, &argv);                          \
                                                                        \
    if (argc == 0) {                                                    \
      f = unit op f;                                                    \
    }                                                                   \
    for (i = 0; i < argc; ++i) {                                        \
      if (pic_int_p(argv[i])) {                                         \
        f op##= pic_int(argv[i]);                                       \
      }                                                                 \
      else {                                                            \
        pic_errorf(pic, #op ": number required");                       \
      }                                                                 \
    }                                                                   \
                                                                        \
    return pic_int_value(f);                                            \
  }

#if PIC_ENABLE_FLOAT
DEFINE_ARITH_INV_OP(-, sub, 0, true)
DEFINE_ARITH_INV_OP(/, div, 1, false)
#else
DEFINE_ARITH_INV_OP2(-, sub, 0)
DEFINE_ARITH_INV_OP2(/, div, 1)
#endif

static pic_value
pic_number_number_to_string(pic_state *pic)
{
#if PIC_ENABLE_FLOAT
  double f;
  bool e;
  int radix = 10;
  pic_str *str;

  pic_get_args(pic, "F|i", &f, &e, &radix);

  if (radix < 2 || radix > 36) {
    pic_errorf(pic, "number->string: invalid radix %d (between 2 and 36, inclusive)", radix);
  }

  if (e) {
    int ival = (int) f;
    int ilen = number_string_length(ival, radix);
    size_t s = ilen + 1;
    char *buf = pic_malloc(pic, s);

    number_string(ival, radix, ilen, buf);

    str = pic_make_str(pic, buf, s - 1);

    pic_free(pic, buf);
  }
  else {
    struct pic_port *port = pic_open_output_string(pic);

    xfprintf(pic, port->file, "%f", f);

    str = pic_get_output_string(pic, port);

    pic_close_port(pic, port);
  }

  return pic_obj_value(str);
#else
  int f;
  bool e;
  int radix = 10;
  pic_str *str;
  size_t s;
  char *buf;
  int ival, ilen;

  pic_get_args(pic, "i|i", &f, &e, &radix);

  if (radix < 2 || radix > 36) {
    pic_errorf(pic, "number->string: invalid radix %d (between 2 and 36, inclusive)", radix);
  }

  ival = f;
  ilen = number_string_length(ival, radix);
  s = ilen + 1;

  buf = pic_malloc(pic, s);

  number_string(ival, radix, ilen, buf);

  str = pic_make_str(pic, buf, s - 1);

  pic_free(pic, buf);

  return pic_obj_value(str);
#endif
}

static pic_value
pic_number_string_to_number(pic_state *pic)
{
#if PIC_ENABLE_FLOAT
  const char *str;
  int radix = 10;
  long num;
  char *eptr;
  double flo;

  pic_get_args(pic, "z|i", &str, &radix);

  num = strtol(str, &eptr, radix);
  if (*eptr == '\0') {
    return pic_valid_int(num)
      ? pic_int_value((int)num)
      : pic_float_value(num);
  }

  flo = strtod(str, &eptr);
  if (*eptr == '\0') {
    return pic_float_value(flo);
  }

  pic_errorf(pic, "invalid string given: %s", str);
#else
  const char *str;
  int radix = 10;
  long num;
  char *eptr;

  pic_get_args(pic, "z|i", &str, &radix);

  num = strtol(str, &eptr, radix);
  if (*eptr == '\0') {
    return pic_int_value(num);
  }

  pic_errorf(pic, "invalid string given: %s", str);
#endif
}

void
pic_init_number(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_number_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "=", pic_number_eq);
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, ">", pic_number_gt);
  pic_defun(pic, "<=", pic_number_le);
  pic_defun(pic, ">=", pic_number_ge);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "+", pic_number_add);
  pic_defun(pic, "-", pic_number_sub);
  pic_defun(pic, "*", pic_number_mul);
  pic_defun(pic, "/", pic_number_div);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "number->string", pic_number_number_to_string);
  pic_defun(pic, "string->number", pic_number_string_to_number);
  pic_gc_arena_restore(pic, ai);
}
