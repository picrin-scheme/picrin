/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/cont.h"
#include "picrin/port.h"

#if ! PIC_ENABLE_FLOAT
static pic_value
pic_number_id(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_int_value(i);
}
#endif

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
pic_number_real_p(pic_state *pic)
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
pic_number_integer_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_int_p(v)) {
    return pic_true_value();
  }
#if PIC_ENABLE_FLOAT
  if (pic_float_p(v)) {
    double f = pic_float(v);

    if (isinf(f)) {
      return pic_false_value();
    }

    if (f == round(f)) {
      return pic_true_value();
    }
  }
#endif
  return pic_false_value();
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

#if PIC_ENABLE_FLOAT
static pic_value
pic_number_abs(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(f < 0 ? -f : f);
  }
  else {
    return pic_float_value(fabs(f));
  }
}
#else
static pic_value
pic_number_abs(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_int_value(i < 0 ? -i : i);
}
#endif

#if PIC_ENABLE_FLOAT

static pic_value
pic_number_floor2(pic_state *pic)
{
  int i, j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    int k;

    k = (i < 0 && j < 0) || (0 <= i && 0 <= j)
      ? i / j
      : (i / j) - 1;

    return pic_values2(pic, pic_int_value(k), pic_int_value(i - k * j));
  }
  else {
    double q, r;

    q = floor((double)i/j);
    r = i - j * q;
    return pic_values2(pic, pic_float_value(q), pic_float_value(r));
  }
}

static pic_value
pic_number_trunc2(pic_state *pic)
{
  int i, j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_values2(pic, pic_int_value(i/j), pic_int_value(i - (i/j) * j));
  }
  else {
    double q, r;

    q = trunc((double)i/j);
    r = i - j * q;

    return pic_values2(pic, pic_float_value(q), pic_float_value(r));
  }
}

#else

static pic_value
pic_number_floor2(pic_state *pic)
{
  int i, j, k;

  pic_get_args(pic, "ii", &i, &j);

  k = (i < 0 && j < 0) || (0 <= i && 0 <= j)
    ? i / j
    : (i / j) - 1;

  return pic_values2(pic, pic_int_value(k), pic_int_value(i - k * j));
}

static pic_value
pic_number_trunc2(pic_state *pic)
{
  int i, j;

  pic_get_args(pic, "ii", &i, &j);

  return pic_values2(pic, pic_int_value(i/j), pic_int_value(i - (i/j) * j));
}

#endif

#if PIC_ENABLE_FLOAT

static pic_value
pic_number_floor(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value((int)f);
  }
  else {
    return pic_float_value(floor(f));
  }
}

static pic_value
pic_number_ceil(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value((int)f);
  }
  else {
    return pic_float_value(ceil(f));
  }
}

static pic_value
pic_number_trunc(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value((int)f);
  }
  else {
    return pic_float_value(trunc(f));
  }
}

static pic_value
pic_number_round(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value((int)f);
  }
  else {
    return pic_float_value(round(f));
  }
}

#endif

#if PIC_ENABLE_FLOAT
static pic_value
pic_number_expt(pic_state *pic)
{
  double f, g, h;
  bool e1, e2;

  pic_get_args(pic, "FF", &f, &e1, &g, &e2);

  h = pow(f, g);
  if (e1 && e2) {
    if (h <= INT_MAX) {
      return pic_int_value((int)h);
    }
  }
  return pic_float_value(h);
}
#else
static pic_value
pic_number_expt(pic_state *pic)
{
  int x, y, i, e = 1, r = 1, s = 0;

  pic_get_args(pic, "ii", &x, &y);

  if (y < 0) {
    s = 1;
    y = -y;
  }
  e = x;
  for (i = 0; y; ++i) {
    if ((y & 1) != 0) {
      r *= e;
    }
    e *= e;
    y >>= 1;
  }
  if (s != 0) {
    r = 1 / r;
  }
  return pic_int_value(r);
}
#endif

#if PIC_ENABLE_FLOAT

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

static pic_value
pic_number_number_to_string(pic_state *pic)
{
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

    xfprintf(port->file, "%f", f);

    str = pic_get_output_string(pic, port);

    pic_close_port(pic, port);
  }

  return pic_obj_value(str);
}

static pic_value
pic_number_string_to_number(pic_state *pic)
{
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
}

#else

static pic_value
pic_number_number_to_string(pic_state *pic)
{
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
}

static pic_value
pic_number_string_to_number(pic_state *pic)
{
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
}

#endif

void
pic_init_number(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_real_p);
  pic_defun(pic, "complex?", pic_number_real_p);
  pic_defun(pic, "real?", pic_number_real_p);
  pic_defun(pic, "rational?", pic_number_real_p);
  pic_defun(pic, "integer?", pic_number_integer_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
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

  pic_defun(pic, "floor/", pic_number_floor2);
  pic_defun(pic, "truncate/", pic_number_trunc2);
  pic_gc_arena_restore(pic, ai);

#if PIC_ENABLE_FLOAT
  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceil);
  pic_defun(pic, "truncate", pic_number_trunc);
  pic_defun(pic, "round", pic_number_round);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);
#else
  pic_defun(pic, "floor", pic_number_id);
  pic_defun(pic, "ceiling", pic_number_id);
  pic_defun(pic, "truncate", pic_number_id);
  pic_defun(pic, "round", pic_number_id);

  pic_defun(pic, "inexact", pic_number_id);
  pic_defun(pic, "exact", pic_number_id);
  pic_gc_arena_restore(pic, ai);
#endif

  pic_defun(pic, "abs", pic_number_abs);
  pic_defun(pic, "expt", pic_number_expt);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "number->string", pic_number_number_to_string);
  pic_defun(pic, "string->number", pic_number_string_to_number);
  pic_gc_arena_restore(pic, ai);
}
