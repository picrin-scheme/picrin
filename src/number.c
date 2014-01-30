/**
 * See Copyright Notice in picrin.h
 */

#include <math.h>
#include <limits.h>
#include <stdlib.h>

#include "picrin.h"

static int
gcd(int a, int b)
{
  if (a > b)
    return gcd(b, a);
  if (a < 0)
    return gcd(-a, b);
  if (a > 0)
    return gcd(b % a, a);
  return b;
}

static int
lcm(int a, int b)
{
  return abs(a * b) / gcd(a, b);
}

static pic_value
pic_number_real_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_float_p(v) || pic_int_p(v));
}

static pic_value
pic_number_integer_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_int_p(v)) {
    return pic_true_value();
  }
  if (pic_float_p(v)) {
    double f = pic_float(v);

    if (f == round(f)) {
      return pic_true_value();
    }
  }
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

  return pic_bool_value(pic_float_p(v));
}

static pic_value
pic_number_finite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_int_p(v))
    return pic_true_value();
  if (pic_float_p(v) && ! (isinf(pic_float(v)) || isnan(pic_float(v))))
    return pic_true_value();
  else
    return pic_false_value();
}

static pic_value
pic_number_infinite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_float_p(v) && isinf(pic_float(v)))
    return pic_true_value();
  else
    return pic_false_value();
}

static pic_value
pic_number_nan_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_float_p(v) && isnan(pic_float(v)))
    return pic_true_value();
  else
    return pic_false_value();
}

#define DEFINE_ARITH_CMP(op, name)			\
  static pic_value					\
  pic_number_##name(pic_state *pic)			\
  {							\
    size_t argc;					\
    pic_value *argv;					\
    size_t i;						\
    double f,g;						\
    							\
    pic_get_args(pic, "ff*", &f, &g, &argc, &argv);	\
    							\
    if (! (f op g))					\
      return pic_false_value();				\
    							\
    for (i = 0; i < argc; ++i) {			\
      if (pic_float_p(argv[i]))				\
	g = pic_float(argv[i]);				\
      else if (pic_int_p(argv[i]))			\
	g = pic_int(argv[i]);				\
      else						\
	pic_error(pic, #op ": number required");	\
      							\
      if (! (f op g))					\
	return pic_false_value();			\
    }							\
    							\
    return pic_true_value();				\
  }

DEFINE_ARITH_CMP(==, eq)
DEFINE_ARITH_CMP(<, lt)
DEFINE_ARITH_CMP(>, gt)
DEFINE_ARITH_CMP(<=, le)
DEFINE_ARITH_CMP(>=, ge)

static pic_value
pic_number_zero_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f == 0);
}

static pic_value
pic_number_positive_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f > 0);
}

static pic_value
pic_number_negative_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f < 0);
}

static pic_value
pic_number_odd_p(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_bool_value(i % 2 != 0);
}

static pic_value
pic_number_even_p(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_bool_value(i % 2 == 0);
}

static pic_value
pic_number_max(pic_state *pic)
{
  size_t argc;
  pic_value *argv;
  size_t i;
  double f;
  bool e = true;

  pic_get_args(pic, "*", &argc, &argv);

  f = -INFINITY;
  for (i = 0; i < argc; ++i) {
    if (pic_int_p(argv[i])) {
      f = fmax(f, pic_int(argv[i]));
    }
    else if (pic_float_p(argv[i])) {
      e = false;
      f = fmax(f, pic_float(argv[i]));
    }
    else {
      pic_error(pic, "min: number required");
    }
  }

  return e ? pic_int_value(f) : pic_float_value(f);
}

static pic_value
pic_number_min(pic_state *pic)
{
  size_t argc;
  pic_value *argv;
  size_t i;
  double f;
  bool e = true;

  pic_get_args(pic, "*", &argc, &argv);

  f = INFINITY;
  for (i = 0; i < argc; ++i) {
    if (pic_int_p(argv[i])) {
      f = fmin(f, pic_int(argv[i]));
    }
    else if (pic_float_p(argv[i])) {
      e = false;
      f = fmin(f, pic_float(argv[i]));
    }
    else {
      pic_error(pic, "min: number required");
    }
  }

  return e ? pic_int_value(f) : pic_float_value(f);
}

#define DEFINE_ARITH_OP(op, name, unit)                         \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc;                                                \
    pic_value *argv;                                            \
    size_t i;                                                   \
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
        pic_error(pic, #op ": number required");                \
      }                                                         \
    }                                                           \
                                                                \
    return e ? pic_int_value((int)f) : pic_float_value(f);      \
  }

DEFINE_ARITH_OP(+, add, 0)
DEFINE_ARITH_OP(*, mul, 1)

#define DEFINE_ARITH_INV_OP(op, name, unit, exact)                      \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
   size_t argc;                                                         \
   pic_value *argv;                                                     \
   size_t i;                                                            \
   double f;                                                            \
   bool e;                                                              \
                                                                        \
   pic_get_args(pic, "F*", &f, &e, &argc, &argv);                       \
                                                                        \
   e = e && exact;                                                      \
                                                                        \
   if (argc == 0) {                                                     \
     f = unit op f;                                                     \
   }                                                                    \
   for (i = 0; i < argc; ++i) {                                         \
     if (pic_int_p(argv[i])) {                                          \
       f op##= pic_int(argv[i]);                                        \
     }                                                                  \
     else if (pic_float_p(argv[i])) {                                   \
       e = false;                                                       \
       f op##= pic_float(argv[i]);                                      \
     }                                                                  \
     else {                                                             \
       pic_error(pic, #op ": number required");                         \
     }                                                                  \
   }                                                                    \
                                                                        \
   return e ? pic_int_value((int)f) : pic_float_value(f);               \
  }

DEFINE_ARITH_INV_OP(-, sub, 0, true)
DEFINE_ARITH_INV_OP(/, div, 1, false)

static pic_value
pic_number_abs(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(fabs(f));
  }
  else {
    return pic_float_value(fabs(f));
  }
}

static pic_value
pic_number_floor_quotient(pic_state *pic)
{
  int i,j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_int_value((int)floor((double)i/j));
  }
  else {
    return pic_float_value(floor((double)i/j));
  }
}

static pic_value
pic_number_floor_remainder(pic_state *pic)
{
  int i,j,q;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  q = (int)floor((double)i/j);
  if (e1 && e2) {
    return pic_int_value(i - j * q);
  }
  else {
    return pic_float_value(i - j * q);
  }
}

static pic_value
pic_number_trunc_quotient(pic_state *pic)
{
  int i,j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_int_value((int)trunc((double)i/j));
  }
  else {
    return pic_float_value(trunc((double)i/j));
  }
}

static pic_value
pic_number_trunc_remainder(pic_state *pic)
{
  int i,j,q;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  q = (int)trunc((double)i/j);
  if (e1 && e2) {
    return pic_int_value(i - j * q);
  }
  else {
    return pic_float_value(i - j * q);
  }
}

static pic_value
pic_number_gcd(pic_state *pic)
{
  size_t argc;
  pic_value *args;
  int r;
  bool e = true;

  pic_get_args(pic, "*", &argc, &args);

  r = 0;
  while (argc-- > 0) {
    if (pic_int_p(args[argc])) {
      r = gcd(r, pic_int(args[argc]));
    }
    else if (pic_float_p(args[argc])) {
      e = false;
      r = gcd(r, pic_float(args[argc]));
    }
    else {
      pic_error(pic, "gcd: number required");
    }
  }
  return e ? pic_int_value(r) : pic_float_value(r);
}

static pic_value
pic_number_lcm(pic_state *pic)
{
  size_t argc;
  pic_value *args;
  int r;
  bool e = true;

  pic_get_args(pic, "*", &argc, &args);

  r = 1;
  while (argc-- > 0) {
    if (pic_int_p(args[argc])) {
      r = lcm(r, pic_int(args[argc]));
    }
    else if (pic_float_p(args[argc])) {
      e = false;
      r = lcm(r, pic_float(args[argc]));
    }
    else {
      pic_error(pic, "lcm: number required");
    }
  }
  return e ? pic_int_value(r) : pic_float_value(r);
}

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

static pic_value
pic_number_exp(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  return pic_float_value(exp(f));
}

static pic_value
pic_number_log(pic_state *pic)
{
  double f,g;
  int argc;

  argc = pic_get_args(pic, "f|f", &f, &g);
  if (argc == 2) {
    return pic_float_value(log(f));
  }
  else {
    return pic_float_value(log(f) / log(g));
  }
}

static pic_value
pic_number_sin(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = sin(f);
  return pic_float_value(f);
}

static pic_value
pic_number_cos(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = cos(f);
  return pic_float_value(f);
}

static pic_value
pic_number_tan(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = tan(f);
  return pic_float_value(f);
}

static pic_value
pic_number_acos(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = acos(f);
  return pic_float_value(f);
}

static pic_value
pic_number_asin(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = asin(f);
  return pic_float_value(f);
}

static pic_value
pic_number_atan(pic_state *pic)
{
  double f,g;
  int argc;

  argc = pic_get_args(pic, "f|f", &f, &g);
  if (argc == 2) {
    f = atan(f);
    return pic_float_value(f);
  }
  else {
    return pic_float_value(atan2(f,g));
  }
}

static pic_value
pic_number_square(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    long long i = (long long)f;

    if (i * i <= INT_MAX) {
      return pic_int_value(i * i);
    }
  }
  return pic_float_value(f * f);
}

static pic_value
pic_number_sqrt(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(sqrt(f));
}

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

  return pic_int_value((int)round(f));
}

void
pic_init_number(pic_state *pic)
{
  int ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_real_p);
  pic_defun(pic, "complex?", pic_number_real_p);
  pic_defun(pic, "real?", pic_number_real_p);
  pic_defun(pic, "rational?", pic_number_integer_p);
  pic_defun(pic, "integer?", pic_number_integer_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_defun(pic, "exact-integer?", pic_number_exact_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "=", pic_number_eq);
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, ">", pic_number_gt);
  pic_defun(pic, "<=", pic_number_le);
  pic_defun(pic, ">=", pic_number_ge);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "zero?", pic_number_zero_p);
  pic_defun(pic, "positive?", pic_number_positive_p);
  pic_defun(pic, "negative?", pic_number_negative_p);
  pic_defun(pic, "odd?", pic_number_odd_p);
  pic_defun(pic, "even?", pic_number_even_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "min", pic_number_min);
  pic_defun(pic, "max", pic_number_max);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "+", pic_number_add);
  pic_defun(pic, "-", pic_number_sub);
  pic_defun(pic, "*", pic_number_mul);
  pic_defun(pic, "/", pic_number_div);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "abs", pic_number_abs);
  pic_defun(pic, "floor-quotient", pic_number_floor_quotient);
  pic_defun(pic, "floor-remainder", pic_number_floor_remainder);
  pic_defun(pic, "truncate-quotient", pic_number_trunc_quotient);
  pic_defun(pic, "truncate-remainder", pic_number_trunc_remainder);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "gcd", pic_number_gcd);
  pic_defun(pic, "lcm", pic_number_lcm);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceil);
  pic_defun(pic, "truncate", pic_number_trunc);
  pic_defun(pic, "round", pic_number_round);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "square", pic_number_square);
  pic_defun(pic, "expt", pic_number_expt);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);

  DEFLIBRARY(pic, "(scheme inexact)")
  {
    pic_defun(pic, "finite?", pic_number_finite_p);
    pic_defun(pic, "infinite?", pic_number_infinite_p);
    pic_defun(pic, "nan?", pic_number_nan_p);

    pic_defun(pic, "exp", pic_number_exp);
    pic_defun(pic, "log", pic_number_log);
    pic_defun(pic, "sin", pic_number_sin);
    pic_defun(pic, "cos", pic_number_cos);
    pic_defun(pic, "tan", pic_number_tan);
    pic_defun(pic, "acos", pic_number_acos);
    pic_defun(pic, "asin", pic_number_asin);
    pic_defun(pic, "atan", pic_number_atan);

    pic_defun(pic, "sqrt", pic_number_sqrt);
  }
  ENDLIBRARY(pic);
}
