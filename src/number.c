#include <math.h>
#include <limits.h>

#include "picrin.h"

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
  pic_defun(pic, "infinite?", pic_number_infinite_p);
  pic_defun(pic, "nan?", pic_number_nan_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "abs", pic_number_abs);

  pic_defun(pic, "floor-quotient", pic_number_floor_quotient);
  pic_defun(pic, "floor-remainder", pic_number_floor_remainder);
  pic_defun(pic, "truncate-quotient", pic_number_trunc_quotient);
  pic_defun(pic, "truncate-remainder", pic_number_trunc_remainder);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceil);
  pic_defun(pic, "truncate", pic_number_trunc);
  pic_defun(pic, "round", pic_number_round);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exp", pic_number_exp);
  pic_defun(pic, "log", pic_number_log);
  pic_defun(pic, "sin", pic_number_sin);
  pic_defun(pic, "cos", pic_number_cos);
  pic_defun(pic, "tan", pic_number_tan);
  pic_defun(pic, "acos", pic_number_acos);
  pic_defun(pic, "asin", pic_number_asin);
  pic_defun(pic, "atan", pic_number_atan);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "square", pic_number_square);
  pic_defun(pic, "sqrt", pic_number_sqrt);
  pic_defun(pic, "expt", pic_number_expt);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);
}
