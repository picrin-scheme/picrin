#include "picrin.h"
#include "picrin/extra.h"

#include <math.h>

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

    return pic_return(pic, 2, pic_int_value(pic, k), pic_int_value(pic, i - k * j));
  } else {
    double q, r;

    q = floor((double)i/j);
    r = i - j * q;
    return pic_return(pic, 2, pic_float_value(pic, q), pic_float_value(pic, r));
  }
}

static pic_value
pic_number_trunc2(pic_state *pic)
{
  int i, j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_return(pic, 2, pic_int_value(pic, i/j), pic_int_value(pic, i - (i/j) * j));
  } else {
    double q, r;

    q = trunc((double)i/j);
    r = i - j * q;

    return pic_return(pic, 2, pic_float_value(pic, q), pic_float_value(pic, r));
  }
}

static pic_value
pic_number_floor(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(pic, (int)f);
  } else {
    return pic_float_value(pic, floor(f));
  }
}

static pic_value
pic_number_ceil(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(pic, (int)f);
  } else {
    return pic_float_value(pic, ceil(f));
  }
}

static pic_value
pic_number_trunc(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(pic, (int)f);
  } else {
    return pic_float_value(pic, trunc(f));
  }
}

static pic_value
pic_number_round(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(pic, (int)f);
  } else {
    return pic_float_value(pic, round(f));
  }
}

static pic_value
pic_number_finite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_int_p(pic, v))
    return pic_true_value(pic);
  if (pic_float_p(pic, v) && ! (isinf(pic_float(pic, v)) || isnan(pic_float(pic, v))))
    return pic_true_value(pic);
  else
    return pic_false_value(pic);
}

static pic_value
pic_number_infinite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_float_p(pic, v) && isinf(pic_float(pic, v)))
    return pic_true_value(pic);
  else
    return pic_false_value(pic);
}

static pic_value
pic_number_nan_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_float_p(pic, v) && isnan(pic_float(pic, v)))
    return pic_true_value(pic);
  else
    return pic_false_value(pic);
}

static pic_value
pic_number_exp(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  return pic_float_value(pic, exp(f));
}

static pic_value
pic_number_log(pic_state *pic)
{
  double f,g;
  int argc;

  argc = pic_get_args(pic, "f|f", &f, &g);
  if (argc == 1) {
    return pic_float_value(pic, log(f));
  }
  else {
    return pic_float_value(pic, log(f) / log(g));
  }
}

static pic_value
pic_number_sin(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = sin(f);
  return pic_float_value(pic, f);
}

static pic_value
pic_number_cos(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = cos(f);
  return pic_float_value(pic, f);
}

static pic_value
pic_number_tan(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = tan(f);
  return pic_float_value(pic, f);
}

static pic_value
pic_number_acos(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = acos(f);
  return pic_float_value(pic, f);
}

static pic_value
pic_number_asin(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = asin(f);
  return pic_float_value(pic, f);
}

static pic_value
pic_number_atan(pic_state *pic)
{
  double f,g;
  int argc;

  argc = pic_get_args(pic, "f|f", &f, &g);
  if (argc == 1) {
    f = atan(f);
    return pic_float_value(pic, f);
  }
  else {
    return pic_float_value(pic, atan2(f,g));
  }
}

static pic_value
pic_number_sqrt(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(pic, sqrt(f));
}

static pic_value
pic_number_abs(pic_state *pic)
{
  double f;
  bool e;

  pic_get_args(pic, "F", &f, &e);

  if (e) {
    return pic_int_value(pic, f < 0 ? -f : f);
  }
  else {
    return pic_float_value(pic, fabs(f));
  }
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
      return pic_int_value(pic, (int)h);
    }
  }
  return pic_float_value(pic, h);
}

void
pic_init_math(pic_state *pic)
{
  pic_deflibrary(pic, "picrin.math");

  pic_defun(pic, "floor/", pic_number_floor2);
  pic_defun(pic, "truncate/", pic_number_trunc2);
  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceil);
  pic_defun(pic, "truncate", pic_number_trunc);
  pic_defun(pic, "round", pic_number_round);

  pic_defun(pic, "finite?", pic_number_finite_p);
  pic_defun(pic, "infinite?", pic_number_infinite_p);
  pic_defun(pic, "nan?", pic_number_nan_p);
  pic_defun(pic, "sqrt", pic_number_sqrt);
  pic_defun(pic, "exp", pic_number_exp);
  pic_defun(pic, "log", pic_number_log);
  pic_defun(pic, "sin", pic_number_sin);
  pic_defun(pic, "cos", pic_number_cos);
  pic_defun(pic, "tan", pic_number_tan);
  pic_defun(pic, "acos", pic_number_acos);
  pic_defun(pic, "asin", pic_number_asin);
  pic_defun(pic, "atan", pic_number_atan);
  pic_defun(pic, "abs", pic_number_abs);
  pic_defun(pic, "expt", pic_number_expt);
}
