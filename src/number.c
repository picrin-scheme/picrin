#include <math.h>

#include "picrin.h"

static pic_value
pic_number_real_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_float_p(v));
}

static pic_value
pic_number_integer_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_float_p(v)) {
    double f = pic_float(v);

    if (f == round(f)) {
      return pic_true_value();
    }
  }
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

static pic_value
pic_number_lt(pic_state *pic)
{
  double f,g;

  pic_get_args(pic, "ff", &f, &g);
  if (f < g) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_number_abs(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  return pic_float_value(fabs(f));
}

static pic_value
pic_number_floor(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(floor(f));
}

static pic_value
pic_number_ceiling(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(ceil(f));
}

static pic_value
pic_number_truncate(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(trunc(f));
}

static pic_value
pic_number_round(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(round(f));
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
  if (argc == 1) {
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
  if (argc == 1) {
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

  pic_get_args(pic, "f", &f);

  return pic_float_value(f * f);
}

static pic_value
pic_number_sqrt(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = sqrt(f);
  return pic_float_value(f);
}

static pic_value
pic_number_expt(pic_state *pic)
{
  double f,g;

  pic_get_args(pic, "ff", &f, &g);
  return pic_float_value(pow(f,g));
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

  pic_defun(pic, "infinite?", pic_number_infinite_p);
  pic_defun(pic, "nan?", pic_number_nan_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "<", pic_number_lt);

  pic_defun(pic, "abs", pic_number_abs);

  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceiling);
  pic_defun(pic, "truncate", pic_number_truncate);
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
}
