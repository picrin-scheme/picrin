#include "picrin.h"

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
pic_number_sqrt(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(sqrt(f));
}

void
pic_init_math(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin number)") {
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
  }
}
