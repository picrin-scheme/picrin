#include <math.h>

#include "picrin.h"

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
pic_number_sqrt(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);
  f = sqrt(f);
  return pic_float_value(f);
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
  double f;

  pic_get_args(pic, "f", &f);
  f = atan(f);
  return pic_float_value(f);
}

void
pic_init_number(pic_state *pic)
{
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, "sqrt", pic_number_sqrt);
  pic_defun(pic, "sin", pic_number_sin);
  pic_defun(pic, "cos", pic_number_cos);
  pic_defun(pic, "tan", pic_number_tan);
  pic_defun(pic, "acos", pic_number_acos);
  pic_defun(pic, "asin", pic_number_asin);
  pic_defun(pic, "atan", pic_number_atan);
}
