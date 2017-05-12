/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include "value.h"
#include "object.h"
#include "state.h"

bool
pic_eq_p(pic_state *PIC_UNUSED(pic), pic_value x, pic_value y)
{
  return value_eq_p(&x, &y);
}

bool
pic_eqv_p(pic_state *PIC_UNUSED(pic), pic_value x, pic_value y)
{
  return value_eq_p(&x, &y);
}

bool
pic_equal_p(pic_state *pic, pic_value x, pic_value y)
{
 LOOP:

  if (pic_eqv_p(pic, x, y)) {
    return true;
  }
  if (pic_type(pic, x) != pic_type(pic, y)) {
    return false;
  }

  switch (pic_type(pic, x)) {
  case PIC_TYPE_STRING: {
    int xlen, ylen;
    const char *xstr, *ystr;

    xstr = pic_str(pic, x, &xlen);
    ystr = pic_str(pic, y, &ylen);

    if (xlen != ylen) {
      return false;
    }
    return memcmp(xstr, ystr, xlen) == 0;
  }
  case PIC_TYPE_BLOB: {
    int xlen, ylen;
    const unsigned char *xbuf, *ybuf;

    xbuf = pic_blob(pic, x, &xlen);
    ybuf = pic_blob(pic, y, &ylen);

    if (xlen != ylen) {
      return false;
    }
    return memcmp(xbuf, ybuf, xlen) == 0;
  }
  case PIC_TYPE_PAIR: {
    if (! pic_equal_p(pic, pic_car(pic, x), pic_car(pic, y))) {
      return false;
    }
    x = pic_cdr(pic, x);
    y = pic_cdr(pic, y);
    goto LOOP;                  /* tail-call optimization */
  }
  case PIC_TYPE_VECTOR: {
    int i, xlen, ylen;

    xlen = pic_vec_len(pic, x);
    ylen = pic_vec_len(pic, y);

    if (xlen != ylen) {
      return false;
    }
    for (i = 0; i < xlen; ++i) {
      if (! pic_equal_p(pic, pic_vec_ref(pic, x, i), pic_vec_ref(pic, y, i)))
        return false;
    }
    return true;
  }
  case PIC_TYPE_DICT: {
    int it = 0;
    pic_value key, val;

    if (pic_dict_size(pic, x) != pic_dict_size(pic, y)) {
      return false;
    }
    while (pic_dict_next(pic, x, &it, &key, &val)) {
      if (! pic_dict_has(pic, y, key))
        return false;
      if (! pic_equal_p(pic, val, pic_dict_ref(pic, y, key)))
        return false;
    }
    return true;
  }
  case PIC_TYPE_RECORD: {
    if (! pic_eq_p(pic, pic_record_type(pic, x), pic_record_type(pic, y))) {
      return false;
    }
    x = pic_record_datum(pic, x);
    y = pic_record_datum(pic, y);
    goto LOOP;
  }
  case PIC_TYPE_DATA: {
    return pic_data(pic, x) == pic_data(pic, y);
  }
  default:
    return false;
  }
}

static pic_value
pic_bool_eq_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic, pic_eq_p(pic, x, y));
}

static pic_value
pic_bool_eqv_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic, pic_eqv_p(pic, x, y));
}

static pic_value
pic_bool_equal_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic, pic_equal_p(pic, x, y));
}

static pic_value
pic_bool_not(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_false_p(pic, v));
}

static pic_value
pic_bool_boolean_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_bool_p(pic, v));
}

static pic_value
pic_bool_boolean_eq_p(pic_state *pic)
{
  int argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! (pic_true_p(pic, argv[i]) || pic_false_p(pic, argv[i]))) {
      return pic_false_value(pic);
    }
    if (! pic_eq_p(pic, argv[i], argv[0])) {
      return pic_false_value(pic);
    }
  }
  return pic_true_value(pic);
}

void
pic_init_bool(pic_state *pic)
{
  pic_defun(pic, "eq?", pic_bool_eq_p);
  pic_defun(pic, "eqv?", pic_bool_eqv_p);
  pic_defun(pic, "equal?", pic_bool_equal_p);
  pic_defun(pic, "not", pic_bool_not);
  pic_defun(pic, "boolean?", pic_bool_boolean_p);
  pic_defun(pic, "boolean=?", pic_bool_boolean_eq_p);
}
