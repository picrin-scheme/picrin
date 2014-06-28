/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/string.h"

bool
pic_equal_p(pic_state *pic, pic_value x, pic_value y)
{
  enum pic_tt type;

  if (pic_eqv_p(x, y))
    return true;

  type = pic_type(x);
  if (type != pic_type(y))
    return false;
  switch (type) {
  case PIC_TT_PAIR:
    return pic_equal_p(pic, pic_car(pic, x), pic_car(pic, y)) && pic_equal_p(pic, pic_cdr(pic, x), pic_cdr(pic, y));
  case PIC_TT_BLOB: {
    size_t i;
    struct pic_blob *u = pic_blob_ptr(x), *v = pic_blob_ptr(y);

    if(u->len != v->len){
      return false;
    }
    for(i = 0; i < u->len; ++i){
      if(u->data[i] != v->data[i])
        return false;
    }
    return true;
  }
  case PIC_TT_VECTOR: {
    size_t i;
    struct pic_vector *u = pic_vec_ptr(x), *v = pic_vec_ptr(y);

    if(u->len != v->len){
      return false;
    }
    for(i = 0; i < u->len; ++i){
      if(! pic_equal_p(pic, u->data[i], v->data[i]))
        return false;
    }
    return true;
  }
  case PIC_TT_STRING:
    return pic_strcmp(pic_str_ptr(x), pic_str_ptr(y)) == 0;
  default:
    return false;
  }
}

static pic_value
pic_bool_eq_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_eq_p(x, y));
}

static pic_value
pic_bool_eqv_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_eqv_p(x, y));
}

static pic_value
pic_bool_equal_p(pic_state *pic)
{
  pic_value x, y;

  pic_get_args(pic, "oo", &x, &y);

  return pic_bool_value(pic_equal_p(pic, x, y));
}

static pic_value
pic_bool_not(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_false_p(v) ? pic_true_value() : pic_false_value();
}

static pic_value
pic_bool_boolean_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return (pic_true_p(v) || pic_false_p(v)) ? pic_true_value() : pic_false_value();
}

void
pic_init_bool(pic_state *pic)
{
  pic_defun(pic, "eq?", pic_bool_eq_p);
  pic_defun(pic, "eqv?", pic_bool_eqv_p);
  pic_defun(pic, "equal?", pic_bool_equal_p);

  pic_defun(pic, "not", pic_bool_not);
  pic_defun(pic, "boolean?", pic_bool_boolean_p);
}
