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
pic_string_equal_p(struct pic_string *str1, struct pic_string *str2)
{
  return pic_strcmp(str1, str2) == 0;
}

bool
pic_blob_equal_p(struct pic_blob *blob1, struct pic_blob *blob2)
{
  size_t i;

  if (blob1->len != blob2->len) {
    return false;
  }
  for (i = 0; i < blob1->len; ++i) {
    if (blob1->data[i] != blob2->data[i])
      return false;
  }
  return true;
}

static bool
pic_internal_equal_p(pic_state *pic, pic_value x, pic_value y, size_t depth, xhash *ht)
{
  xh_entry *e;
  enum pic_tt type;
  pic_value local = pic_nil_value();
  size_t rapid_count = 0;

  if (depth > 10) {
    if (depth > 200) {
      pic_errorf(pic, "Stack overflow in equal\n");
    }
    if (NULL == ht) {
      xh_init_ptr(ht, sizeof(void *));
    }
    switch (pic_type(x)) {
    case PIC_TT_PAIR:
    case PIC_TT_VECTOR: {
      e = xh_get(ht, pic_obj_ptr(x));
      if (e) {
        /* `x' was seen already.  */
        return true;
      } else {
        xh_put(ht, pic_obj_ptr(x), NULL);
      }
    }
    default:
      break;
    }
  }

 LOOP:

  if (pic_eqv_p(x, y))
    return true;

  type = pic_type(x);

  if (type != pic_type(y)) {
    return false;
  }

  switch (type) {
  case PIC_TT_PAIR:
    if (pic_nil_p(local)) {
      local = x;
    }
    if (pic_internal_equal_p(pic, pic_car(pic, x), pic_car(pic, y), depth + 1, ht)) {
      x = pic_cdr(pic, x);
      y = pic_cdr(pic, y);
      ++rapid_count;

      if (rapid_count == 2) {
        rapid_count = 0;
        local = pic_cdr(pic, local);
        if (pic_eq_p(local, x)) {
          return true;
        }
      }
      goto LOOP;
    }else{
      return false;
    }
  case PIC_TT_BLOB: {
    size_t i;
    struct pic_blob *u = pic_blob_ptr(x), *v = pic_blob_ptr(y);

    if (u->len != v->len) {
      return false;
    }
    for (i = 0; i < u->len; ++i) {
      if (u->data[i] != v->data[i])
        return false;
    }
    return true;
  }
  case PIC_TT_VECTOR: {
    size_t i;
    struct pic_vector *u = pic_vec_ptr(x), *v = pic_vec_ptr(y);

    if (u->len != v->len) {
      return false;
    }
    for (i = 0; i < u->len; ++i) {
      if (! pic_internal_equal_p(pic, u->data[i], v->data[i], depth + 1, ht))
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

bool
pic_equal_p(pic_state *pic, pic_value x, pic_value y){
  xhash ht;
  xh_init_ptr(&ht, 0);
  return pic_internal_equal_p(pic, x, y, 0, &ht);
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
