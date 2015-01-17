/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/string.h"

static bool
str_equal_p(struct pic_string *str1, struct pic_string *str2)
{
  return pic_strcmp(str1, str2) == 0;
}

static bool
blob_equal_p(struct pic_blob *blob1, struct pic_blob *blob2)
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
internal_equal_p(pic_state *pic, pic_value x, pic_value y, size_t depth, xhash *xh, bool xh_initted_p)
{
  pic_value local = pic_nil_value();
  size_t c;

  if (depth > 10) {
    if (depth > 200) {
      pic_errorf(pic, "Stack overflow in equal\n");
    }
    if (pic_pair_p(x) || pic_vec_p(x)) {
      if (! xh_initted_p) {
        xh_init_ptr(xh, 0);
        xh_initted_p = true;
      }

      if (xh_get_ptr(xh, pic_obj_ptr(x)) != NULL) {
        return true;            /* `x' was seen already.  */
      } else {
        xh_put_ptr(xh, pic_obj_ptr(x), NULL);
      }
    }
  }

  c = 0;

 LOOP:

  if (pic_eqv_p(x, y))
    return true;

  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_STRING:
    return str_equal_p(pic_str_ptr(x), pic_str_ptr(y));

  case PIC_TT_BLOB:
    return blob_equal_p(pic_blob_ptr(x), pic_blob_ptr(y));

  case PIC_TT_PAIR: {
    if (pic_nil_p(local)) {
      local = x;
    }
    if (internal_equal_p(pic, pic_car(pic, x), pic_car(pic, y), depth + 1, xh, xh_initted_p)) {
      x = pic_cdr(pic, x);
      y = pic_cdr(pic, y);

      c++;

      if (c == 2) {
        c = 0;
        local = pic_cdr(pic, local);
        if (pic_eq_p(local, x)) {
          return true;
        }
      }
      goto LOOP;
    } else {
      return false;
    }
  }
  case PIC_TT_VECTOR: {
    size_t i;
    struct pic_vector *u, *v;

    u = pic_vec_ptr(x);
    v = pic_vec_ptr(y);

    if (u->len != v->len) {
      return false;
    }
    for (i = 0; i < u->len; ++i) {
      if (! internal_equal_p(pic, u->data[i], v->data[i], depth + 1, xh, xh_initted_p))
        return false;
    }
    return true;
  }
  default:
    return false;
  }
}

bool
pic_equal_p(pic_state *pic, pic_value x, pic_value y)
{
  xhash ht;

  return internal_equal_p(pic, x, y, 0, &ht, false);
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

static pic_value
pic_bool_boolean_eq_p(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! (pic_true_p(argv[i]) || pic_false_p(argv[i]))) {
      return pic_false_value();
    }
    if (! pic_eq_p(argv[i], argv[0])) {
      return pic_false_value();
    }
  }
  return pic_true_value();
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
