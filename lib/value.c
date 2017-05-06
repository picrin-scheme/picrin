/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "value.h"
#include "state.h"

int
pic_int(pic_state *PIC_UNUSED(pic), pic_value v)
{
  assert(pic_int_p(pic, v));
  return value_int(&v);
}

double
pic_float(pic_state *PIC_UNUSED(pic), pic_value v)
{
  assert(pic_float_p(pic, v));
  return value_float(&v);
}

char
pic_char(pic_state *PIC_UNUSED(pic), pic_value v)
{
  assert(pic_char_p(pic, v));
  return value_char(&v);
}

void *
pic_ptr(pic_state *PIC_UNUSED(pic), pic_value v)
{
  assert(pic_obj_p(pic, v));
  return value_ptr(&v);
}

int
pic_type(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return value_type(&v);
}

pic_value
pic_int_value(pic_state *PIC_UNUSED(pic), int i)
{
  pic_value v;
  make_int_value(&v, i);
  return v;
}

pic_value
pic_float_value(pic_state *PIC_UNUSED(pic), double f)
{
  pic_value v;
  make_float_value(&v, f);
  return v;
}

pic_value
pic_char_value(pic_state *PIC_UNUSED(pic), char c)
{
  pic_value v;
  make_char_value(&v, c);
  return v;
}

pic_value
pic_obj_value(pic_state *PIC_UNUSED(pic), void *p, int type)
{
  pic_value v;
  make_obj_value(&v, p, type);
  return v;
}

#define DEFVAL(name, type)                      \
  pic_value name(pic_state *PIC_UNUSED(pic)) {              \
    pic_value v;                                \
    make_value(&v, type);                       \
    return v;                                   \
  }

DEFVAL(pic_nil_value, PIC_TYPE_NIL)
DEFVAL(pic_eof_object, PIC_TYPE_EOF)
DEFVAL(pic_true_value, PIC_TYPE_TRUE)
DEFVAL(pic_false_value, PIC_TYPE_FALSE)
DEFVAL(pic_undef_value, PIC_TYPE_UNDEF)
DEFVAL(pic_invalid_value, PIC_TYPE_INVALID)

pic_value
pic_bool_value(pic_state *PIC_UNUSED(pic), bool b)
{
  pic_value v;
  make_value(&v, (b ? PIC_TYPE_TRUE : PIC_TYPE_FALSE));
  return v;
}

#define DEFPRED(name, type)                             \
  bool name(pic_state *PIC_UNUSED(pic), pic_value v) {  \
    return pic_type(pic, v) == type;                    \
  }

DEFPRED(pic_invalid_p, PIC_TYPE_INVALID)
DEFPRED(pic_float_p, PIC_TYPE_FLOAT)
DEFPRED(pic_int_p, PIC_TYPE_INT)
DEFPRED(pic_char_p, PIC_TYPE_CHAR)
DEFPRED(pic_eof_p, PIC_TYPE_EOF)
DEFPRED(pic_undef_p, PIC_TYPE_UNDEF)
DEFPRED(pic_true_p, PIC_TYPE_TRUE)
DEFPRED(pic_nil_p, PIC_TYPE_NIL)
DEFPRED(pic_false_p, PIC_TYPE_FALSE)
DEFPRED(pic_str_p, PIC_TYPE_STRING)
DEFPRED(pic_vec_p, PIC_TYPE_VECTOR)
DEFPRED(pic_blob_p, PIC_TYPE_BLOB)
DEFPRED(pic_dict_p, PIC_TYPE_DICT)
DEFPRED(pic_attr_p, PIC_TYPE_ATTR)
DEFPRED(pic_rec_p, PIC_TYPE_RECORD)
DEFPRED(pic_sym_p, PIC_TYPE_SYMBOL)
DEFPRED(pic_pair_p, PIC_TYPE_PAIR)
DEFPRED(pic_proc_func_p, PIC_TYPE_PROC_FUNC)
DEFPRED(pic_proc_irep_p, PIC_TYPE_PROC_IREP)
DEFPRED(pic_irep_p, PIC_TYPE_IREP)

bool
pic_bool_p(pic_state *pic, pic_value v)
{
  return pic_true_p(pic, v) || pic_false_p(pic, v);
}

bool
pic_proc_p(pic_state *pic, pic_value v)
{
  return pic_proc_func_p(pic, v) || pic_proc_irep_p(pic, v);
}

bool
pic_obj_p(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return value_obj_p(&v);
}
