#include <stdlib.h>
#include <stdbool.h>

#include "picrin.h"

enum pic_tt
pic_type(pic_value v)
{
  switch (pic_vtype(v)) {
  case PIC_VTYPE_NIL:
    return PIC_TT_NIL;
  case PIC_VTYPE_TRUE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_FALSE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_UNDEF:
    return PIC_TT_UNDEF;
  case PIC_VTYPE_FLOAT:
    return PIC_TT_FLOAT;
  case PIC_VTYPE_INT:
    return PIC_TT_INT;
  case PIC_VTYPE_SYMBOL:
    return PIC_TT_SYMBOL;
  case PIC_VTYPE_CHAR:
    return PIC_TT_CHAR;
  case PIC_VTYPE_EOF:
    return PIC_TT_EOF;
  case PIC_VTYPE_HEAP:
    return ((struct pic_object *)pic_ptr(v))->tt;
  }
  /* logic flaw (suppress warnings gcc will emit) */
  abort();
}

const char *
pic_type_repr(enum pic_tt tt)
{
  static const char *reprs[13] = {
    "nil",
    "boolean",
    "float",
    "int",
    "symbol",
    "eof",
    "undef",
    "pair",
    "string",
    "vector",
    "proc",
    "port",
    "env"
  };

  return reprs[tt];
}

pic_value
pic_nil_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_NIL);
  return v;
}

pic_value
pic_true_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_TRUE);
  return v;
}

pic_value
pic_false_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FALSE);
  return v;
}

pic_value
pic_bool_value(bool b)
{
  pic_value v;

  pic_init_value(v, b ? PIC_VTYPE_TRUE : PIC_VTYPE_FALSE);
  return v;
}

#if PIC_NAN_BOXING

pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v.u.data = (void*)((long long)v.u.data | ((long long)ptr));
  return v;
}

pic_value
pic_float_value(double f)
{
  pic_value v;

  if (f != f) {
    v.u.type_ = 0x7ff80000;
    v.u.i = 0;
  } else {
    v.u.f = f;
  }
  return v;
}

#else

pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v.u.data = ptr;
  return v;
}

pic_value
pic_float_value(double f)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FLOAT);
  v.u.f = f;
  return v;
}

#endif

pic_value
pic_int_value(int i)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_INT);
  v.u.i = i;
  return v;
}

pic_value
pic_symbol_value(pic_sym sym)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_SYMBOL);
  v.u.sym = sym;
  return v;
}

pic_value
pic_char_value(char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v.u.c = c;
  return v;
}

pic_value
pic_undef_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_UNDEF);
  return v;
}

#if PIC_NAN_BOXING

bool
pic_eq_p(pic_value x, pic_value y)
{
  return x.u.data == y.u.data;
}

bool
pic_eqv_p(pic_value x, pic_value y)
{
  return x.u.data == y.u.data;
}

#else

bool
pic_eq_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_sym(x) == pic_sym(y);
  default:
    return false;
  }
}

bool
pic_eqv_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_sym(x) == pic_sym(y);
  case PIC_TT_FLOAT:
    return pic_float(x) == pic_float(y);
  case PIC_TT_INT:
    return pic_int(x) == pic_int(y);
  default:
    return false;
  }
}

#endif
