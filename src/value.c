#include <stdlib.h>
#include <stdbool.h>

#include "picrin/value.h"

enum pic_tt
pic_type(pic_value v)
{
  switch (v.type) {
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
  case PIC_VTYPE_EOF:
    return PIC_TT_EOF;
  case PIC_VTYPE_HEAP:
    return ((struct pic_object *)v.u.data)->tt;
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

  v.type = PIC_VTYPE_NIL;
  v.u.data = NULL;
  return v;
}

pic_value
pic_true_value()
{
  pic_value v;

  v.type = PIC_VTYPE_TRUE;
  v.u.data = NULL;
  return v;
}

pic_value
pic_false_value()
{
  pic_value v;

  v.type = PIC_VTYPE_FALSE;
  v.u.data = NULL;
  return v;
}

pic_value
pic_bool_value(bool b)
{
  pic_value v;

  v.type = b ? PIC_VTYPE_TRUE : PIC_VTYPE_FALSE;
  v.u.data = NULL;
  return v;
}

pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  v.type = PIC_VTYPE_HEAP;
  v.u.data = ptr;
  return v;
}

pic_value
pic_float_value(double f)
{
  pic_value v;

  v.type = PIC_VTYPE_FLOAT;
  v.u.f = f;
  return v;
}

pic_value
pic_int_value(int i)
{
  pic_value v;

  v.type = PIC_VTYPE_INT;
  v.u.i = i;
  return v;
}

pic_value
pic_symbol_value(pic_sym sym)
{
  pic_value v;

  v.type = PIC_VTYPE_SYMBOL;
  v.u.sym = sym;
  return v;
}

pic_value
pic_undef_value()
{
  pic_value v;

  v.type = PIC_VTYPE_UNDEF;
  v.u.data = NULL;
  return v;
}
