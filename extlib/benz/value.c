/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/object.h"

#if PIC_NAN_BOXING

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 111111111111TTTT 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   char  : 111111111111TTTT 0000000000000000 CCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCC
 */

#define pic_init_value(v,vtype) (v = (0xfff0000000000000ul | ((uint64_t)(vtype) << 48)))

int
pic_vtype(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return 0xfff0 >= (v >> 48) ? PIC_TYPE_FLOAT : ((v >> 48) & 0xf);
}

double
pic_float(pic_state *PIC_UNUSED(pic), pic_value v)
{
  union { double f; uint64_t i; } u;
  u.i = v;
  return u.f;
}

int
pic_int(pic_state *PIC_UNUSED(pic), pic_value v)
{
  union { int i; unsigned u; } u;
  u.u = v & 0xfffffffful;
  return u.i;
}

char
pic_char(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v & 0xfffffffful;
}

struct object *
pic_obj_ptr(pic_value v)
{
  return (struct object *)(0xfffffffffffful & v);
}

#else

#define pic_init_value(v,vtype) ((v).type = (vtype), (v).u.data = NULL)

int
pic_vtype(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return (int)(v.type);
}

double
pic_float(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.f;
}

int
pic_int(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.i;
}

char
pic_char(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.c;
}

struct object *
pic_obj_ptr(pic_value v)
{
  return (struct object *)(v.u.data);
}

#endif

#if PIC_NAN_BOXING

pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_IVAL_END);
  v |= 0xfffffffffffful & (uint64_t)ptr;
  return v;
}

pic_value
pic_float_value(pic_state *PIC_UNUSED(pic), double f)
{
  union { double f; uint64_t i; } u;

  if (f != f) {
    return 0x7ff8000000000000ul;
  } else {
    u.f = f;
    return u.i;
  }
}

pic_value
pic_int_value(pic_state *PIC_UNUSED(pic), int i)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_INT);
  v |= (unsigned)i;
  return v;
}

pic_value
pic_char_value(pic_state *PIC_UNUSED(pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_CHAR);
  v |= (unsigned char)c;
  return v;
}

#else

pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_IVAL_END);
  v.u.data = ptr;
  return v;
}

pic_value
pic_float_value(pic_state *PIC_UNUSED(pic), double f)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_FLOAT);
  v.u.f = f;
  return v;
}

pic_value
pic_int_value(pic_state *PIC_UNUSED(pic), int i)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_INT);
  v.u.i = i;
  return v;
}

pic_value
pic_char_value(pic_state *PIC_UNUSED(pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_CHAR);
  v.u.c = c;
  return v;
}

#endif

#define DEFVAL(name, type)                      \
  pic_value name(pic_state *PIC_UNUSED(pic)) {  \
    pic_value v;                                \
    pic_init_value(v, type);                    \
    return v;                                   \
  }

DEFVAL(pic_nil_value, PIC_TYPE_NIL)
DEFVAL(pic_eof_object, PIC_TYPE_EOF)
DEFVAL(pic_true_value, PIC_TYPE_TRUE)
DEFVAL(pic_false_value, PIC_TYPE_FALSE)
DEFVAL(pic_undef_value, PIC_TYPE_UNDEF)
DEFVAL(pic_invalid_value, PIC_TYPE_INVALID)

int
pic_type(pic_state *PIC_UNUSED(pic), pic_value v)
{
  int tt = pic_vtype(pic, v);

  if (tt < PIC_IVAL_END) {
    return tt;
  }
  return ((struct basic *)pic_obj_ptr(v))->tt;
}

const char *
pic_typename(pic_state *pic, int type)
{
  switch (type) {
  case PIC_TYPE_NIL:
    return "null";
  case PIC_TYPE_TRUE:
  case PIC_TYPE_FALSE:
    return "boolean";
  case PIC_TYPE_FLOAT:
    return "float";
  case PIC_TYPE_INT:
    return "int";
  case PIC_TYPE_SYMBOL:
    return "symbol";
  case PIC_TYPE_CHAR:
    return "char";
  case PIC_TYPE_EOF:
    return "eof-object";
  case PIC_TYPE_UNDEF:
    return "undefined";
  case PIC_TYPE_INVALID:
    return "invalid";
  case PIC_TYPE_PAIR:
    return "pair";
  case PIC_TYPE_STRING:
    return "string";
  case PIC_TYPE_VECTOR:
    return "vector";
  case PIC_TYPE_BLOB:
    return "bytevector";
  case PIC_TYPE_PORT:
    return "port";
  case PIC_TYPE_ERROR:
    return "error";
  case PIC_TYPE_ID:
    return "identifier";
  case PIC_TYPE_CXT:
    return "context";
  case PIC_TYPE_FUNC:
  case PIC_TYPE_IREP:
    return "procedure";
  case PIC_TYPE_ENV:
    return "environment";
  case PIC_TYPE_DATA:
    return "data";
  case PIC_TYPE_DICT:
    return "dictionary";
  case PIC_TYPE_WEAK:
    return "ephemeron";
  case PIC_TYPE_RECORD:
    return "record";
  case PIC_TYPE_CP:
    return "checkpoint";
  default:
    pic_error(pic, "pic_typename: invalid type given", 1, pic_int_value(pic, type));
  }
}
