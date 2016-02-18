/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_TYPE_H
#define PICRIN_TYPE_H

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * `invalid` value will never be seen from user-end:
 *  it is only used for repsenting internal special state
 */

enum pic_vtype {
  PIC_VTYPE_NIL = 1,
  PIC_VTYPE_TRUE,
  PIC_VTYPE_FALSE,
  PIC_VTYPE_UNDEF,
  PIC_VTYPE_INVALID,
  PIC_VTYPE_FLOAT,
  PIC_VTYPE_INT,
  PIC_VTYPE_CHAR,
  PIC_VTYPE_EOF,
  PIC_VTYPE_HEAP
};

#if PIC_NAN_BOXING

#include <stdint.h>

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 1111111111110110 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   char  : 1111111111111000 0000000000000000 CCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCC
 */

typedef uint64_t pic_value;

#define pic_ptr(v) ((void *)(0xfffffffffffful & (v)))
#define pic_init_value(v,vtype) (v = (0xfff0000000000000ul | ((uint64_t)(vtype) << 48)))

static inline enum pic_vtype
pic_vtype(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return 0xfff0 >= (v >> 48) ? PIC_VTYPE_FLOAT : ((v >> 48) & 0xf);
}

static inline double
pic_float(pic_state PIC_UNUSED(*pic), pic_value v)
{
  union { double f; uint64_t i; } u;
  u.i = v;
  return u.f;
}

static inline int
pic_int(pic_state PIC_UNUSED(*pic), pic_value v)
{
  union { int i; unsigned u; } u;
  u.u = v & 0xfffffffful;
  return u.i;
}

static inline char
pic_char(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return v & 0xfffffffful;
}

#else

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
    double f;
    int i;
    char c;
  } u;
} pic_value;

#define pic_ptr(v) ((v).u.data)
#define pic_vtype(pic,v) ((v).type)
#define pic_init_value(v,vtype) ((v).type = (vtype), (v).u.data = NULL)

PIC_INLINE double
pic_float(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return v.u.f;
}

PIC_INLINE int
pic_int(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return v.u.i;
}

PIC_INLINE char
pic_char(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return v.u.c;
}

#endif

enum pic_tt {
  /* immediate */
  PIC_TT_NIL,
  PIC_TT_BOOL,
  PIC_TT_FLOAT,
  PIC_TT_INT,
  PIC_TT_CHAR,
  PIC_TT_EOF,
  PIC_TT_UNDEF,
  PIC_TT_INVALID,
  /* heap */
  PIC_TT_SYMBOL,
  PIC_TT_PAIR,
  PIC_TT_STRING,
  PIC_TT_VECTOR,
  PIC_TT_BLOB,
  PIC_TT_PROC,
  PIC_TT_PORT,
  PIC_TT_ERROR,
  PIC_TT_ID,
  PIC_TT_ENV,
  PIC_TT_DATA,
  PIC_TT_DICT,
  PIC_TT_WEAK,
  PIC_TT_RECORD,
  PIC_TT_CXT,
  PIC_TT_CP
};

#define PIC_OBJECT_HEADER			\
  enum pic_tt tt;                               \
  char gc_mark;

struct pic_basic {
  PIC_OBJECT_HEADER
};

struct pic_object;
struct pic_symbol;
struct pic_pair;
struct pic_string;
struct pic_vector;
struct pic_blob;

struct pic_proc;
struct pic_port;
struct pic_error;
struct pic_env;

/* set aliases to basic types */
typedef struct pic_symbol pic_sym;
typedef struct pic_id pic_id;
typedef struct pic_pair pic_pair;
typedef struct pic_vector pic_vec;

#define pic_obj_p(pic,v) (pic_vtype(pic,v) == PIC_VTYPE_HEAP)
#define pic_obj_ptr(v) ((struct pic_object *)pic_ptr(v))

#define pic_invalid_p(pic, v) (pic_vtype(pic, v) == PIC_VTYPE_INVALID)
#define pic_eof_p(pic, v) (pic_vtype(pic, v) == PIC_VTYPE_EOF)

#define pic_test(pic, v) (! pic_false_p(pic, v))

PIC_INLINE bool
pic_valid_int(double v)
{
  return INT_MIN <= v && v <= INT_MAX;
}

PIC_INLINE pic_value pic_invalid_value();
PIC_INLINE pic_value pic_obj_value(void *);

PIC_INLINE enum pic_tt
pic_type(pic_state PIC_UNUSED(*pic), pic_value v)
{
  switch (pic_vtype(pic, v)) {
  case PIC_VTYPE_NIL:
    return PIC_TT_NIL;
  case PIC_VTYPE_TRUE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_FALSE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_UNDEF:
    return PIC_TT_UNDEF;
  case PIC_VTYPE_INVALID:
    return PIC_TT_INVALID;
  case PIC_VTYPE_FLOAT:
    return PIC_TT_FLOAT;
  case PIC_VTYPE_INT:
    return PIC_TT_INT;
  case PIC_VTYPE_CHAR:
    return PIC_TT_CHAR;
  case PIC_VTYPE_EOF:
    return PIC_TT_EOF;
  case PIC_VTYPE_HEAP:
    return ((struct pic_basic *)pic_ptr(v))->tt;
  }

  PIC_UNREACHABLE();
}

PIC_INLINE const char *
pic_type_repr(pic_state PIC_UNUSED(*pic), enum pic_tt tt)
{
  switch (tt) {
  case PIC_TT_NIL:
    return "nil";
  case PIC_TT_BOOL:
    return "boolean";
  case PIC_TT_FLOAT:
    return "float";
  case PIC_TT_INT:
    return "int";
  case PIC_TT_SYMBOL:
    return "symbol";
  case PIC_TT_CHAR:
    return "char";
  case PIC_TT_EOF:
    return "eof";
  case PIC_TT_UNDEF:
    return "undef";
  case PIC_TT_INVALID:
    return "invalid";
  case PIC_TT_PAIR:
    return "pair";
  case PIC_TT_STRING:
    return "string";
  case PIC_TT_VECTOR:
    return "vector";
  case PIC_TT_BLOB:
    return "blob";
  case PIC_TT_PORT:
    return "port";
  case PIC_TT_ERROR:
    return "error";
  case PIC_TT_ID:
    return "id";
  case PIC_TT_CXT:
    return "cxt";
  case PIC_TT_PROC:
    return "proc";
  case PIC_TT_ENV:
    return "env";
  case PIC_TT_DATA:
    return "data";
  case PIC_TT_DICT:
    return "dict";
  case PIC_TT_WEAK:
    return "weak";
  case PIC_TT_RECORD:
    return "record";
  case PIC_TT_CP:
    return "checkpoint";
  }
  PIC_UNREACHABLE();
}

PIC_INLINE pic_value
pic_nil_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_NIL);
  return v;
}

PIC_INLINE pic_value
pic_true_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_TRUE);
  return v;
}

PIC_INLINE pic_value
pic_false_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FALSE);
  return v;
}

PIC_INLINE pic_value
pic_bool_value(pic_state PIC_UNUSED(*pic), bool b)
{
  pic_value v;

  pic_init_value(v, b ? PIC_VTYPE_TRUE : PIC_VTYPE_FALSE);
  return v;
}

#if PIC_NAN_BOXING

PIC_INLINE pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v |= 0xfffffffffffful & (uint64_t)ptr;
  return v;
}

PIC_INLINE pic_value
pic_float_value(pic_state PIC_UNUSED(*pic), double f)
{
  union { double f; uint64_t i; } u;

  if (f != f) {
    return 0x7ff8000000000000ul;
  } else {
    u.f = f;
    return u.i;
  }
}

PIC_INLINE pic_value
pic_int_value(pic_state PIC_UNUSED(*pic), int i)
{
  union { int i; unsigned u; } u;
  pic_value v;

  u.i = i;

  pic_init_value(v, PIC_VTYPE_INT);
  v |= u.u;
  return v;
}

PIC_INLINE pic_value
pic_char_value(pic_state PIC_UNUSED(*pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v |= c;
  return v;
}

#else

PIC_INLINE pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v.u.data = ptr;
  return v;
}

PIC_INLINE pic_value
pic_float_value(pic_state PIC_UNUSED(*pic), double f)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FLOAT);
  v.u.f = f;
  return v;
}

PIC_INLINE pic_value
pic_int_value(pic_state PIC_UNUSED(*pic), int i)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_INT);
  v.u.i = i;
  return v;
}

PIC_INLINE pic_value
pic_char_value(pic_state PIC_UNUSED(*pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v.u.c = c;
  return v;
}

#endif

PIC_INLINE pic_value
pic_undef_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_UNDEF);
  return v;
}

PIC_INLINE pic_value
pic_invalid_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_INVALID);
  return v;
}

#if PIC_NAN_BOXING

PIC_INLINE bool
pic_eq_p(pic_state PIC_UNUSED(*pic), pic_value x, pic_value y)
{
  return x == y;
}

PIC_INLINE bool
pic_eqv_p(pic_state PIC_UNUSED(*pic), pic_value x, pic_value y)
{
  return x == y;
}

#else

PIC_INLINE bool
pic_eq_p(pic_state PIC_UNUSED(*pic), pic_value x, pic_value y)
{
  if (pic_type(pic, x) != pic_type(pic, y))
    return false;

  switch (pic_type(pic, x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_BOOL:
    return pic_vtype(pic, x) == pic_vtype(pic, y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

PIC_INLINE bool
pic_eqv_p(pic_state PIC_UNUSED(*pic), pic_value x, pic_value y)
{
  if (pic_type(pic, x) != pic_type(pic, y))
    return false;

  switch (pic_type(pic, x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_BOOL:
    return pic_vtype(pic, x) == pic_vtype(pic, y);
  case PIC_TT_FLOAT:
    return pic_float(pic, x) == pic_float(pic, y);
  case PIC_TT_INT:
    return pic_int(pic, x) == pic_int(pic, y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

#endif

pic_value pic_add(pic_state *, pic_value, pic_value);
pic_value pic_sub(pic_state *, pic_value, pic_value);
pic_value pic_mul(pic_state *, pic_value, pic_value);
pic_value pic_div(pic_state *, pic_value, pic_value);
bool pic_eq(pic_state *, pic_value, pic_value);
bool pic_lt(pic_state *, pic_value, pic_value);
bool pic_le(pic_state *, pic_value, pic_value);
bool pic_gt(pic_state *, pic_value, pic_value);
bool pic_ge(pic_state *, pic_value, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
