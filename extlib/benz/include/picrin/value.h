/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VALUE_H
#define PICRIN_VALUE_H

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * `undef` values never seen from user-end: that is,
 *  it's used only for repsenting internal special state
 */

enum pic_vtype {
  PIC_VTYPE_NIL = 1,
  PIC_VTYPE_TRUE,
  PIC_VTYPE_FALSE,
  PIC_VTYPE_UNDEF,
#if PIC_ENABLE_FLOAT
  PIC_VTYPE_FLOAT,
#endif
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
pic_vtype(pic_value v)
{
  return 0xfff0 >= (v >> 48) ? PIC_VTYPE_FLOAT : ((v >> 48) & 0xf);
}

static inline double
pic_float(pic_value v)
{
  union { double f; uint64_t i; } u;
  u.i = v;
  return u.f;
}

static inline int
pic_int(pic_value v)
{
  union { int i; unsigned u; } u;
  u.u = v & 0xfffffffful;
  return u.i;
}

#define pic_char(v) ((v) & 0xfffffffful)

#elif PIC_WORD_BOXING

typedef long pic_value;

#define pic_ptr(v) ((void *)(v))
#define pic_init_value(v,vtype) do {            \
    v = (vtype << 3) + 7;                       \
  } while (0)

PIC_INLINE enum pic_vtype
pic_vtype(pic_value v)
{
  if ((v & 1) == 0) {
    return PIC_VTYPE_HEAP;
  }
  if ((v & 2) == 0) {
    return PIC_VTYPE_INT;
  }
  if ((v & 4) == 0) {
    return PIC_VTYPE_CHAR;
  }
  return v >> 3;
}

PIC_INLINE int
pic_int(pic_value v)
{
  return v >> 2;
}

PIC_INLINE char
pic_char(pic_value v)
{
  return v >> 3;
}

#else

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
#if PIC_ENABLE_FLOAT
    double f;
#endif
    int i;
    char c;
  } u;
} pic_value;

#define pic_ptr(v) ((v).u.data)
#define pic_vtype(v) ((v).type)
#define pic_init_value(v,vtype) ((v).type = (vtype), (v).u.data = NULL)

#if PIC_ENABLE_FLOAT
# define pic_float(v) ((v).u.f)
#endif
#define pic_int(v) ((v).u.i)
#define pic_char(v) ((v).u.c)

#endif

enum pic_tt {
  /* immediate */
  PIC_TT_NIL,
  PIC_TT_BOOL,
#if PIC_ENABLE_FLOAT
  PIC_TT_FLOAT,
#endif
  PIC_TT_INT,
  PIC_TT_CHAR,
  PIC_TT_EOF,
  PIC_TT_UNDEF,
  /* heap */
  PIC_TT_SYMBOL,
  PIC_TT_PAIR,
  PIC_TT_STRING,
  PIC_TT_VECTOR,
  PIC_TT_BLOB,
  PIC_TT_PROC,
  PIC_TT_PORT,
  PIC_TT_ERROR,
  PIC_TT_ENV,
  PIC_TT_SENV,
  PIC_TT_LIB,
  PIC_TT_IREP,
  PIC_TT_DATA,
  PIC_TT_DICT,
  PIC_TT_RECORD
};

#define PIC_OBJECT_HEADER			\
  enum pic_tt tt;

struct pic_object {
  PIC_OBJECT_HEADER
};

struct pic_symbol;
struct pic_pair;
struct pic_string;
struct pic_vector;
struct pic_blob;

struct pic_proc;
struct pic_port;
struct pic_error;

/* set aliases to basic types */
typedef pic_value pic_list;
typedef struct pic_symbol pic_sym;
typedef struct pic_pair pic_pair;
typedef struct pic_string pic_str;
typedef struct pic_vector pic_vec;
typedef struct pic_blob pic_blob;

#define pic_obj_p(v) (pic_vtype(v) == PIC_VTYPE_HEAP)
#define pic_obj_ptr(v) ((struct pic_object *)pic_ptr(v))

#define pic_nil_p(v) (pic_vtype(v) == PIC_VTYPE_NIL)
#define pic_true_p(v) (pic_vtype(v) == PIC_VTYPE_TRUE)
#define pic_false_p(v) (pic_vtype(v) == PIC_VTYPE_FALSE)
#define pic_undef_p(v) (pic_vtype(v) == PIC_VTYPE_UNDEF)
#define pic_float_p(v) (pic_vtype(v) == PIC_VTYPE_FLOAT)
#define pic_int_p(v) (pic_vtype(v) == PIC_VTYPE_INT)
#define pic_char_p(v) (pic_vtype(v) == PIC_VTYPE_CHAR)
#define pic_eof_p(v) (pic_vtype(v) == PIC_VTYPE_EOF)

#define pic_test(v) (! pic_false_p(v))

PIC_INLINE enum pic_tt pic_type(pic_value);
PIC_INLINE const char *pic_type_repr(enum pic_tt);

#define pic_assert_type(pic, v, type)                           \
  if (! pic_##type##_p(v)) {                                    \
    pic_errorf(pic, "expected " #type ", but got ~s", v);       \
  }

#if PIC_ENABLE_FLOAT
PIC_INLINE bool
pic_valid_int(double v)
{
  return INT_MIN <= v && v <= INT_MAX;
}

#else
PIC_INLINE bool
pic_valid_int(int v)
{
  PIC_UNUSED(v);
  return true;
}
#endif

PIC_INLINE pic_value pic_nil_value();
PIC_INLINE pic_value pic_true_value();
PIC_INLINE pic_value pic_false_value();
PIC_INLINE pic_value pic_bool_value(bool);
PIC_INLINE pic_value pic_undef_value();
PIC_INLINE pic_value pic_obj_value(void *);
#if PIC_ENABLE_FLOAT
PIC_INLINE pic_value pic_float_value(double);
#endif
PIC_INLINE pic_value pic_int_value(int);
PIC_INLINE pic_value pic_size_value(size_t);
PIC_INLINE pic_value pic_char_value(char c);
PIC_INLINE pic_value pic_none_value();

PIC_INLINE bool pic_eq_p(pic_value, pic_value);
PIC_INLINE bool pic_eqv_p(pic_value, pic_value);

PIC_INLINE enum pic_tt
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
#if PIC_ENABLE_FLOAT
  case PIC_VTYPE_FLOAT:
    return PIC_TT_FLOAT;
#endif
  case PIC_VTYPE_INT:
    return PIC_TT_INT;
  case PIC_VTYPE_CHAR:
    return PIC_TT_CHAR;
  case PIC_VTYPE_EOF:
    return PIC_TT_EOF;
  case PIC_VTYPE_HEAP:
    return ((struct pic_object *)pic_ptr(v))->tt;
  }

  PIC_UNREACHABLE();
}

PIC_INLINE const char *
pic_type_repr(enum pic_tt tt)
{
  switch (tt) {
  case PIC_TT_NIL:
    return "nil";
  case PIC_TT_BOOL:
    return "boolean";
#if PIC_ENABLE_FLOAT
  case PIC_TT_FLOAT:
    return "float";
#endif
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
  case PIC_TT_ENV:
    return "env";
  case PIC_TT_PROC:
    return "proc";
  case PIC_TT_SENV:
    return "senv";
  case PIC_TT_LIB:
    return "lib";
  case PIC_TT_IREP:
    return "irep";
  case PIC_TT_DATA:
    return "data";
  case PIC_TT_DICT:
    return "dict";
  case PIC_TT_RECORD:
    return "record";
  }
  PIC_UNREACHABLE();
}

PIC_INLINE pic_value
pic_nil_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_NIL);
  return v;
}

PIC_INLINE pic_value
pic_true_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_TRUE);
  return v;
}

PIC_INLINE pic_value
pic_false_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FALSE);
  return v;
}

PIC_INLINE pic_value
pic_bool_value(bool b)
{
  pic_value v;

  pic_init_value(v, b ? PIC_VTYPE_TRUE : PIC_VTYPE_FALSE);
  return v;
}

PIC_INLINE pic_value
pic_size_value(size_t s)
{
#if PIC_ENABLE_FLOAT
  if (sizeof(unsigned) < sizeof(size_t)) {
    if (s > (size_t)INT_MAX) {
      return pic_float_value(s);
    }
  }
#endif
  return pic_int_value((int)s);
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
pic_float_value(double f)
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
pic_int_value(int i)
{
  union { int i; unsigned u; } u;
  pic_value v;

  u.i = i;

  pic_init_value(v, PIC_VTYPE_INT);
  v |= u.u;
  return v;
}

PIC_INLINE pic_value
pic_char_value(char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v |= c;
  return v;
}

#elif PIC_WORD_BOXING

PIC_INLINE pic_value
pic_obj_value(void *ptr)
{
  return (pic_value)ptr;
}

PIC_INLINE pic_value
pic_int_value(int i)
{
  return (i << 2) + 1;
}

PIC_INLINE pic_value
pic_char_value(char c)
{
  return (c << 3) + 3;
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

#if PIC_ENABLE_FLOAT

PIC_INLINE pic_value
pic_float_value(double f)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FLOAT);
  v.u.f = f;
  return v;
}

#endif

PIC_INLINE pic_value
pic_int_value(int i)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_INT);
  v.u.i = i;
  return v;
}

PIC_INLINE pic_value
pic_char_value(char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v.u.c = c;
  return v;
}

#endif

PIC_INLINE pic_value
pic_undef_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_UNDEF);
  return v;
}

PIC_INLINE pic_value
pic_none_value()
{
#if PIC_NONE_IS_FALSE
  return pic_false_value();
#else
# error enable PIC_NONE_IS_FALSE
#endif
}

#if PIC_NAN_BOXING || PIC_WORD_BOXING

PIC_INLINE bool
pic_eq_p(pic_value x, pic_value y)
{
  return x == y;
}

PIC_INLINE bool
pic_eqv_p(pic_value x, pic_value y)
{
  return x == y;
}

#else

PIC_INLINE bool
pic_eq_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_BOOL:
    return pic_vtype(x) == pic_vtype(y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

PIC_INLINE bool
pic_eqv_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_BOOL:
    return pic_vtype(x) == pic_vtype(y);
#if PIC_ENABLE_FLOAT
  case PIC_TT_FLOAT:
    return pic_float(x) == pic_float(y);
#endif
  case PIC_TT_INT:
    return pic_int(x) == pic_int(y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

#endif

#if defined(__cplusplus)
}
#endif

#endif
