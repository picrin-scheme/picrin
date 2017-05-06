/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VALUE_H
#define PICRIN_VALUE_H

#if defined(__cplusplus)
extern "C" {
#endif

enum {
  PIC_TYPE_INVALID   = 1,
  PIC_TYPE_FLOAT     = 2,
  PIC_TYPE_INT       = 3,
  PIC_TYPE_CHAR      = 4,
  PIC_TYPE_EOF       = 5,
  PIC_TYPE_UNDEF     = 6,
  PIC_TYPE_TRUE      = 8,
  PIC_TYPE_NIL       = 7,
  PIC_TYPE_FALSE     = 9,
  PIC_IVAL_END       = 10,
/* -------------------- */
  PIC_TYPE_SYMBOL    = 16,
  PIC_TYPE_STRING    = 17,
  PIC_TYPE_BLOB      = 18,
  PIC_TYPE_DATA      = 19,
  PIC_TYPE_PAIR      = 20,
  PIC_TYPE_VECTOR    = 21,
  PIC_TYPE_DICT      = 22,
  PIC_TYPE_RECORD    = 23,
  PIC_TYPE_ATTR      = 24,
  PIC_TYPE_PORT      = 25,
  PIC_TYPE_IREP      = 27,
  PIC_TYPE_FRAME     = 28,
  PIC_TYPE_PROC_FUNC = 29,
  PIC_TYPE_PROC_IREP = 30,
  PIC_TYPE_MAX       = 63
};

#if !PIC_NAN_BOXING

PIC_STATIC_INLINE void
make_value(struct value *v, int type)
{
  static const struct value zero = { 0 };
  *v = zero;
  v->type = type;
}

PIC_STATIC_INLINE void
make_int_value(struct value *v, int i)
{
  make_value(v, PIC_TYPE_INT);
  v->u.i = i;
}

PIC_STATIC_INLINE void
make_float_value(struct value *v, double f)
{
  make_value(v, PIC_TYPE_FLOAT);
  v->u.f = f;
}

PIC_STATIC_INLINE struct value
make_char_value(struct value *v, char c)
{
  make_value(v, PIC_TYPE_CHAR);
  v->u.c = c;
}

PIC_STATIC_INLINE void
make_obj_value(struct value *v, void *p, int type)
{
  make_value(v, type);
  v->u.p = p;
}

PIC_STATIC_INLINE int
value_type(struct value *v)
{
  return (int)(v->type);
}

PIC_STATIC_INLINE int
value_int(struct value *v)
{
  return v->u.i;
}

PIC_STATIC_INLINE double
value_float(struct value *v)
{
  return v->u.f;
}

PIC_STATIC_INLINE char
value_char(struct value *v)
{
  return v->u.c;
}

PIC_STATIC_INLINE void *
value_ptr(struct value *v)
{
  return v->u.p;
}

PIC_STATIC_INLINE bool
value_eq_p(struct value *x, struct value *y)
{
  return memcmp(x, y, sizeof(struct value)) == 0;
}

PIC_STATIC_INLINE bool
value_eqv_p(struct value *x, struct value *y)
{
  return memcmp(x, y, sizeof(struct value)) == 0;
}

#else  /* NAN_BOXING */

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT TTPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 111111111111TTTT TT00000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   char  : 111111111111TTTT TT00000000000000 CCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCC
 */

PIC_STATIC_INLINE void
make_value(struct value *v, int type)
{
  v->v = 0xfff0000000000000ul | ((uint64_t)(type) << 46);
}

PIC_STATIC_INLINE void
make_int_value(struct value *v, int i)
{
  make_value(v, PIC_TYPE_INT);
  v->v |= (unsigned)i;
}

PIC_STATIC_INLINE void
make_float_value(struct value *v, double f)
{
  if (f != f) {
    v->v = 0x7ff8000000000000ul;
  } else {
    union { double f; uint64_t i; } u;
    u.f = f;
    v->v = u.i;
  }
}

PIC_STATIC_INLINE void
make_char_value(struct value *v, char c)
{
  make_value(v, PIC_TYPE_CHAR);
  v->v |= (unsigned char)c;
}

PIC_STATIC_INLINE void
make_obj_value(struct value *v, void *ptr, int type)
{
  make_value(v, type);
  v->v |= 0x3ffffffffffful & ((uint64_t)ptr >> 2);
}

PIC_STATIC_INLINE int
value_type(struct value *v)
{
  return 0xfff0000000000000ul >= v->v ? PIC_TYPE_FLOAT : ((v->v >> 46) & 0x3f);
}

PIC_STATIC_INLINE int
value_int(struct value *v)
{
  union { int i; unsigned u; } u;
  u.u = v->v & 0xfffffffful;
  return u.i;
}

PIC_STATIC_INLINE double
value_float(struct value *v)
{
  union { double f; uint64_t i; } u;
  u.i = v->v;
  return u.f;
}

PIC_STATIC_INLINE char
value_char(struct value *v)
{
  return v->v & 0xfffffffful;
}

PIC_STATIC_INLINE void *
value_ptr(struct value *v)
{
  return (void *)((0x3ffffffffffful & v->v) << 2);
}

PIC_STATIC_INLINE bool
value_eq_p(struct value *x, struct value *y)
{
  return x->v == y->v;
}

PIC_STATIC_INLINE bool
value_eqv_p(struct value *x, struct value *y)
{
  return x->v == y->v;
}

#endif  /* NAN_BOXING end */


#define DEFPRED(name, type)                     \
  PIC_STATIC_INLINE bool                        \
  value_##name##_p(struct value *v) {           \
    return value_type(v) == type;               \
  }

DEFPRED(invalid, PIC_TYPE_INVALID)
DEFPRED(float, PIC_TYPE_FLOAT)
DEFPRED(int, PIC_TYPE_INT)
DEFPRED(char, PIC_TYPE_CHAR)
DEFPRED(eof, PIC_TYPE_EOF)
DEFPRED(undef, PIC_TYPE_UNDEF)
DEFPRED(true, PIC_TYPE_TRUE)
DEFPRED(nil, PIC_TYPE_NIL)
DEFPRED(false, PIC_TYPE_FALSE)
DEFPRED(str, PIC_TYPE_STRING)
DEFPRED(vec, PIC_TYPE_VECTOR)
DEFPRED(blob, PIC_TYPE_BLOB)
DEFPRED(dict, PIC_TYPE_DICT)
DEFPRED(attr, PIC_TYPE_ATTR)
DEFPRED(rec, PIC_TYPE_RECORD)
DEFPRED(sym, PIC_TYPE_SYMBOL)
DEFPRED(pair, PIC_TYPE_PAIR)
DEFPRED(proc_func, PIC_TYPE_PROC_FUNC)
DEFPRED(proc_irep, PIC_TYPE_PROC_IREP)
DEFPRED(irep, PIC_TYPE_IREP)
DEFPRED(data, PIC_TYPE_DATA)
DEFPRED(port, PIC_TYPE_PORT)

#undef DEFPRED

PIC_STATIC_INLINE bool
value_obj_p(struct value *v)
{
  return value_type(v) > PIC_IVAL_END;
}

void *pic_ptr(pic_state *, pic_value);
int pic_type(pic_state *, pic_value);
pic_value pic_invalid_value(pic_state *);
pic_value pic_obj_value(pic_state *, void *, int);
bool pic_invalid_p(pic_state *, pic_value);
bool pic_attr_p(pic_state *, pic_value);
bool pic_rec_p(pic_state *, pic_value);
bool pic_irep_p(pic_state *, pic_value);
bool pic_proc_func_p(pic_state *, pic_value);
bool pic_proc_irep_p(pic_state *, pic_value);
bool pic_obj_p(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
