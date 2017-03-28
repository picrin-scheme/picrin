/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VALUE_H
#define PICRIN_VALUE_H

#if defined(__cplusplus)
extern "C" {
#endif

#if 0
#define pic_int pic_int_inline
#define pic_float pic_float_inline
#define pic_char pic_char_inline
#define pic_int_value pic_int_value_inline
#define pic_float_value pic_float_value_inline
#define pic_char_value pic_char_value_inline
#define obj_value obj_value_inline /* TODO */
#define pic_nil_value pic_nil_value_inline
#define pic_eof_object pic_eof_object_inline
#define pic_true_value pic_true_value_inline
#define pic_false_value pic_false_value_inline
#define pic_undef_value pic_undef_value_inline
#define pic_invalid_value pic_invalid_value_inline
#define pic_bool_value pic_bool_value_inline
#define pic_invalid_p pic_invalid_p_inline
#define pic_float_p pic_float_p_inline
#define pic_int_p pic_int_p_inline
#define pic_char_p pic_char_p_inline
#define pic_eof_p pic_eof_p_inline
#define pic_undef_p pic_undef_p_inline
#define pic_true_p pic_true_p_inline
#define pic_nil_p pic_nil_p_inline
#define pic_false_p pic_false_p_inline
#define pic_bool_p pic_bool_p_inline
#define pic_str_p pic_str_p_inline
#define pic_vec_p pic_vec_p_inline
#define pic_blob_p pic_blob_p_inline
#define pic_error_p pic_error_p_inline
#define pic_id_p pic_id_p_inline
#define pic_dict_p pic_dict_p_inline
#define pic_weak_p pic_weak_p_inline
#define pic_env_p pic_env_p_inline
#define pic_rec_p pic_rec_p_inline
#define pic_sym_p pic_sym_p_inline
#define pic_pair_p pic_pair_p_inline
#define pic_cp_p pic_cp_p_inline
#define pic_func_p pic_func_p_inline
#define pic_irep_p pic_irep_p_inline
#define pic_proc_p pic_proc_p_inline
#endif

/* #ifndef INLINE */
/* # if GENERATE_EXTERNAL_DEFINITION */
/* #  define INLINE PIC_EXTERN_INLINE */
/* # else */
/* #  define INLINE PIC_INLINE */
/* # endif */
/* #endif */

#define INLINE PIC_STATIC_INLINE

enum {
  PIC_TYPE_INVALID = 1,
  PIC_TYPE_FLOAT   = 2,
  PIC_TYPE_INT     = 3,
  PIC_TYPE_CHAR    = 4,
  PIC_TYPE_EOF     = 5,
  PIC_TYPE_UNDEF   = 6,
  PIC_TYPE_TRUE    = 8,
  PIC_TYPE_NIL     = 7,
  PIC_TYPE_FALSE   = 9,
  PIC_IVAL_END     = 10,
/* -------------------- */
  PIC_TYPE_STRING  = 16,
  PIC_TYPE_VECTOR  = 17,
  PIC_TYPE_BLOB    = 18,
  PIC_TYPE_PORT    = 20,
  PIC_TYPE_ERROR   = 21,
  PIC_TYPE_ID      = 22,
  PIC_TYPE_ENV     = 23,
  PIC_TYPE_DATA    = 24,
  PIC_TYPE_DICT    = 25,
  PIC_TYPE_WEAK    = 26,
  PIC_TYPE_RECORD  = 27,
  PIC_TYPE_SYMBOL  = 28,
  PIC_TYPE_PAIR    = 29,
  PIC_TYPE_CXT     = 30,
  PIC_TYPE_CP      = 31,
  PIC_TYPE_FUNC    = 32,
  PIC_TYPE_IREP    = 33
};

PIC_STATIC_INLINE int obj_tt(void *); /* defined in object.h */

#if !PIC_NAN_BOXING

PIC_STATIC_INLINE pic_value
make_value(int type)
{
  pic_value v;
  v.type = type;
  v.u.data = NULL;
  return v;
}

PIC_STATIC_INLINE struct object *
obj_ptr(pic_value v)
{
  return (struct object *)(v.u.data);
}

PIC_STATIC_INLINE bool
obj_p(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.type > PIC_IVAL_END;
}

PIC_STATIC_INLINE pic_value
obj_value(void *ptr)
{
  pic_value v = make_value(obj_tt(ptr));
  v.u.data = ptr;
  return v;
}

PIC_STATIC_INLINE int
value_type(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return (int)(v.type);
}

INLINE int
pic_int(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.i;
}

INLINE double
pic_float(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.f;
}

INLINE char
pic_char(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.u.c;
}

INLINE pic_value
pic_int_value(pic_state *PIC_UNUSED(pic), int i)
{
  pic_value v = make_value(PIC_TYPE_INT);
  v.u.i = i;
  return v;
}

INLINE pic_value
pic_float_value(pic_state *PIC_UNUSED(pic), double f)
{
  pic_value v = make_value(PIC_TYPE_FLOAT);
  v.u.f = f;
  return v;
}

INLINE pic_value
pic_char_value(pic_state *PIC_UNUSED(pic), char c)
{
  pic_value v = make_value(PIC_TYPE_CHAR);
  v.u.c = c;
  return v;
}

#else  /* NAN_BOXING */

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT TTPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 111111111111TTTT TT00000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   char  : 111111111111TTTT TT00000000000000 CCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCC
 */

PIC_STATIC_INLINE pic_value
make_value(int type)
{
  pic_value v;
  v.v = 0xfff0000000000000ul | ((uint64_t)(type) << 46);
  return v;
}

PIC_STATIC_INLINE struct object *
obj_ptr(pic_value v)
{
  return (struct object *)((0x3ffffffffffful & v.v) << 2);
}

PIC_STATIC_INLINE bool
obj_p(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.v > ((0x3ffC0ul + (0x3f & PIC_IVAL_END)) << 46);
}

PIC_STATIC_INLINE pic_value
obj_value(void *ptr)
{
  pic_value v = make_value(obj_tt(ptr));
  v.v |= 0x3ffffffffffful & ((uint64_t)ptr >> 2);
  return v;
}

PIC_STATIC_INLINE int
value_type(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return 0xfff0000000000000ul >= v.v ? PIC_TYPE_FLOAT : ((v.v >> 46) & 0x3f);
}

INLINE int
pic_int(pic_state *PIC_UNUSED(pic), pic_value v)
{
  union { int i; unsigned u; } u;
  u.u = v.v & 0xfffffffful;
  return u.i;
}

INLINE double
pic_float(pic_state *PIC_UNUSED(pic), pic_value v)
{
  union { double f; uint64_t i; } u;
  u.i = v.v;
  return u.f;
}

INLINE char
pic_char(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.v & 0xfffffffful;
}

INLINE pic_value
pic_int_value(pic_state *PIC_UNUSED(pic), int i)
{
  pic_value v = make_value(PIC_TYPE_INT);
  v.v |= (unsigned)i;
  return v;
}

INLINE pic_value
pic_float_value(pic_state *PIC_UNUSED(pic), double f)
{
  union { double f; uint64_t i; } u;
  pic_value v;

  if (f != f) {
    v.v = 0x7ff8000000000000ul;
  } else {
    u.f = f;
    v.v = u.i;
  }
  return v;
}

INLINE pic_value
pic_char_value(pic_state *PIC_UNUSED(pic), char c)
{
  pic_value v = make_value(PIC_TYPE_CHAR);
  v.v |= (unsigned char)c;
  return v;
}

#endif  /* NAN_BOXING end */

#define DEFVAL(name, type)                              \
  INLINE pic_value name(pic_state *PIC_UNUSED(pic)) {   \
    return make_value(type);                            \
  }

DEFVAL(pic_nil_value, PIC_TYPE_NIL)
DEFVAL(pic_eof_object, PIC_TYPE_EOF)
DEFVAL(pic_true_value, PIC_TYPE_TRUE)
DEFVAL(pic_false_value, PIC_TYPE_FALSE)
DEFVAL(pic_undef_value, PIC_TYPE_UNDEF)
DEFVAL(pic_invalid_value, PIC_TYPE_INVALID)

INLINE pic_value
pic_bool_value(pic_state *PIC_UNUSED(pic), bool b)
{
  return make_value(b ? PIC_TYPE_TRUE : PIC_TYPE_FALSE);
}

#define DEFPRED(name, type)                             \
  INLINE bool name(pic_state *pic, pic_value obj) {     \
    return value_type(pic, obj) == type;                \
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
DEFPRED(pic_error_p, PIC_TYPE_ERROR)
DEFPRED(pic_dict_p, PIC_TYPE_DICT)
DEFPRED(pic_weak_p, PIC_TYPE_WEAK)
DEFPRED(pic_env_p, PIC_TYPE_ENV)
DEFPRED(pic_rec_p, PIC_TYPE_RECORD)
DEFPRED(pic_sym_p, PIC_TYPE_SYMBOL)
DEFPRED(pic_pair_p, PIC_TYPE_PAIR)
DEFPRED(pic_cp_p, PIC_TYPE_CP)
DEFPRED(pic_func_p, PIC_TYPE_FUNC)
DEFPRED(pic_irep_p, PIC_TYPE_IREP)

INLINE bool
pic_bool_p(pic_state *pic, pic_value obj)
{
  return pic_true_p(pic, obj) || pic_false_p(pic, obj);
}

INLINE bool
pic_proc_p(pic_state *pic, pic_value o)
{
  return pic_func_p(pic, o) || pic_irep_p(pic, o);
}

INLINE bool
pic_id_p(pic_state *pic, pic_value o)
{
  return value_type(pic, o) == PIC_TYPE_ID || pic_sym_p(pic, o);
}

#if defined(__cplusplus)
}
#endif

#endif
