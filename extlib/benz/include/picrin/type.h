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

#define pic_invalid_p(pic, v) (pic_vtype(pic, v) == PIC_TYPE_INVALID)

#if PIC_NAN_BOXING

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 111111111111TTTT 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   char  : 111111111111TTTT 0000000000000000 CCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCC
 */

#define pic_init_value(v,vtype) (v = (0xfff0000000000000ul | ((uint64_t)(vtype) << 48)))

PIC_INLINE int
pic_vtype(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return 0xfff0 >= (v >> 48) ? PIC_TYPE_FLOAT : ((v >> 48) & 0xf);
}

PIC_INLINE double
pic_float(pic_state PIC_UNUSED(*pic), pic_value v)
{
  union { double f; uint64_t i; } u;
  u.i = v;
  return u.f;
}

PIC_INLINE int
pic_int(pic_state PIC_UNUSED(*pic), pic_value v)
{
  union { int i; unsigned u; } u;
  u.u = v & 0xfffffffful;
  return u.i;
}

PIC_INLINE char
pic_char(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return v & 0xfffffffful;
}

PIC_INLINE struct pic_object *
pic_obj_ptr(pic_value v)
{
  return (struct pic_object *)(0xfffffffffffful & v);
}

#else

#define pic_init_value(v,vtype) ((v).type = (vtype), (v).u.data = NULL)

PIC_INLINE int
pic_vtype(pic_state PIC_UNUSED(*pic), pic_value v)
{
  return (int)(v.type);
}

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

PIC_INLINE struct pic_object *
pic_obj_ptr(pic_value v)
{
  return (struct pic_object *)(v.u.data);
}

#endif

PIC_INLINE bool
pic_valid_int(double v)
{
  return INT_MIN <= v && v <= INT_MAX;
}

PIC_INLINE pic_value
pic_nil_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_NIL);
  return v;
}

PIC_INLINE pic_value
pic_eof_object(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_EOF);
  return v;
}

PIC_INLINE pic_value
pic_true_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_TRUE);
  return v;
}

PIC_INLINE pic_value
pic_false_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_FALSE);
  return v;
}

PIC_INLINE pic_value
pic_bool_value(pic_state PIC_UNUSED(*pic), bool b)
{
  pic_value v;

  pic_init_value(v, b ? PIC_TYPE_TRUE : PIC_TYPE_FALSE);
  return v;
}

PIC_INLINE pic_value
pic_undef_value(pic_state PIC_UNUSED(*pic))
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_UNDEF);
  return v;
}

PIC_INLINE pic_value
pic_invalid_value()
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_INVALID);
  return v;
}

#if PIC_NAN_BOXING

PIC_INLINE pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_IVAL_END);
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
  pic_value v;

  pic_init_value(v, PIC_TYPE_INT);
  v |= (unsigned)i;
  return v;
}

PIC_INLINE pic_value
pic_char_value(pic_state PIC_UNUSED(*pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_CHAR);
  v |= (unsigned char)c;
  return v;
}

#else

PIC_INLINE pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_IVAL_END);
  v.u.data = ptr;
  return v;
}

PIC_INLINE pic_value
pic_float_value(pic_state PIC_UNUSED(*pic), double f)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_FLOAT);
  v.u.f = f;
  return v;
}

PIC_INLINE pic_value
pic_int_value(pic_state PIC_UNUSED(*pic), int i)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_INT);
  v.u.i = i;
  return v;
}

PIC_INLINE pic_value
pic_char_value(pic_state PIC_UNUSED(*pic), char c)
{
  pic_value v;

  pic_init_value(v, PIC_TYPE_CHAR);
  v.u.c = c;
  return v;
}

#endif

#if defined(__cplusplus)
}
#endif

#endif
