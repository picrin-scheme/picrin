/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_NUMBER_H__
#define PICRIN_NUMBER_H__

#if defined(__cplusplus)
extern "C" {
#endif

#include <math.h>

#define pic_bigint_p(v) (pic_type(v) == PIC_TT_BIGINT)
#define pic_bigint_ptr(o) ((struct pic_bigint *)pic_ptr(o))

#define pic_bigrat_p(v) (pic_type(v) == PIC_TT_BIGRAT)
#define pic_bigrat_ptr(o) ((struct pic_bigrat *)pic_ptr(o))

#define pic_bigfloat_p(v) (pic_type(v) == PIC_TT_BIGFLOAT)
#define pic_bigfloat_ptr(o) ((struct pic_bigfloat *)pic_ptr(o))

pic_value pic_read_bigint(pic_state *, char *, int);
pic_value pic_read_bigrat(pic_state *, char *, int);
pic_value pic_read_bigfloat(pic_state *, char *, int);

pic_bigint *pic_bigint_new(pic_state *);
pic_bigrat *pic_bigrat_new(pic_state *);
pic_bigfloat *pic_bigfloat_new(pic_state *);

#define pic_number_p(o) (pic_int_p(o) || pic_float_p(o) || pic_bigint_p(o) || pic_bigrat_p(o) || pic_bigfloat_p(o))

static inline int
pic_number_to_int(pic_value o)
{
  if (pic_int_p(o)) {
    return pic_int(o);
  } else if (pic_float_p(o)) {
    return pic_float(o);
  } else if (pic_bigint_p(o)) {
    return mpz_get_si(pic_bigint_ptr(o)->z);
  } else if (pic_bigrat_p(o)) {
    return mpq_get_d(pic_bigrat_ptr(o)->q);
  } else {
    return mpfr_get_si(pic_bigfloat_ptr(o)->f, MPFR_RNDN);
  }
}

static inline float
pic_number_to_float(pic_value o)
{
  if (pic_int_p(o)) {
    return pic_int(o);
  } else if (pic_float_p(o)) {
    return pic_float(o);
  } else if (pic_bigint_p(o)) {
    return mpz_get_d(pic_bigint_ptr(o)->z);
  } else if (pic_bigrat_p(o)) {
    return mpq_get_d(pic_bigrat_ptr(o)->q);
  } else {
    return mpfr_get_d(pic_bigfloat_ptr(o)->f, MPFR_RNDN);
  }
}

enum exactness {
  EXACT,
  MAYBE,
  INEXACT
};

static inline void
pic_number_normalize(pic_state *pic, pic_value *v, enum exactness exactp)
{
  switch(pic_type(*v)){
  case PIC_TT_INT:
    break;
  case PIC_TT_FLOAT:
    if(exactp == EXACT ||
       (exactp == MAYBE && pic_float(*v) == trunc(pic_float(*v)))){
      if(INT_MIN <= pic_float(*v) && pic_float(*v))
        *v = pic_int_value(trunc(pic_float(*v)));
      else{
        pic_bigint *z = pic_bigint_new(pic);
        mpz_set_d(z->z, pic_float(*v));
        *v = pic_obj_value(z);
      }
    }
    break;
  case PIC_TT_BIGINT:{
    pic_bigint *z  = pic_bigint_ptr(*v);
    if(mpz_fits_sint_p(z->z)){
      *v = pic_int_value(mpz_get_si(z->z));
    }
    break;
  }
  case PIC_TT_BIGRAT:{
    pic_bigrat *q = pic_bigrat_ptr(*v);
    if(((int) mpz_get_si(mpq_denref(q->q))) == 1){
      if(mpz_fits_sint_p(mpq_numref(q->q))){
        *v = pic_int_value((int) mpz_get_si(mpq_numref(q->q)));
      }
      else{
        pic_bigint *z =pic_bigint_new(pic);
        mpq_get_num(z->z, q->q);
        *v = pic_obj_value(z);
      }
    }
    break;
  }
  case PIC_TT_BIGFLOAT:{
    pic_bigfloat *f = pic_bigfloat_ptr(*v);
    if(exactp == EXACT && (mpfr_integer_p(f->f) && exactp == MAYBE)){
      if(mpfr_fits_sint_p(f->f, MPFR_RNDN)){
        *v = pic_int_value(mpfr_get_si(f->f, MPFR_RNDN));
      }
      else{
        pic_bigint *z = pic_bigint_new(pic);
        mpfr_get_z(z->z, f->f, MPFR_RNDN);
        *v = pic_obj_value(z);
      }
    }
    break;
  }
  default:{
    pic_debug(pic, *v);
    pic_errorf(pic, "internal error: pic_number_normalize got non number object\n");
  }
  }
}

pic_value pic_add(pic_state *, pic_value, pic_value);
pic_value pic_sub(pic_state *, pic_value, pic_value);
pic_value pic_mul(pic_state *, pic_value, pic_value);
pic_value pic_div(pic_state *, pic_value, pic_value);

bool pic_eq(pic_state *, pic_value, pic_value);
bool pic_gt(pic_state *, pic_value, pic_value);
bool pic_ge(pic_state *, pic_value, pic_value);
bool pic_lt(pic_state *, pic_value, pic_value);
bool pic_le(pic_state *, pic_value, pic_value);

pic_value pic_number_to_string(pic_state *, pic_value, int);

#if defined(__cplusplus)
}
#endif

#endif
