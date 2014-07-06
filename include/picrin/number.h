/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_NUMBER_H__
#define PICRIN_NUMBER_H__

#if defined(__cplusplus)
extern "C" {
#endif

#include <math.h>
#include <gmp.h>
#include <mpfr.h>

#define pic_bigint_p(v) (pic_type(v) == PIC_TT_BIGINT)
#define pic_bigint_ptr(o) ((struct pic_bigint *)pic_ptr(o))

#define pic_rational_p(v) (pic_type(v) == PIC_TT_RATIONAL)
#define pic_rational_ptr(o) ((struct pic_rational *)pic_ptr(o))

#define pic_bigfloat_p(v) (pic_type(v) == PIC_TT_BIGFLOAT)
#define pic_bigfloat_ptr(o) ((struct pic_bigfloat *)pic_ptr(o))

pic_value pic_read_bigint(pic_state *, char *, int);
pic_value pic_read_rational(pic_state *, char *, int);
pic_value pic_read_bigfloat(pic_state *, char *, int);

pic_bigint *pic_bigint_new(pic_state *);
pic_rational *pic_rational_new(pic_state *);
pic_bigfloat *pic_bigfloat_new(pic_state *);

#define pic_number_p(o)    (pic_int_p(o) || pic_float_p(o) || pic_bigint_p(o) || pic_rational_p(o) || pic_bigfloat_p(o))

#define pic_number_to_int(o)                                            \
    (pic_int_p(o)      ? pic_int(o)                             :       \
     pic_float_p(o)    ? (int)pic_float(o)                      :       \
     pic_bigint_p(o)   ?      mpz_get_si(pic_bigint_ptr(o)->z)  :       \
     pic_rational_p(o) ? (int)mpq_get_d(pic_rational_ptr(o)->q) :       \
     mpfr_get_si(pic_bigfloat_ptr(o)->f))
    
    
#define pic_number_to_float(o)                                  \
    (pic_int_p(o)      ? (double)pic_int(o)                 :   \
     pic_float_p(o)    ? pic_float(o)                       :   \
     pic_bigint_p(o)   ? mpz_get_d(pic_bigint_ptr(o)->z)    :   \
     pic_rational_p(o) ? mpq_get_d(pic_rational_ptr(o)->q)  :   \
     mpfr_get_d(pic_bigfloat_ptr(o)->f, MPFR_RNDN))

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
  case PIC_TT_RATIONAL:{
    pic_rational *q = pic_rational_ptr(*v);
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

#define mpfr_si_add(rop, op1, op2, rnd) mpfr_add_si(rop, op2, op1, rnd)
#define mpfr_d_add(rop, op1, op2, rnd) mpfr_add_d(rop, op2, op1, rnd)
#define mpfr_z_add(rop, op1, op2, rnd) mpfr_add_z(rop, op2, op1, rnd)
#define mpfr_q_add(rop, op1, op2, rnd) mpfr_add_q(rop, op2, op1, rnd)

#define mpfr_q_sub(rop, op1, op2, rnd) mpfr_sub_q(rop, op2, op1, rnd);mpfr_neg(rop, rop, rnd);

#define mpfr_si_mul(rop, op1, op2, rnd) mpfr_mul_si(rop, op2, op1, rnd)
#define mpfr_d_mul(rop, op1, op2, rnd) mpfr_mul_d(rop, op2, op1, rnd)
#define mpfr_z_mul(rop, op1, op2, rnd) mpfr_mul_z(rop, op2, op1, rnd)
#define mpfr_q_mul(rop, op1, op2, rnd) mpfr_mul_q(rop, op2, op1, rnd)

#define mpfr_z_div(rop, op1, op2, rnd) mpfr_div_z(rop, op2, op1, rnd);mpfr_ui_div(rop, 1, rop, rnd);
#define mpfr_q_div(rop, op1, op2, rnd) mpfr_div_q(rop, op2, op1, rnd);mpfr_ui_div(rop, 1, rop, rnd);

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
