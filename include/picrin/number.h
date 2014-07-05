/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_NUMBER_H__
#define PICRIN_NUMBER_H__

#if defined(__cplusplus)
extern "C" {
#endif

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

#define pic_number_p(o)    (pic_int_p(o) || pic_float_p(o) || pic_bigint_p(o) || pic_rational_p(o)) || pic_bigfloat_p(o))

#define pic_number_to_int(o)                                            \
    (pic_int_p(o)      ? pic_int(o)                             :       \
     pic_float_p(o)    ? (int)pic_float(o)                      :       \
     pic_bigint_p(o)   ?      mpz_get_si(pic_bigint_ptr(o)->z)  :       \
     pic_rational_p(o) ? (int)mpq_get_d(pic_rational_ptr(o)->q) :       \
     mpfr_get_si(pic_bigfloat_ptr))
    
    
#define pic_number_to_float(o)                                  \
    (pic_int_p(o)      ? (double)pic_int(o)                 :   \
     pic_float_p(o)    ? pic_float(o)                       :   \
     pic_bigint_p(o)   ? mpz_get_d(pic_bigint_ptr(o)->z)    :   \
     pic_rational_p(o) ? mpq_get_d(pic_rational_ptr(o)->q)) :   \

static inline void
pic_number_normalize(pic_state *pic, pic_value *v)
{
  UNUSED(pic);
  if(pic_vtype(*v) == PIC_VTYPE_HEAP){
    if(pic_rational_p(*v)){
      pic_rational *q = pic_rational_ptr(*v);
      if(((int) mpz_get_si(mpq_denref(q->q))) == 1){
        mpz_t z;
        mpz_init(z);
        mpq_get_num(z, q->q);
        mpq_clear(q->q);
        q->tt = PIC_TT_BIGINT;
        mpz_init(((pic_bigint *)q)->z);
        mpz_set(((pic_bigint *)q)->z, z);
        mpz_clear(z);
      }
    }
    if(pic_bigint_p(*v)){
      pic_bigint *z  = pic_bigint_ptr(*v);
      if(mpz_fits_sint_p(z->z)){
        pic_init_value(*v, PIC_VTYPE_INT);
        v->u.i = mpz_get_si(z->z);
        /* mpz_clear(z->z); */
        /* pic_free(pic, z); */
      }
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



pic_bigint *pic_bigint_add(pic_state *, pic_bigint *, pic_bigint *);
pic_bigint *pic_bigint_sub(pic_state *, pic_bigint *, pic_bigint *);
pic_bigint *pic_bigint_mul(pic_state *, pic_bigint *, pic_bigint *);
pic_rational *pic_bigint_div(pic_state *, pic_bigint *, pic_bigint *);

pic_rational *pic_rational_add(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_sub(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_mul(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_div(pic_state *, pic_rational *, pic_rational *);

pic_bigfloat *pic_bigfloat_add(pic_state *, pic_bigfloat *, pic_bigfloat *);
pic_bigfloat *pic_bigfloat_sub(pic_state *, pic_bigfloat *, pic_bigfloat *);
pic_bigfloat *pic_bigfloat_mul(pic_state *, pic_bigfloat *, pic_bigfloat *);
pic_bigfloat *pic_bigfloat_div(pic_state *, pic_bigfloat *, pic_bigfloat *);
#if defined(__cplusplus)
}
#endif

#endif
