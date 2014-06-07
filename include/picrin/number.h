/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_NUMBER_H__
#define PICRIN_NUMBER_H__

#if defined(__cplusplus)
extern "C" {
#endif

#include <gmp.h>

#define pic_bigint_p(v) (pic_type(v) == PIC_TT_BIGINT)
#define pic_bigint_ptr(o) ((struct pic_bigint *)pic_ptr(o))

#define pic_rational_p(v) (pic_type(v) == PIC_TT_RATIONAL)
#define pic_rational_ptr(o) ((struct pic_rational *)pic_ptr(o))
pic_bigint *pic_read_bigint(pic_state *, char *, int);
pic_rational *pic_read_rational(pic_state *, char *, int);

pic_bigint *pic_bigint_new(pic_state *, mpz_t);
pic_rational *pic_rational_new(pic_state *, mpq_t);

#define pic_number_p(o)    (pic_int_p(o) || pic_float_p(o) || pic_bigint_p(o) || pic_rational_p(o))

#define pic_number_to_int(o)                                          \
    (pic_int_p(o)    ? pic_int(o)                             :       \
     pic_float_p(o)  ? (int)pic_float(o)                      :       \
     pic_bigint_p(o) ? (int)mpz_get_si(pic_bigint_ptr(o)->z)  :       \
                       (int)mpq_get_d(pic_rational_ptr(o)->q))
    
#define pic_number_to_float(o)                                          \
    (pic_int_p(o)    ? (double)pic_int(o)                :              \
     pic_float_p(o)  ? pic_float(o)                      :              \
     pic_bigint_p(o) ? mpz_get_d(pic_bigint_ptr(o)->z)   :              \
                       mpq_get_d(pic_rational_ptr(o)->q))

static inline void
pic_number_normalize(pic_value *v)
{
  if(pic_vtype(*v) == PIC_VTYPE_HEAP){
    if(pic_rational_p(*v)){
    pic_rational *q = pic_rational_ptr(*v);
    if(mpz_get_si(mpq_denref(q->q)) == 1){
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
    pic_bigint *bi  = pic_bigint_ptr(*v);
    if(mpz_fits_sint_p(bi->z)){
      pic_init_value(*v, PIC_VTYPE_INT);
      v->u.i = mpz_get_si(bi->z);
      /* mpz_clear(bi->z); */
    }
  }
}}
pic_bigint *pic_bigint_add(pic_state *, pic_bigint *, pic_bigint *);
pic_bigint *pic_bigint_sub(pic_state *, pic_bigint *, pic_bigint *);
pic_bigint *pic_bigint_mul(pic_state *, pic_bigint *, pic_bigint *);
pic_rational *pic_bigint_div(pic_state *, pic_bigint *, pic_bigint *);

pic_rational *pic_rational_add(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_sub(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_mul(pic_state *, pic_rational *, pic_rational *);
pic_rational *pic_rational_div(pic_state *, pic_rational *, pic_rational *);

#if defined(__cplusplus)
}
#endif

#endif
