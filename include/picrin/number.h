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

pic_bigint *pic_bigint_new(pic_state *);
pic_bigrat *pic_bigrat_new(pic_state *);
pic_bigfloat *pic_bigfloat_new(pic_state *);

enum exactness {
  EXACT,
  MAYBE,
  INEXACT
};

void pic_number_normalize(pic_state *, pic_value *, enum exactness);

pic_value pic_add(pic_state *, pic_value, pic_value);
pic_value pic_sub(pic_state *, pic_value, pic_value);
pic_value pic_mul(pic_state *, pic_value, pic_value);
pic_value pic_div(pic_state *, pic_value, pic_value);

bool pic_eq(pic_state *, pic_value, pic_value);
bool pic_gt(pic_state *, pic_value, pic_value);
bool pic_ge(pic_state *, pic_value, pic_value);
bool pic_lt(pic_state *, pic_value, pic_value);
bool pic_le(pic_state *, pic_value, pic_value);

pic_value pic_read_bigint(pic_state *, char *, int);
pic_value pic_read_bigrat(pic_state *, char *, int);
pic_value pic_read_bigfloat(pic_state *, char *, int);

pic_value pic_number_to_string(pic_state *, pic_value, int);

#if defined(__cplusplus)
}
#endif

#endif
