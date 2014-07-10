/**
 * See Copyright Notice in picrin.h
 */

#include <math.h>
#include <limits.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/number.h"
#include "picrin/string.h"

void mpz_pow(mpz_t rop, const mpz_t base, const mpz_t expt)
{
  if(mpz_sgn(expt) < 0){
    /* divided by zoro error */
    return;
  }
  if(mpz_fits_uint_p(expt)){
    mpz_pow_ui(rop, base, mpz_get_ui(expt));
    return;
  }
  mpz_fdiv_q_ui(rop, expt, 2);
  mpz_pow(rop, base, rop);
  mpz_mul(rop, rop, rop);
  if(!mpz_even_p(expt))
    mpz_mul(rop, rop, base);
}

pic_value
pic_read_bigint(pic_state *pic, char *str, int radix)
{
  mpz_t z;
  mpz_init_set_str(z, str, radix);
  if(mpz_fits_sint_p(z)){
    pic_value res;
    pic_init_value(res, PIC_VTYPE_INT);
    pic_int(res) = mpz_get_si(z);
    return res;
  }
  else{
    pic_bigint *res = pic_bigint_new(pic);
    mpz_set(res->z, z);
    mpz_clear(z);
    return pic_obj_value(res);
  }
}

pic_value
pic_read_rational(pic_state *pic, char *str, int radix)
{
  mpq_t q;
  mpq_init(q);
  mpq_set_str(q, str, radix);
  mpq_canonicalize(q);
  if((mpz_get_si(mpq_denref(q))) == 1){
    if(mpz_fits_sint_p(mpq_numref(q))){
      pic_value res = pic_int_value(mpz_get_si(mpq_numref(q)));
      mpq_clear(q);
      return res;
    }
    else{
      pic_bigint *res = pic_bigint_new(pic);
      mpq_get_num(res->z, q);
      mpq_clear(q);
      return pic_obj_value(res);
    }
  }
  else{
    pic_rational *res = pic_rational_new(pic);
    mpq_set(res->q, q);
    mpq_clear(q);
    return pic_obj_value(res);
  }
}

pic_value
pic_read_bigfloat(pic_state *pic, char *str, int radix)
{
  pic_bigfloat *res = pic_bigfloat_new(pic);
  mpfr_init_set_str(res->f, str, radix, MPFR_RNDN);
  return pic_obj_value(res);    
}

pic_bigint *
pic_bigint_new(pic_state *pic)
{
    pic_bigint *res;
    res = (pic_bigint *)pic_obj_alloc(pic , sizeof(pic_bigint), PIC_TT_BIGINT);
    mpz_init(res->z);
    return res;
}

pic_rational *
pic_rational_new(pic_state *pic)
{
    pic_rational *res;
    res = (pic_rational *)pic_obj_alloc(pic , sizeof(pic_rational), PIC_TT_RATIONAL);
    mpq_init(res->q);
    return res;
}

pic_bigfloat *
pic_bigfloat_new(pic_state *pic)
{
    pic_bigfloat *res;
    res = (pic_bigfloat *)pic_obj_alloc(pic , sizeof(pic_bigfloat), PIC_TT_BIGFLOAT);
    mpfr_init(res->f);
    return res;
}

#define DEFINE_BIGINT_ARITH(op)                                 \
  pic_bigint *                                                  \
  pic_bigint_##op(pic_state *pic, mpz_t x, mpz_t y)             \
  {                                                             \
    pic_bigint *res = pic_bigint_new(pic);                      \
    mpz_##op(res->z, x, y);                                     \
    return res;                                                 \
  }                                                             \

DEFINE_BIGINT_ARITH(add)
DEFINE_BIGINT_ARITH(sub)
DEFINE_BIGINT_ARITH(mul)

pic_rational *
pic_bigint_div(pic_state *pic, mpz_t x, mpz_t y)
{
  pic_rational *res = pic_rational_new(pic);
  mpq_set_num(res->q, x);
  mpq_set_den(res->q, y);
  mpq_canonicalize(res->q);
  return res;
}

#define DEFINE_ARITH_FUNCTION(op, name, guard)                          \
  pic_value                                                             \
  pic_##name(pic_state *pic, pic_value a, pic_value b)                  \
  {                                                                     \
    pic_value result;                                                   \
                                                                        \
    switch(pic_type(a)){                                                \
    case PIC_TT_INT:{                                                   \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:{                                                 \
        double f = (double) pic_int(a) op (double) pic_int(b);          \
        if (INT_MIN <= f && f <= INT_MAX && (guard)) {                  \
          result = pic_float_value(f);                                  \
        }                                                               \
        else {                                                          \
          mpz_t c, d;                                                   \
          mpz_init_set_si(c, pic_int(a));                               \
          mpz_init_set_si(d, pic_int(b));                               \
          result = pic_obj_value(pic_bigint_##name(pic, c, d));         \
        }                                                               \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT:{                                               \
        result = pic_float_value(pic_int(a) op pic_float(b));           \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        pic_bigint *c = pic_bigint_new(pic);                            \
        mpz_set_si(c->z, pic_int(a));                                   \
        mpz_##name(c->z, c->z, pic_bigint_ptr(b)->z);                   \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_si(c->q,  pic_int(a), 1);                               \
        mpq_##name(c->q, c->q, pic_rational_ptr(b)->q);                 \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_si(c->f, pic_bigfloat_ptr(b)->f, pic_int(a), MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    }                                                                   \
    case PIC_TT_FLOAT: {                                                \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:{                                                 \
        result = pic_float_value(pic_float(a) op pic_int(b));           \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT:{                                               \
        result = pic_float_value(pic_float(a) op pic_float(b));         \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        result = pic_float_value( pic_float(a) op                       \
                                  mpz_get_d(pic_bigint_ptr(b)->z));     \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        result = pic_float_value(pic_float(a) op                        \
                                 mpq_get_d(pic_rational_ptr(b)->q));    \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        result = pic_float_value(pic_float(a) op                        \
                                 mpfr_get_d(pic_bigfloat_ptr(b)->f, MPFR_RNDN)); \
        break;                                                          \
      }                                                                 \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    }                                                                   \
    case PIC_TT_BIGINT:{                                                \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:{                                                 \
        pic_bigint *c = pic_bigint_new(pic);                            \
        mpz_set_si(c->z, pic_int(b));                                   \
        mpz_##name(c->z, pic_bigint_ptr(a)->z, c->z);                   \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT:{                                               \
        result = pic_float_value(mpz_get_d(pic_bigint_ptr(a)->z)        \
                                 op pic_int(b));                        \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        result = pic_obj_value(pic_bigint_##name(pic, pic_bigint_ptr(a)->z, \
                                                 pic_bigint_ptr(b)->z)); \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_z(c->q, pic_bigint_ptr(a)->z);                          \
        mpq_##name(c->q, c->q, pic_rational_ptr(b)->q);                 \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_z(c->f, pic_bigfloat_ptr(b)->f, pic_bigint_ptr(a)->z, MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      break;                                                            \
      }                                                                 \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    }                                                                   \
    case PIC_TT_RATIONAL:{                                              \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:{                                                 \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_d(c->q, (double) pic_int(b));                           \
        mpq_##name(c->q, pic_rational_ptr(a)->q, c->q);                 \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT:{                                               \
        result = pic_float_value(pic_float(a) op                        \
                                 mpq_get_d(pic_rational_ptr(b)->q));    \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_z(c->q, pic_bigint_ptr(b)->z);                          \
        mpq_##name(c->q, pic_rational_ptr(a)->q, c->q);                 \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_##name(c->q, pic_rational_ptr(a)->q, pic_rational_ptr(b)->q); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_q(c->f, pic_bigfloat_ptr(b)->f, pic_rational_ptr(a)->q, MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    }                                                                   \
    case PIC_TT_BIGFLOAT:{                                              \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:{                                                 \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_si(c->f, pic_bigfloat_ptr(a)->f, pic_int(b), MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT:{                                               \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_d(c->f, pic_bigfloat_ptr(a)->f,  pic_float(b), MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_z(c->f, pic_bigfloat_ptr(a)->f,  pic_bigint_ptr(b)->z, MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_q(c->f, pic_bigfloat_ptr(a)->f, pic_rational_ptr(b)->q, MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        pic_bigfloat *c = pic_bigfloat_new(pic);                        \
        mpfr_##name##_q(c->f, pic_bigfloat_ptr(b)->f, pic_rational_ptr(a)->q, MPFR_RNDN); \
        result = pic_obj_value(c);                                      \
        break;                                                          \
      }                                                                 \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    }                                                                   \
    default:                                                            \
    ERROR:                                                              \
      pic_errorf(pic, #op " got non-number operands");                  \
    }                                                                   \
    pic_number_normalize(pic, &result, MAYBE);                          \
    return result;                                                      \
  }

DEFINE_ARITH_FUNCTION(+, add, true)
DEFINE_ARITH_FUNCTION(-, sub, true)
DEFINE_ARITH_FUNCTION(*, mul, true)
DEFINE_ARITH_FUNCTION(/, div, f == round(f))

#define DEFINE_COMP_FUNCTION(op, name)                                  \
  bool                                                                  \
  pic_##name(pic_state *pic, pic_value a, pic_value b)                  \
  {                                                                     \
    switch(pic_type(a)){                                                \
    case PIC_TT_INT:                                                    \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:                                                  \
        return pic_int(a) op pic_int(b);                                \
      case PIC_TT_FLOAT:                                                \
        return pic_int(a) op pic_float(b);                              \
      case PIC_TT_BIGINT:                                               \
        return 0 op mpz_cmp_si(pic_bigint_ptr(b)->z, pic_int(a));       \
      case PIC_TT_RATIONAL:                                             \
        return 0 op mpq_cmp_si(pic_rational_ptr(b)->q, pic_int(a), 1);  \
      case PIC_TT_BIGFLOAT:                                             \
        return 0 op mpfr_cmp_si(pic_bigfloat_ptr(b)->f, pic_int(a));    \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    case PIC_TT_FLOAT:                                                  \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:                                                  \
        return pic_float(a) op pic_int(b);                              \
      case PIC_TT_FLOAT:                                                \
        return pic_float(a) op pic_float(b);                            \
      case PIC_TT_BIGINT:                                               \
        return 0 op mpz_cmp_d(pic_bigint_ptr(b)->z, pic_float(a));      \
      case PIC_TT_RATIONAL:{                                            \
        mpq_t q;                                                        \
        mpq_init(q);                                                    \
        mpq_set_d(q, pic_float(a));                                     \
        bool res = mpq_cmp(q, pic_rational_ptr(b)->q) op 0;             \
        mpq_clear(q);                                                   \
        return res;                                                     \
      }                                                                 \
      case PIC_TT_BIGFLOAT:                                             \
        return 0 op mpfr_cmp_d(pic_bigfloat_ptr(b)->f, pic_float(a));   \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    case PIC_TT_BIGINT:                                                 \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:                                                  \
        return mpz_cmp_si(pic_bigint_ptr(a)->z, pic_int(b)) op 0;       \
      case PIC_TT_FLOAT:                                                \
        return mpz_cmp_d(pic_bigint_ptr(a)->z, pic_float(b)) op 0;      \
      case PIC_TT_BIGINT:                                               \
        return mpz_cmp(pic_bigint_ptr(a)->z, pic_bigint_ptr(b)->z) op 0; \
      case PIC_TT_RATIONAL:{                                            \
        mpfr_t f;                                                       \
        mpfr_init_set_z(f, pic_bigint_ptr(a)->z, MPFR_RNDN);            \
        bool res = mpfr_cmp_q(f, pic_rational_ptr(b)->q) op 0;          \
        mpfr_clear(f);                                                  \
        return res;                                                     \
      }                                                                 \
      case PIC_TT_BIGFLOAT:                                             \
        return 0 op mpfr_cmp_z(pic_bigfloat_ptr(b)->f, pic_bigint_ptr(a)->z); \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    case PIC_TT_RATIONAL:                                               \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:                                                  \
        return mpq_cmp_si(pic_rational_ptr(a)->q, ((long) pic_int(b)), 1) op 0; \
      case PIC_TT_FLOAT:{                                               \
        mpq_t q;                                                        \
        mpq_init(q);                                                    \
        mpq_set_d(q, pic_float(b));                                     \
        bool res = mpq_cmp(pic_rational_ptr(a)->q, q) op 0;             \
        mpq_clear(q);                                                   \
        return res;                                                     \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        mpfr_t f;                                                       \
        mpfr_init_set_q(f, pic_rational_ptr(a)->q, MPFR_RNDN);          \
        bool res = mpfr_cmp_z(f, pic_bigint_ptr(b)->z) op 0;            \
        mpfr_clear(f);                                                  \
        return res;                                                     \
      }                                                                 \
      case PIC_TT_RATIONAL:                                             \
        return mpq_cmp(pic_rational_ptr(a)->q, pic_rational_ptr(b)->q) op 0; \
      case PIC_TT_BIGFLOAT:                                             \
        return 0 op mpfr_cmp_q(pic_bigfloat_ptr(b)->f, pic_rational_ptr(a)->q); \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    case PIC_TT_BIGFLOAT:                                               \
      switch(pic_type(b)){                                              \
      case PIC_TT_INT:                                                  \
        return mpfr_cmp_si(pic_bigfloat_ptr(a)->f, pic_int(b)) op 0;    \
      case PIC_TT_FLOAT:                                                \
        return mpfr_cmp_d(pic_bigfloat_ptr(a)->f, pic_float(b)) op 0;   \
      case PIC_TT_BIGINT:                                               \
        return mpfr_cmp_z(pic_bigfloat_ptr(a)->f, pic_bigint_ptr(b)->z) op 0; \
      case PIC_TT_RATIONAL:                                             \
        return mpfr_cmp_q(pic_bigfloat_ptr(a)->f, pic_rational_ptr(b)->q) op 0; \
      case PIC_TT_BIGFLOAT:                                             \
        return mpfr_cmp(pic_bigfloat_ptr(a)->f, pic_bigfloat_ptr(b)->f) op 0; \
      default:                                                          \
        goto ERROR;                                                     \
      }                                                                 \
      break;                                                            \
    default:                                                            \
    ERROR:                                                              \
      pic_errorf(pic, #op " got non-number operands");                  \
    }                                                                   \
  }

DEFINE_COMP_FUNCTION(==, eq)
DEFINE_COMP_FUNCTION(<, lt)
DEFINE_COMP_FUNCTION(>, gt)
DEFINE_COMP_FUNCTION(<=, le)
DEFINE_COMP_FUNCTION(>=, ge)


static pic_value
pic_number_real_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_float_p(v) || pic_int_p(v) ||
                        pic_bigint_p(v) || pic_rational_p(v) || pic_bigfloat_p(v));
}

static pic_value
pic_number_integer_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  switch(pic_type(v)){
  case PIC_TT_INT:
  case PIC_TT_BIGINT:
    return pic_true_value();
  case PIC_TT_FLOAT:{
    double f = pic_float(v);

    if (isinf(f)) {
      return pic_false_value();
    }

    if (f == round(f)) {
      return pic_true_value();
    }
    else{
      return pic_false_value();      
    }
  }
  case PIC_TT_BIGFLOAT:{
    mpfr_integer_p(pic_bigfloat_ptr(v)->f);
      return pic_true_value();
  }
  default:;
  }
  return pic_false_value();
}

static pic_value
pic_number_rational_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  switch(pic_type(v)){
  case PIC_TT_INT:
  case PIC_TT_BIGINT:
  case PIC_TT_RATIONAL:
    return pic_true_value();
  case PIC_TT_FLOAT:{
    double f = pic_float(v);

    if (isinf(f)) {
      return pic_false_value();
    }

    if (f == round(f)) {
      return pic_true_value();
    }
    else{
      return pic_false_value();      
    }
  }
  case PIC_TT_BIGFLOAT:{
    mpfr_integer_p(pic_bigfloat_ptr(v)->f);
      return pic_true_value();
  }
  default:;
  }
  return pic_false_value();
}

static pic_value
pic_number_exact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  return pic_bool_value(pic_int_p(v) || pic_bigint_p(v) || pic_rational_p(v));
}

static pic_value
pic_number_inexact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  return pic_bool_value(pic_float_p(v) || pic_bigfloat_p(v));
}

static pic_value
pic_number_finite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_int_p(v) || pic_bigint_p(v) || pic_rational_p(v))
    return pic_true_value();
  if ((pic_float_p(v) && ! (isinf(pic_float(v)) || isnan(pic_float(v)))) ||
      (pic_bigfloat_p(v) && ! mpfr_regular_p(pic_bigfloat_ptr(v)->f)))
    return pic_true_value();
  else
    return pic_false_value();
}

static pic_value
pic_number_infinite_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if ((pic_float_p(v) && isinf(pic_float(v))) ||
      (pic_bigfloat_p(v) && mpfr_inf_p(pic_bigfloat_ptr(v)->f)))
    return pic_true_value();
  else
    return pic_false_value();
}

static pic_value
pic_number_nan_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if ((pic_float_p(v) && isnan(pic_float(v)))||
      (pic_bigfloat_p(v) && mpfr_nan_p(pic_bigfloat_ptr(v)->f)) )
    return pic_true_value();
  else
    return pic_false_value();
}

#define DEFINE_ARITH_COMP(op, name)                                     \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc;                                                        \
    pic_value *argv;                                                    \
    pic_value v,w;                                                      \
    pic_get_args(pic, "n*", &v, &argc, &argv);                          \
    if(argc == 0)                                                       \
      pic_errorf(pic, #op " expected 2 or more operands but got 1");    \
    for(size_t i = 0; i < argc; i++) {                                  \
      w = argv[i];                                                      \
      if(! pic_##name(pic, v, w)){                                      \
        return pic_false_value();                                       \
      }                                                                 \
      v = w;                                                            \
    }                                                                   \
    return pic_true_value();                                            \
  }

DEFINE_ARITH_COMP(==, eq)
DEFINE_ARITH_COMP(<, lt)
DEFINE_ARITH_COMP(>, gt)
DEFINE_ARITH_COMP(<=, le)
DEFINE_ARITH_COMP(>=, ge)

static pic_value
pic_number_zero_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f == 0);
}

static pic_value
pic_number_positive_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f > 0);
}

static pic_value
pic_number_negative_p(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_bool_value(f < 0);
}

static pic_value
pic_number_odd_p(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_bool_value(i % 2 != 0);
}

static pic_value
pic_number_even_p(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);

  return pic_bool_value(i % 2 == 0);
}

#define DEFINE_MIN_MAX(op, name, sign)                          \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc;                                                \
    pic_value *argv;                                            \
    size_t i;                                                   \
    mpfr_t f;                                                   \
    pic_value v, result;                                        \
    enum exactness exactp = EXACT;                              \
                                                                \
    pic_get_args(pic, "*", &argc, &argv);                       \
                                                                \
    mpfr_init(f);                                               \
    mpfr_set_inf(f, sign 1);                                    \
    result = pic_float_value(sign INFINITY);                    \
    for (i = 0; i < argc; ++i) {                                \
      v = argv[i];                                              \
      switch(pic_type(v)){                                      \
      case PIC_TT_INT:                                          \
        if(mpfr_cmp_si(f, pic_int(v)) op 0){                    \
          result = v;                                           \
          mpfr_set_si(f, pic_int(v), MPFR_RNDN);                \
        }                                                       \
        break;                                                  \
      case PIC_TT_FLOAT:                                        \
        exactp = INEXACT;                                       \
        if(mpfr_cmp_d(f, pic_float(v)) op 0){                   \
          result = v;                                           \
          mpfr_set_d(f, pic_float(v), MPFR_RNDN);               \
        }                                                       \
        break;                                                  \
      case PIC_TT_BIGINT:                                       \
        if(mpfr_cmp_z(f, pic_bigint_ptr(v)->z) op 0){           \
          result = v;                                           \
          mpfr_set_z(f, pic_bigint_ptr(v)->z, MPFR_RNDN);       \
        }                                                       \
        break;                                                  \
      case PIC_TT_RATIONAL:                                     \
        if(mpfr_cmp_q(f, pic_rational_ptr(v)->q) op 0){         \
          result = v;                                           \
          mpfr_set_q(f, pic_rational_ptr(v)->q, MPFR_RNDN);     \
        }                                                       \
        break;                                                  \
      case PIC_TT_BIGFLOAT:                                     \
        exactp = INEXACT;                                       \
        if(mpfr_cmp(f, pic_bigfloat_ptr(v)->f) op 0){           \
          result = v;                                           \
          mpfr_set(f, pic_bigfloat_ptr(v)->f, MPFR_RNDN);       \
        }                                                       \
        break;                                                  \
      default:                                                  \
        pic_errorf(pic, #name ": number required");             \
      }                                                         \
    }                                                           \
    mpfr_clear(f);                                              \
    return exactp == EXACT ? result :                           \
      pic_float_value(pic_number_to_float(result));             \
  }

DEFINE_MIN_MAX(>, min, +);
DEFINE_MIN_MAX(<, max, -);


#define DEFINE_ARITH_OP(name, unit)                                     \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc;                                                        \
    pic_value *argv;                                                    \
    pic_value a;                                                        \
                                                                        \
    pic_get_args(pic, "*", &argc, &argv);                               \
                                                                        \
    a = pic_int_value(unit);                                            \
    for (size_t i = 0; i < argc; ++i) {                                 \
      a = pic_##name(pic, a, argv[i]);                                  \
    }                                                                   \
    return a;                                                           \
  }

DEFINE_ARITH_OP(add, 0)
DEFINE_ARITH_OP(mul, 1)
DEFINE_ARITH_OP(sub, 0)
DEFINE_ARITH_OP(div, 1)

static pic_value
pic_number_abs(pic_state *pic)
{
  pic_value n;
  pic_get_args(pic, "n", &n);

  switch(pic_type(n)){
  case PIC_TT_INT:
    return pic_int_value(abs(pic_int(n)));
  case PIC_TT_FLOAT:
    return pic_float_value(fabs(pic_float(n)));
  case PIC_TT_BIGINT:{
    pic_bigint *z;
    z = pic_bigint_new(pic);
    mpz_abs(z->z, pic_bigint_ptr(n)->z);
    return pic_obj_value(z);
  }    
  case PIC_TT_RATIONAL:{
    pic_rational *q;
    q = pic_rational_new(pic);
    mpq_abs(q->q, pic_rational_ptr(n)->q);
    return pic_obj_value(q);
  }
  case PIC_TT_BIGFLOAT:{
    pic_bigfloat *f;
    f = pic_bigfloat_new(pic);
    mpfr_abs(f->f, pic_bigfloat_ptr(n)->f, MPFR_RNDN);
    return pic_obj_value(f);
  }
  default:
    pic_errorf(pic, "logic flow");
  }
}

static pic_value
pic_number_floor_quotient(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  pic_value v;
  f = pic_bigfloat_new(pic);
  mpfr_init(g);

  pic_get_args(pic, "rr", &(f->f), &g);

  mpfr_div(f->f, f->f, g, MPFR_RNDD);
  mpfr_floor(f->f, f->f);
  mpfr_clear(g);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, true);
  return v;  
}

static pic_value
pic_number_floor_remainder(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g, h;
  pic_value v;
  bool e1, e2;

  f = pic_bigfloat_new(pic);
  mpfr_init(g);
  mpfr_init(h);

  pic_get_args(pic, "RR", &(f->f), &e1, &g, &e2);

  mpfr_div(h, f->f, g, MPFR_RNDD);
  mpfr_floor(h, h);
  mpfr_mul(h, h, g, MPFR_RNDN);
  mpfr_sub(f->f, f->f, h, MPFR_RNDD);
  mpfr_clear(g);
  mpfr_clear(h);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, e1&&e2);
  return v;
}

static pic_value
pic_number_trunc_quotient(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  pic_value v;
  f = pic_bigfloat_new(pic);
  mpfr_init(g);

  pic_get_args(pic, "rr", &(f->f), &g);

  mpfr_div(f->f, f->f, g, MPFR_RNDN);
  mpfr_trunc(f->f, f->f);
  mpfr_clear(g);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, true);
  return v;  
}

static pic_value
pic_number_trunc_remainder(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  pic_value v;
  bool e1, e2;

  f = pic_bigfloat_new(pic);
  mpfr_init(g);

  pic_get_args(pic, "RR", &(f->f), &e1, &g, &e2);

  mpfr_remainder(f->f, f->f, g, MPFR_RNDN);
  mpfr_clear(g);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, e1&&e2);
  return v;
}

#define DEFINE_FACTOR_FUNCTION(name, unit)                              \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc;                                                        \
    pic_value *args, v;                                                 \
    pic_bigint *z;                                                      \
    mpz_t y;                                                            \
    bool e = true;                                                      \
                                                                        \
    z = pic_bigint_new(pic);                                            \
    mpz_set_si(z->z, unit);                                             \
    pic_get_args(pic, "*", &argc, &args);                               \
                                                                        \
    while (argc-- > 0) {                                                \
      v = args[argc];                                                   \
      switch(pic_type(v)){                                              \
      case PIC_TT_INT:                                                  \
        mpz_##name##_ui(z->z, z->z, (unsigned long) abs(pic_int(v)));   \
        break;                                                          \
      case PIC_TT_FLOAT:                                                \
        e = false;                                                      \
        mpz_##name##_ui(z->z, z->z,(unsigned long) fabs(pic_float(v))); \
        break;                                                          \
      case PIC_TT_BIGINT:                                               \
        mpz_##name(z->z, z->z, pic_bigint_ptr(v)->z);                   \
        break;                                                          \
      case PIC_TT_RATIONAL:                                             \
        e = false;                                                      \
        mpz_init(y);                                                    \
        mpz_set_q(y, pic_rational_ptr(v)->q);                           \
        mpz_##name(z->z, z->z, y);                                      \
        mpz_clear(y);                                                   \
        break;                                                          \
      case PIC_TT_BIGFLOAT:                                             \
        e = false;                                                      \
        mpz_init(y);                                                    \
        mpfr_get_z(y, pic_bigfloat_ptr(v)->f, MPFR_RNDD);               \
        mpz_##name(z->z, z->z, y);                                      \
        mpz_clear(y);                                                   \
        break;                                                          \
      default:                                                          \
        pic_error(pic, #name ": number required");                      \
      }                                                                 \
    }                                                                   \
    if(e){                                                              \
      pic_value res = pic_obj_value(z);                                 \
      pic_number_normalize(pic, &res, true);                            \
      return res;                                                       \
    }                                                                   \
    else{                                                               \
      pic_bigfloat *f = pic_bigfloat_new(pic);                          \
      mpfr_set_z(f->f, z->z, MPFR_RNDN);                                \
      return pic_obj_value(f);                                          \
    }                                                                   \
  }

DEFINE_FACTOR_FUNCTION(gcd, 0)
DEFINE_FACTOR_FUNCTION(lcm, 1)

#define DEFINE_ROUNDING_FUNCTION(name)                          \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    pic_value v;                                                \
                                                                \
    pic_get_args(pic, "n", &v);                                 \
                                                                \
    switch(pic_type(v)){                                        \
    case PIC_TT_INT: case PIC_TT_BIGINT:                        \
      return v;                                                 \
    case PIC_TT_FLOAT:                                          \
      return pic_float_value( name(pic_float(v)));              \
    case PIC_TT_RATIONAL:{                                      \
      pic_bigfloat *f;                                          \
      f = pic_bigfloat_new(pic);                                \
      mpfr_init_set_q(f->f, pic_rational_ptr(v)->q, MPFR_RNDN); \
      mpfr_##name(f->f, f->f);                                  \
      return pic_obj_value(f);                                  \
    }                                                           \
    case PIC_TT_BIGFLOAT:{                                      \
      pic_bigfloat *f;                                          \
      f = pic_bigfloat_new(pic);                                \
      mpfr_##name(f->f, pic_bigfloat_ptr(v)->f);                \
      return pic_obj_value(f);                                  \
    }                                                           \
    default:                                                    \
      pic_errorf(pic, "logic flow");                            \
    }                                                           \
  }

DEFINE_ROUNDING_FUNCTION(floor);
DEFINE_ROUNDING_FUNCTION(ceil);
DEFINE_ROUNDING_FUNCTION(trunc);
DEFINE_ROUNDING_FUNCTION(round);



static pic_value
pic_number_exp(pic_state *pic)
{
  pic_bigfloat *f;
  pic_value v;
  
  f = pic_bigfloat_new(pic);
  pic_get_args(pic, "r", &(f->f));
  mpfr_exp(f->f, f->f, MPFR_RNDN);
  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, false);
  return v;
}

static pic_value
pic_number_log(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  int argc;
  
  f = pic_bigfloat_new(pic);
  mpfr_init(g);
  
  argc = pic_get_args(pic, "r|r", &(f->f), &g);
  if (argc == 1) {
    mpfr_log(f->f, f->f, MPFR_RNDN);
  }
  else {
    mpfr_log(f->f, f->f, MPFR_RNDN);
    mpfr_log(g, g, MPFR_RNDN);
    mpfr_div(f->f, f->f, g, MPFR_RNDN);
    mpfr_clear(g);
  }
  return pic_obj_value(f);
}

#define DEFINE_TRIANGLE_FUNCTION(name)          \
  static pic_value                              \
  pic_number_##name(pic_state *pic)             \
  {                                             \
    pic_bigfloat *f;                            \
                                                \
    f = pic_bigfloat_new(pic);                  \
    pic_get_args(pic, "r", &(f->f));            \
    mpfr_##name(f->f, f->f, MPFR_RNDN);         \
    return pic_obj_value(f);                    \
  }

DEFINE_TRIANGLE_FUNCTION(sin);
DEFINE_TRIANGLE_FUNCTION(cos);
DEFINE_TRIANGLE_FUNCTION(tan);
DEFINE_TRIANGLE_FUNCTION(asin);
DEFINE_TRIANGLE_FUNCTION(acos);

static pic_value
pic_number_atan(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  int argc;

  f = pic_bigfloat_new(pic);
  mpfr_init(g);
  argc = pic_get_args(pic, "r|r", &(f->f), &g);
  if (argc == 1) {
    mpfr_atan(f->f, f->f, MPFR_RNDN);
  }
  else {
    mpfr_atan2(f->f, f->f, g, MPFR_RNDN);
  }
  mpfr_clear(g);
  return pic_obj_value(f);
}

static pic_value
pic_number_square(pic_state *pic)
{
  pic_value n;

  pic_get_args(pic, "n", &n);

  switch(pic_type(n)){
  case PIC_TT_INT: {
    long long i = (long long)pic_int(n);
    if (i * i <= INT_MAX) {
      return pic_int_value(i * i);
    }
    else {
      pic_bigint *z;
      z = pic_bigint_new(pic);
      mpz_ui_pow_ui(z->z, (unsigned long int)i, 2);
      return pic_obj_value(z);
    }
  }
  case PIC_TT_FLOAT:
    return pic_float_value(pic_float(n) * pic_float(n));
  case PIC_TT_BIGINT:{
    pic_bigint *z;
    z = pic_bigint_new(pic);
    mpz_pow_ui(z->z, pic_bigint_ptr(n)->z, 2);
    return pic_obj_value(z);
  }
  case PIC_TT_RATIONAL:{
    pic_rational *q;
    q = pic_rational_new(pic);
    mpz_pow_ui(mpq_numref(q->q), mpq_numref(pic_rational_ptr(n)->q), 2);
    mpz_pow_ui(mpq_denref(q->q), mpq_denref(pic_rational_ptr(n)->q), 2);
    return pic_obj_value(q);
  }
  case PIC_TT_BIGFLOAT:{
    pic_bigfloat *f;
    f = pic_bigfloat_new(pic);
    mpfr_sqr(f->f, pic_bigfloat_ptr(n)->f, MPFR_RNDN);
    return pic_obj_value(f);
  }
  default:
    pic_errorf(pic, "logic flow");
  }
}

static pic_value
pic_number_sqrt(pic_state *pic)
{
  pic_bigfloat *f;
  pic_value v;

  f = pic_bigfloat_new(pic);

  pic_get_args(pic, "r", &(f->f));

  mpfr_sqrt(f->f, f->f, MPFR_RNDN);
  v = pic_obj_value(f);
  pic_number_normalize(pic, &v, false);

  return v;
}

static pic_value
pic_number_integer_sqrt(pic_state *pic)
{
  pic_bigint *z;
  pic_value v;

  z = pic_bigint_new(pic);

  pic_get_args(pic, "Z", &(z->z));

  mpz_sqrt(z->z, z->z);
  v = pic_obj_value(z);
  pic_number_normalize(pic, &v, true);

  return v;
}

static pic_value
pic_number_expt(pic_state *pic)
{
  pic_value a, b;

  pic_get_args(pic, "nn", &a, &b);

  switch(pic_type(a)){
  case PIC_TT_INT:
    switch(pic_type(b)){
    case PIC_TT_INT:{
      double i = pow((double)pic_int(a), (double)pic_int(b));
      if(INT_MIN <= i && i <= INT_MAX){
        return pic_int_value((int)i);
      }
      else{
        pic_bigint *c = pic_bigint_new(pic);
        mpz_set_si(c->z, pic_int(a));
        mpz_pow_ui(c->z, c->z, abs(pic_int(b)));
        if( pic_int(b) >= 0){
          return pic_obj_value(c);
        }
        else{
          pic_rational *d = pic_rational_new(pic);
          mpq_set_z(d->q, c->z);
          mpq_inv(d->q, d->q);
          return pic_obj_value(d);
        }
      }
    }
    case PIC_TT_FLOAT:
      return pic_float_value(pow((double) pic_int(a),  pic_float(b)));
    case PIC_TT_BIGINT:{
      pic_bigint *c = pic_bigint_ptr(b);
      if(mpz_sgn(c->z) >= 0){
        pic_bigint *d = pic_bigint_new(pic);
        mpz_set_si(d->z, pic_int(a));
        mpz_pow(d->z, d->z, c->z);
        return pic_obj_value(d);
      }
      else{
        pic_rational *d = pic_rational_new(pic);
        mpz_t z;
        mpz_init(z);
        mpz_neg(z, c->z);
        mpq_set_si(d->q, 1, pic_int(a));
        mpz_pow(mpq_denref(d->q), mpq_denref(d->q), z);
        mpz_clear(z);
        return pic_obj_value(d);        
      }
    }
    case PIC_TT_RATIONAL:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_t d;
      mpfr_init_set_q(d, pic_rational_ptr(b)->q, MPFR_RNDN);
      mpfr_ui_pow(c->f, pic_int(a), d, MPFR_RNDN);
      mpfr_clear(d);
      return pic_obj_value(c);
    }
    case PIC_TT_BIGFLOAT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_ui_pow(c->f, pic_int(a), pic_bigfloat_ptr(b)->f, MPFR_RNDN);
      return pic_obj_value(c);
    }
    default:
      goto ERROR;
    }
    break;
  case PIC_TT_FLOAT:
    switch(pic_type(b)){
    case PIC_TT_INT:
      return pic_float_value(pow(pic_float(a), (double) pic_int(b)));
    case PIC_TT_FLOAT:
      return pic_float_value(pow(pic_float(a), pic_float(b)));
    case PIC_TT_BIGINT:
      return pic_float_value(pow(pic_float(a), mpz_get_d(pic_bigint_ptr(b)->z)));
    case PIC_TT_RATIONAL:
      return pic_float_value(pow(pic_float(a), mpq_get_d(pic_rational_ptr(b)->q)));
    case PIC_TT_BIGFLOAT:
      return pic_float_value(pow(pic_float(a), mpfr_get_d(pic_bigfloat_ptr(b)->f, MPFR_RNDN)));
    default:
      goto ERROR;
    }
    break;
  case PIC_TT_BIGINT:
    switch(pic_type(b)){
    case PIC_TT_INT:
      if(pic_int(b) >= 0){
        pic_bigint *c = pic_bigint_new(pic);
        mpz_pow_ui(c->z, pic_bigint_ptr(a)->z, (unsigned)pic_int(b));
        return pic_obj_value(c);
      }
      else{
        pic_rational *c = pic_rational_new(pic);
        mpq_set_z(c->q, pic_bigint_ptr(a)->z);
        mpz_pow_ui(mpq_denref(c->q), pic_bigint_ptr(a)->z, (unsigned) -pic_int(b));
        mpq_inv(c->q, c->q);
        return pic_obj_value(c);
      }
    case PIC_TT_FLOAT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_t d;
      mpfr_set_z(c->f, pic_bigint_ptr(a)->z, MPFR_RNDN);
      mpfr_init_set_d(d, pic_float(b), MPFR_RNDN);
      mpfr_pow(c->f, c->f, d, MPFR_RNDN);
      mpfr_clear(d);
      return pic_obj_value(c);
    }
    case PIC_TT_BIGINT:{
      pic_bigint *c = pic_bigint_new(pic);
      mpz_pow(c->z, pic_bigint_ptr(a)->z, pic_bigint_ptr(b)->z);
      return pic_obj_value(c);
    }
    case PIC_TT_RATIONAL:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_t d;
      mpfr_init_set_q(d, pic_rational_ptr(b)->q, MPFR_RNDN);
      mpfr_set_z(c->f, pic_bigint_ptr(a)->z, MPFR_RNDN);
      mpfr_pow(c->f, c->f, d, MPFR_RNDN);
      mpfr_clear(d);
      return pic_obj_value(c);
    }
    case PIC_TT_BIGFLOAT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_set_z(c->f, pic_bigint_ptr(a)->z, MPFR_RNDN);
      mpfr_pow(c->f, c->f, pic_bigfloat_ptr(b)->f, MPFR_RNDN);
      return pic_obj_value(c);
    }
    default:
      goto ERROR;
    }
    break;
  case PIC_TT_RATIONAL:
    switch(pic_type(b)){
    case PIC_TT_INT:{
      pic_rational *c = pic_rational_new(pic);
      mpz_pow_ui(mpq_denref(c->q), mpq_denref(pic_rational_ptr(a)->q), abs(pic_int(b)));
      mpz_pow_ui(mpq_numref(c->q), mpq_numref(pic_rational_ptr(a)->q), abs(pic_int(b)));
      if(pic_int(b)>= 0)
        mpq_inv(c->q, c->q);
      return pic_obj_value(c);
    }
    case PIC_TT_FLOAT:
      return pic_float_value(pow(mpq_get_d(pic_rational_ptr(a)->q), pic_float(b)));
    case PIC_TT_BIGINT:{
      pic_rational *c = pic_rational_new(pic);
      mpz_t d;
      mpz_init(d);
      mpz_abs(d, pic_bigint_ptr(b)->z);
      mpz_pow(mpq_denref(c->q), mpq_denref(pic_rational_ptr(a)->q), d);
      mpz_pow(mpq_numref(c->q), mpq_numref(pic_rational_ptr(a)->q), d);
      if(mpz_sgn(pic_bigint_ptr(b)->z) >= 0)
        mpq_inv(c->q, c->q);
      return pic_obj_value(c);
    }
    case PIC_TT_RATIONAL:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_t d;
      mpfr_set_q(c->f, pic_rational_ptr(a)->q, MPFR_RNDN);
      mpfr_init_set_q(d, pic_rational_ptr(b)->q, MPFR_RNDN);
      mpfr_pow(c->f, c->f, d, MPFR_RNDN);
      mpfr_clear(d);
      return pic_obj_value(c);
    }
    case PIC_TT_BIGFLOAT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_set_q(c->f, pic_rational_ptr(a)->q, MPFR_RNDN);
      mpfr_pow(c->f, c->f, pic_bigfloat_ptr(b)->f, MPFR_RNDN);
      return pic_obj_value(c);
    }
    default:
      goto ERROR;
    }
    break;
  case PIC_TT_BIGFLOAT:
    switch(pic_type(b)){
    case PIC_TT_INT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_pow_si(c->f, pic_bigfloat_ptr(a)->f, pic_int(b), MPFR_RNDN);
      return pic_obj_value(c);
    }
    case PIC_TT_FLOAT:
      return pic_float_value(pow(mpfr_get_d(pic_bigfloat_ptr(a)->f, MPFR_RNDN), pic_float(b)));
    case PIC_TT_BIGINT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_pow_z(c->f, pic_bigfloat_ptr(a)->f, pic_bigint_ptr(b)->z, MPFR_RNDN);
      return pic_obj_value(c);
    }
    case PIC_TT_RATIONAL:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_set_q(c->f, pic_rational_ptr(b)->q, MPFR_RNDN);
      mpfr_pow(c->f, pic_bigfloat_ptr(a)->f, c->f, MPFR_RNDN);
      return pic_obj_value(c);
    }
    case PIC_TT_BIGFLOAT:{
      pic_bigfloat *c = pic_bigfloat_new(pic);
      mpfr_pow(c->f, pic_bigfloat_ptr(a)->f, pic_bigfloat_ptr(b)->f, MPFR_RNDN);
      return pic_obj_value(c);
    }
    default:
      goto ERROR;
    }
    break;
  default:
  ERROR:
    pic_errorf(pic, "expt got non-number operands");
  }
}

static pic_value
pic_number_inexact(pic_state *pic)
{
  double f;

  pic_get_args(pic, "f", &f);

  return pic_float_value(f);
}

static pic_value
pic_number_exact(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  switch(pic_type(v)){
  case PIC_TT_FLOAT:{
    pic_bigint *z;
    pic_value w;
    z = pic_bigint_new(pic);
    mpz_set_d(z->z, pic_float(v));
    w = pic_obj_value(z);
    pic_number_normalize(pic, &w, true);
    return w;
  }
  case PIC_TT_BIGFLOAT:{
    pic_bigint *z;
    pic_value w;
    z = pic_bigint_new(pic);
    mpfr_get_z(z->z, pic_bigfloat_ptr(v)->f, MPFR_RNDN);
    w = pic_obj_value(z);
    pic_number_normalize(pic, &w, true);
    return w;
  }
 default:{
    return v;
  }
  }
}

pic_value
pic_number_to_string(pic_state *pic, pic_value n, int radix)
{
  pic_bigfloat *f;
  char *buf;

  switch(pic_type(n)){
  case PIC_TT_INT:{
    mpz_t z;
    mpz_init_set_si(z, pic_int(n));
    buf = mpz_get_str(NULL, radix, z);
    mpz_clear(z);
    break;
    }
  case PIC_TT_FLOAT:{
    if(isnan(pic_float(n))){
      buf = "+nan.0";
    }
    else if(isinf(pic_float(n))){
      buf = pic_float(n) > 0 ? "+inf.0" : "-inf.0";
    }
    else{
      f = pic_bigfloat_new(pic);
      mpfr_set_d(f->f, pic_float(n), MPFR_RNDN);
      goto PRINT_FLOAT;
    }
    break;
  }
  case PIC_TT_BIGINT:{
    buf = mpz_get_str(NULL, radix, pic_bigint_ptr(n)->z);
    break;
    }
  case PIC_TT_RATIONAL:{
    buf = mpq_get_str(NULL, radix, pic_rational_ptr(n)->q);
    break;
    }
  case PIC_TT_BIGFLOAT:{
    f = pic_bigfloat_ptr(n);
    PRINT_FLOAT:
    {
      mpfr_exp_t *e;
      char *frac;
      int ilen;
      int slen;
      int offset;
      offset = 0;
      e = pic_alloc(pic, sizeof(mpfr_exp_t));
      memset(e, 0, sizeof(*e));
      frac = mpfr_get_str(NULL, e, radix, 0, f->f, MPFR_RNDN);
      if(!frac){
        pic_errorf(pic, "an error occured in number->string");
      }
      if(frac[0] == '-')
        ilen = *e+1;
      else
        ilen = *e;
      slen = strlen(frac);
      while(frac[--slen] == '0' && ilen < slen);slen++;
      buf = pic_alloc(pic, slen + 2 + (*e == 0?1:0)); /* \0 and floating point (and leading 0)*/
      memcpy(buf, frac, ilen);
      if(*e == 0)
        buf[ilen + offset++] = '0';
      buf[ilen + offset++] = '.';
      memcpy(buf+ilen+offset, frac+ilen, slen - ilen + 1);
      buf[slen+offset] = '\0';
      mpfr_free_str(frac);
      pic_free(pic, e);
    }
    break;
  }
  default:
    pic_errorf(pic, "logic flow");
  }
  return pic_obj_value(pic_str_new(pic, buf, strlen(buf)));

}

static pic_value
pic_number_number_to_string(pic_state *pic)
{
  pic_value n;
  int radix = 10;

  pic_get_args(pic, "n|i", &n, &radix);
  return pic_number_to_string(pic, n, radix);

}


static pic_value
pic_number_string_to_number(pic_state *pic)
{
  char *str;
  int radix = 10,offset;
  pic_rational *q;
  pic_bigfloat *f;
  pic_value v;
  bool radixsetp = false;
  enum exactness exactp = MAYBE;

  pic_get_args(pic, "z|i", &str, &radix);
  q = pic_rational_new(pic);
  for(offset = 0; offset < 4; offset +=2){
    if(str[offset] == '#'){
    switch(str[offset + 1]){
    case 'b': case 'B':
      if(radixsetp)
        goto ERROR;
      radix = 2;
      radixsetp = true;
      break;
    case 'o': case 'O':
      if(radixsetp)
        goto ERROR;
      radix = 8;
      radixsetp = true;
      break;
    case 'd': case 'D':
      if(radixsetp)
        goto ERROR;
      radix = 10;
      radixsetp = true;
      break;
    case 'x': case 'X':
      if(radixsetp)
        goto ERROR;
      radix = 16;
      radixsetp = true;
      break;
    case 'e': case 'E':
      if(exactp != MAYBE)
        goto ERROR;
      exactp = EXACT;
      break;
    case 'i': case 'I':
      if(exactp != MAYBE)
        goto ERROR;
      exactp = INEXACT;
      break;
    default:
    ERROR:
      pic_errorf(pic, "invalid dispatch character %c", str[offset+1]);
    }
    }
    else
      break;
  }
  if(mpq_set_str(q->q, str+offset, radix) == 0){
    mpq_canonicalize(q->q);
    if(exactp == INEXACT){
      f = pic_bigfloat_new(pic);
      mpfr_set_q(f->f, q->q, MPFR_RNDN);
      v = pic_obj_value(f);
    }
    else{
      v = pic_obj_value(q);
    }
  }else{
    f = pic_bigfloat_new(pic);
    if(mpfr_init_set_str(f->f, str+offset, radix, MPFR_RNDN) == 0){
      if(exactp == EXACT){        
        if(mpfr_fits_sint_p(f->f, MPFR_RNDN)){
          v = pic_int_value(mpfr_get_si(f->f, MPFR_RNDN));
        }
        else {
          pic_bigint *z = pic_bigint_new(pic);
          mpfr_get_z(z->z, f->f, MPFR_RNDN);
          v = pic_obj_value(z);
        }
      }
      else{
        v = pic_obj_value(f);
      }
    }
    else{
      pic_errorf(pic, "cannot parse \"%s\"", str);
    }
  }
  pic_number_normalize(pic, &v, exactp);
  return v;
}

void
pic_init_number(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_real_p);
  pic_defun(pic, "complex?", pic_number_real_p);
  pic_defun(pic, "real?", pic_number_real_p);
  pic_defun(pic, "rational?", pic_number_rational_p);
  pic_defun(pic, "integer?", pic_number_integer_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "=", pic_number_eq);
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, ">", pic_number_gt);
  pic_defun(pic, "<=", pic_number_le);
  pic_defun(pic, ">=", pic_number_ge);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "zero?", pic_number_zero_p);
  pic_defun(pic, "positive?", pic_number_positive_p);
  pic_defun(pic, "negative?", pic_number_negative_p);
  pic_defun(pic, "odd?", pic_number_odd_p);
  pic_defun(pic, "even?", pic_number_even_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "min", pic_number_min);
  pic_defun(pic, "max", pic_number_max);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "+", pic_number_add);
  pic_defun(pic, "-", pic_number_sub);
  pic_defun(pic, "*", pic_number_mul);
  pic_defun(pic, "/", pic_number_div);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "abs", pic_number_abs);
  pic_defun(pic, "floor-quotient", pic_number_floor_quotient);
  pic_defun(pic, "floor-remainder", pic_number_floor_remainder);
  pic_defun(pic, "truncate-quotient", pic_number_trunc_quotient);
  pic_defun(pic, "truncate-remainder", pic_number_trunc_remainder);
  pic_defun(pic, "modulo", pic_number_floor_remainder);
  pic_defun(pic, "quotient", pic_number_trunc_quotient);
  pic_defun(pic, "remainder", pic_number_trunc_remainder);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "gcd", pic_number_gcd);
  pic_defun(pic, "lcm", pic_number_lcm);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "floor", pic_number_floor);
  pic_defun(pic, "ceiling", pic_number_ceil);
  pic_defun(pic, "truncate", pic_number_trunc);
  pic_defun(pic, "round", pic_number_round);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "square", pic_number_square);
  pic_defun(pic, "integer-sqrt", pic_number_integer_sqrt);
  pic_defun(pic, "expt", pic_number_expt);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "number->string", pic_number_number_to_string);
  pic_defun(pic, "string->number", pic_number_string_to_number);
  pic_gc_arena_restore(pic, ai);

  pic_deflibrary ("(scheme inexact)") {
    pic_defun(pic, "finite?", pic_number_finite_p);
    pic_defun(pic, "infinite?", pic_number_infinite_p);
    pic_defun(pic, "nan?", pic_number_nan_p);

    pic_defun(pic, "exp", pic_number_exp);
    pic_defun(pic, "log", pic_number_log);
    pic_defun(pic, "sin", pic_number_sin);
    pic_defun(pic, "cos", pic_number_cos);
    pic_defun(pic, "tan", pic_number_tan);
    pic_defun(pic, "acos", pic_number_acos);
    pic_defun(pic, "asin", pic_number_asin);
    pic_defun(pic, "atan", pic_number_atan);

    pic_defun(pic, "sqrt", pic_number_sqrt);
  }
}
