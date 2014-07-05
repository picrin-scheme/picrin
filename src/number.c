/**
 * See Copyright Notice in picrin.h
 */

#include <math.h>
#include <limits.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/number.h"
#include "picrin/string.h"

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
pic_bigint *                                                    \
pic_bigint_##op(pic_state *pic, pic_bigint *x, pic_bigint *y)   \
{                                                               \
  pic_bigint *res = pic_bigint_new(pic);                        \
  mpz_##op(res->z, x->z, y->z);                                 \
  return res;                                                   \
}                                                               \

DEFINE_BIGINT_ARITH(add)
DEFINE_BIGINT_ARITH(sub)
DEFINE_BIGINT_ARITH(mul)

pic_rational *
pic_bigint_div(pic_state *pic, pic_bigint *x, pic_bigint *y)
{
  pic_rational *res = pic_rational_new(pic);
  mpq_set_num(res->q, x->z);
  mpq_set_den(res->q, y->z);
  mpq_canonicalize(res->q);
  return res;
}

#define DEFINE_RATIONAL_ARITH(op)                                   \
pic_rational *                                                      \
pic_rational_##op(pic_state *pic, pic_rational *x, pic_rational *y) \
{                                                                   \
  pic_rational *res = pic_rational_new(pic);                        \
  mpq_##op(res->q, x->q, y->q);                                     \
  return  res;                                                      \
}                                                                   \

DEFINE_RATIONAL_ARITH(add);
DEFINE_RATIONAL_ARITH(sub);
DEFINE_RATIONAL_ARITH(mul);
DEFINE_RATIONAL_ARITH(div);

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

  return pic_bool_value(pic_int_p(v) || pic_bigint_p(v) || pic_rational_p(v) || pic_bigfloat_p(v));
}

static pic_value
pic_number_inexact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "n", &v);

  return pic_bool_value(pic_float_p(v));
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

#define DEFINE_ARITH_CMP(op, name)                              \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc;                                                \
    pic_value *argv;                                            \
    size_t i;                                                   \
    mpfr_t f;                                                   \
    pic_value g;                                                \
    mpfr_init(f);                                               \
    pic_get_args(pic, "rn*", &f, &g, &argc, &argv);             \
    i = -1;                                                     \
    while ( ++i < argc) {                                       \
      switch(pic_type(g)){                                      \
      case PIC_TT_INT:                                          \
        if (!(mpfr_cmp_si(f, pic_int(g)) op 0)){                \
          mpfr_clear(f);                                        \
          return pic_false_value();                             \
        }                                                       \
        else                                                    \
          mpfr_set_si(f, pic_int(g), MPFR_RNDN);                \
        break;                                                  \
      case PIC_TT_FLOAT:                                        \
        if (!(mpfr_cmp_d(f, pic_float(g)) op 0)){               \
          mpfr_clear(f);                                        \
          return pic_false_value();                             \
        }                                                       \
        else                                                    \
          mpfr_set_d(f, pic_float(g), MPFR_RNDN);               \
        break;                                                  \
      case PIC_TT_BIGINT:                                       \
        if (!(mpfr_cmp_z(f, pic_bigint_ptr(g)->z) op 0)){       \
          mpfr_clear(f);                                        \
          return pic_false_value();                             \
        }                                                       \
        else                                                    \
          mpfr_set_z(f, pic_bigint_ptr(g)->z, MPFR_RNDN);       \
        break;                                                  \
      case PIC_TT_RATIONAL:                                     \
        if (!(mpfr_cmp_q(f, pic_rational_ptr(g)->q) op 0)){     \
          mpfr_clear(f);                                        \
          return pic_false_value();                             \
        }                                                       \
        else                                                    \
          mpfr_set_q(f, pic_rational_ptr(g)->q, MPFR_RNDN);     \
        break;                                                  \
      case PIC_TT_BIGFLOAT:                                     \
        if (!(mpfr_cmp(f, pic_bigfloat_ptr(g)->f) op 0)){       \
          mpfr_clear(f);                                        \
          return pic_false_value();                             \
        }                                                       \
        else                                                    \
          mpfr_set(f, pic_bigfloat_ptr(g)->f, MPFR_RNDN);       \
        break;                                                  \
      default:                                                  \
        pic_error(pic, #op ": number required");                \
      }                                                         \
      g = argv[i];                                              \
    }                                                           \
    mpfr_clear(f);                                              \
    return pic_true_value();                                    \
  }

DEFINE_ARITH_CMP(==, eq)
DEFINE_ARITH_CMP(<, lt)
DEFINE_ARITH_CMP(>, gt)
DEFINE_ARITH_CMP(<=, le)
DEFINE_ARITH_CMP(>=, ge)

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
    return result;                                              \
  }

DEFINE_MIN_MAX(>, min, +);
DEFINE_MIN_MAX(<, max, -);


#define DEFINE_ARITH_OP(op, name, unit)                                 \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
    size_t argc;                                                        \
    pic_value *argv;                                                    \
    size_t i;                                                           \
    pic_value a, b;                                                     \
                                                                        \
    pic_get_args(pic, "*", &argc, &argv);                               \
                                                                        \
    a = pic_float_value(unit);                                          \
    for (i = 0; i < argc; ++i) {                                        \
      b = argv[i];                                                      \
      switch(pic_type(a)){                                              \
      case PIC_TT_INT:{                                                 \
        switch(pic_type(b)){                                            \
        case PIC_TT_INT:{                                               \
          double f = (double) pic_int(a) op (double) pic_int(b);        \
          if (INT_MIN <= f && f <= INT_MAX) {                           \
            a = pic_int_value((int)f);                                  \
          }                                                             \
          else {                                                        \
            pic_bigint *c = pic_bigint_new(pic);                        \
            pic_bigint *d = pic_bigint_new(pic);                        \
            mpz_set_si(c->z, pic_int(a));                               \
            mpz_set_si(d->z, pic_int(b));                               \
            mpz_##name(c->z, c->z, d->z);                               \
            a = pic_obj_value(c);                                       \
          }                                                             \
                                                                        \
          break;                                                        \
        }                                                               \
        case PIC_TT_FLOAT:{                                             \
          a = pic_float_value(pic_int(a) op pic_float(b));              \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGINT:{                                            \
          pic_bigint *c = pic_bigint_new(pic);                          \
          mpz_set_si(c->z, pic_int(a));                                 \
          mpz_##name(c->z, c->z, pic_bigint_ptr(b)->z);                 \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_RATIONAL:{                                          \
          pic_rational *c = pic_rational_new(pic);                      \
          mpq_set_si(c->q,  pic_int(a), 1);                             \
          mpq_##name(c->q, c->q, pic_rational_ptr(b)->q);               \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGFLOAT:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_si_##name(c->f, pic_int(a), pic_bigfloat_ptr(b)->f, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
        }                                                               \
        default:{                                                       \
          pic_errorf(pic, #op " got non-number operands");              \
          break;                                                        \
        }                                                               \
        }                                                               \
        break;                                                          \
      }                                                                 \
      case PIC_TT_FLOAT: {                                              \
        switch(pic_type(b)){                                            \
        case PIC_TT_INT:{                                               \
          a = pic_float_value(pic_float(a) op pic_int(b));              \
          break;                                                        \
        }                                                               \
        case PIC_TT_FLOAT:{                                             \
          a = pic_float_value(pic_float(a) op pic_float(b));            \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGINT:{                                            \
          a = pic_float_value( pic_int(a) op                            \
                               mpz_get_d(pic_bigint_ptr(b)->z));        \
          break;                                                        \
        }                                                               \
        case PIC_TT_RATIONAL:{                                          \
          a = pic_float_value(mpq_get_d(pic_rational_ptr(b)->q)         \
                              op pic_float(a));                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGFLOAT:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_d_##name(c->f, pic_float(a), pic_bigfloat_ptr(b)->f, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
        }                                                               \
        default:{                                                       \
          pic_errorf(pic, #op " got non-number operands");              \
          break;                                                        \
        }                                                               \
        }                                                               \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGINT:{                                              \
        switch(pic_type(b)){                                            \
        case PIC_TT_INT:{                                               \
          pic_bigint *c = pic_bigint_new(pic);                          \
          mpz_set_si(c->z, pic_int(b));                                 \
          mpz_##name(c->z, pic_bigint_ptr(a)->z, c->z);                 \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_FLOAT:{                                             \
          a = pic_float_value(mpz_get_d(pic_bigint_ptr(a)->z)           \
                              op pic_int(b));                           \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGINT:{                                            \
          a = pic_obj_value(pic_bigint_##name(pic,                      \
                                              pic_bigint_ptr(a),        \
                                              pic_bigint_ptr(b)));      \
          break;                                                        \
        }                                                               \
        case PIC_TT_RATIONAL:{                                          \
          pic_rational *c = pic_rational_new(pic);                      \
          mpq_set_z(c->q, pic_bigint_ptr(a)->z);                        \
          mpq_##name(c->q, c->q, pic_rational_ptr(b)->q);               \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGFLOAT:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_z_##name(c->f, pic_bigint_ptr(a)->z, pic_bigfloat_ptr(b)->f, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
        }                                                               \
        default:{                                                       \
          pic_errorf(pic, #op " got non-number operands");              \
          break;                                                        \
        }                                                               \
        }                                                               \
        break;                                                          \
      }                                                                 \
      case PIC_TT_RATIONAL:{                                            \
        switch(pic_type(b)){                                            \
        case PIC_TT_INT:{                                               \
          pic_rational *c = pic_rational_new(pic);                      \
          mpq_set_d(c->q, (double) pic_int(b));                         \
          mpq_##name(c->q, pic_rational_ptr(a)->q, c->q);               \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_FLOAT:{                                             \
          a = pic_float_value(pic_float(a) op                           \
                              mpq_get_d(pic_rational_ptr(b)->q));       \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGINT:{                                            \
          pic_rational *c = pic_rational_new(pic);                      \
          mpq_set_z(c->q, pic_bigint_ptr(b)->z);                        \
          mpq_##name(c->q, pic_rational_ptr(a)->q, c->q);               \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_RATIONAL:{                                          \
          a = pic_obj_value(pic_rational_##name(pic,                    \
                                                pic_rational_ptr(a),    \
                                                pic_rational_ptr(b)));  \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGFLOAT:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_q_##name(c->f, pic_rational_ptr(a)->q, pic_bigfloat_ptr(b)->f, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
        }                                                               \
        default:{                                                       \
          pic_errorf(pic, #op " got non-number operands");              \
          break;                                                        \
        }                                                               \
        }                                                               \
        break;                                                          \
      }                                                                 \
      case PIC_TT_BIGFLOAT:{                                            \
        switch(pic_type(b)){                                            \
        case PIC_TT_INT:{                                               \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_##name##_si(c->f, pic_bigfloat_ptr(a)->f, pic_int(b), MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_FLOAT:{                                             \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_##name##_d(c->f, pic_bigfloat_ptr(a)->f,  pic_float(b), MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGINT:{                                            \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_##name##_z(c->f, pic_bigfloat_ptr(a)->f,  pic_bigint_ptr(b)->z, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_RATIONAL:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_##name##_q(c->f, pic_bigfloat_ptr(a)->f, pic_rational_ptr(b)->q, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
          break;                                                        \
        }                                                               \
        case PIC_TT_BIGFLOAT:{                                          \
          pic_bigfloat *c = pic_bigfloat_new(pic);                      \
          mpfr_q_##name(c->f, pic_rational_ptr(a)->q, pic_bigfloat_ptr(b)->f, MPFR_RNDN); \
          a = pic_obj_value(c);                                         \
        }                                                               \
        default:{                                                       \
          pic_errorf(pic, #op " got non-number operands");              \
          break;                                                        \
        }                                                               \
        }                                                               \
        break;                                                          \
      }                                                                 \
      default:{                                                         \
        pic_errorf(pic, #op " got non-number operands");                \
        break;                                                          \
      }                                                                 \
      }                                                                 \
    }                                                                   \
    return a;                                                           \
  }

DEFINE_ARITH_OP(+, add, 0)
DEFINE_ARITH_OP(*, mul, 1)
DEFINE_ARITH_OP(-, sub, 0)
DEFINE_ARITH_OP(/, div, 1)

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
  pic_number_normalize(pic, &v);
  return v;  
}

static pic_value
pic_number_floor_remainder(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g, h;
  pic_value v;
  f = pic_bigfloat_new(pic);
  mpfr_init(g);
  mpfr_init(h);

  pic_get_args(pic, "rr", &(f->f), &g);

  mpfr_div(h, f->f, g, MPFR_RNDD);
  mpfr_floor(h, h);
  mpfr_mul(h, h, g, MPFR_RNDN);
  mpfr_sub(f->f, f->f, h, MPFR_RNDD);
  mpfr_clear(g);
  mpfr_clear(h);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v);
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
  pic_number_normalize(pic, &v);
  return v;  
}

static pic_value
pic_number_trunc_remainder(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  pic_value v;
  f = pic_bigfloat_new(pic);
  mpfr_init(g);

  pic_get_args(pic, "rr", &(f->f), &g);

  mpfr_remainder(f->f, f->f, g, MPFR_RNDN);
  mpfr_clear(g);

  v = pic_obj_value(f);
  pic_number_normalize(pic, &v);
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
        mpz_##name##_ui(z->z, z->z, (unsigned long) pic_int(v));        \
        break;                                                          \
      case PIC_TT_FLOAT:                                                \
        e = false;                                                      \
        mpz_##name##_ui(z->z, z->z,(unsigned long) pic_float(v));       \
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
      pic_number_normalize(pic, &res);                                  \
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
    pic_get_args(pic, "o", &v);                                 \
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

  f = pic_bigfloat_new(pic);
  pic_get_args(pic, "r", &f);
  mpfr_exp(f->f, f->f, MPFR_RNDN);
  return pic_obj_value(f);
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
  pic_number_normalize(pic, &v);

  return v;
}

static pic_value
pic_number_expt(pic_state *pic)
{
  pic_bigfloat *f;
  mpfr_t g;
  pic_value v;

  f = pic_bigfloat_new(pic);
  mpfr_init(g);

  pic_get_args(pic, "rr", &(f->f), &g);

  mpfr_pow(f->f, f->f, g, MPFR_RNDN);
  mpfr_clear(g);
  v = pic_obj_value(f);
  pic_number_normalize(pic, &v);
  return v;
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

  if(pic_float_p(v)){
    pic_bigint *z;
    pic_value w;
    z = pic_bigint_new(pic);
    mpz_set_d(z->z, pic_float(v));
    w = pic_obj_value(z);
    pic_number_normalize(pic, &w);
    return w;
  }
  else{
    return v;
  }
}

static pic_value
pic_number_number_to_string(pic_state *pic)
{                               /* :TODO: */
  pic_value n;
  int radix = 10;
  char *buf;

  pic_get_args(pic, "n|i", &n, &radix);


  switch(pic_type(n)){
  case PIC_TT_INT:{
    mpz_t z;
    mpz_init_set_si(z, pic_int(n));
    buf = mpz_get_str(NULL, radix, z);
    mpz_clear(z);
    break;
    }
  case PIC_TT_FLOAT:{
    mpfr_t f;
    mpfr_exp_t e;
    mpfr_init_set_d(f, pic_float(n), MPFR_RNDN);
    buf = mpfr_get_str(NULL, &e, radix, 0, f, MPFR_RNDN);
    mpfr_clear(f);
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
    mpfr_exp_t e;
    buf = mpfr_get_str(NULL, &e, radix, 0, pic_bigfloat_ptr(n)->f, MPFR_RNDN);
    break;
  }
  default:
    pic_errorf(pic, "logic flow");
  }
  return pic_obj_value(pic_str_new(pic, buf, sizeof buf - 1));

}

static pic_value
pic_number_string_to_number(pic_state *pic)
{                               /* :TODO: */
  UNUSED(pic);
  return pic_obj_value(NULL);
  
}

void
pic_init_number(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_real_p);
  pic_defun(pic, "complex?", pic_number_real_p);
  pic_defun(pic, "real?", pic_number_real_p);
  pic_defun(pic, "rational?", pic_number_integer_p);
  pic_defun(pic, "integer?", pic_number_integer_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_defun(pic, "exact-integer?", pic_number_exact_p);
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
