/**
 * See Copyright Notice in picrin.h
 */

#include <math.h>
#include <gmp.h>
#include <mpfr.h>
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

static int
gcd(int a, int b)
{
  if (a > b)
    return gcd(b, a);
  if (a < 0)
    return gcd(-a, b);
  if (a > 0)
    return gcd(b % a, a);
  return b;
}

static double
lcm(int a, int b)
{
  return fabs((double)a * b) / gcd(a, b);
}

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
    mpfr_set_inf(f, sign 1);                                   \
    result = pic_float_value(sign INFINITY);                   \
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

/* :TODO: */
#define DEFINE_ARITH_OP(op, name, unit)                         \
  static pic_value                                              \
  pic_number_##name(pic_state *pic)                             \
  {                                                             \
    size_t argc;                                                \
    pic_value *argv;                                            \
    size_t i;                                                   \
    double f;                                                   \
    bool e = true;                                              \
                                                                \
    pic_get_args(pic, "*", &argc, &argv);                       \
                                                                \
    f = unit;                                                   \
    for (i = 0; i < argc; ++i) {                                \
      if (pic_int_p(argv[i])) {                                 \
        f op##= pic_int(argv[i]);                               \
      }                                                         \
      else if (pic_float_p(argv[i])) {                          \
        e = false;                                              \
        f op##= pic_float(argv[i]);                             \
      }                                                         \
      else {                                                    \
        pic_error(pic, #op ": number required");                \
      }                                                         \
    }                                                           \
                                                                \
    return e ? pic_int_value((int)f) : pic_float_value(f);      \
  }

DEFINE_ARITH_OP(+, add, 0)
DEFINE_ARITH_OP(*, mul, 1)
                               /* :TODO: */
#define DEFINE_ARITH_INV_OP(op, name, unit, exact)                      \
  static pic_value                                                      \
  pic_number_##name(pic_state *pic)                                     \
  {                                                                     \
   size_t argc;                                                         \
   pic_value *argv;                                                     \
   size_t i;                                                            \
   double f;                                                            \
   bool e;                                                              \
                                                                        \
   pic_get_args(pic, "F*", &f, &e, &argc, &argv);                       \
                                                                        \
   e = e && exact;                                                      \
                                                                        \
   if (argc == 0) {                                                     \
     f = unit op f;                                                     \
   }                                                                    \
   for (i = 0; i < argc; ++i) {                                         \
     if (pic_int_p(argv[i])) {                                          \
       f op##= pic_int(argv[i]);                                        \
     }                                                                  \
     else if (pic_float_p(argv[i])) {                                   \
       e = false;                                                       \
       f op##= pic_float(argv[i]);                                      \
     }                                                                  \
     else {                                                             \
       pic_error(pic, #op ": number required");                         \
     }                                                                  \
   }                                                                    \
                                                                        \
   return e ? pic_int_value((int)f) : pic_float_value(f);               \
  }

DEFINE_ARITH_INV_OP(-, sub, 0, true)
DEFINE_ARITH_INV_OP(/, div, 1, false)

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
{                               /* :TODO: */
  int i,j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_int_value((int)floor((double)i/j));
  }
  else {
    return pic_float_value(floor((double)i/j));
  }
}

static pic_value
pic_number_floor_remainder(pic_state *pic)
{                               /* :TODO: */
  int i,j,q;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  q = (int)floor((double)i/j);
  if (e1 && e2) {
    return pic_int_value(i - j * q);
  }
  else {
    return pic_float_value(i - j * q);
  }
}

static pic_value
pic_number_trunc_quotient(pic_state *pic)
{                               /* :TODO: */
  int i,j;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  if (e1 && e2) {
    return pic_int_value((int)trunc((double)i/j));
  }
  else {
    return pic_float_value(trunc((double)i/j));
  }
}

static pic_value
pic_number_trunc_remainder(pic_state *pic)
{                               /* :TODO: */
  int i,j,q;
  bool e1, e2;

  pic_get_args(pic, "II", &i, &e1, &j, &e2);

  q = (int)trunc((double)i/j);
  if (e1 && e2) {
    return pic_int_value(i - j * q);
  }
  else {
    return pic_float_value(i - j * q);
  }
}

static pic_value
pic_number_gcd(pic_state *pic)
{                               /* :TODO: */
  size_t argc;
  pic_value *args;
  int r;
  bool e = true;

  pic_get_args(pic, "*", &argc, &args);

  r = 0;
  while (argc-- > 0) {
    if (pic_int_p(args[argc])) {
      r = gcd(r, pic_int(args[argc]));
    }
    else if (pic_float_p(args[argc])) {
      e = false;
      r = gcd(r, pic_float(args[argc]));
    }
    else {
      pic_error(pic, "gcd: number required");
    }
  }
  return e ? pic_int_value(r) : pic_float_value(r);
}

static pic_value
pic_number_lcm(pic_state *pic)
{                               /* :TODO: */
  size_t argc;
  pic_value *args;
  double r;
  bool e = true;

  pic_get_args(pic, "*", &argc, &args);

  r = 1;
  while (argc-- > 0) {
    if (pic_int_p(args[argc])) {
      r = lcm(r, pic_int(args[argc]));
    }
    else if (pic_float_p(args[argc])) {
      e = false;
      r = lcm(r, pic_float(args[argc]));
    }
    else {
      pic_error(pic, "lcm: number required");
    }
  }
  return e && pic_valid_int(r) ? pic_int_value(r) : pic_float_value(r);
}

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

  mpfr_init(f->f);
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
  argc = pic_get_args(pic, "r|r", &f, &g);
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

  pic_get_args(pic, "r", &f);

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

  pic_get_args(pic, "rr", &f, &g);

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
    char buf[snprintf(NULL, 0, "%i", pic_int(n)) + 1];
    snprintf(buf, sizeof buf, "%i", pic_int(n));
    break;
    }
  case PIC_TT_FLOAT:{
    char buf[snprintf(NULL, 0, "%a", pic_float(n)) + 1];
    snprintf(buf, sizeof buf, "%a", pic_float(n));
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
    char buf[mpfr_snprintf(NULL, 0, "%R", pic_bigfloat_ptr(n)->f) + 1];
    mpfr_snprintf(buf,sizeof buf, "%R", pic_bigfloat_ptr(n)->f);
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
