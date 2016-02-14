/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
pic_number_number_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_float_p(v) || pic_int_p(v));
}

static pic_value
pic_number_exact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_int_p(v));
}

static pic_value
pic_number_inexact_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_float_p(v));
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
  double f;

  pic_get_args(pic, "f", &f);

  return pic_int_value((int)f);
}

#define pic_define_aop(name, op, guard)                                 \
  pic_value                                                             \
  name(pic_state *pic, pic_value a, pic_value b)                        \
  {                                                                     \
    PIC_NORETURN void pic_errorf(pic_state *, const char *, ...);       \
    double f;                                                           \
    if (pic_int_p(a) && pic_int_p(b)) {                                 \
      f = (double)pic_int(a) op (double)pic_int(b);                     \
      return (INT_MIN <= f && f <= INT_MAX && guard)                    \
        ? pic_int_value((int)f)                                         \
        : pic_float_value(f);                                           \
    } else if (pic_float_p(a) && pic_float_p(b)) {                      \
      return pic_float_value(pic_float(a) op pic_float(b));             \
    } else if (pic_int_p(a) && pic_float_p(b)) {                        \
      return pic_float_value(pic_int(a) op pic_float(b));               \
    } else if (pic_float_p(a) && pic_int_p(b)) {                        \
      return pic_float_value(pic_float(a) op pic_int(b));               \
    } else {                                                            \
      pic_errorf(pic, #name ": non-number operand given");              \
    }                                                                   \
    PIC_UNREACHABLE();                                                  \
  }

pic_define_aop(pic_add, +, true)
pic_define_aop(pic_sub, -, true)
pic_define_aop(pic_mul, *, true)
pic_define_aop(pic_div, /, f == (int)f)

#define pic_define_cmp(name, op)                                        \
  bool                                                                  \
  name(pic_state *pic, pic_value a, pic_value b)                        \
  {                                                                     \
    PIC_NORETURN void pic_errorf(pic_state *, const char *, ...);       \
    if (pic_int_p(a) && pic_int_p(b)) {                                 \
      return pic_int(a) op pic_int(b);                                  \
    } else if (pic_float_p(a) && pic_float_p(b)) {                      \
      return pic_float(a) op pic_float(b);                              \
    } else if (pic_int_p(a) && pic_float_p(b)) {                        \
      return pic_int(a) op pic_float(b);                                \
    } else if (pic_float_p(a) && pic_int_p(b)) {                        \
      return pic_float(a) op pic_int(b);                                \
    } else {                                                            \
      pic_errorf(pic, #name ": non-number operand given");              \
    }                                                                   \
    PIC_UNREACHABLE();                                                  \
  }

pic_define_cmp(pic_eq, ==)
pic_define_cmp(pic_lt, <)
pic_define_cmp(pic_le, <=)
pic_define_cmp(pic_gt, >)
pic_define_cmp(pic_ge, >=)

#define DEFINE_CMP(op)                                  \
  static pic_value                                      \
  pic_number_##op(pic_state *pic)                       \
  {                                                     \
    int argc, i;                                        \
    pic_value *argv;                                    \
                                                        \
    pic_get_args(pic, "*", &argc, &argv);               \
                                                        \
    if (argc < 2) {                                     \
      return pic_true_value();                          \
    }                                                   \
                                                        \
    for (i = 1; i < argc; ++i) {                        \
      if (! pic_##op(pic, argv[i - 1], argv[i])) {      \
        return pic_false_value();                       \
      }                                                 \
    }                                                   \
    return pic_true_value();                            \
  }

DEFINE_CMP(eq)
DEFINE_CMP(lt)
DEFINE_CMP(le)
DEFINE_CMP(gt)
DEFINE_CMP(ge)

#define DEFINE_AOP(op, v1, c0)                  \
  static pic_value                              \
  pic_number_##op(pic_state *pic)               \
  {                                             \
    int argc, i;                                \
    pic_value *argv, tmp;                       \
                                                \
    pic_get_args(pic, "*", &argc, &argv);       \
                                                \
    if (argc == 0) {                            \
      c0;                                       \
    }                                           \
    else if (argc == 1) {                       \
      return v1;                                \
    }                                           \
                                                \
    tmp = argv[0];                              \
    for (i = 1; i < argc; ++i) {                \
      tmp = pic_##op(pic, tmp, argv[i]);        \
    }                                           \
    return tmp;                                 \
  }

DEFINE_AOP(add, argv[0], do {
    return pic_int_value(0);
  } while (0))
DEFINE_AOP(mul, argv[0], do {
    return pic_int_value(1);
  } while (0))
DEFINE_AOP(sub, pic_sub(pic, pic_int_value(0), argv[0]), do {
    pic_errorf(pic, "-: at least one argument required");
  } while (0))
DEFINE_AOP(div, pic_div(pic, pic_int_value(1), argv[0]), do {
    pic_errorf(pic, "/: at least one argument required");
  } while (0))

static int
number_string_length(int val, int radix)
{
  unsigned long v = val; /* in case val == INT_MIN */
  int count = 0;
  if (val == 0) {
    return 1;
  }
  if (val < 0) {
    v = -val;
    count = 1;
  }
  while (v > 0) {
    ++count;
    v /= radix;
  }
  return count;
}

static void
number_string(int val, int radix, int length, char *buffer) {
  const char digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
  unsigned long v = val;
  int i;
  if (val == 0) {
    buffer[0] = '0';
    buffer[1] = '\0';
    return;
  }
  if (val < 0) {
    buffer[0] = '-';
    v = -val;
  }

  for(i = length - 1; v > 0; --i) {
    buffer[i] = digits[v % radix];
    v /= radix;
  }
  buffer[length] = '\0';
  return;
}

static pic_value
pic_number_number_to_string(pic_state *pic)
{
  double f;
  bool e;
  int radix = 10;
  pic_str *str;

  pic_get_args(pic, "F|i", &f, &e, &radix);

  if (radix < 2 || radix > 36) {
    pic_errorf(pic, "number->string: invalid radix %d (between 2 and 36, inclusive)", radix);
  }

  if (e) {
    int ival = (int) f;
    int ilen = number_string_length(ival, radix);
    int s = ilen + 1;
    char *buf = pic_malloc(pic, s);

    number_string(ival, radix, ilen, buf);

    str = pic_make_str(pic, buf, s - 1);

    pic_free(pic, buf);
  }
  else {
    struct pic_port *port = pic_open_output_string(pic);

    xfprintf(pic, port->file, "%f", f);

    str = pic_get_output_string(pic, port);

    pic_close_port(pic, port);
  }

  return pic_obj_value(str);
}

static pic_value
pic_number_string_to_number(pic_state *pic)
{
  const char *str;
  int radix = 10;
  long num;
  char *eptr;
  pic_value flo;

  pic_get_args(pic, "z|i", &str, &radix);

  num = strtol(str, &eptr, radix);
  if (*eptr == '\0') {
    return pic_valid_int(num)
      ? pic_int_value((int)num)
      : pic_float_value(num);
  }

  pic_try {
    flo = pic_read_cstr(pic, str);
  }
  pic_catch {
    /* swallow error */
    flo = pic_false_value();
  }

  if (pic_int_p(flo) || pic_float_p(flo)) {
    return flo;
  }

  return pic_false_value();
}

void
pic_init_number(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_defun(pic, "number?", pic_number_number_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "exact?", pic_number_exact_p);
  pic_defun(pic, "inexact?", pic_number_inexact_p);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "inexact", pic_number_inexact);
  pic_defun(pic, "exact", pic_number_exact);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "=", pic_number_eq);
  pic_defun(pic, "<", pic_number_lt);
  pic_defun(pic, ">", pic_number_gt);
  pic_defun(pic, "<=", pic_number_le);
  pic_defun(pic, ">=", pic_number_ge);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "+", pic_number_add);
  pic_defun(pic, "-", pic_number_sub);
  pic_defun(pic, "*", pic_number_mul);
  pic_defun(pic, "/", pic_number_div);
  pic_gc_arena_restore(pic, ai);

  pic_defun(pic, "number->string", pic_number_number_to_string);
  pic_defun(pic, "string->number", pic_number_string_to_number);
  pic_gc_arena_restore(pic, ai);
}
