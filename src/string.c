/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/pair.h"
#include "picrin/port.h"

static pic_str *
str_new_rope(pic_state *pic, xrope *rope)
{
  pic_str *str;

  str = (pic_str *)pic_obj_alloc(pic, sizeof(pic_str), PIC_TT_STRING);
  str->rope = rope;             /* delegate ownership */
  return str;
}

pic_str *
pic_str_new(pic_state *pic, const char *imbed, size_t len)
{
  if (imbed == NULL && len > 0) {
    pic_errorf(pic, "zero length specified against NULL ptr");
  }
  return str_new_rope(pic, xr_new_copy(imbed, len));
}

pic_str *
pic_str_new_cstr(pic_state *pic, const char *cstr)
{
  return pic_str_new(pic, cstr, strlen(cstr));
}

pic_str *
pic_str_new_fill(pic_state *pic, size_t len, char fill)
{
  size_t i;
  char *cstr;
  pic_str *str;

  cstr = (char *)pic_alloc(pic, len + 1);
  cstr[len] = '\0';
  for (i = 0; i < len; ++i) {
    cstr[i] = fill;
  }

  str = pic_str_new(pic, cstr, len);

  pic_free(pic, cstr);
  return str;
}

size_t
pic_strlen(pic_str *str)
{
  return xr_len(str->rope);
}

char
pic_str_ref(pic_state *pic, pic_str *str, size_t i)
{
  char c;

  c = xr_at(str->rope, i);
  if (c == -1) {
    pic_errorf(pic, "index out of range %d", i);
  }
  return c;
}

static xrope *
xr_put(xrope *rope, size_t i, char c)
{
  xrope *x, *y;
  char buf[1];

  if (xr_len(rope) <= i) {
    return NULL;
  }

  buf[0] = c;

  x = xr_sub(rope, 0, i);
  y = xr_new_copy(buf, 1);
  rope = xr_cat(x, y);
  XROPE_DECREF(x);
  XROPE_DECREF(y);

  x = rope;
  y = xr_sub(rope, i + 1, xr_len(rope));
  rope = xr_cat(x, y);
  XROPE_DECREF(x);
  XROPE_DECREF(y);

  return rope;
}

void
pic_str_set(pic_state *pic, pic_str *str, size_t i, char c)
{
  xrope *x;

  x = xr_put(str->rope, i, c);
  if (x == NULL) {
    pic_errorf(pic, "index out of range %d", i);
  }
  XROPE_DECREF(str->rope);
  str->rope = x;
}

pic_str *
pic_strcat(pic_state *pic, pic_str *a, pic_str *b)
{
  return str_new_rope(pic, xr_cat(a->rope, b->rope));
}

pic_str *
pic_substr(pic_state *pic, pic_str *str, size_t s, size_t e)
{
  return str_new_rope(pic, xr_sub(str->rope, s, e));
}

int
pic_strcmp(pic_str *str1, pic_str *str2)
{
  return strcmp(xr_cstr(str1->rope), xr_cstr(str2->rope));
}

const char *
pic_str_cstr(pic_str *str)
{
  return xr_cstr(str->rope);
}

pic_value
pic_vfformat(pic_state *pic, xFILE *file, const char *fmt, va_list ap)
{
  char c;
  pic_value irrs = pic_nil_value();

  while ((c = *fmt++)) {
    switch (c) {
    default:
      xfputc(c, file);
      break;
    case '%':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '%':
        xfputc('%', file);
        break;
      case 'c':
        xfprintf(file, "%c", va_arg(ap, int));
        break;
      case 's':
        xfprintf(file, "%s", va_arg(ap, const char *));
        break;
      case 'd':
        xfprintf(file, "%d", va_arg(ap, int));
        break;
      case 'p':
        xfprintf(file, "%p", va_arg(ap, void *));
        break;
      case 'f':
        xfprintf(file, "%f", va_arg(ap, double));
        break;
      }
      break;
    case '~':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '~':
        xfputc('~', file);
        break;
      case '%':
        xfputc('\n', file);
        break;
      case 'S':
        irrs = pic_cons(pic, pic_fdebug(pic, va_arg(ap, pic_value), file), irrs);
        break;
      }
      break;
    }
  }
 exit:

  return pic_reverse(pic, irrs);
}

pic_value
pic_vformat(pic_state *pic, const char *fmt, va_list ap)
{
  struct pic_port *port;
  pic_value irrs;

  port = pic_open_output_string(pic);

  irrs = pic_vfformat(pic, port->file, fmt, ap);
  irrs = pic_cons(pic, pic_obj_value(pic_get_output_string(pic, port)), irrs);

  pic_close_port(pic, port);
  return irrs;
}

pic_value
pic_format(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value objs;

  va_start(ap, fmt);
  objs = pic_vformat(pic, fmt, ap);
  va_end(ap);

  return objs;
}

static pic_value
pic_str_string_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_str_p(v));
}

static pic_value
pic_str_make_string(pic_state *pic)
{
  int len;
  char c = ' ';

  pic_get_args(pic, "i|c", &len, &c);

  return pic_obj_value(pic_str_new_fill(pic, len, c));
}

static pic_value
pic_str_string_length(pic_state *pic)
{
  pic_str *str;

  pic_get_args(pic, "s", &str);

  return pic_int_value(pic_strlen(str));
}

static pic_value
pic_str_string_ref(pic_state *pic)
{
  pic_str *str;
  int k;

  pic_get_args(pic, "si", &str, &k);

  return pic_char_value(pic_str_ref(pic, str, k));
}

static pic_value
pic_str_string_set(pic_state *pic)
{
  pic_str *str;
  char c;
  int k;

  pic_get_args(pic, "sic", &str, &k, &c);

  pic_str_set(pic, str, k, c);
  return pic_none_value();
}

#define DEFINE_STRING_CMP(name, op)                                     \
  static pic_value                                                      \
  pic_str_string_##name(pic_state *pic)                                 \
  {                                                                     \
    size_t argc;                                                        \
    pic_value *argv;                                                    \
    size_t i;                                                           \
                                                                        \
    pic_get_args(pic, "*", &argc, &argv);                               \
                                                                        \
    if (argc < 1 || ! pic_str_p(argv[0])) {                             \
      return pic_false_value();                                         \
    }                                                                   \
                                                                        \
    for (i = 1; i < argc; ++i) {                                        \
      if (! pic_str_p(argv[i])) {                                       \
	return pic_false_value();                                       \
      }                                                                 \
      if (! (pic_strcmp(pic_str_ptr(argv[i-1]), pic_str_ptr(argv[i])) op 0)) { \
	return pic_false_value();                                       \
      }                                                                 \
    }                                                                   \
    return pic_true_value();                                            \
  }

DEFINE_STRING_CMP(eq, ==)
DEFINE_STRING_CMP(lt, <)
DEFINE_STRING_CMP(gt, >)
DEFINE_STRING_CMP(le, <=)
DEFINE_STRING_CMP(ge, >=)

static pic_value
pic_str_string_copy(pic_state *pic)
{
  pic_str *str;
  int n, start, end;

  n = pic_get_args(pic, "s|ii", &str, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_strlen(str);
  }

  return pic_obj_value(pic_substr(pic, str, start, end));
}

static pic_value
pic_str_string_copy_ip(pic_state *pic)
{
  pic_str *to, *from;
  int n, at, start, end;

  n = pic_get_args(pic, "sis|ii", &to, &at, &from, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = pic_strlen(from);
  }

  while (start < end) {
    pic_str_set(pic, to, at++, pic_str_ref(pic, from, start++));
  }
  return pic_none_value();
}

static pic_value
pic_str_string_append(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;
  pic_str *str;

  pic_get_args(pic, "*", &argc, &argv);

  str = pic_str_new(pic, NULL, 0);
  for (i = 0; i < argc; ++i) {
    if (! pic_str_p(argv[i])) {
      pic_error(pic, "type error");
    }
    str = pic_strcat(pic, str, pic_str_ptr(argv[i]));
  }
  return pic_obj_value(str);
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  pic_str *str;
  char c;
  int n, start, end;

  n = pic_get_args(pic, "sc|ii", &str, &c, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_strlen(str);
  }

  while (start < end) {
    pic_str_set(pic, str, start++, c);
  }
  return pic_none_value();
}

void
pic_init_str(pic_state *pic)
{
  pic_defun(pic, "string?", pic_str_string_p);
  pic_defun(pic, "make-string", pic_str_make_string);
  pic_defun(pic, "string-length", pic_str_string_length);
  pic_defun(pic, "string-ref", pic_str_string_ref);
  pic_defun(pic, "string-set!", pic_str_string_set);

  pic_defun(pic, "string=?", pic_str_string_eq);
  pic_defun(pic, "string<?", pic_str_string_lt);
  pic_defun(pic, "string>?", pic_str_string_gt);
  pic_defun(pic, "string<=?", pic_str_string_le);
  pic_defun(pic, "string>=?", pic_str_string_ge);

  pic_defun(pic, "string-copy", pic_str_string_copy);
  pic_defun(pic, "string-copy!", pic_str_string_copy_ip);
  pic_defun(pic, "string-append", pic_str_string_append);
  pic_defun(pic, "string-fill!", pic_str_string_fill_ip);
}
