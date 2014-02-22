/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/pair.h"
#include "picrin/port.h"

pic_str *
pic_str_new(pic_state *pic, const char *cstr, size_t len)
{
  pic_str *str;
  char *copy;

  if (cstr) {
    copy = pic_strdup(pic, cstr);
  } else {
    copy = (char *)pic_alloc(pic, len);
  }

  str = (pic_str *)pic_obj_alloc(pic, sizeof(pic_str), PIC_TT_STRING);
  str->len = len;
  str->str = copy;
  return str;
}

pic_str *
pic_str_new_cstr(pic_state *pic, const char *cstr)
{
  return pic_str_new(pic, cstr, strlen(cstr));
}

char
pic_str_ref(pic_state *pic, pic_str *str, size_t n)
{
  UNUSED(pic);

  return str->str[n];
}

pic_str *
pic_strcat(pic_state *pic, pic_str *a, pic_str *b)
{
  size_t len;
  char *buf;

  len = a->len + b->len;
  buf = pic_alloc(pic, len + 1);

  memcpy(buf, a->str, a->len);
  memcpy(buf + a->len, b->str, b->len);
  buf[len] = '\0';

  return pic_str_new(pic, buf, len);
}

pic_str *
pic_substr(pic_state *pic, pic_str *str, size_t s, size_t e)
{
  size_t len;
  char *buf;

  len = e - s;
  buf = pic_alloc(pic, len + 1);

  memcpy(buf, str->str + s, len);
  buf[len] = '\0';

  return pic_str_new(pic, buf, len);
}

pic_value
pic_vfformat(pic_state *pic, XFILE *file, const char *fmt, va_list ap)
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
  int k, i;
  char c = ' ', *cstr;

  pic_get_args(pic, "i|c", &k, &c);

  cstr = (char *)pic_alloc(pic, k + 1);
  for (i = 0; i < k; ++i) {
    cstr[i] = c;
  }
  cstr[k] = '\0';

  return pic_obj_value(pic_str_new(pic, cstr, k));
}

static pic_value
pic_str_string_length(pic_state *pic)
{
  char *str;
  size_t len;

  pic_get_args(pic, "s", &str, &len);

  return pic_int_value(len);
}

static pic_value
pic_str_string_ref(pic_state *pic)
{
  char *str;
  size_t len;
  int k;

  pic_get_args(pic, "si", &str, &len, &k);

  return pic_char_value(str[k]);
}

static pic_value
pic_str_string_set(pic_state *pic)
{
  char *str, c;
  size_t len;
  int k;

  pic_get_args(pic, "sic", &str, &len, &k, &c);

  str[k] = c;
  return pic_none_value();
}

#define DEFINE_STRING_CMP(name, op)			\
  static pic_value					\
  pic_str_string_##name(pic_state *pic)			\
  {							\
    size_t argc;					\
    pic_value *argv;					\
    size_t i;						\
							\
    pic_get_args(pic, "*", &argc, &argv);		\
							\
    if (argc < 1 || ! pic_str_p(argv[0])) {		\
      return pic_false_value();				\
    }							\
							\
    for (i = 1; i < argc; ++i) {			\
      if (! pic_str_p(argv[i])) {			\
	return pic_false_value();			\
      }							\
      if (! (strcmp(pic_str_ptr(argv[i-1])->str,	\
		    pic_str_ptr(argv[i])->str)		\
	     op 0)) {					\
	return pic_false_value();			\
      }							\
    }							\
    return pic_true_value();				\
  }

DEFINE_STRING_CMP(eq, ==)
DEFINE_STRING_CMP(lt, <)
DEFINE_STRING_CMP(gt, >)
DEFINE_STRING_CMP(le, <=)
DEFINE_STRING_CMP(ge, >=)

static pic_value
pic_str_string_copy(pic_state *pic)
{
  size_t len, start, end, i;
  char *str;
  int n;
  pic_str *copy;

  n = pic_get_args(pic, "s|ii", &str, &len, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  copy = pic_str_new(pic, NULL, end - start);
  for (i = 0; i < end - start; ++i) {
    copy->str[i] = str[start + i];
  }
  return pic_obj_value(copy);
}

static pic_value
pic_str_string_copy_ip(pic_state *pic)
{
  size_t to_len, from_len, at, start, end;
  char *to_str, *from_str;
  int n;

  n = pic_get_args(pic, "sis|ii", &to_str, &to_len, &at, &from_str, &from_len, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = from_len;
  }

  while (start < end) {
    to_str[at++] = from_str[start++];
  }
  return pic_none_value();
}

static pic_value
pic_str_string_append(pic_state *pic)
{
  size_t argc, len, i;
  pic_value *argv;
  char *buf;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  buf = NULL;
  for (i = 0; i < argc; ++i) {
    if (! pic_str_p(argv[i])) {
      pic_error(pic, "type error");
    }
    buf = pic_realloc(pic, buf, len + pic_str_ptr(argv[i])->len);
    /* copy! */
    memcpy(buf + len, pic_str_ptr(argv[i])->str, pic_str_ptr(argv[i])->len);
    len += pic_str_ptr(argv[i])->len;
  }
  return pic_obj_value(pic_str_new(pic, buf, len));
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  size_t len, start, end;
  char *str, c;
  int n;

  n = pic_get_args(pic, "sc|ii", &str, &len, &c, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  while (start < end) {
    str[start++] = c;
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
