/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"

struct pic_string *
pic_str_new(pic_state *pic, const char *cstr, size_t len)
{
  struct pic_string *str;

  str = (struct pic_string *)pic_obj_alloc(pic, sizeof(struct pic_string), PIC_TT_STRING);
  str->len = len;
  str->str = pic_strdup(pic, cstr);
  return str;
}

struct pic_string *
pic_str_new_cstr(pic_state *pic, const char *cstr)
{
  size_t len;

  len = strlen(cstr);
  return pic_str_new(pic, cstr, len);
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
  pic_defun(pic, "string-append", pic_str_string_append);
}
