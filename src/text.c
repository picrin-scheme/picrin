/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/text.h"


struct pic_text *
pic_text_new(pic_state *pic, size_t capacity, const char * imbed, size_t len)
{
  struct pic_text *tr;

  tr = (struct pic_text *)pic_obj_alloc(pic, sizeof(struct pic_text), PIC_TT_TEXT);
  tr->data = (char *)pic_alloc(pic, capacity * sizeof(char));
  memcpy((void *)tr->data, (const void *)imbed, len);
  tr->cap = capacity;
  tr->len = len;
  return tr;
}

const char *
pic_text_cstr(struct pic_text *tr)
{
  return tr->data;
}

size_t
pic_text_len(struct pic_text *tr)
{
  return tr->len;
}

size_t
pic_text_capacity(struct pic_text *tr)
{
  return tr->cap;
}

void
pic_text_set(pic_state *pic, struct pic_text *tr, size_t pos, char ch)
{
  if (pos >= tr->len) {
    pic_errorf(pic, "text: index out of range %d", pos); 
  }
  tr->data[pos] = ch; /* modify pos-th character */
}

char
pic_text_ref(pic_state *pic, struct pic_text *tr, size_t pos)
{
  if (pos >= tr->len) {
    pic_errorf(pic, "text: index out of range %d", pos); 
  }
  return tr->data[pos]; /* return pos-th character */
}

void
pic_text_append(pic_state *pic, struct pic_text *text, const char *str, size_t len)
{
  size_t res_length = text->len + len;
  if (res_length > text->cap) {
    size_t new_length = res_length * 2;
    text->data = (char *) pic_realloc(pic, text->data, new_length);
    text->cap  = new_length;
  }
  memcpy((void *)(text->data + text->len), (const void *)str, len);
  text->len += len;
}

static pic_value
pic_text_text_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_text_p(v));
}

static pic_value
pic_text_string_text(pic_state *pic)
{
  pic_str *str;
  const char *cstr;
  size_t len;
  struct pic_text *text;

  pic_get_args(pic, "s", &str);

  cstr = pic_str_cstr(str);
  len = pic_strlen(str);
  text = pic_text_new(pic, 2 * len, cstr, len);
  return pic_obj_value(text);
}

static pic_value
pic_text_text_string(pic_state *pic)
{
  struct pic_text *text;
  pic_str *str;

  pic_get_args(pic, "t", &text);
  str = pic_str_new(pic, text->data, text->len);
  return pic_obj_value(str);
}

static pic_value
pic_text_text_string_set_ip(pic_state *pic)
{
  struct pic_text *tr;
  int k;
  char c;

  pic_get_args(pic, "tic", &tr, &k, &c);

  pic_text_set(pic, tr, k, c);
  return pic_none_value();
}

static pic_value
pic_text_text_string_ref(pic_state *pic)
{
  struct pic_text *tr;
  int k;
  char c;

  pic_get_args(pic, "ti", &tr, &k);

  c = pic_text_ref(pic, tr, k);
  return pic_char_value(c);
}

static pic_value
pic_text_text_string_length(pic_state *pic)
{
  struct pic_text *tr;

  pic_get_args(pic, "t", &tr);

  return pic_int_value(pic_text_len(tr));
}

static pic_value
pic_text_text_string_append(pic_state *pic)
{
  struct pic_text *tr;
  pic_str *str;
  const char *cstr;
  size_t len;

  pic_get_args(pic, "ts", &tr, &str);

  cstr = pic_str_cstr(str);
  len = pic_strlen(str);
  pic_text_append(pic, tr, cstr, len);
  return pic_none_value();
}

void
pic_init_text(pic_state *pic)
{
  pic_defun(pic, "text?", pic_text_text_p);
  pic_defun(pic, "string->text", pic_text_string_text);
  pic_defun(pic, "text->string", pic_text_text_string);
  pic_defun(pic, "text-string-set!", pic_text_text_string_set_ip);
  pic_defun(pic, "text-string-ref", pic_text_text_string_ref);
  pic_defun(pic, "text-string-length", pic_text_text_string_length);
  pic_defun(pic, "text-string-append!", pic_text_text_string_append);
}

