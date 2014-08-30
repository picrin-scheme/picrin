/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/text.h"


pic_trans *
pic_trans_new(pic_state *pic, size_t capacity, const char * imbed, size_t len)
{
  pic_trans *tr;

  tr = (pic_trans *)pic_obj_alloc(pic, sizeof(pic_trans), PIC_TT_TRANSIENT);
  tr->data = (char *)pic_alloc(pic, capacity * sizeof(char));
  memcpy((void *)tr->data, (const void *)imbed, len);
  tr->cap = capacity;
  tr->len = len;
  return tr;
}

const char *
pic_trans_cstr(pic_trans *tr)
{
  return tr->data;
}

size_t
pic_trans_len(pic_trans *tr)
{
  return tr->len;
}

size_t
pic_trans_capacity(pic_trans *tr)
{
  return tr->cap;
}

void
pic_trans_set(pic_state *pic, pic_trans *tr, size_t pos, char ch)
{
  if (pos >= tr->len) {
    pic_errorf(pic, "transient: index out of range %d", pos); 
  }
  tr->data[pos] = ch; /* modify pos-th character */
}

char
pic_trans_ref(pic_state *pic, pic_trans *tr, size_t pos)
{
  if (pos >= tr->len) {
    pic_errorf(pic, "transient: index out of range %d", pos); 
  }
  return tr->data[pos]; /* return pos-th character */
}

void
pic_trans_append(pic_state *pic, pic_trans *trans, const char *str, size_t len)
{
  size_t res_length = trans->len + len;
  if (res_length > trans->cap) {
    size_t new_length = res_length * 2;
    trans->data = (char *) pic_realloc(pic, trans->data, new_length);
    trans->cap  = new_length;
  }
  memcpy((void *)(trans->data + trans->len), (const void *)str, len);
  trans->len += len;
}

static pic_value
pic_trans_transient_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_trans_p(v));
}

static pic_value
pic_trans_string_transient(pic_state *pic)
{
  pic_str *str;
  const char *cstr;
  size_t len;
  pic_trans *trans;

  pic_get_args(pic, "s", &str);

  cstr = pic_str_cstr(str);
  len = pic_strlen(str);
  trans = pic_trans_new(pic, 2 * len, cstr, len);
  return pic_obj_value(trans);
}

static pic_value
pic_trans_transient_string(pic_state *pic)
{
  pic_trans *trans;
  pic_str *str;

  pic_get_args(pic, "t", &trans);
  str = pic_str_new(pic, trans->data, trans->len);
  return pic_obj_value(str);
}

static pic_value
pic_trans_transient_string_set_ip(pic_state *pic)
{
  pic_trans *tr;
  int k;
  char c;

  pic_get_args(pic, "tic", &tr, &k, &c);

  pic_trans_set(pic, tr, k, c);
  return pic_none_value();
}

static pic_value
pic_trans_transient_string_ref(pic_state *pic)
{
  pic_trans *tr;
  int k;
  char c;

  pic_get_args(pic, "ti", &tr, &k);

  c = pic_trans_ref(pic, tr, k);
  return pic_char_value(c);
}

static pic_value
pic_trans_transient_string_length(pic_state *pic)
{
  pic_trans *tr;

  pic_get_args(pic, "t", &tr);

  return pic_int_value(pic_trans_len(tr));
}

static pic_value
pic_trans_transient_string_append(pic_state *pic)
{
  pic_trans *tr;
  pic_str *str;
  const char *cstr;
  size_t len;

  pic_get_args(pic, "ts", &tr, &str);

  cstr = pic_str_cstr(str);
  len = pic_strlen(str);
  pic_trans_append(pic, tr, cstr, len);
  return pic_none_value();
}

void
pic_init_trans(pic_state *pic)
{
  pic_defun(pic, "transient?", pic_trans_transient_p);
  pic_defun(pic, "string->transient", pic_trans_string_transient);
  pic_defun(pic, "transient->string", pic_trans_transient_string);
  pic_defun(pic, "transient-string-set!", pic_trans_transient_string_set_ip);
  pic_defun(pic, "transient-string-ref", pic_trans_transient_string_ref);
  pic_defun(pic, "transient-string-length", pic_trans_transient_string_length);
  pic_defun(pic, "transient-string-append!", pic_trans_transient_string_append);
}

