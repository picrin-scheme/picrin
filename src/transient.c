/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/transient.h"


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

void
pic_init_trans(pic_state *pic)
{
  pic_defun(pic, "string->transient", pic_trans_string_transient);
  pic_defun(pic, "transient->string", pic_trans_transient_string);
}

