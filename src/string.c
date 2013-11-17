#include <string.h>

#include "picrin.h"

struct pic_string *
pic_str_new(pic_state *pic, const char *cstr, size_t len)
{
  struct pic_string *str;

  str = (struct pic_string *)pic_obj_alloc(pic, sizeof(struct pic_string), PIC_TT_STRING);
  str->len = len;
  str->str = strdup(cstr);
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

void
pic_init_str(pic_state *pic)
{
  pic_defun(pic, "string?", pic_str_string_p);
}
