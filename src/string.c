#include <string.h>

#include "picrin.h"

pic_value
pic_str_new(pic_state *pic, const char *cstr, size_t len)
{
  struct pic_string *str;
  char *new_str;

  new_str = (char *)pic_alloc(pic, len + 1);
  strncpy(new_str, cstr, len);
  new_str[len] = '\0';

  str = (struct pic_string *)pic_obj_alloc(pic, sizeof(struct pic_string), PIC_TT_STRING);
  str->len = len;
  str->str = new_str;

  return pic_obj_value(str);
}

pic_value
pic_str_new_cstr(pic_state *pic, const char *cstr)
{
  size_t len;

  len = strlen(cstr);
  return pic_str_new(pic, cstr, len);
}
