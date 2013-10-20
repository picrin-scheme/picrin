#include <string.h>

#include "picrin.h"
#include "picrin/string.h"

pic_value
pic_str_new_cstr(pic_state *pic, const char *cstr)
{
  struct pic_string *str;
  size_t len;
  char *new_str;

  len = strlen(cstr);
  new_str = (char *)pic_alloc(pic, len + 1);
  strncpy(new_str, cstr, len + 1);

  str = (struct pic_string *)pic_obj_alloc(pic, sizeof(struct pic_string), PIC_TT_STRING);
  str->len = len;
  str->str = new_str;

  return pic_obj_value(str);
}
