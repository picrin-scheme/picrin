#include "picrin.h"
#include "picrin/string.h"

void
pic_str_set(pic_state *pic, pic_str *str, size_t i, char c)
{
  pic_str *x, *y, *z, *tmp;

  if (pic_strlen(str) <= i) {
    pic_errorf(pic, "index out of range %d", i);
  }

  x = pic_substr(pic, str, 0, i);
  y = pic_make_str_fill(pic, 1, c);
  z = pic_substr(pic, str, i + 1, pic_strlen(str));

  tmp = pic_strcat(pic, x, pic_strcat(pic, y, z));

  pic_rope_incref(pic, tmp->rope);
  pic_rope_decref(pic, str->rope);
  str->rope = tmp->rope;
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
  if (to == from) {
    from = pic_substr(pic, from, 0, end);
  }

  while (start < end) {
    pic_str_set(pic, to, at++, pic_str_ref(pic, from, start++));
  }
  return pic_none_value();
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  pic_str *str;
  char c;
  int n, start, end;

  n = pic_get_args(pic, "sc|ii", &str, &c, &start, &end);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = pic_strlen(str);
  }

  while (start < end) {
    pic_str_set(pic, str, start++, c);
  }
  return pic_none_value();
}

void
pic_init_mutable_string(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin string)") {
    pic_defun(pic, "string-set!", pic_str_string_set);
    pic_defun(pic, "string-copy!", pic_str_string_copy_ip);
    pic_defun(pic, "string-fill!", pic_str_string_fill_ip);
  }
}
