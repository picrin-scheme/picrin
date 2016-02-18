#include "picrin.h"

void
pic_str_set(pic_state *pic, struct pic_string *str, int i, char c)
{
  struct pic_string *x, *y, *z, *tmp;
  char buf[1];

  if (pic_str_len(str) <= i) {
    pic_errorf(pic, "index out of range %d", i);
  }

  buf[0] = c;

  x = pic_str_sub(pic, str, 0, i);
  y = pic_make_str(pic, buf, 1);
  z = pic_str_sub(pic, str, i + 1, pic_str_len(str));

  tmp = pic_str_cat(pic, x, pic_str_cat(pic, y, z));

  pic_rope_incref(pic, tmp->rope);
  pic_rope_decref(pic, str->rope);
  str->rope = tmp->rope;
}

static pic_value
pic_str_string_set(pic_state *pic)
{
  struct pic_string *str;
  char c;
  int k;

  pic_get_args(pic, "sic", &str, &k, &c);

  pic_str_set(pic, str, k, c);
  return pic_undef_value();
}

static pic_value
pic_str_string_copy_ip(pic_state *pic)
{
  struct pic_string *to, *from;
  int n, at, start, end;

  n = pic_get_args(pic, "sis|ii", &to, &at, &from, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = pic_str_len(from);
  }
  if (to == from) {
    from = pic_str_sub(pic, from, 0, end);
  }

  while (start < end) {
    pic_str_set(pic, to, at++, pic_str_ref(pic, from, start++));
  }
  return pic_undef_value();
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  struct pic_string *str;
  char c;
  int n, start, end;

  n = pic_get_args(pic, "sc|ii", &str, &c, &start, &end);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = pic_str_len(str);
  }

  while (start < end) {
    pic_str_set(pic, str, start++, c);
  }
  return pic_undef_value();
}

void
pic_init_mutable_string(pic_state *pic)
{
  pic_deflibrary(pic, "picrin.string");

  pic_defun(pic, "string-set!", pic_str_string_set);
  pic_defun(pic, "string-copy!", pic_str_string_copy_ip);
  pic_defun(pic, "string-fill!", pic_str_string_fill_ip);
}
