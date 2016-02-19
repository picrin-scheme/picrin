#include "picrin.h"
#include "picrin/object.h"

void
pic_str_update(pic_state *pic, pic_value dst, pic_value src)
{
  pic_rope_incref(pic, pic_str_ptr(pic, src)->rope);
  pic_rope_decref(pic, pic_str_ptr(pic, dst)->rope);
  pic_str_ptr(pic, dst)->rope = pic_str_ptr(pic, src)->rope;
}

static pic_value
pic_str_string_set(pic_state *pic)
{
  pic_value str, x, y, z;
  char c;
  int k, len;

  pic_get_args(pic, "sic", &str, &k, &c);

  len = pic_str_len(pic, str);

  VALID_INDEX(pic, len, k);

  x = pic_str_sub(pic, str, 0, k);
  y = pic_str_value(pic, &c, 1);
  z = pic_str_sub(pic, str, k + 1, len);

  pic_str_update(pic, str, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

static pic_value
pic_str_string_copy_ip(pic_state *pic)
{
  pic_value to, from, x, y, z;
  int n, at, start, end, tolen, fromlen;

  n = pic_get_args(pic, "sis|ii", &to, &at, &from, &start, &end);

  tolen = pic_str_len(pic, to);
  fromlen = pic_str_len(pic, from);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = fromlen;
  }

  VALID_ATRANGE(pic, tolen, at, fromlen, start, end);

  x = pic_str_sub(pic, to, 0, at);
  y = pic_str_sub(pic, from, start, end);
  z = pic_str_sub(pic, to, at + end - start, tolen);

  pic_str_update(pic, to, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  pic_value str, x, y, z;
  char c, *buf;
  int n, start, end, len;

  n = pic_get_args(pic, "sc|ii", &str, &c, &start, &end);

  len = pic_str_len(pic, str);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  buf = pic_alloca(pic, end - start);
  memset(buf, c, end - start);

  x = pic_str_sub(pic, str, 0, start);
  y = pic_str_value(pic, buf, end - start);
  z = pic_str_sub(pic, str, end, len);

  pic_str_update(pic, str, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

void
pic_init_mutable_string(pic_state *pic)
{
  pic_deflibrary(pic, "picrin.string");

  pic_defun(pic, "string-set!", pic_str_string_set);
  pic_defun(pic, "string-copy!", pic_str_string_copy_ip);
  pic_defun(pic, "string-fill!", pic_str_string_fill_ip);
}
