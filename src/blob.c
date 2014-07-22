/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/blob.h"

char *
pic_strndup(pic_state *pic, const char *s, size_t n)
{
  char *r;

  r = pic_alloc(pic, n + 1);
  memcpy(r, s, n);
  r[n] = '\0';
  return r;
}

char *
pic_strdup(pic_state *pic, const char *s)
{
  return pic_strndup(pic, s, strlen(s));
}

struct pic_blob *
pic_blob_new(pic_state *pic, size_t len)
{
  struct pic_blob *bv;

  bv = (struct pic_blob *)pic_obj_alloc(pic, sizeof(struct pic_blob), PIC_TT_BLOB);
  bv->data = pic_alloc(pic, len);
  bv->len = len;
  return bv;
}

static pic_value
pic_blob_bytevector_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_blob_p(v));
}

static pic_value
pic_blob_make_bytevector(pic_state *pic)
{
  pic_blob *blob;
  int k, b = 0, i;

  pic_get_args(pic, "i|i", &k, &b);

  if (b < 0 || b > 255)
    pic_error(pic, "byte out of range");

  blob = pic_blob_new(pic, k);
  for (i = 0; i < k; ++i) {
    blob->data[i] = b;
  }

  return pic_obj_value(blob);
}

static pic_value
pic_blob_bytevector_length(pic_state *pic)
{
  struct pic_blob *bv;

  pic_get_args(pic, "b", &bv);

  return pic_int_value(bv->len);
}

static pic_value
pic_blob_bytevector_u8_ref(pic_state *pic)
{
  struct pic_blob *bv;
  int k;

  pic_get_args(pic, "bi", &bv, &k);

  return pic_int_value(bv->data[k]);
}

static pic_value
pic_blob_bytevector_u8_set(pic_state *pic)
{
  struct pic_blob *bv;
  int k, v;

  pic_get_args(pic, "bii", &bv, &k, &v);

  if (v < 0 || v > 255)
    pic_error(pic, "byte out of range");

  bv->data[k] = v;
  return pic_none_value();
}

static pic_value
pic_blob_bytevector_copy_i(pic_state *pic)
{
  pic_blob *to, *from;
  int n, at, start, end;

  n = pic_get_args(pic, "bib|ii", &to, &at, &from, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = from->len;
  }

  if (to == from && (start <= at && at < end)) {
    /* copy in reversed order */
    at += end - start;
    while (start < end) {
      to->data[--at] = from->data[--end];
    }
    return pic_none_value();
  }

  while (start < end) {
    to->data[at++] = from->data[start++];
  }

  return pic_none_value();
}

static pic_value
pic_blob_bytevector_copy(pic_state *pic)
{
  pic_blob *from, *to;
  int n, start, end, i = 0;

  n = pic_get_args(pic, "b|ii", &from, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = from->len;
  }

  to = pic_blob_new(pic, end - start);
  while (start < end) {
    to->data[i++] = from->data[start++];
  }

  return pic_obj_value(to);
}

void
pic_init_blob(pic_state *pic)
{
  pic_defun(pic, "bytevector?", pic_blob_bytevector_p);
  pic_defun(pic, "make-bytevector", pic_blob_make_bytevector);
  pic_defun(pic, "bytevector-length", pic_blob_bytevector_length);
  pic_defun(pic, "bytevector-u8-ref", pic_blob_bytevector_u8_ref);
  pic_defun(pic, "bytevector-u8-set!", pic_blob_bytevector_u8_set);
  pic_defun(pic, "bytevector-copy!", pic_blob_bytevector_copy_i);
  pic_defun(pic, "bytevector-copy", pic_blob_bytevector_copy);
}
