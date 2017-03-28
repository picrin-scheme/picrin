/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "value.h"
#include "object.h"

pic_value
pic_blob_value(pic_state *pic, const unsigned char *buf, int len)
{
  struct blob *bv;

  bv = (struct blob *)pic_obj_alloc(pic, sizeof(struct blob), PIC_TYPE_BLOB);
  bv->data = pic_malloc(pic, len);
  bv->len = len;
  if (buf) {
    memcpy(bv->data, buf, len);
  }
  return obj_value(bv);
}

unsigned char *
pic_blob(pic_state *PIC_UNUSED(pic), pic_value blob, int *len)
{
  if (len) {
    *len = pic_blob_ptr(pic, blob)->len;
  }
  return pic_blob_ptr(pic, blob)->data;
}

static pic_value
pic_blob_bytevector_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_blob_p(pic, v));
}

static pic_value
pic_blob_bytevector(pic_state *pic)
{
  pic_value *argv, blob;
  int argc, i;
  unsigned char *data;

  pic_get_args(pic, "*", &argc, &argv);

  blob = pic_blob_value(pic, 0, argc);

  data = pic_blob(pic, blob, NULL);

  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], int);

    if (pic_int(pic, argv[i]) < 0 || pic_int(pic, argv[i]) > 255) {
      pic_error(pic, "byte out of range", 0);
    }

    *data++ = (unsigned char)pic_int(pic, argv[i]);
  }

  return blob;
}

static pic_value
pic_blob_make_bytevector(pic_state *pic)
{
  pic_value blob;
  int k, b = 0;

  pic_get_args(pic, "i|i", &k, &b);

  if (b < 0 || b > 255)
    pic_error(pic, "byte out of range", 0);

  if (k < 0) {
    pic_error(pic, "make-bytevector: negative length given", 1, pic_int_value(pic, k));
  }

  blob = pic_blob_value(pic, 0, k);

  memset(pic_blob(pic, blob, NULL), (unsigned char)b, k);

  return blob;
}

static pic_value
pic_blob_bytevector_length(pic_state *pic)
{
  int len;

  pic_get_args(pic, "b", NULL, &len);

  return pic_int_value(pic, len);
}

static pic_value
pic_blob_bytevector_u8_ref(pic_state *pic)
{
  unsigned char *buf;
  int len, k;

  pic_get_args(pic, "bi", &buf, &len, &k);

  VALID_INDEX(pic, len, k);

  return pic_int_value(pic, buf[k]);
}

static pic_value
pic_blob_bytevector_u8_set(pic_state *pic)
{
  unsigned char *buf;
  int len, k, v;

  pic_get_args(pic, "bii", &buf, &len, &k, &v);

  if (v < 0 || v > 255)
    pic_error(pic, "byte out of range", 0);

  VALID_INDEX(pic, len, k);

  buf[k] = (unsigned char)v;

  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy_i(pic_state *pic)
{
  unsigned char *to, *from;
  int n, at, start, end, tolen, fromlen;

  n = pic_get_args(pic, "bib|ii", &to, &tolen, &at, &from, &fromlen, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = fromlen;
  }

  VALID_ATRANGE(pic, tolen, at, fromlen, start, end);

  memmove(to + at, from + start, end - start);

  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy(pic_state *pic)
{
  unsigned char *buf;
  int n, start, end, len;

  n = pic_get_args(pic, "b|ii", &buf, &len, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  return pic_blob_value(pic, buf + start, end - start);
}

static pic_value
pic_blob_bytevector_append(pic_state *pic)
{
  int argc, i, l, len;
  unsigned char *buf, *dst;
  pic_value *argv, blob;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], blob);
    pic_blob(pic, argv[i], &l);
    len += l;
  }

  blob = pic_blob_value(pic, NULL, len);

  dst = pic_blob(pic, blob, NULL);
  len = 0;
  for (i = 0; i < argc; ++i) {
    buf = pic_blob(pic, argv[i], &l);
    memcpy(dst + len, buf, l);
    len += l;
  }

  return blob;
}

static pic_value
pic_blob_list_to_bytevector(pic_state *pic)
{
  pic_value blob;
  unsigned char *data;
  pic_value list, e, it;

  pic_get_args(pic, "o", &list);

  blob = pic_blob_value(pic, 0, pic_length(pic, list));

  data = pic_blob(pic, blob, NULL);

  pic_for_each (e, list, it) {
    TYPE_CHECK(pic, e, int);

    if (pic_int(pic, e) < 0 || pic_int(pic, e) > 255)
      pic_error(pic, "byte out of range", 0);

    *data++ = (unsigned char)pic_int(pic, e);
  }
  return blob;
}

static pic_value
pic_blob_bytevector_to_list(pic_state *pic)
{
  pic_value list;
  unsigned char *buf;
  int n, len, start, end, i;

  n = pic_get_args(pic, "b|ii", &buf, &len, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  list = pic_nil_value(pic);
  for (i = start; i < end; ++i) {
    pic_push(pic, pic_int_value(pic, buf[i]), list);
  }
  return pic_reverse(pic, list);
}

void
pic_init_blob(pic_state *pic)
{
  pic_defun(pic, "bytevector?", pic_blob_bytevector_p);
  pic_defun(pic, "bytevector", pic_blob_bytevector);
  pic_defun(pic, "make-bytevector", pic_blob_make_bytevector);
  pic_defun(pic, "bytevector-length", pic_blob_bytevector_length);
  pic_defun(pic, "bytevector-u8-ref", pic_blob_bytevector_u8_ref);
  pic_defun(pic, "bytevector-u8-set!", pic_blob_bytevector_u8_set);
  pic_defun(pic, "bytevector-copy!", pic_blob_bytevector_copy_i);
  pic_defun(pic, "bytevector-copy", pic_blob_bytevector_copy);
  pic_defun(pic, "bytevector-append", pic_blob_bytevector_append);
  pic_defun(pic, "bytevector->list", pic_blob_bytevector_to_list);
  pic_defun(pic, "list->bytevector", pic_blob_list_to_bytevector);
}
