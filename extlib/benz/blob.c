/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_blob *
pic_make_blob(pic_state *pic, int len)
{
  struct pic_blob *bv;

  bv = (struct pic_blob *)pic_obj_alloc(pic, sizeof(struct pic_blob), PIC_TYPE_BLOB);
  bv->data = pic_malloc(pic, len);
  bv->len = len;
  return bv;
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
  pic_value *argv;
  int argc, i;
  struct pic_blob *blob;
  unsigned char *data;

  pic_get_args(pic, "*", &argc, &argv);

  blob = pic_make_blob(pic, argc);

  data = blob->data;

  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], int);

    if (pic_int(pic, argv[i]) < 0 || pic_int(pic, argv[i]) > 255) {
      pic_errorf(pic, "byte out of range");
    }

    *data++ = (unsigned char)pic_int(pic, argv[i]);
  }

  return pic_obj_value(blob);
}

static pic_value
pic_blob_make_bytevector(pic_state *pic)
{
  struct pic_blob *blob;
  int k, i, b = 0;

  pic_get_args(pic, "i|i", &k, &b);

  if (b < 0 || b > 255)
    pic_errorf(pic, "byte out of range");

  blob = pic_make_blob(pic, k);
  for (i = 0; i < k; ++i) {
    blob->data[i] = (unsigned char)b;
  }

  return pic_obj_value(blob);
}

static pic_value
pic_blob_bytevector_length(pic_state *pic)
{
  struct pic_blob *bv;

  pic_get_args(pic, "b", &bv);

  return pic_int_value(pic, bv->len);
}

static pic_value
pic_blob_bytevector_u8_ref(pic_state *pic)
{
  struct pic_blob *bv;
  int k;

  pic_get_args(pic, "bi", &bv, &k);

  return pic_int_value(pic, bv->data[k]);
}

static pic_value
pic_blob_bytevector_u8_set(pic_state *pic)
{
  struct pic_blob *bv;
  int k, v;

  pic_get_args(pic, "bii", &bv, &k, &v);

  if (v < 0 || v > 255)
    pic_errorf(pic, "byte out of range");

  bv->data[k] = (unsigned char)v;
  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy_i(pic_state *pic)
{
  struct pic_blob *to, *from;
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
    return pic_undef_value(pic);
  }

  while (start < end) {
    to->data[at++] = from->data[start++];
  }

  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy(pic_state *pic)
{
  struct pic_blob *from, *to;
  int n, start, end, i = 0;

  n = pic_get_args(pic, "b|ii", &from, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = from->len;
  }

  if (end < start) {
    pic_errorf(pic, "make-bytevector: end index must not be less than start index");
  }

  to = pic_make_blob(pic, end - start);
  while (start < end) {
    to->data[i++] = from->data[start++];
  }

  return pic_obj_value(to);
}

static pic_value
pic_blob_bytevector_append(pic_state *pic)
{
  int argc, i, j, len;
  pic_value *argv;
  struct pic_blob *blob;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], blob);
    len += pic_blob_ptr(argv[i])->len;
  }

  blob = pic_make_blob(pic, len);

  len = 0;
  for (i = 0; i < argc; ++i) {
    for (j = 0; j < pic_blob_ptr(argv[i])->len; ++j) {
      blob->data[len + j] = pic_blob_ptr(argv[i])->data[j];
    }
    len += pic_blob_ptr(argv[i])->len;
  }

  return pic_obj_value(blob);
}

static pic_value
pic_blob_list_to_bytevector(pic_state *pic)
{
  struct pic_blob *blob;
  unsigned char *data;
  pic_value list, e, it;

  pic_get_args(pic, "o", &list);

  blob = pic_make_blob(pic, pic_length(pic, list));

  data = blob->data;

  pic_for_each (e, list, it) {
    pic_assert_type(pic, e, int);

    if (pic_int(pic, e) < 0 || pic_int(pic, e) > 255)
      pic_errorf(pic, "byte out of range");

    *data++ = (unsigned char)pic_int(pic, e);
  }
  return pic_obj_value(blob);
}

static pic_value
pic_blob_bytevector_to_list(pic_state *pic)
{
  struct pic_blob *blob;
  pic_value list;
  int n, start, end, i;

  n = pic_get_args(pic, "b|ii", &blob, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = blob->len;
  }

  list = pic_nil_value(pic);

  for (i = start; i < end; ++i) {
    pic_push(pic, pic_int_value(pic, blob->data[i]), list);
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
