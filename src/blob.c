#include <string.h>

#include "picrin.h"
#include "picrin/blob.h"

struct pic_blob *
pic_blob_new(pic_state *pic, char *dat, int len)
{
  struct pic_blob *bv;

  bv = (struct pic_blob *)pic_obj_alloc(pic, sizeof(struct pic_blob), PIC_TT_BLOB);
  bv->data = strndup(dat, len);
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
  int k, b = 0, i;
  char *dat;

  pic_get_args(pic, "i|i", &k, &b);

  if (b < 0 || b > 255)
    pic_error(pic, "byte out of range");

  dat = pic_alloc(pic, k);
  for (i = 0; i < k; ++i) {
    dat[i] = b;
  }

  return pic_obj_value(pic_blob_new(pic, dat, k));
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

void
pic_init_blob(pic_state *pic)
{
  pic_defun(pic, "bytevector?", pic_blob_bytevector_p);
  pic_defun(pic, "make-bytevector", pic_blob_make_bytevector);
  pic_defun(pic, "bytevector-length", pic_blob_bytevector_length);
  pic_defun(pic, "bytevector-u8-ref", pic_blob_bytevector_u8_ref);
  pic_defun(pic, "bytevector-u8-set!", pic_blob_bytevector_u8_set);
}
