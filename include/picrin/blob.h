/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_BLOB_H__
#define PICRIN_BLOB_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_blob {
  PIC_OBJECT_HEADER
  char *data;
  size_t len;
};

#define pic_blob_p(v) (pic_type(v) == PIC_TT_BLOB)
#define pic_blob_ptr(v) ((struct pic_blob *)pic_ptr(v))

struct pic_blob *pic_blob_new(pic_state *, size_t);

#if defined(__cplusplus)
}
#endif

#endif
