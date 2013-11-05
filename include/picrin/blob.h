#ifndef BLOB_H__
#define BLOB_H__

struct pic_blob {
  PIC_OBJECT_HEADER
  char *data;
  int len;
};

#define pic_blob_p(v) (pic_type(v) == PIC_TT_BLOB)
#define pic_blob_ptr(v) ((struct pic_blob *)(v).u.data)

struct pic_blob *pic_blob_new(pic_state *, char *, int len);

#endif
