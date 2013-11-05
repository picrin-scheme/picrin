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
