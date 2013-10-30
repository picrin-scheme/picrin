#include "picrin.h"
#include "picrin/pair.h"

struct pic_vector *
pic_vec_new(pic_state *pic, size_t len)
{
  struct pic_vector *vec;

  vec = (struct pic_vector *)pic_obj_alloc(pic, sizeof(struct pic_vector), PIC_TT_VECTOR);
  vec->len = len;
  vec->data = (pic_value *)pic_alloc(pic, sizeof(pic_value) * len);
  return vec;
}

struct pic_vector *
pic_vec_new_from_list(pic_state *pic, pic_value data)
{
  struct pic_vector *vec;
  int i, len;

  len = pic_length(pic, data);

  vec = pic_vec_new(pic, len);
  for (i = 0; i < len; ++i) {
    vec->data[i] = pic_car(pic, data);
    data = pic_cdr(pic, data);
  }
  return vec;
}
