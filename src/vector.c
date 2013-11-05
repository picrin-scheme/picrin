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

static pic_value
pic_vec_vector_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_vec_p(v));
}

static pic_value
pic_vec_make_vector(pic_state *pic)
{
  pic_value v;
  int k, n, i;
  struct pic_vector *vec;

  n = pic_get_args(pic, "i|o", &k, &v);

  vec = pic_vec_new(pic, k);
  if (n == 3) {
    for (i = 0; i < k; ++i) {
      vec->data[i] = v;
    }
  }
  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_length(pic_state *pic)
{
  struct pic_vector *v;

  pic_get_args(pic, "v", &v);

  return pic_int_value(v->len);
}

static pic_value
pic_vec_vector_ref(pic_state *pic)
{
  struct pic_vector *v;
  int k;

  pic_get_args(pic, "vi", &v, &k);

  return v->data[k];
}

void
pic_init_vector(pic_state *pic)
{
  pic_defun(pic, "vector?", pic_vec_vector_p);
  pic_defun(pic, "make-vector", pic_vec_make_vector);
  pic_defun(pic, "vector-length", pic_vec_vector_length);
  pic_defun(pic, "vector-ref", pic_vec_vector_ref);
}
