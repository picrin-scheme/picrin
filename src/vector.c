/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/vector.h"
#include "picrin/pair.h"

struct pic_vector *
pic_vec_new(pic_state *pic, size_t len)
{
  struct pic_vector *vec;
  size_t i;

  vec = (struct pic_vector *)pic_obj_alloc(pic, sizeof(struct pic_vector), PIC_TT_VECTOR);
  vec->len = len;
  vec->data = (pic_value *)pic_alloc(pic, sizeof(pic_value) * len);
  for (i = 0; i < len; ++i) {
    vec->data[i] = pic_none_value();
  }
  return vec;
}

struct pic_vector *
pic_vec_new_from_list(pic_state *pic, pic_value data)
{
  struct pic_vector *vec;
  size_t i, len;

  len = pic_length(pic, data);

  vec = pic_vec_new(pic, len);
  for (i = 0; i < len; ++i) {
    vec->data[i] = pic_car(pic, data);
    data = pic_cdr(pic, data);
  }
  return vec;
}

void
pic_vec_extend_ip(pic_state *pic, struct pic_vector *vec, size_t size)
{
  size_t len, i;

  len = vec->len;
  vec->len = size;
  vec->data = (pic_value *)pic_realloc(pic, vec->data, sizeof(pic_value) * size);
  for (i = len; i < size; ++i) {
    vec->data[i] = pic_none_value();
  }
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
  int n, k;
  size_t i;
  struct pic_vector *vec;

  n = pic_get_args(pic, "i|o", &k, &v);

  vec = pic_vec_new(pic, k);
  if (n == 2) {
    for (i = 0; i < (size_t)k; ++i) {
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

  if (k < 0 || v->len <= (size_t)k) {
    pic_error(pic, "vector-ref: index out of range");
  }
  return v->data[k];
}

static pic_value
pic_vec_vector_set(pic_state *pic)
{
  struct pic_vector *v;
  int k;
  pic_value o;

  pic_get_args(pic, "vio", &v, &k, &o);

  if (k < 0 || v->len <= (size_t)k) {
    pic_error(pic, "vector-set!: index out of range");
  }
  v->data[k] = o;
  return pic_none_value();
}

static pic_value
pic_vec_list_to_vector(pic_state *pic)
{
  struct pic_vector *vec;
  pic_value list, e, *data;

  pic_get_args(pic, "o", &list);

  vec = pic_vec_new(pic, pic_length(pic, list));

  data = vec->data;

  pic_for_each (e, list) {
    *data++ = e;
  }
  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_to_list(pic_state *pic)
{
  struct pic_vector *vec;
  pic_value list;
  int n, start, end, i;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = vec->len;
  }

  list = pic_nil_value();

  for (i = start; i < end; ++i) {
    pic_push(pic, vec->data[i], list);
  }
  return pic_reverse(pic, list);
}

void
pic_init_vector(pic_state *pic)
{
  pic_defun(pic, "vector?", pic_vec_vector_p);
  pic_defun(pic, "make-vector", pic_vec_make_vector);
  pic_defun(pic, "vector-length", pic_vec_vector_length);
  pic_defun(pic, "vector-ref", pic_vec_vector_ref);
  pic_defun(pic, "vector-set!", pic_vec_vector_set);
  pic_defun(pic, "list->vector", pic_vec_list_to_vector);
  pic_defun(pic, "vector->list", pic_vec_vector_to_list);
}
