/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/vector.h"
#include "picrin/string.h"
#include "picrin/pair.h"

struct pic_vector *
pic_make_vec(pic_state *pic, size_t len)
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
pic_make_vec_from_list(pic_state *pic, pic_value data)
{
  struct pic_vector *vec;
  size_t len, i;

  len = pic_length(pic, data);

  vec = pic_make_vec(pic, len);
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
pic_vec_vector(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;
  pic_vec *vec;

  pic_get_args(pic, "*", &argc, &argv);

  vec = pic_make_vec(pic, (size_t)argc);

  for (i = 0; i < argc; ++i) {
    vec->data[i] = argv[i];
  }

  return pic_obj_value(vec);
}

static pic_value
pic_vec_make_vector(pic_state *pic)
{
  pic_value v;
  int n;
  size_t k, i;
  struct pic_vector *vec;

  n = pic_get_args(pic, "k|o", &k, &v);

  vec = pic_make_vec(pic, k);
  if (n == 2) {
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

  return pic_size_value(v->len);
}

static pic_value
pic_vec_vector_ref(pic_state *pic)
{
  struct pic_vector *v;
  size_t k;

  pic_get_args(pic, "vk", &v, &k);

  if (v->len <= k) {
    pic_errorf(pic, "vector-ref: index out of range");
  }
  return v->data[k];
}

static pic_value
pic_vec_vector_set(pic_state *pic)
{
  struct pic_vector *v;
  size_t k;
  pic_value o;

  pic_get_args(pic, "vko", &v, &k, &o);

  if (v->len <= k) {
    pic_errorf(pic, "vector-set!: index out of range");
  }
  v->data[k] = o;
  return pic_none_value();
}

static pic_value
pic_vec_vector_copy_i(pic_state *pic)
{
  pic_vec *to, *from;
  int n;
  size_t at, start, end;

  n = pic_get_args(pic, "vkv|kk", &to, &at, &from, &start, &end);

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
pic_vec_vector_copy(pic_state *pic)
{
  pic_vec *vec, *to;
  int n;
  size_t start, end, i = 0;

  n = pic_get_args(pic, "v|kk", &vec, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = vec->len;
  }

  if (end < start) {
    pic_errorf(pic, "vector-copy: end index must not be less than start index");
  }

  to = pic_make_vec(pic, end - start);
  while (start < end) {
    to->data[i++] = vec->data[start++];
  }

  return pic_obj_value(to);
}

static pic_value
pic_vec_vector_append(pic_state *pic)
{
  pic_value *argv;
  size_t argc, i, j, len;
  pic_vec *vec;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], vec);
    len += pic_vec_ptr(argv[i])->len;
  }

  vec = pic_make_vec(pic, len);

  len = 0;
  for (i = 0; i < argc; ++i) {
    for (j = 0; j < pic_vec_ptr(argv[i])->len; ++j) {
      vec->data[len + j] = pic_vec_ptr(argv[i])->data[j];
    }
    len += pic_vec_ptr(argv[i])->len;
  }

  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_fill_i(pic_state *pic)
{
  pic_vec *vec;
  pic_value obj;
  int n;
  size_t start, end;

  n = pic_get_args(pic, "vo|kk", &vec, &obj, &start, &end);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = vec->len;
  }

  while (start < end) {
    vec->data[start++] = obj;
  }

  return pic_none_value();
}

static pic_value
pic_vec_vector_map(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i, len, j;
  pic_value *argv, vals;
  pic_vec *vec;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], vec);

    len = len < pic_vec_ptr(argv[i])->len
      ? len
      : pic_vec_ptr(argv[i])->len;
  }

  vec = pic_make_vec(pic, len);

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value();
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ptr(argv[j])->data[i], vals);
    }
    vec->data[i] = pic_apply(pic, proc, vals);
  }

  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i, len, j;
  pic_value *argv, vals;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], vec);

    len = len < pic_vec_ptr(argv[i])->len
      ? len
      : pic_vec_ptr(argv[i])->len;
  }

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value();
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ptr(argv[j])->data[i], vals);
    }
    pic_apply(pic, proc, vals);
  }

  return pic_none_value();
}

static pic_value
pic_vec_list_to_vector(pic_state *pic)
{
  struct pic_vector *vec;
  pic_value list, e, it, *data;

  pic_get_args(pic, "o", &list);

  vec = pic_make_vec(pic, pic_length(pic, list));

  data = vec->data;

  pic_for_each (e, list, it) {
    *data++ = e;
  }
  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_to_list(pic_state *pic)
{
  struct pic_vector *vec;
  pic_value list;
  int n;
  size_t start, end, i;

  n = pic_get_args(pic, "v|kk", &vec, &start, &end);

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

static pic_value
pic_vec_vector_to_string(pic_state *pic)
{
  pic_vec *vec;
  char *buf;
  int n;
  size_t start, end, i;
  pic_str *str;

  n = pic_get_args(pic, "v|kk", &vec, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = vec->len;
  }

  if (end < start) {
    pic_errorf(pic, "vector->string: end index must not be less than start index");
  }

  buf = pic_alloc(pic, end - start);

  for (i = start; i < end; ++i) {
    pic_assert_type(pic, vec->data[i], char);

    buf[i - start] = pic_char(vec->data[i]);
  }

  str = pic_make_str(pic, buf, end - start);
  pic_free(pic, buf);

  return pic_obj_value(str);
}

static pic_value
pic_vec_string_to_vector(pic_state *pic)
{
  pic_str *str;
  int n;
  size_t start, end;
  size_t i;
  pic_vec *vec;

  n = pic_get_args(pic, "s|kk", &str, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_str_len(str);
  }

  if (end < start) {
    pic_errorf(pic, "string->vector: end index must not be less than start index");
  }

  vec = pic_make_vec(pic, end - start);

  for (i = 0; i < end - start; ++i) {
    vec->data[i] = pic_char_value(pic_str_ref(pic, str, i + start));
  }
  return pic_obj_value(vec);
}

void
pic_init_vector(pic_state *pic)
{
  pic_defun(pic, "vector?", pic_vec_vector_p);
  pic_defun(pic, "vector", pic_vec_vector);
  pic_defun(pic, "make-vector", pic_vec_make_vector);
  pic_defun(pic, "vector-length", pic_vec_vector_length);
  pic_defun(pic, "vector-ref", pic_vec_vector_ref);
  pic_defun(pic, "vector-set!", pic_vec_vector_set);
  pic_defun(pic, "vector-copy!", pic_vec_vector_copy_i);
  pic_defun(pic, "vector-copy", pic_vec_vector_copy);
  pic_defun(pic, "vector-append", pic_vec_vector_append);
  pic_defun(pic, "vector-fill!", pic_vec_vector_fill_i);
  pic_defun(pic, "vector-map", pic_vec_vector_map);
  pic_defun(pic, "vector-for-each", pic_vec_vector_for_each);
  pic_defun(pic, "list->vector", pic_vec_list_to_vector);
  pic_defun(pic, "vector->list", pic_vec_vector_to_list);
  pic_defun(pic, "string->vector", pic_vec_string_to_vector);
  pic_defun(pic, "vector->string", pic_vec_vector_to_string);
}
