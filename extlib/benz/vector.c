/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_vector *
pic_make_vec(pic_state *pic, int len)
{
  struct pic_vector *vec;
  int i;

  vec = (struct pic_vector *)pic_obj_alloc(pic, sizeof(struct pic_vector), PIC_TYPE_VECTOR);
  vec->len = len;
  vec->data = (pic_value *)pic_malloc(pic, sizeof(pic_value) * len);
  for (i = 0; i < len; ++i) {
    vec->data[i] = pic_undef_value(pic);
  }
  return vec;
}

static pic_value
pic_vec_vector_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_vec_p(pic, v));
}

static pic_value
pic_vec_vector(pic_state *pic)
{
  int argc, i;
  pic_value *argv;
  pic_vec *vec;

  pic_get_args(pic, "*", &argc, &argv);

  vec = pic_make_vec(pic, argc);

  for (i = 0; i < argc; ++i) {
    vec->data[i] = argv[i];
  }

  return pic_obj_value(vec);
}

static pic_value
pic_vec_make_vector(pic_state *pic)
{
  pic_value v;
  int n, k, i;
  struct pic_vector *vec;

  n = pic_get_args(pic, "i|o", &k, &v);

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

  return pic_int_value(pic, v->len);
}

static pic_value
pic_vec_vector_ref(pic_state *pic)
{
  struct pic_vector *v;
  int k;

  pic_get_args(pic, "vi", &v, &k);

  if (v->len <= k) {
    pic_errorf(pic, "vector-ref: index out of range");
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

  if (v->len <= k) {
    pic_errorf(pic, "vector-set!: index out of range");
  }
  v->data[k] = o;
  return pic_undef_value(pic);
}

static pic_value
pic_vec_vector_copy_i(pic_state *pic)
{
  pic_vec *to, *from;
  int n, at, start, end;

  n = pic_get_args(pic, "viv|ii", &to, &at, &from, &start, &end);

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
pic_vec_vector_copy(pic_state *pic)
{
  pic_vec *vec, *to;
  int n, start, end, i = 0;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

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
  int argc, i, j, len;
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
  int n, start, end;

  n = pic_get_args(pic, "vo|ii", &vec, &obj, &start, &end);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = vec->len;
  }

  while (start < end) {
    vec->data[start++] = obj;
  }

  return pic_undef_value(pic);
}

static pic_value
pic_vec_vector_map(pic_state *pic)
{
  struct pic_proc *proc;
  int argc, i, len, j;
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
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ptr(argv[j])->data[i], vals);
    }
    vec->data[i] = pic_funcall(pic, "picrin.base", "apply", 2, pic_obj_value(proc), vals);
  }

  return pic_obj_value(vec);
}

static pic_value
pic_vec_vector_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  int argc, i, len, j;
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
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ptr(argv[j])->data[i], vals);
    }
    pic_funcall(pic, "picrin.base", "apply", 2, pic_obj_value(proc), vals);
  }

  return pic_undef_value(pic);
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
  int n, start, end, i;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = vec->len;
  }

  list = pic_nil_value(pic);

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
  int n, start, end, i;
  struct pic_string *str;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = vec->len;
  }

  if (end < start) {
    pic_errorf(pic, "vector->string: end index must not be less than start index");
  }

  buf = pic_malloc(pic, end - start);

  for (i = start; i < end; ++i) {
    pic_assert_type(pic, vec->data[i], char);

    buf[i - start] = pic_char(pic, vec->data[i]);
  }

  str = pic_str_value(pic, buf, end - start);
  pic_free(pic, buf);

  return pic_obj_value(str);
}

static pic_value
pic_vec_string_to_vector(pic_state *pic)
{
  struct pic_string *str;
  int n, start, end, i;
  pic_vec *vec;

  n = pic_get_args(pic, "s|ii", &str, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_str_len(pic, str);
  }

  if (end < start) {
    pic_errorf(pic, "string->vector: end index must not be less than start index");
  }

  vec = pic_make_vec(pic, end - start);

  for (i = 0; i < end - start; ++i) {
    vec->data[i] = pic_char_value(pic, pic_str_ref(pic, str, i + start));
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
