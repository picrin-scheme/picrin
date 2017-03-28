/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "value.h"
#include "object.h"

pic_value
pic_make_vec(pic_state *pic, int len, pic_value *argv)
{
  struct vector *vec;
  int i;

  vec = (struct vector *)pic_obj_alloc(pic, sizeof(struct vector), PIC_TYPE_VECTOR);
  vec->len = len;
  vec->data = (pic_value *)pic_malloc(pic, sizeof(pic_value) * len);
  if (argv == NULL) {
    for (i = 0; i < len; ++i) {
      vec->data[i] = pic_undef_value(pic);
    }
  } else {
    memcpy(vec->data, argv, sizeof(pic_value) * len);
  }
  return obj_value(vec);
}

pic_value
pic_vec_ref(pic_state *PIC_UNUSED(pic), pic_value vec, int k)
{
  return pic_vec_ptr(pic, vec)->data[k];
}

void
pic_vec_set(pic_state *PIC_UNUSED(pic), pic_value vec, int k, pic_value val)
{
  pic_vec_ptr(pic, vec)->data[k] = val;
}

int
pic_vec_len(pic_state *PIC_UNUSED(pic), pic_value vec)
{
  return pic_vec_ptr(pic, vec)->len;
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
  int argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return pic_make_vec(pic, argc, argv);
}

static pic_value
pic_vec_make_vector(pic_state *pic)
{
  pic_value vec, init;
  int n, k, i;

  n = pic_get_args(pic, "i|o", &k, &init);

  if (k < 0) {
    pic_error(pic, "make-vector: negative length given", 1, pic_int_value(pic, k));
  }

  vec = pic_make_vec(pic, k, NULL);
  if (n == 2) {
    for (i = 0; i < k; ++i) {
      pic_vec_set(pic, vec, i, init);
    }
  }
  return vec;
}

static pic_value
pic_vec_vector_length(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "v", &v);

  return pic_int_value(pic, pic_vec_len(pic, v));
}

static pic_value
pic_vec_vector_ref(pic_state *pic)
{
  pic_value v;
  int k;

  pic_get_args(pic, "vi", &v, &k);

  VALID_INDEX(pic, pic_vec_len(pic, v), k);

  return pic_vec_ref(pic, v, k);
}

static pic_value
pic_vec_vector_set(pic_state *pic)
{
  pic_value v, o;
  int k;

  pic_get_args(pic, "vio", &v, &k, &o);

  VALID_INDEX(pic, pic_vec_len(pic, v), k);

  pic_vec_set(pic, v, k, o);

  return pic_undef_value(pic);
}

static pic_value
pic_vec_vector_copy_i(pic_state *pic)
{
  pic_value to, from;
  int n, at, start, end, tolen, fromlen;

  n = pic_get_args(pic, "viv|ii", &to, &at, &from, &start, &end);

  tolen = pic_vec_len(pic, to);
  fromlen = pic_vec_len(pic, from);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = fromlen;
  }

  VALID_ATRANGE(pic, tolen, at, fromlen, start, end);

  memmove(pic_vec_ptr(pic, to)->data + at, pic_vec_ptr(pic, from)->data + start, sizeof(pic_value) * (end - start));

  return pic_undef_value(pic);
}

static pic_value
pic_vec_vector_copy(pic_state *pic)
{
  pic_value from;
  int n, start, end, fromlen;

  n = pic_get_args(pic, "v|ii", &from, &start, &end);

  fromlen = pic_vec_len(pic, from);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = fromlen;
  }

  VALID_RANGE(pic, fromlen, start, end);

  return pic_make_vec(pic, end - start, pic_vec_ptr(pic, from)->data + start);
}

static pic_value
pic_vec_vector_append(pic_state *pic)
{
  pic_value *argv, vec;
  int argc, i, len;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], vec);
    len += pic_vec_len(pic, argv[i]);
  }

  vec = pic_make_vec(pic, len, NULL);

  len = 0;
  for (i = 0; i < argc; ++i) {
    int l = pic_vec_len(pic, argv[i]);
    memcpy(pic_vec_ptr(pic, vec)->data + len, pic_vec_ptr(pic, argv[i])->data, sizeof(pic_value) * l);
    len += l;
  }

  return vec;
}

static pic_value
pic_vec_vector_fill_i(pic_state *pic)
{
  pic_value vec, obj;
  int n, start, end, len;

  n = pic_get_args(pic, "vo|ii", &vec, &obj, &start, &end);

  len = pic_vec_len(pic, vec);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  while (start < end) {
    pic_vec_set(pic, vec, start++, obj);
  }

  return pic_undef_value(pic);
}

static pic_value
pic_vec_vector_map(pic_state *pic)
{
  int argc, i, len, j;
  pic_value proc, *argv, vec, vals;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_error(pic, "vector-map: wrong number of arguments (1 for at least 2)", 0);
  }

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    int l;
    TYPE_CHECK(pic, argv[i], vec);
    l = pic_vec_len(pic, argv[i]);
    len = len < l ? len : l;
  }

  vec = pic_make_vec(pic, len, NULL);

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ref(pic, argv[j], i), vals);
    }
    vals = pic_reverse(pic, vals);
    pic_vec_set(pic, vec, i, pic_funcall(pic, "picrin.base", "apply", 2, proc, vals));
  }

  return vec;
}

static pic_value
pic_vec_vector_for_each(pic_state *pic)
{
  int argc, i, len, j;
  pic_value proc, *argv, vals;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_error(pic, "vector-for-each: wrong number of arguments (1 for at least 2)", 0);
  }

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    int l;
    TYPE_CHECK(pic, argv[i], vec);
    l = pic_vec_len(pic, argv[i]);
    len = len < l ? len : l;
  }

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_vec_ref(pic, argv[j], i), vals);
    }
    vals = pic_reverse(pic, vals);
    pic_funcall(pic, "picrin.base", "apply", 2, proc, vals);
  }

  return pic_undef_value(pic);
}

static pic_value
pic_vec_list_to_vector(pic_state *pic)
{
  pic_value list, vec, e, it;
  int len, i = 0;

  pic_get_args(pic, "o", &list);

  len = pic_length(pic, list);

  vec = pic_make_vec(pic, len, NULL);
  pic_for_each (e, list, it) {
    pic_vec_set(pic, vec, i++, e);
  }
  return vec;
}

static pic_value
pic_vec_vector_to_list(pic_state *pic)
{
  pic_value vec;
  pic_value list;
  int n, start, end, i, len;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

  len = pic_vec_len(pic, vec);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  list = pic_nil_value(pic);
  for (i = start; i < end; ++i) {
    pic_push(pic, pic_vec_ref(pic, vec, i), list);
  }
  return pic_reverse(pic, list);
}

static pic_value
pic_vec_vector_to_string(pic_state *pic)
{
  pic_value vec, t;
  char *buf;
  int n, start, end, i, len;

  n = pic_get_args(pic, "v|ii", &vec, &start, &end);

  len = pic_vec_len(pic, vec);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  buf = pic_alloca(pic, end - start);
  for (i = start; i < end; ++i) {
    t = pic_vec_ref(pic, vec, i);

    TYPE_CHECK(pic, t, char);

    buf[i - start] = pic_char(pic, t);
  }

  return pic_str_value(pic, buf, end - start);
}

static pic_value
pic_vec_string_to_vector(pic_state *pic)
{
  pic_value str, vec;
  int n, start, end, len, i;
  const char *cstr;

  n = pic_get_args(pic, "s|ii", &str, &start, &end);

  cstr = pic_str(pic, str, &len);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  vec = pic_make_vec(pic, end - start, NULL);

  for (i = 0; i < end - start; ++i) {
    pic_vec_set(pic, vec, i, pic_char_value(pic, cstr[i + start]));
  }
  return vec;
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
