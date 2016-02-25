/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"

struct chunk {
  const char *str;
  int refcnt;
  int len;
  char buf[1];
};

void
pic_chunk_incref(pic_state *PIC_UNUSED(pic), struct chunk *chunk)
{
  chunk->refcnt++;
}

void
pic_chunk_decref(pic_state *pic, struct chunk *chunk)
{
  if (! --chunk->refcnt) {
    pic_free(pic, chunk);
  }
}

static struct chunk *
make_chunk(pic_state *pic, const char *str, int len)
{
  struct chunk *c;

  c = pic_malloc(pic, offsetof(struct chunk, buf) + len + 1);
  c->refcnt = 1;
  c->str = c->buf;
  c->len = len;
  c->buf[len] = 0;
  if (str) {
    memcpy(c->buf, str, len);
  }
  return c;
}

static struct chunk *
make_chunk_lit(pic_state *pic, const char *str, int len)
{
  struct chunk *c;

  c = pic_malloc(pic, offsetof(struct chunk, buf));
  c->refcnt = 1;
  c->str = str;
  c->len = len;
  return c;
}

#define weight(x) ((x) ? (x)->weight : 0)

static struct rope *
make_leaf(pic_state *pic, struct chunk *chunk)
{
  struct rope *rope;

  rope = (struct rope *)pic_obj_alloc(pic, sizeof(struct rope), PIC_TYPE_LEAF);
  rope->weight = chunk->len;
  rope->u.leaf.chunk = chunk;   /* delegate ownership */
  rope->u.leaf.str = chunk->str;
  return rope;
}

static struct rope *
make_node(pic_state *pic, struct rope *left, struct rope *right)
{
  struct rope *rope;

  rope = (struct rope *)pic_obj_alloc(pic, sizeof(struct rope), PIC_TYPE_LEAF);
  rope->weight = weight(left) + weight(right);
  rope->u.node.left = left;
  rope->u.node.right = right;
  return rope;
}

static pic_value
make_str(pic_state *pic, struct rope *rope)
{
  struct string *str;

  str = (struct string *)pic_obj_alloc(pic, sizeof(struct string), PIC_TYPE_STRING);
  str->rope = rope;
  return pic_obj_value(str);
}

static struct rope *
merge(pic_state *pic, struct rope *left, struct rope *right)
{
  if (left == 0)
    return right;
  if (right == 0)
    return left;

  /* Randomized BST */

  if (rand() % (left->weight + right->weight) < left->weight) {
    return make_node(pic, left->u.node.left, make_node(pic, left->u.node.right, right));
  } else {
    return make_node(pic, make_node(pic, left, right->u.node.left), right->u.node.right);
  }
}

static void
split(pic_state *pic, struct rope *tree, int k, struct rope **left, struct rope **right)
{
  struct rope *rope;

  if (! tree) {
    if (left) *left = 0;
    if (right) *right = 0;
  } else if (tree->tt == PIC_TYPE_LEAF) {
    if (left) {
      if (k == 0) {
        *left = 0;
      } else {
        *left = make_leaf(pic, tree->u.leaf.chunk);
        (*left)->weight = k;
        pic_chunk_incref(pic, tree->u.leaf.chunk);
      }
    }
    if (right) {
      if (k == tree->weight) {
        *right = 0;
      } else {
        *right = make_leaf(pic, tree->u.leaf.chunk);
        (*right)->weight = tree->weight - k;
        (*right)->u.leaf.str += k;
        pic_chunk_incref(pic, tree->u.leaf.chunk);
      }
    }
  } else if (k < weight(tree->u.node.left)) {
    split(pic, tree->u.node.left, k, left, &rope);
    if (right) *right = make_node(pic, rope, tree->u.node.right);
  } else {
    split(pic, tree->u.node.right, k - weight(tree->u.node.left), &rope, right);
    if (left) *left = make_node(pic, tree->u.node.left, rope);
  }
}

static void
flatten(pic_state *pic, struct rope *rope, struct chunk *chunk, char *buf)
{
  if (! rope) {
    return;
  }
  if (rope->tt == PIC_TYPE_LEAF) {
    memcpy(buf, rope->u.leaf.str, rope->weight);
  } else {
    flatten(pic, rope->u.node.left, chunk, buf);
    flatten(pic, rope->u.node.right, chunk, buf + rope->u.node.left->weight);
  }

  pic_chunk_decref(pic, rope->u.leaf.chunk);
  pic_chunk_incref(pic, chunk);

  rope->tt = PIC_TYPE_LEAF;
  rope->u.leaf.chunk = chunk;
  rope->u.leaf.str = buf;
}

pic_value
pic_str_value(pic_state *pic, const char *str, int len)
{
  struct chunk *c;

  if (len > 0) {
    c = make_chunk(pic, str, len);
  } else {
    if (len == 0) {
      str = "";
    }
    c = make_chunk_lit(pic, str, -len);
  }
  return make_str(pic, make_leaf(pic, c));
}

pic_value
pic_strf_value(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value str;

  va_start(ap, fmt);
  str = pic_vstrf_value(pic, fmt, ap);
  va_end(ap);

  return str;
}

pic_value
pic_vstrf_value(pic_state *pic, const char *fmt, va_list ap)
{
  pic_value str;
  xFILE *file;
  const char *buf;
  int len;

  file = xfopen_buf(pic, NULL, 0, "w");

  xvfprintf(pic, file, fmt, ap);
  xfget_buf(pic, file, &buf, &len);
  str = pic_str_value(pic, buf, len);
  xfclose(pic, file);
  return str;
}

int
pic_str_len(pic_state *pic, pic_value str)
{
  return pic_str_ptr(pic, str)->rope->weight;
}

char
pic_str_ref(pic_state *pic, pic_value str, int i)
{
  struct rope *rope;

  rope = pic_str_ptr(pic, str)->rope;

  while (i < rope->weight) {
    if (rope->tt == PIC_TYPE_LEAF) {
      return rope->u.leaf.str[i];
    }
    if (i < rope->u.node.left->weight) {
      rope = rope->u.node.left;
    } else {
      i -= rope->u.node.left->weight;
      rope = rope->u.node.right;
    }
  }
  PIC_UNREACHABLE();
}

pic_value
pic_str_cat(pic_state *pic, pic_value a, pic_value b)
{
  return make_str(pic, merge(pic, pic_str_ptr(pic, a)->rope, pic_str_ptr(pic, b)->rope));
}

pic_value
pic_str_sub(pic_state *pic, pic_value str, int s, int e)
{
  struct rope *rope = pic_str_ptr(pic, str)->rope, *tmp;

  split(pic, rope, e, &tmp, NULL);
  split(pic, tmp, s, NULL, &tmp);
  return make_str(pic, tmp);
}

int
pic_str_cmp(pic_state *pic, pic_value str1, pic_value str2)
{
  return strcmp(pic_str(pic, str1), pic_str(pic, str2));
}

int
pic_str_hash(pic_state *pic, pic_value str)
{
  const char *s;
  int h = 0;

  s = pic_str(pic, str);
  while (*s) {
    h = (h << 5) - h + *s++;
  }
  return h;
}

const char *
pic_str(pic_state *pic, pic_value str)
{
  struct rope *rope = pic_str_ptr(pic, str)->rope;

  if (rope->tt == PIC_TYPE_LEAF && rope->u.leaf.chunk->buf + rope->u.leaf.chunk->len == rope->u.leaf.str + rope->weight) {
    return rope->u.leaf.str;    /* reuse cached chunk */
  } else {
    struct chunk *chunk = make_chunk(pic, 0, rope->weight);

    flatten(pic, rope, chunk, chunk->buf);

    pic_chunk_decref(pic, chunk);

    return chunk->buf;
  }
}

static pic_value
pic_str_string_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_str_p(pic, v));
}

static pic_value
pic_str_string(pic_state *pic)
{
  int argc, i;
  pic_value *argv;
  char *buf;

  pic_get_args(pic, "*", &argc, &argv);

  buf = pic_alloca(pic, argc);

  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], char);
    buf[i] = pic_char(pic, argv[i]);
  }

  return pic_str_value(pic, buf, argc);
}

static pic_value
pic_str_make_string(pic_state *pic)
{
  int len;
  char c = ' ';
  char *buf;

  pic_get_args(pic, "i|c", &len, &c);

  if (len < 0) {
    pic_error(pic, "make-string: negative length given", 1, pic_int_value(pic, len));
  }

  buf = pic_alloca(pic, len);

  memset(buf, c, len);

  return pic_str_value(pic, buf, len);
}

static pic_value
pic_str_string_length(pic_state *pic)
{
  pic_value str;

  pic_get_args(pic, "s", &str);

  return pic_int_value(pic, pic_str_len(pic, str));
}

static pic_value
pic_str_string_ref(pic_state *pic)
{
  pic_value str;
  int k;

  pic_get_args(pic, "si", &str, &k);

  VALID_INDEX(pic, pic_str_len(pic, str), k);

  return pic_char_value(pic, pic_str_ref(pic, str, k));
}

#define UPDATE(pic, dst, src) pic_str_ptr(pic, dst)->rope = pic_str_ptr(pic, src)->rope

static pic_value
pic_str_string_set(pic_state *pic)
{
  pic_value str, x, y, z;
  char c;
  int k, len;

  pic_get_args(pic, "sic", &str, &k, &c);

  len = pic_str_len(pic, str);

  VALID_INDEX(pic, len, k);

  x = pic_str_sub(pic, str, 0, k);
  y = pic_str_value(pic, &c, 1);
  z = pic_str_sub(pic, str, k + 1, len);

  UPDATE(pic, str, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

#define DEFINE_STRING_CMP(name, op)                             \
  static pic_value                                              \
  pic_str_string_##name(pic_state *pic)                         \
  {                                                             \
    int argc, i;                                                \
    pic_value *argv;                                            \
                                                                \
    pic_get_args(pic, "*", &argc, &argv);                       \
                                                                \
    if (argc < 1 || ! pic_str_p(pic, argv[0])) {                \
      return pic_false_value(pic);                              \
    }                                                           \
                                                                \
    for (i = 1; i < argc; ++i) {                                \
      if (! pic_str_p(pic, argv[i])) {                          \
        return pic_false_value(pic);                            \
      }                                                         \
      if (! (pic_str_cmp(pic, argv[i-1], argv[i]) op 0)) {      \
        return pic_false_value(pic);                            \
      }                                                         \
    }                                                           \
    return pic_true_value(pic);                                 \
  }

DEFINE_STRING_CMP(eq, ==)
DEFINE_STRING_CMP(lt, <)
DEFINE_STRING_CMP(gt, >)
DEFINE_STRING_CMP(le, <=)
DEFINE_STRING_CMP(ge, >=)

static pic_value
pic_str_string_copy(pic_state *pic)
{
  pic_value str;
  int n, start, end, len;

  n = pic_get_args(pic, "s|ii", &str, &start, &end);

  len = pic_str_len(pic, str);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  return pic_str_sub(pic, str, start, end);
}

static pic_value
pic_str_string_copy_ip(pic_state *pic)
{
  pic_value to, from, x, y, z;
  int n, at, start, end, tolen, fromlen;

  n = pic_get_args(pic, "sis|ii", &to, &at, &from, &start, &end);

  tolen = pic_str_len(pic, to);
  fromlen = pic_str_len(pic, from);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = fromlen;
  }

  VALID_ATRANGE(pic, tolen, at, fromlen, start, end);

  x = pic_str_sub(pic, to, 0, at);
  y = pic_str_sub(pic, from, start, end);
  z = pic_str_sub(pic, to, at + end - start, tolen);

  UPDATE(pic, to, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

static pic_value
pic_str_string_fill_ip(pic_state *pic)
{
  pic_value str, x, y, z;
  char c, *buf;
  int n, start, end, len;

  n = pic_get_args(pic, "sc|ii", &str, &c, &start, &end);

  len = pic_str_len(pic, str);

  switch (n) {
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  buf = pic_alloca(pic, end - start);
  memset(buf, c, end - start);

  x = pic_str_sub(pic, str, 0, start);
  y = pic_str_value(pic, buf, end - start);
  z = pic_str_sub(pic, str, end, len);

  UPDATE(pic, str, pic_str_cat(pic, x, pic_str_cat(pic, y, z)));

  return pic_undef_value(pic);
}

static pic_value
pic_str_string_append(pic_state *pic)
{
  int argc, i;
  pic_value *argv;
  pic_value str = pic_lit_value(pic, "");

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], str);
    str = pic_str_cat(pic, str, argv[i]);
  }
  return str;
}

static pic_value
pic_str_string_map(pic_state *pic)
{
  pic_value proc, *argv, vals, val;
  int argc, i, len, j;
  char *buf;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_error(pic, "string-map: one or more strings expected, but got zero", 0);
  }

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    int l;
    TYPE_CHECK(pic, argv[i], str);
    l = pic_str_len(pic, argv[i]);
    len = len < l ? len : l;
  }

  buf = pic_alloca(pic, len);

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_char_value(pic, pic_str_ref(pic, argv[j], i)), vals);
    }
    vals = pic_reverse(pic, vals);
    val = pic_funcall(pic, "picrin.base", "apply", 2, proc, vals);

    TYPE_CHECK(pic, val, char);

    buf[i] = pic_char(pic, val);
  }
  return pic_str_value(pic, buf, len);
}

static pic_value
pic_str_string_for_each(pic_state *pic)
{
  pic_value proc, *argv, vals;
  int argc, i, len, j;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_error(pic, "string-map: one or more strings expected, but got zero", 0);
  }

  len = INT_MAX;
  for (i = 0; i < argc; ++i) {
    int l;
    TYPE_CHECK(pic, argv[i], str);
    l = pic_str_len(pic, argv[i]);
    len = len < l ? len : l;
  }

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value(pic);
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_char_value(pic, pic_str_ref(pic, argv[j], i)), vals);
    }
    vals = pic_reverse(pic, vals);
    pic_funcall(pic, "picrin.base", "apply", 2, proc, vals);
  }
  return pic_undef_value(pic);
}

static pic_value
pic_str_list_to_string(pic_state *pic)
{
  pic_value list, e, it;
  int i;
  char *buf;

  pic_get_args(pic, "o", &list);

  buf = pic_alloca(pic, pic_length(pic, list));

  i = 0;
  pic_for_each (e, list, it) {
    TYPE_CHECK(pic, e, char);

    buf[i++] = pic_char(pic, e);
  }

  return pic_str_value(pic, buf, i);
}

static pic_value
pic_str_string_to_list(pic_state *pic)
{
  pic_value str, list;
  int n, start, end, len, i;

  n = pic_get_args(pic, "s|ii", &str, &start, &end);

  len = pic_str_len(pic, str);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  list = pic_nil_value(pic);
  for (i = start; i < end; ++i) {
    pic_push(pic, pic_char_value(pic, pic_str_ref(pic, str, i)), list);
  }
  return pic_reverse(pic, list);
}

void
pic_init_str(pic_state *pic)
{
  pic_defun(pic, "string?", pic_str_string_p);
  pic_defun(pic, "string", pic_str_string);
  pic_defun(pic, "make-string", pic_str_make_string);
  pic_defun(pic, "string-length", pic_str_string_length);
  pic_defun(pic, "string-ref", pic_str_string_ref);
  pic_defun(pic, "string-set!", pic_str_string_set);
  pic_defun(pic, "string-copy", pic_str_string_copy);
  pic_defun(pic, "string-copy!", pic_str_string_copy_ip);
  pic_defun(pic, "string-fill!", pic_str_string_fill_ip);
  pic_defun(pic, "string-append", pic_str_string_append);
  pic_defun(pic, "string-map", pic_str_string_map);
  pic_defun(pic, "string-for-each", pic_str_string_for_each);
  pic_defun(pic, "list->string", pic_str_list_to_string);
  pic_defun(pic, "string->list", pic_str_string_to_list);

  pic_defun(pic, "string=?", pic_str_string_eq);
  pic_defun(pic, "string<?", pic_str_string_lt);
  pic_defun(pic, "string>?", pic_str_string_gt);
  pic_defun(pic, "string<=?", pic_str_string_le);
  pic_defun(pic, "string>=?", pic_str_string_ge);
}
