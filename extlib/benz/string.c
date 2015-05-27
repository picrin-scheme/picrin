/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/pair.h"
#include "picrin/port.h"
#include "picrin/error.h"

struct pic_chunk {
  char *str;
  int refcnt;
  size_t len;
  char autofree;
};

struct pic_rope {
  int refcnt;
  size_t weight;
  struct pic_chunk *chunk;
  size_t offset;
  struct pic_rope *left, *right;
};

#define CHUNK_INCREF(c) do {                    \
    (c)->refcnt++;                              \
  } while (0)

#define CHUNK_DECREF(c) do {                    \
    struct pic_chunk *c_ = (c);                 \
    if (! --c_->refcnt) {                       \
      if (c_->autofree)                         \
        pic_free(pic, c_->str);                 \
      pic_free(pic, c_);                         \
    }                                           \
  } while (0)

void
pic_rope_incref(pic_state *pic, struct pic_rope *x) {
  PIC_UNUSED(pic);

  x->refcnt++;
}

void
pic_rope_decref(pic_state *pic, struct pic_rope *x) {
  if (! --x->refcnt) {
    if (x->chunk) {
      CHUNK_DECREF(x->chunk);
      pic_free(pic, x);
      return;
    }
    pic_rope_decref(pic, x->left);
    pic_rope_decref(pic, x->right);
    pic_free(pic, x);
  }
}

static struct pic_chunk *
pic_make_chunk(pic_state *pic, const char *str, size_t len)
{
  char *buf;
  struct pic_chunk *c;

  buf = pic_malloc(pic, len + 1);
  buf[len] = 0;

  memcpy(buf, str, len);

  c = pic_malloc(pic, sizeof(struct pic_chunk));
  c->refcnt = 1;
  c->str = buf;
  c->len = len;
  c->autofree = 1;

  return c;
}

static struct pic_rope *
pic_make_rope(pic_state *pic, struct pic_chunk *c)
{
  struct pic_rope *x;

  x = pic_malloc(pic, sizeof(struct pic_rope));
  x->refcnt = 1;
  x->left = NULL;
  x->right = NULL;
  x->weight = c->len;
  x->offset = 0;
  x->chunk = c;                 /* delegate ownership */

  return x;
}

static pic_str *
pic_make_string(pic_state *pic, struct pic_rope *rope)
{
  pic_str *str;

  str = (pic_str *)pic_obj_alloc(pic, sizeof(pic_str), PIC_TT_STRING);
  str->rope = rope;             /* delegate ownership */
  return str;
}

static size_t
rope_len(struct pic_rope *x)
{
  return x->weight;
}

static char
rope_at(struct pic_rope *x, size_t i)
{
  while (i < x->weight) {
    if (x->chunk) {
      return x->chunk->str[x->offset + i];
    }
    if (i < x->left->weight) {
      x = x->left;
    } else {
      x = x->right;
      i -= x->left->weight;
    }
  }
  return -1;
}

static struct pic_rope *
rope_cat(pic_state *pic, struct pic_rope *x, struct pic_rope *y)
{
  struct pic_rope *z;

  z = pic_malloc(pic, sizeof(struct pic_rope));
  z->refcnt = 1;
  z->left = x;
  z->right = y;
  z->weight = x->weight + y->weight;
  z->offset = 0;
  z->chunk = NULL;

  pic_rope_incref(pic, x);
  pic_rope_incref(pic, y);

  return z;
}

static struct pic_rope *
rope_sub(pic_state *pic, struct pic_rope *x, size_t i, size_t j)
{
  assert(i <= j);
  assert(j <= x->weight);

  if (i == 0 && x->weight == j) {
    pic_rope_incref(pic, x);
    return x;
  }

  if (x->chunk) {
    struct pic_rope *y;

    y = pic_malloc(pic, sizeof(struct pic_rope));
    y->refcnt = 1;
    y->left = NULL;
    y->right = NULL;
    y->weight = j - i;
    y->offset = x->offset + i;
    y->chunk = x->chunk;

    CHUNK_INCREF(x->chunk);

    return y;
  }

  if (j <= x->left->weight) {
    return rope_sub(pic, x->left, i, j);
  }
  else if (x->left->weight <= i) {
    return rope_sub(pic, x->right, i - x->left->weight, j - x->left->weight);
  }
  else {
    struct pic_rope *r, *l;

    l = rope_sub(pic, x->left, i, x->left->weight);
    r = rope_sub(pic, x->right, 0, j - x->left->weight);
    x = rope_cat(pic, l, r);

    pic_rope_decref(pic, l);
    pic_rope_decref(pic, r);

    return x;
  }
}

static void
flatten(pic_state *pic, struct pic_rope *x, struct pic_chunk *c, size_t offset)
{
  if (x->chunk) {
    memcpy(c->str + offset, x->chunk->str + x->offset, x->weight);
    CHUNK_DECREF(x->chunk);

    x->chunk = c;
    x->offset = offset;
    CHUNK_INCREF(c);
    return;
  }
  flatten(pic, x->left, c, offset);
  flatten(pic, x->right, c, offset + x->left->weight);

  pic_rope_decref(pic, x->left);
  pic_rope_decref(pic, x->right);
  x->left = x->right = NULL;
  x->chunk = c;
  x->offset = offset;
  CHUNK_INCREF(c);
}

static const char *
rope_cstr(pic_state *pic, struct pic_rope *x)
{
  struct pic_chunk *c;

  if (x->chunk && x->offset == 0 && x->weight == x->chunk->len) {
    return x->chunk->str;       /* reuse cached chunk */
  }

  c = pic_malloc(pic, sizeof(struct pic_chunk));
  c->refcnt = 1;
  c->len = x->weight;
  c->autofree = 1;
  c->str = pic_malloc(pic, c->len + 1);
  c->str[c->len] = '\0';

  flatten(pic, x, c, 0);

  CHUNK_DECREF(c);
  return c->str;
}

pic_str *
pic_make_str(pic_state *pic, const char *str, size_t len)
{
  if (str == NULL && len > 0) {
    pic_errorf(pic, "zero length specified against NULL ptr");
  }
  return pic_make_string(pic, pic_make_rope(pic, pic_make_chunk(pic, str, len)));
}

pic_str *
pic_make_str_cstr(pic_state *pic, const char *cstr)
{
  return pic_make_str(pic, cstr, strlen(cstr));
}

pic_str *
pic_make_str_fill(pic_state *pic, size_t len, char fill)
{
  size_t i;
  char *buf = pic_malloc(pic, len);
  pic_str *str;

  for (i = 0; i < len; ++i) {
    buf[i] = fill;
  }
  buf[i] = '\0';

  str = pic_make_str(pic, buf, len);

  pic_free(pic, buf);

  return str;
}

size_t
pic_str_len(pic_str *str)
{
  return rope_len(str->rope);
}

char
pic_str_ref(pic_state *pic, pic_str *str, size_t i)
{
  int c;

  c = rope_at(str->rope, i);
  if (c == -1) {
    pic_errorf(pic, "index out of range %d", i);
  }
  return (char)c;
}

pic_str *
pic_str_cat(pic_state *pic, pic_str *a, pic_str *b)
{
  return pic_make_string(pic, rope_cat(pic, a->rope, b->rope));
}

pic_str *
pic_str_sub(pic_state *pic, pic_str *str, size_t s, size_t e)
{
  return pic_make_string(pic, rope_sub(pic, str->rope, s, e));
}

int
pic_str_cmp(pic_state *pic, pic_str *str1, pic_str *str2)
{
  return strcmp(pic_str_cstr(pic, str1), pic_str_cstr(pic, str2));
}

const char *
pic_str_cstr(pic_state *pic, pic_str *str)
{
  return rope_cstr(pic, str->rope);
}

pic_value
pic_xvfformat(pic_state *pic, xFILE *file, const char *fmt, va_list ap)
{
  char c;
  pic_value irrs = pic_nil_value();

  while ((c = *fmt++)) {
    switch (c) {
    default:
      xfputc(c, file);
      break;
    case '%':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '%':
        xfputc('%', file);
        break;
      case 'c':
        xfprintf(file, "%c", va_arg(ap, int));
        break;
      case 's':
        xfprintf(file, "%s", va_arg(ap, const char *));
        break;
      case 'd':
        xfprintf(file, "%d", va_arg(ap, int));
        break;
      case 'p':
        xfprintf(file, "%p", va_arg(ap, void *));
        break;
#if PIC_ENABLE_FLOAT
      case 'f':
        xfprintf(file, "%f", va_arg(ap, double));
        break;
#endif
      }
      break;
    case '~':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '~':
        xfputc('~', file);
        break;
      case '%':
        xfputc('\n', file);
        break;
      case 'a':
        irrs = pic_cons(pic, pic_fdisplay(pic, va_arg(ap, pic_value), file), irrs);
        break;
      case 's':
        irrs = pic_cons(pic, pic_fwrite(pic, va_arg(ap, pic_value), file), irrs);
        break;
      }
      break;
    }
  }
 exit:

  return pic_reverse(pic, irrs);
}

pic_value
pic_xvformat(pic_state *pic, const char *fmt, va_list ap)
{
  struct pic_port *port;
  pic_value irrs;

  port = pic_open_output_string(pic);

  irrs = pic_xvfformat(pic, port->file, fmt, ap);
  irrs = pic_cons(pic, pic_obj_value(pic_get_output_string(pic, port)), irrs);

  pic_close_port(pic, port);
  return irrs;
}

pic_value
pic_xformat(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value objs;

  va_start(ap, fmt);
  objs = pic_xvformat(pic, fmt, ap);
  va_end(ap);

  return objs;
}

void
pic_vfformat(pic_state *pic, xFILE *file, const char *fmt, va_list ap)
{
  pic_xvfformat(pic, file, fmt, ap);
}

pic_str *
pic_vformat(pic_state *pic, const char *fmt, va_list ap)
{
  struct pic_port *port;
  pic_str *str;

  port = pic_open_output_string(pic);

  pic_vfformat(pic, port->file, fmt, ap);
  str = pic_get_output_string(pic, port);

  pic_close_port(pic, port);
  return str;
}

pic_str *
pic_format(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_str *str;

  va_start(ap, fmt);
  str = pic_vformat(pic, fmt, ap);
  va_end(ap);

  return str;
}

static pic_value
pic_str_string_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_str_p(v));
}

static pic_value
pic_str_string(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;
  pic_str *str;
  char *buf;

  pic_get_args(pic, "*", &argc, &argv);

  buf = pic_alloc(pic, (size_t)argc);

  for (i = 0; i < argc; ++i) {
    pic_assert_type(pic, argv[i], char);
    buf[i] = pic_char(argv[i]);
  }

  str = pic_make_str(pic, buf, (size_t)argc);
  pic_free(pic, buf);

  return pic_obj_value(str);
}

static pic_value
pic_str_make_string(pic_state *pic)
{
  size_t len;
  char c = ' ';

  pic_get_args(pic, "k|c", &len, &c);

  return pic_obj_value(pic_make_str_fill(pic, len, c));
}

static pic_value
pic_str_string_length(pic_state *pic)
{
  pic_str *str;

  pic_get_args(pic, "s", &str);

  return pic_size_value(pic_str_len(str));
}

static pic_value
pic_str_string_ref(pic_state *pic)
{
  pic_str *str;
  size_t k;

  pic_get_args(pic, "sk", &str, &k);

  return pic_char_value(pic_str_ref(pic, str, k));
}

#define DEFINE_STRING_CMP(name, op)                                     \
  static pic_value                                                      \
  pic_str_string_##name(pic_state *pic)                                 \
  {                                                                     \
    size_t argc, i;                                                     \
    pic_value *argv;                                                    \
                                                                        \
    pic_get_args(pic, "*", &argc, &argv);                               \
                                                                        \
    if (argc < 1 || ! pic_str_p(argv[0])) {                             \
      return pic_false_value();                                         \
    }                                                                   \
                                                                        \
    for (i = 1; i < argc; ++i) {                                        \
      if (! pic_str_p(argv[i])) {                                       \
	return pic_false_value();                                       \
      }                                                                 \
      if (! (pic_str_cmp(pic, pic_str_ptr(argv[i-1]), pic_str_ptr(argv[i])) op 0)) { \
	return pic_false_value();                                       \
      }                                                                 \
    }                                                                   \
    return pic_true_value();                                            \
  }

DEFINE_STRING_CMP(eq, ==)
DEFINE_STRING_CMP(lt, <)
DEFINE_STRING_CMP(gt, >)
DEFINE_STRING_CMP(le, <=)
DEFINE_STRING_CMP(ge, >=)

static pic_value
pic_str_string_copy(pic_state *pic)
{
  pic_str *str;
  int n;
  size_t start, end;

  n = pic_get_args(pic, "s|kk", &str, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_str_len(str);
  }

  return pic_obj_value(pic_str_sub(pic, str, start, end));
}

static pic_value
pic_str_string_append(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;
  pic_str *str;

  pic_get_args(pic, "*", &argc, &argv);

  str = pic_make_str(pic, NULL, 0);
  for (i = 0; i < argc; ++i) {
    if (! pic_str_p(argv[i])) {
      pic_errorf(pic, "type error");
    }
    str = pic_str_cat(pic, str, pic_str_ptr(argv[i]));
  }
  return pic_obj_value(str);
}

static pic_value
pic_str_string_map(pic_state *pic)
{
  struct pic_proc *proc;
  pic_value *argv, vals, val;
  size_t argc, i, len, j;
  pic_str *str;
  char *buf;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_errorf(pic, "string-map: one or more strings expected, but got zero");
  } else {
    pic_assert_type(pic, argv[0], str);
    len = pic_str_len(pic_str_ptr(argv[0]));
  }
  for (i = 1; i < argc; ++i) {
    pic_assert_type(pic, argv[i], str);

    len = len < pic_str_len(pic_str_ptr(argv[i]))
      ? len
      : pic_str_len(pic_str_ptr(argv[i]));
  }
  buf = pic_malloc(pic, len);

  pic_try {
    for (i = 0; i < len; ++i) {
      vals = pic_nil_value();
      for (j = 0; j < argc; ++j) {
        pic_push(pic, pic_char_value(pic_str_ref(pic, pic_str_ptr(argv[j]), i)), vals);
      }
      val = pic_apply(pic, proc, vals);

      pic_assert_type(pic, val, char);
      buf[i] = pic_char(val);
    }
    str = pic_make_str(pic, buf, len);
  }
  pic_catch {
    pic_free(pic, buf);
    pic_raise(pic, pic->err);
  }

  pic_free(pic, buf);

  return pic_obj_value(str);
}

static pic_value
pic_str_string_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, len, i, j;
  pic_value *argv, vals;

  pic_get_args(pic, "l*", &proc, &argc, &argv);

  if (argc == 0) {
    pic_errorf(pic, "string-map: one or more strings expected, but got zero");
  } else {
    pic_assert_type(pic, argv[0], str);
    len = pic_str_len(pic_str_ptr(argv[0]));
  }
  for (i = 1; i < argc; ++i) {
    pic_assert_type(pic, argv[i], str);

    len = len < pic_str_len(pic_str_ptr(argv[i]))
      ? len
      : pic_str_len(pic_str_ptr(argv[i]));
  }

  for (i = 0; i < len; ++i) {
    vals = pic_nil_value();
    for (j = 0; j < argc; ++j) {
      pic_push(pic, pic_char_value(pic_str_ref(pic, pic_str_ptr(argv[j]), i)), vals);
    }
    pic_apply(pic, proc, vals);
  }

  return pic_none_value();
}

static pic_value
pic_str_list_to_string(pic_state *pic)
{
  pic_str *str;
  pic_value list, e, it;
  size_t i = 0;
  char *buf;

  pic_get_args(pic, "o", &list);

  if (pic_length(pic, list) == 0) {
    return pic_obj_value(pic_make_str(pic, NULL, 0));
  }

  buf = pic_malloc(pic, pic_length(pic, list));

  pic_try {
    pic_for_each (e, list, it) {
      pic_assert_type(pic, e, char);

      buf[i++] = pic_char(e);
    }

    str = pic_make_str(pic, buf, i);
  }
  pic_catch {
    pic_free(pic, buf);
    pic_raise(pic, pic->err);
  }
  pic_free(pic, buf);

  return pic_obj_value(str);
}

static pic_value
pic_str_string_to_list(pic_state *pic)
{
  pic_str *str;
  pic_value list;
  int n;
  size_t start, end, i;

  n = pic_get_args(pic, "s|kk", &str, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = pic_str_len(str);
  }

  list = pic_nil_value();

  for (i = start; i < end; ++i) {
    pic_push(pic, pic_char_value(pic_str_ref(pic, str, i)), list);
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
  pic_defun(pic, "string-copy", pic_str_string_copy);
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
