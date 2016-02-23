/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"

#if PIC_USE_WRITE

struct writer_control {
  int mode;
  int op;
  int cnt;
  pic_value shared;            /* is object shared? (yes if >0) */
  pic_value labels;            /* object -> int */
};

#define WRITE_MODE 1
#define DISPLAY_MODE 2

#define OP_WRITE 1
#define OP_WRITE_SHARED 2
#define OP_WRITE_SIMPLE 3

static void
writer_control_init(pic_state *pic, struct writer_control *p, int mode, int op)
{
  p->mode = mode;
  p->op = op;
  p->cnt = 0;
  p->shared = pic_make_weak(pic);
  p->labels = pic_make_weak(pic);
}

static void
traverse(pic_state *pic, pic_value obj, struct writer_control *p)
{
  pic_value shared = p->shared;

  if (p->op == OP_WRITE_SIMPLE) {
    return;
  }

  switch (pic_type(pic, obj)) {
  case PIC_TYPE_PAIR:
  case PIC_TYPE_VECTOR:
  case PIC_TYPE_DICT: {

    if (! pic_weak_has(pic, shared, obj)) {
      /* first time */
      pic_weak_set(pic, shared, obj, pic_int_value(pic, 0));

      if (pic_pair_p(pic, obj)) {
        /* pair */
        traverse(pic, pic_car(pic, obj), p);
        traverse(pic, pic_cdr(pic, obj), p);
      } else if (pic_vec_p(pic, obj)) {
        /* vector */
        int i, len = pic_vec_len(pic, obj);
        for (i = 0; i < len; ++i) {
          traverse(pic, pic_vec_ref(pic, obj, i), p);
        }
      } else {
        /* dictionary */
        int it = 0;
        pic_value val;
        while (pic_dict_next(pic, obj, &it, NULL, &val)) {
          traverse(pic, val, p);
        }
      }

      if (p->op == OP_WRITE) {
        if (pic_int(pic, pic_weak_ref(pic, shared, obj)) == 0) {
          pic_weak_del(pic, shared, obj);
        }
      }
    } else {
      /* second time */
      pic_weak_set(pic, shared, obj, pic_int_value(pic, 1));
    }
    break;
  }
  default:
    break;
  }
}

static bool
is_shared_object(pic_state *pic, pic_value obj, struct writer_control *p) {
  pic_value shared = p->shared;

  if (! pic_obj_p(pic, obj)) {
    return false;
  }
  if (! pic_weak_has(pic, shared, obj)) {
    return false;
  }
  return pic_int(pic, pic_weak_ref(pic, shared, obj)) > 0;
}

static void
write_blob(pic_state *pic, pic_value blob, xFILE *file)
{
  const unsigned char *buf;
  int len, i;

  buf = pic_blob(pic, blob, &len);

  xfprintf(pic, file, "#u8(");
  for (i = 0; i < len; ++i) {
    xfprintf(pic, file, "%d", buf[i]);
    if (i + 1 < len) {
      xfprintf(pic, file, " ");
    }
  }
  xfprintf(pic, file, ")");
}

static void
write_char(pic_state *pic, pic_value ch, xFILE *file, struct writer_control *p)
{
  char c = pic_char(pic, ch);

  if (p->mode == DISPLAY_MODE) {
    xfputc(pic, c, file);
    return;
  }
  switch (c) {
  default: xfprintf(pic, file, "#\\%c", c); break;
  case '\a': xfprintf(pic, file, "#\\alarm"); break;
  case '\b': xfprintf(pic, file, "#\\backspace"); break;
  case 0x7f: xfprintf(pic, file, "#\\delete"); break;
  case 0x1b: xfprintf(pic, file, "#\\escape"); break;
  case '\n': xfprintf(pic, file, "#\\newline"); break;
  case '\r': xfprintf(pic, file, "#\\return"); break;
  case ' ': xfprintf(pic, file, "#\\space"); break;
  case '\t': xfprintf(pic, file, "#\\tab"); break;
  }
}

static void
write_str(pic_state *pic, pic_value str, xFILE *file, struct writer_control *p)
{
  int i;
  const char *cstr = pic_str(pic, str);

  if (p->mode == DISPLAY_MODE) {
    xfprintf(pic, file, "%s", pic_str(pic, str));
    return;
  }
  xfprintf(pic, file, "\"");
  for (i = 0; i < pic_str_len(pic, str); ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      xfputc(pic, '\\', file);
    }
    xfputc(pic, cstr[i], file);
  }
  xfprintf(pic, file, "\"");
}

static void
write_float(pic_state *pic, pic_value flo, xFILE *file)
{
  double f = pic_float(pic, flo);

  if (f != f) {
    xfprintf(pic, file, "+nan.0");
  } else if (f == 1.0 / 0.0) {
    xfprintf(pic, file, "+inf.0");
  } else if (f == -1.0 / 0.0) {
    xfprintf(pic, file, "-inf.0");
  } else {
    xfprintf(pic, file, "%f", f);
  }
}

static void write_core(pic_state *, pic_value, xFILE *, struct writer_control *);

static void
write_pair_help(pic_state *pic, pic_value pair, xFILE *file, struct writer_control *p)
{
  pic_value cdr = pic_cdr(pic, pair);

  write_core(pic, pic_car(pic, pair), file, p);

  if (pic_nil_p(pic, cdr)) {
    return;
  }
  else if (pic_pair_p(pic, cdr) && ! is_shared_object(pic, cdr, p)) {
    xfprintf(pic, file, " ");
    write_pair_help(pic, cdr, file, p);
  }
  else {
    xfprintf(pic, file, " . ");
    write_core(pic, cdr, file, p);
  }
}

#define EQ(sym, lit) (strcmp(pic_sym(pic, sym), lit) == 0)

static void
write_pair(pic_state *pic, pic_value pair, xFILE *file, struct writer_control *p)
{
  pic_value tag;

  if (pic_pair_p(pic, pic_cdr(pic, pair)) && pic_nil_p(pic, pic_cddr(pic, pair)) && pic_sym_p(pic, pic_car(pic, pair))) {
    tag = pic_car(pic, pair);
    if (EQ(tag, "quote")) {
      xfprintf(pic, file, "'");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "unquote")) {
      xfprintf(pic, file, ",");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "unquote-splicing")) {
      xfprintf(pic, file, ",@");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "quasiquote")) {
      xfprintf(pic, file, "`");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "syntax-quote")) {
      xfprintf(pic, file, "#'");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "syntax-unquote")) {
      xfprintf(pic, file, "#,");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "syntax-unquote-splicing")) {
      xfprintf(pic, file, "#,@");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
    else if (EQ(tag, "syntax-quasiquote")) {
      xfprintf(pic, file, "#`");
      write_core(pic, pic_cadr(pic, pair), file, p);
      return;
    }
  }
  xfprintf(pic, file, "(");
  write_pair_help(pic, pair, file, p);
  xfprintf(pic, file, ")");
}

static void
write_vec(pic_state *pic, pic_value vec, xFILE *file, struct writer_control *p)
{
  int i, len = pic_vec_len(pic, vec);

  xfprintf(pic, file, "#(");
  for (i = 0; i < len; ++i) {
    write_core(pic, pic_vec_ref(pic, vec, i), file, p);
    if (i + 1 < len) {
      xfprintf(pic, file, " ");
    }
  }
  xfprintf(pic, file, ")");
}

static void
write_dict(pic_state *pic, pic_value dict, xFILE *file, struct writer_control *p)
{
  pic_value key, val;
  int it = 0;

  xfprintf(pic, file, "#.(dictionary");
  while (pic_dict_next(pic, dict, &it, &key, &val)) {
    xfprintf(pic, file, " '%s ", pic_sym(pic, key));
    write_core(pic, val, file, p);
  }
  xfprintf(pic, file, ")");
}

static void
write_core(pic_state *pic, pic_value obj, xFILE *file, struct writer_control *p)
{
  pic_value labels = p->labels;
  int i;

  /* shared objects */
  if (is_shared_object(pic, obj, p)) {
    if (pic_weak_has(pic, labels, obj)) {
      xfprintf(pic, file, "#%d#", pic_int(pic, pic_weak_ref(pic, labels, obj)));
      return;
    }
    i = p->cnt++;
    xfprintf(pic, file, "#%d=", i);
    pic_weak_set(pic, labels, obj, pic_int_value(pic, i));
  }

  switch (pic_type(pic, obj)) {
  case PIC_TYPE_UNDEF:
    xfprintf(pic, file, "#undefined");
    break;
  case PIC_TYPE_NIL:
    xfprintf(pic, file, "()");
    break;
  case PIC_TYPE_TRUE:
    xfprintf(pic, file, "#t");
    break;
  case PIC_TYPE_FALSE:
    xfprintf(pic, file, "#f");
    break;
  case PIC_TYPE_ID:
    xfprintf(pic, file, "#<identifier %s>", pic_str(pic, pic_id_name(pic, obj)));
    break;
  case PIC_TYPE_EOF:
    xfprintf(pic, file, "#.(eof-object)");
    break;
  case PIC_TYPE_INT:
    xfprintf(pic, file, "%d", pic_int(pic, obj));
    break;
  case PIC_TYPE_SYMBOL:
    xfprintf(pic, file, "%s", pic_sym(pic, obj));
    break;
  case PIC_TYPE_FLOAT:
    write_float(pic, obj, file);
    break;
  case PIC_TYPE_BLOB:
    write_blob(pic, obj, file);
    break;
  case PIC_TYPE_CHAR:
    write_char(pic, obj, file, p);
    break;
  case PIC_TYPE_STRING:
    write_str(pic, obj, file, p);
    break;
  case PIC_TYPE_PAIR:
    write_pair(pic, obj, file, p);
    break;
  case PIC_TYPE_VECTOR:
    write_vec(pic, obj, file, p);
    break;
  case PIC_TYPE_DICT:
    write_dict(pic, obj, file, p);
    break;
  default:
    xfprintf(pic, file, "#<%s %p>", pic_typename(pic, pic_type(pic, obj)), pic_obj_ptr(obj));
    break;
  }

  if (p->op == OP_WRITE) {
    if (is_shared_object(pic, obj, p)) {
      pic_weak_del(pic, labels, obj);
    }
  }
}

static void
write(pic_state *pic, pic_value obj, xFILE *file, int mode, int op)
{
  struct writer_control p;

  writer_control_init(pic, &p, mode, op);

  traverse(pic, obj, &p);

  write_core(pic, obj, file, &p);
}

void
pic_vfprintf(pic_state *pic, pic_value port, const char *fmt, va_list ap)
{
  xFILE *file = pic_fileno(pic, port);
  char c;

  while ((c = *fmt++) != '\0') {
    switch (c) {
    default:
      xfputc(pic, c, file);
      break;
    case '%':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(pic, c, file);
        break;
      case '%':
        xfputc(pic, '%', file);
        break;
      case 'c':
        xfprintf(pic, file, "%c", va_arg(ap, int));
        break;
      case 's':
        xfprintf(pic, file, "%s", va_arg(ap, const char *));
        break;
      case 'd':
        xfprintf(pic, file, "%d", va_arg(ap, int));
        break;
      case 'p':
        xfprintf(pic, file, "%p", va_arg(ap, void *));
        break;
      case 'f':
        xfprintf(pic, file, "%f", va_arg(ap, double));
        break;
      }
      break;
    case '~':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(pic, c, file);
        break;
      case '~':
        xfputc(pic, '~', file);
        break;
      case '%':
        xfputc(pic, '\n', file);
        break;
      case 'a':
        write(pic, va_arg(ap, pic_value), file, DISPLAY_MODE, OP_WRITE);
        break;
      case 's':
        write(pic, va_arg(ap, pic_value), file, WRITE_MODE, OP_WRITE);
        break;
      }
      break;
    }
  }
 exit:
  xfflush(pic, file);
}

void
pic_fprintf(pic_state *pic, pic_value port, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  pic_vfprintf(pic, port, fmt, ap);
  va_end(ap);
}

void
pic_printf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  pic_vfprintf(pic, pic_stdout(pic), fmt, ap);
  va_end(ap);
}

static pic_value
pic_write_write(pic_state *pic)
{
  pic_value v, port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, pic_fileno(pic, port), WRITE_MODE, OP_WRITE);
  return pic_undef_value(pic);
}

static pic_value
pic_write_write_simple(pic_state *pic)
{
  pic_value v, port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, pic_fileno(pic, port), WRITE_MODE, OP_WRITE_SIMPLE);
  return pic_undef_value(pic);
}

static pic_value
pic_write_write_shared(pic_state *pic)
{
  pic_value v, port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, pic_fileno(pic, port), WRITE_MODE, OP_WRITE_SHARED);
  return pic_undef_value(pic);
}

static pic_value
pic_write_display(pic_state *pic)
{
  pic_value v, port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, pic_fileno(pic, port), DISPLAY_MODE, OP_WRITE);
  return pic_undef_value(pic);
}

void
pic_init_write(pic_state *pic)
{
  pic_defun(pic, "write", pic_write_write);
  pic_defun(pic, "write-simple", pic_write_write_simple);
  pic_defun(pic, "write-shared", pic_write_write_shared);
  pic_defun(pic, "display", pic_write_display);
}

#endif
