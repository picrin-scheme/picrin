/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

KHASH_DECLARE(l, void *, int)
KHASH_DECLARE(v, void *, int)
KHASH_DEFINE2(l, void *, int, 1, kh_ptr_hash_func, kh_ptr_hash_equal)
KHASH_DEFINE2(v, void *, int, 0, kh_ptr_hash_func, kh_ptr_hash_equal)

struct writer_control {
  pic_state *pic;
  xFILE *file;
  int mode;
  int op;
  khash_t(l) labels;            /* object -> int */
  khash_t(v) visited;           /* object -> int */
  int cnt;
};

#define WRITE_MODE 1
#define DISPLAY_MODE 2

#define OP_WRITE 1
#define OP_WRITE_SHARED 2
#define OP_WRITE_SIMPLE 3

static void
writer_control_init(struct writer_control *p, pic_state *pic, xFILE *file, int mode, int op)
{
  p->pic = pic;
  p->file = file;
  p->mode = mode;
  p->op = op;
  p->cnt = 0;
  kh_init(l, &p->labels);
  kh_init(v, &p->visited);
}

static void
writer_control_destroy(struct writer_control *p)
{
  pic_state *pic = p->pic;
  kh_destroy(l, &p->labels);
  kh_destroy(v, &p->visited);
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
write_char(pic_state *pic, char c, xFILE *file, int mode)
{
  if (mode == DISPLAY_MODE) {
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
write_str(pic_state *pic, pic_value str, xFILE *file, int mode)
{
  int i;
  const char *cstr = pic_str(pic, str);

  if (mode == DISPLAY_MODE) {
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
write_float(pic_state *pic, double f, xFILE *file)
{
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

static void write_core(struct writer_control *p, pic_value);

static void
write_pair_help(struct writer_control *p, pic_value pair)
{
  pic_state *pic = p->pic;
  khash_t(l) *lh = &p->labels;
  khash_t(v) *vh = &p->visited;
  khiter_t it;
  int ret;

  write_core(p, pic_car(pic, pair));

  if (pic_nil_p(pic, pic_cdr(pic, pair))) {
    return;
  }
  else if (pic_pair_p(pic, pic_cdr(pic, pair))) {

    /* shared objects */
    if ((it = kh_get(l, lh, pic_obj_ptr(pic_cdr(pic, pair)))) != kh_end(lh) && kh_val(lh, it) != -1) {
      xfprintf(pic, p->file, " . ");

      kh_put(v, vh, pic_obj_ptr(pic_cdr(pic, pair)), &ret);
      if (ret == 0) {           /* if exists */
        xfprintf(pic, p->file, "#%d#", kh_val(lh, it));
        return;
      }
      xfprintf(pic, p->file, "#%d=", kh_val(lh, it));
    }
    else {
      xfprintf(pic, p->file, " ");
    }

    write_pair_help(p, pic_cdr(pic, pair));

    if (p->op == OP_WRITE) {
      if ((it = kh_get(l, lh, pic_obj_ptr(pic_cdr(pic, pair)))) != kh_end(lh) && kh_val(lh, it) != -1) {
        it = kh_get(v, vh, pic_obj_ptr(pic_cdr(pic, pair)));
        kh_del(v, vh, it);
      }
    }
    return;
  }
  else {
    xfprintf(pic, p->file, " . ");
    write_core(p, pic_cdr(pic, pair));
  }
}

static void
write_pair(struct writer_control *p, pic_value pair)
{
  pic_state *pic = p->pic;
  xFILE *file = p->file;
  pic_value tag;

  if (pic_pair_p(pic, pic_cdr(pic, pair)) && pic_nil_p(pic, pic_cddr(pic, pair)) && pic_sym_p(pic, pic_car(pic, pair))) {
    tag = pic_car(pic, pair);
    if (pic_eq_p(pic, tag, pic->sQUOTE)) {
      xfprintf(pic, file, "'");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sUNQUOTE)) {
      xfprintf(pic, file, ",");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sUNQUOTE_SPLICING)) {
      xfprintf(pic, file, ",@");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sQUASIQUOTE)) {
      xfprintf(pic, file, "`");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sSYNTAX_QUOTE)) {
      xfprintf(pic, file, "#'");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sSYNTAX_UNQUOTE)) {
      xfprintf(pic, file, "#,");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sSYNTAX_UNQUOTE_SPLICING)) {
      xfprintf(pic, file, "#,@");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
    else if (pic_eq_p(pic, tag, pic->sSYNTAX_QUASIQUOTE)) {
      xfprintf(pic, file, "#`");
      write_core(p, pic_cadr(pic, pair));
      return;
    }
  }
  xfprintf(pic, file, "(");
  write_pair_help(p, pair);
  xfprintf(pic, file, ")");
}

static void
write_vec(struct writer_control *p, pic_value vec)
{
  pic_state *pic = p->pic;
  xFILE *file = p->file;
  int i, len = pic_vec_len(pic, vec);

  xfprintf(pic, file, "#(");
  for (i = 0; i < len; ++i) {
    write_core(p, pic_vec_ref(pic, vec, i));
    if (i + 1 < len) {
      xfprintf(pic, file, " ");
    }
  }
  xfprintf(pic, file, ")");
}

static void
write_dict(struct writer_control *p, pic_value dict)
{
  pic_state *pic = p->pic;
  xFILE *file = p->file;
  pic_value key, val;
  int it = 0;

  xfprintf(pic, file, "#.(dictionary");
  while (pic_dict_next(pic, dict, &it, &key, &val)) {
    xfprintf(pic, file, " '%s ", pic_str(pic, pic_sym_name(pic, key)));
    write_core(p, val);
  }
  xfprintf(pic, file, ")");
}

static void
write_core(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;
  khash_t(l) *lh = &p->labels;
  khash_t(v) *vh = &p->visited;
  xFILE *file = p->file;
  khiter_t it;
  int ret;

  /* shared objects */
  if (pic_obj_p(pic, obj) && ((it = kh_get(l, lh, pic_obj_ptr(obj))) != kh_end(lh)) && kh_val(lh, it) != -1) {
    kh_put(v, vh, pic_obj_ptr(obj), &ret);
    if (ret == 0) {             /* if exists */
      xfprintf(pic, file, "#%d#", kh_val(lh, it));
      return;
    }
    xfprintf(pic, file, "#%d=", kh_val(lh, it));
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
  case PIC_TYPE_FLOAT:
    write_float(pic, pic_float(pic, obj), file);
    break;
  case PIC_TYPE_SYMBOL:
    xfprintf(pic, file, "%s", pic_str(pic, pic_sym_name(pic, obj)));
    break;
  case PIC_TYPE_BLOB:
    write_blob(pic, obj, file);
    break;
  case PIC_TYPE_CHAR:
    write_char(pic, pic_char(pic, obj), file, p->mode);
    break;
  case PIC_TYPE_STRING:
    write_str(pic, obj, file, p->mode);
    break;
  case PIC_TYPE_PAIR:
    write_pair(p, obj);
    break;
  case PIC_TYPE_VECTOR:
    write_vec(p, obj);
    break;
  case PIC_TYPE_DICT:
    write_dict(p, obj);
    break;
  default:
    xfprintf(pic, file, "#<%s %p>", pic_typename(pic, pic_type(pic, obj)), pic_obj_ptr(obj));
    break;
  }

  if (p->op == OP_WRITE) {
    if (pic_obj_p(pic, obj) && ((it = kh_get(l, lh, pic_obj_ptr(obj))) != kh_end(lh)) && kh_val(lh, it) != -1) {
      it = kh_get(v, vh, pic_obj_ptr(obj));
      kh_del(v, vh, it);
    }
  }
}

static void
traverse(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;

  if (p->op == OP_WRITE_SIMPLE) {
    return;
  }

  switch (pic_type(pic, obj)) {
  case PIC_TYPE_PAIR:
  case PIC_TYPE_VECTOR:
  case PIC_TYPE_DICT: {
    khash_t(l) *h = &p->labels;
    khiter_t it;
    int ret;

    it = kh_put(l, h, pic_obj_ptr(obj), &ret);
    if (ret != 0) {
      /* first time */
      kh_val(h, it) = -1;

      if (pic_pair_p(pic, obj)) {
        /* pair */
        traverse(p, pic_car(pic, obj));
        traverse(p, pic_cdr(pic, obj));
      } else if (pic_vec_p(pic, obj)) {
        /* vector */
        int i, len = pic_vec_len(pic, obj);
        for (i = 0; i < len; ++i) {
          traverse(p, pic_vec_ref(pic, obj, i));
        }
      } else {
        /* dictionary */
        int it = 0;
        pic_value val;
        while (pic_dict_next(pic, obj, &it, NULL, &val)) {
          traverse(p, val);
        }
      }

      if (p->op == OP_WRITE) {
        it = kh_get(l, h, pic_obj_ptr(obj));
        if (kh_val(h, it) == -1) {
          kh_del(l, h, it);
        }
      }
    } else if (kh_val(h, it) == -1) {
      /* second time */
      kh_val(h, it) = p->cnt++;
    }
    break;
  }
  default:
    break;
  }
}

static void
write(pic_state *pic, pic_value obj, xFILE *file, int mode, int op)
{
  struct writer_control p;

  writer_control_init(&p, pic, file, mode, op);

  traverse(&p, obj);

  write_core(&p, obj);

  writer_control_destroy(&p);
}


pic_value
pic_write(pic_state *pic, pic_value obj)
{
  return pic_fwrite(pic, obj, pic_stdout(pic)->file);
}

pic_value
pic_fwrite(pic_state *pic, pic_value obj, xFILE *file)
{
  write(pic, obj, file, WRITE_MODE, OP_WRITE);
  xfflush(pic, file);
  return obj;
}

pic_value
pic_display(pic_state *pic, pic_value obj)
{
  return pic_fdisplay(pic, obj, pic_stdout(pic)->file);
}

pic_value
pic_fdisplay(pic_state *pic, pic_value obj, xFILE *file)
{
  write(pic, obj, file, DISPLAY_MODE, OP_WRITE);
  xfflush(pic, file);
  return obj;
}

void
pic_printf(pic_state *pic, const char *fmt, ...)
{
  xFILE *file = pic_stdout(pic)->file;
  va_list ap;
  pic_value str;

  va_start(ap, fmt);

  str = pic_vstrf_value(pic, fmt, ap);

  va_end(ap);

  xfprintf(pic, file, "%s", pic_str(pic, str));
  xfflush(pic, file);
}

static pic_value
pic_write_write(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file, WRITE_MODE, OP_WRITE);
  return pic_undef_value(pic);
}

static pic_value
pic_write_write_simple(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file, WRITE_MODE, OP_WRITE_SIMPLE);
  return pic_undef_value(pic);
}

static pic_value
pic_write_write_shared(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file, WRITE_MODE, OP_WRITE_SHARED);
  return pic_undef_value(pic);
}

static pic_value
pic_write_display(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file, DISPLAY_MODE, OP_WRITE);
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
