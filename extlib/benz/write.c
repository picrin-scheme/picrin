/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DECLARE(l, void *, int)
KHASH_DECLARE(v, void *, int)
KHASH_DEFINE2(l, void *, int, 1, kh_ptr_hash_func, kh_ptr_hash_equal)
KHASH_DEFINE2(v, void *, int, 0, kh_ptr_hash_func, kh_ptr_hash_equal)

struct writer_control {
  pic_state *pic;
  xFILE *file;
  int mode;
  khash_t(l) labels;            /* object -> int */
  khash_t(v) visited;           /* object -> int */
  int cnt;
};

#define WRITE_MODE 1
#define DISPLAY_MODE 2

static void
writer_control_init(struct writer_control *p, pic_state *pic, xFILE *file, int mode)
{
  p->pic = pic;
  p->file = file;
  p->mode = mode;
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
traverse_shared(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;

  switch (pic_type(obj)) {
  case PIC_TT_PAIR:
  case PIC_TT_VECTOR: {
    khash_t(l) *h = &p->labels;
    khiter_t it;
    int ret;

    it = kh_put(l, h, pic_ptr(obj), &ret);
    if (ret != 0) {
      /* first time */
      kh_val(h, it) = -1;

      if (pic_pair_p(obj)) {
        /* pair */
        traverse_shared(p, pic_car(pic, obj));
        traverse_shared(p, pic_cdr(pic, obj));
      } else {
        /* vector */
        size_t i;
        for (i = 0; i < pic_vec_ptr(obj)->len; ++i) {
          traverse_shared(p, pic_vec_ptr(obj)->data[i]);
        }
      }
    } else if (kh_val(h, it) == -1) {
      /* second time */
      kh_val(h, it) = p->cnt++;
    }
    break;
  }
  default:
    /* pass */
    break;
  }
}

static void write_core(struct writer_control *p, pic_value);

static void
write_pair(struct writer_control *p, struct pic_pair *pair)
{
  pic_state *pic = p->pic;
  khash_t(l) *lh = &p->labels;
  khash_t(v) *vh = &p->visited;
  khiter_t it;
  int ret;

  write_core(p, pair->car);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  else if (pic_pair_p(pair->cdr)) {

    /* shared objects */
    if ((it = kh_get(l, lh, pic_ptr(pair->cdr))) != kh_end(lh) && kh_val(lh, it) != -1) {
      xfprintf(pic, p->file, " . ");

      kh_put(v, vh, pic_ptr(pair->cdr), &ret);
      if (ret == 0) {           /* if exists */
        xfprintf(pic, p->file, "#%d#", kh_val(lh, it));
        return;
      }
      xfprintf(pic, p->file, "#%d=", kh_val(lh, it));
    }
    else {
      xfprintf(pic, p->file, " ");
    }

    write_pair(p, pic_pair_ptr(pair->cdr));
    return;
  }
  else {
    xfprintf(pic, p->file, " . ");
    write_core(p, pair->cdr);
  }
}

static void
write_str(pic_state *pic, struct pic_string *str, xFILE *file)
{
  size_t i;
  const char *cstr = pic_str_cstr(pic, str);

  for (i = 0; i < pic_str_len(str); ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      xfputc(pic, '\\', file);
    }
    xfputc(pic, cstr[i], file);
  }
}

static void
write_core(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;
  khash_t(l) *lh = &p->labels;
  khash_t(v) *vh = &p->visited;
  xFILE *file = p->file;
  size_t i;
  pic_sym *sym, *tag;
  khiter_t it;
  int ret;
#if PIC_ENABLE_FLOAT
  double f;
#endif

  /* shared objects */
  if (pic_vtype(obj) == PIC_VTYPE_HEAP && ((it = kh_get(l, lh, pic_ptr(obj))) != kh_end(lh)) && kh_val(lh, it) != -1) {
    kh_put(v, vh, pic_ptr(obj), &ret);
    if (ret == 0) {             /* if exists */
      xfprintf(pic, file, "#%d#", kh_val(lh, it));
      return;
    }
    xfprintf(pic, file, "#%d=", kh_val(lh, it));
  }

  switch (pic_type(obj)) {
  case PIC_TT_UNDEF:
    xfprintf(pic, file, "#undefined");
    break;
  case PIC_TT_NIL:
    xfprintf(pic, file, "()");
    break;
  case PIC_TT_BOOL:
    if (pic_true_p(obj))
      xfprintf(pic, file, "#t");
    else
      xfprintf(pic, file, "#f");
    break;
  case PIC_TT_PAIR:
    if (pic_pair_p(pic_cdr(pic, obj)) && pic_nil_p(pic_cddr(pic, obj)) && pic_sym_p(pic_car(pic, obj))) {
      tag = pic_sym_ptr(pic_car(pic, obj));
      if (tag == pic->sQUOTE) {
        xfprintf(pic, file, "'");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sUNQUOTE) {
        xfprintf(pic, file, ",");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sUNQUOTE_SPLICING) {
        xfprintf(pic, file, ",@");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sQUASIQUOTE) {
        xfprintf(pic, file, "`");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sSYNTAX_QUOTE) {
        xfprintf(pic, file, "#'");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sSYNTAX_UNQUOTE) {
        xfprintf(pic, file, "#,");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sSYNTAX_UNQUOTE_SPLICING) {
        xfprintf(pic, file, "#,@");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
      else if (tag == pic->sSYNTAX_QUASIQUOTE) {
        xfprintf(pic, file, "#`");
        write_core(p, pic_list_ref(pic, obj, 1));
        break;
      }
    }
    xfprintf(pic, file, "(");
    write_pair(p, pic_pair_ptr(obj));
    xfprintf(pic, file, ")");
    break;
  case PIC_TT_SYMBOL:
    xfprintf(pic, file, "%s", pic_symbol_name(pic, pic_sym_ptr(obj)));
    break;
  case PIC_TT_CHAR:
    if (p->mode == DISPLAY_MODE) {
      xfputc(pic, pic_char(obj), file);
      break;
    }
    switch (pic_char(obj)) {
    default: xfprintf(pic, file, "#\\%c", pic_char(obj)); break;
    case '\a': xfprintf(pic, file, "#\\alarm"); break;
    case '\b': xfprintf(pic, file, "#\\backspace"); break;
    case 0x7f: xfprintf(pic, file, "#\\delete"); break;
    case 0x1b: xfprintf(pic, file, "#\\escape"); break;
    case '\n': xfprintf(pic, file, "#\\newline"); break;
    case '\r': xfprintf(pic, file, "#\\return"); break;
    case ' ': xfprintf(pic, file, "#\\space"); break;
    case '\t': xfprintf(pic, file, "#\\tab"); break;
    }
    break;
#if PIC_ENABLE_FLOAT
  case PIC_TT_FLOAT:
    f = pic_float(obj);
    if (isnan(f)) {
      xfprintf(pic, file, signbit(f) ? "-nan.0" : "+nan.0");
    } else if (isinf(f)) {
      xfprintf(pic, file, signbit(f) ? "-inf.0" : "+inf.0");
    } else {
      xfprintf(pic, file, "%f", pic_float(obj));
    }
    break;
#endif
  case PIC_TT_INT:
    xfprintf(pic, file, "%d", pic_int(obj));
    break;
  case PIC_TT_EOF:
    xfprintf(pic, file, "#.(eof-object)");
    break;
  case PIC_TT_STRING:
    if (p->mode == DISPLAY_MODE) {
      xfprintf(pic, file, "%s", pic_str_cstr(pic, pic_str_ptr(obj)));
      break;
    }
    xfprintf(pic, file, "\"");
    write_str(pic, pic_str_ptr(obj), file);
    xfprintf(pic, file, "\"");
    break;
  case PIC_TT_VECTOR:
    xfprintf(pic, file, "#(");
    for (i = 0; i < pic_vec_ptr(obj)->len; ++i) {
      write_core(p, pic_vec_ptr(obj)->data[i]);
      if (i + 1 < pic_vec_ptr(obj)->len) {
	xfprintf(pic, file, " ");
      }
    }
    xfprintf(pic, file, ")");
    break;
  case PIC_TT_BLOB:
    xfprintf(pic, file, "#u8(");
    for (i = 0; i < pic_blob_ptr(obj)->len; ++i) {
      xfprintf(pic, file, "%d", pic_blob_ptr(obj)->data[i]);
      if (i + 1 < pic_blob_ptr(obj)->len) {
	xfprintf(pic, file, " ");
      }
    }
    xfprintf(pic, file, ")");
    break;
  case PIC_TT_DICT:
    xfprintf(pic, file, "#.(dictionary");
    pic_dict_for_each (sym, pic_dict_ptr(obj), it) {
      xfprintf(pic, file, " '%s ", pic_symbol_name(pic, sym));
      write_core(p, pic_dict_ref(pic, pic_dict_ptr(obj), sym));
    }
    xfprintf(pic, file, ")");
    break;
  case PIC_TT_ID:
    xfprintf(pic, file, "#<identifier %s>", pic_symbol_name(pic, pic_var_name(pic, obj)));
    break;
  default:
    xfprintf(pic, file, "#<%s %p>", pic_type_repr(pic_type(obj)), pic_ptr(obj));
    break;
  }
}

static void
write(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control p;

  writer_control_init(&p, pic, file, WRITE_MODE);

  traverse_shared(&p, obj);      /* FIXME */

  write_core(&p, obj);

  writer_control_destroy(&p);
}

static void
write_simple(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control p;

  writer_control_init(&p, pic, file, WRITE_MODE);

  /* no traverse here! */

  write_core(&p, obj);

  writer_control_destroy(&p);
}

static void
write_shared(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control p;

  writer_control_init(&p, pic, file, WRITE_MODE);

  traverse_shared(&p, obj);

  write_core(&p, obj);

  writer_control_destroy(&p);
}

static void
display(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control p;

  writer_control_init(&p, pic, file, DISPLAY_MODE);

  traverse_shared(&p, obj);      /* FIXME */

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
  write(pic, obj, file);
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
  display(pic, obj, file);
  xfflush(pic, file);
  return obj;
}

void
pic_printf(pic_state *pic, const char *fmt, ...)
{
  xFILE *file = pic_stdout(pic)->file;
  va_list ap;
  pic_str *str;

  va_start(ap, fmt);

  str = pic_str_ptr(pic_car(pic, pic_xvformat(pic, fmt, ap)));

  va_end(ap);

  xfprintf(pic, file, "%s", pic_str_cstr(pic, str));
  xfflush(pic, file);
}

static pic_value
pic_write_write(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file);
  return pic_undef_value();
}

static pic_value
pic_write_write_simple(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_simple(pic, v, port->file);
  return pic_undef_value();
}

static pic_value
pic_write_write_shared(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_shared(pic, v, port->file);
  return pic_undef_value();
}

static pic_value
pic_write_display(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  display(pic, v, port->file);
  return pic_undef_value();
}

void
pic_init_write(pic_state *pic)
{
  pic_defun(pic, "write", pic_write_write);
  pic_defun(pic, "write-simple", pic_write_write_simple);
  pic_defun(pic, "write-shared", pic_write_write_shared);
  pic_defun(pic, "display", pic_write_display);
}
