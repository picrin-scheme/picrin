/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/port.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/dict.h"
#include "picrin/record.h"
#include "picrin/proc.h"

static bool
is_tagged(pic_state *pic, pic_sym tag, pic_value pair)
{
  return pic_pair_p(pic_cdr(pic, pair))
    && pic_nil_p(pic_cddr(pic, pair))
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(tag));
}

static bool
is_quote(pic_state *pic, pic_value pair)
{
  return is_tagged(pic, pic->sQUOTE, pair);
}

static bool
is_unquote(pic_state *pic, pic_value pair)
{
  return is_tagged(pic, pic->sUNQUOTE, pair);
}

static bool
is_unquote_splicing(pic_state *pic, pic_value pair)
{
  return is_tagged(pic, pic->sUNQUOTE_SPLICING, pair);
}

static bool
is_quasiquote(pic_state *pic, pic_value pair)
{
  return is_tagged(pic, pic->sQUASIQUOTE, pair);
}

struct writer_control {
  pic_state *pic;
  xFILE *file;
  int mode;
  xhash labels;                 /* object -> int */
  xhash visited;                /* object -> int */
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
  xh_init_ptr(&p->labels, sizeof(int));
  xh_init_ptr(&p->visited, sizeof(int));
}

static void
writer_control_destroy(struct writer_control *p)
{
  xh_destroy(&p->labels);
  xh_destroy(&p->visited);
}

static void
traverse_shared(struct writer_control *p, pic_value obj)
{
  xh_entry *e;
  size_t i;
  int c;

  switch (pic_type(obj)) {
  case PIC_TT_PAIR:
  case PIC_TT_VECTOR:
    e = xh_get_ptr(&p->labels, pic_obj_ptr(obj));
    if (e == NULL) {
      c = -1;
      xh_put_ptr(&p->labels, pic_obj_ptr(obj), &c);
    }
    else if (xh_val(e, int) == -1) {
      c = p->cnt++;
      xh_put_ptr(&p->labels, pic_obj_ptr(obj), &c);
      break;
    }
    else {
      break;
    }

    if (pic_pair_p(obj)) {
      traverse_shared(p, pic_car(p->pic, obj));
      traverse_shared(p, pic_cdr(p->pic, obj));
    }
    else {
      for (i = 0; i < pic_vec_ptr(obj)->len; ++i) {
        traverse_shared(p, pic_vec_ptr(obj)->data[i]);
      }
    }
    break;
  default:
    /* pass */
    break;
  }
}

static void write_core(struct writer_control *p, pic_value);

static void
write_pair(struct writer_control *p, struct pic_pair *pair)
{
  xh_entry *e;
  int c;

  write_core(p, pair->car);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  else if (pic_pair_p(pair->cdr)) {

    /* shared objects */
    if ((e = xh_get_ptr(&p->labels, pic_obj_ptr(pair->cdr))) && xh_val(e, int) != -1) {
      xfprintf(p->file, " . ");

      if ((xh_get_ptr(&p->visited, pic_obj_ptr(pair->cdr)))) {
        xfprintf(p->file, "#%d#", xh_val(e, int));
        return;
      }
      else {
        xfprintf(p->file, "#%d=", xh_val(e, int));
        c = 1;
        xh_put_ptr(&p->visited, pic_obj_ptr(pair->cdr), &c);
      }
    }
    else {
      xfprintf(p->file, " ");
    }

    write_pair(p, pic_pair_ptr(pair->cdr));
    return;
  }
  else {
    xfprintf(p->file, " . ");
    write_core(p, pair->cdr);
  }
}

static void
write_str(pic_state *pic, struct pic_string *str, xFILE *file)
{
  size_t i;
  const char *cstr = pic_str_cstr(str);

  PIC_UNUSED(pic);

  for (i = 0; i < pic_strlen(str); ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      xfputc('\\', file);
    }
    xfputc(cstr[i], file);
  }
}

static void
write_record(pic_state *pic, struct pic_record *rec, xFILE *file)
{
  const pic_sym sWRITER = pic_intern_cstr(pic, "writer");
  pic_value type, writer, str;

#if DEBUG

  xfprintf(file, "#<record %p>", rec);

#else

  type = pic_record_type(pic, rec);
  if (! pic_record_p(type)) {
    pic_errorf(pic, "\"@@type\" property of record object is not of record type");
  }
  writer = pic_record_ref(pic, pic_record_ptr(type), sWRITER);
  if (! pic_proc_p(writer)) {
    pic_errorf(pic, "\"writer\" property of record type object is not a procedure");
  }
  str = pic_apply1(pic, pic_proc_ptr(writer), pic_obj_value(rec));
  if (! pic_str_p(str)) {
    pic_errorf(pic, "return value from writer procedure is not of string type");
  }
  xfprintf(file, "%s", pic_str_cstr(pic_str_ptr(str)));

#endif
}

static void
write_core(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;
  xFILE *file = p->file;
  size_t i;
  xh_entry *e, *it;
  int c;
  double f;

  /* shared objects */
  if (pic_vtype(obj) == PIC_VTYPE_HEAP
      && (e = xh_get_ptr(&p->labels, pic_obj_ptr(obj)))
      && xh_val(e, int) != -1) {
    if ((xh_get_ptr(&p->visited, pic_obj_ptr(obj)))) {
      xfprintf(file, "#%d#", xh_val(e, int));
      return;
    }
    else {
      xfprintf(file, "#%d=", xh_val(e, int));
      c = 1;
      xh_put_ptr(&p->visited, pic_obj_ptr(obj), &c);
    }
  }

  switch (pic_type(obj)) {
  case PIC_TT_UNDEF:
    xfprintf(file, "#<undef>");
    break;
  case PIC_TT_NIL:
    xfprintf(file, "()");
    break;
  case PIC_TT_BOOL:
    if (pic_true_p(obj))
      xfprintf(file, "#t");
    else
      xfprintf(file, "#f");
    break;
  case PIC_TT_PAIR:
    if (is_quote(pic, obj)) {
      xfprintf(file, "'");
      write_core(p, pic_list_ref(pic, obj, 1));
      break;
    }
    else if (is_unquote(pic, obj)) {
      xfprintf(file, ",");
      write_core(p, pic_list_ref(pic, obj, 1));
      break;
    }
    else if (is_unquote_splicing(pic, obj)) {
      xfprintf(file, ",@");
      write_core(p, pic_list_ref(pic, obj, 1));
      break;
    }
    else if (is_quasiquote(pic, obj)) {
      xfprintf(file, "`");
      write_core(p, pic_list_ref(pic, obj, 1));
      break;
    }
    xfprintf(file, "(");
    write_pair(p, pic_pair_ptr(obj));
    xfprintf(file, ")");
    break;
  case PIC_TT_SYMBOL:
    xfprintf(file, "%s", pic_symbol_name(pic, pic_sym(obj)));
    break;
  case PIC_TT_CHAR:
    if (p->mode == DISPLAY_MODE) {
      xfputc(pic_char(obj), file);
      break;
    }
    switch (pic_char(obj)) {
    default: xfprintf(file, "#\\%c", pic_char(obj)); break;
    case '\a': xfprintf(file, "#\\alarm"); break;
    case '\b': xfprintf(file, "#\\backspace"); break;
    case 0x7f: xfprintf(file, "#\\delete"); break;
    case 0x1b: xfprintf(file, "#\\escape"); break;
    case '\n': xfprintf(file, "#\\newline"); break;
    case '\r': xfprintf(file, "#\\return"); break;
    case ' ': xfprintf(file, "#\\space"); break;
    case '\t': xfprintf(file, "#\\tab"); break;
    }
    break;
  case PIC_TT_FLOAT:
    f = pic_float(obj);
    if (isnan(f)) {
      xfprintf(file, signbit(f) ? "-nan.0" : "+nan.0");
    } else if (isinf(f)) {
      xfprintf(file, signbit(f) ? "-inf.0" : "+inf.0");
    } else {
      xfprintf(file, "%f", pic_float(obj));
    }
    break;
  case PIC_TT_INT:
    xfprintf(file, "%d", pic_int(obj));
    break;
  case PIC_TT_EOF:
    xfprintf(file, "#.(eof-object)");
    break;
  case PIC_TT_STRING:
    if (p->mode == DISPLAY_MODE) {
      xfprintf(file, "%s", pic_str_cstr(pic_str_ptr(obj)));
      break;
    }
    xfprintf(file, "\"");
    write_str(pic, pic_str_ptr(obj), file);
    xfprintf(file, "\"");
    break;
  case PIC_TT_VECTOR:
    xfprintf(file, "#(");
    for (i = 0; i < pic_vec_ptr(obj)->len; ++i) {
      write_core(p, pic_vec_ptr(obj)->data[i]);
      if (i + 1 < pic_vec_ptr(obj)->len) {
	xfprintf(file, " ");
      }
    }
    xfprintf(file, ")");
    break;
  case PIC_TT_BLOB:
    xfprintf(file, "#u8(");
    for (i = 0; i < pic_blob_ptr(obj)->len; ++i) {
      xfprintf(file, "%d", pic_blob_ptr(obj)->data[i]);
      if (i + 1 < pic_blob_ptr(obj)->len) {
	xfprintf(file, " ");
      }
    }
    xfprintf(file, ")");
    break;
  case PIC_TT_DICT:
    xfprintf(file, "#.(dictionary");
    for (it = xh_begin(&pic_dict_ptr(obj)->hash); it != NULL; it = xh_next(it)) {
      xfprintf(file, " '%s ", pic_symbol_name(pic, xh_key(it, pic_sym)));
      write_core(p, xh_val(it, pic_value));
    }
    xfprintf(file, ")");
    break;
  case PIC_TT_RECORD:
    write_record(pic, pic_record_ptr(obj), file);
    break;
  default:
    xfprintf(file, "#<%s %p>", pic_type_repr(pic_type(obj)), pic_ptr(obj));
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
  return pic_fwrite(pic, obj, xstdout);
}

pic_value
pic_fwrite(pic_state *pic, pic_value obj, xFILE *file)
{
  write(pic, obj, file);
  xfflush(file);
  return obj;
}

pic_value
pic_display(pic_state *pic, pic_value obj)
{
  return pic_fdisplay(pic, obj, xstdout);
}

pic_value
pic_fdisplay(pic_state *pic, pic_value obj, xFILE *file)
{
  display(pic, obj, file);
  xfflush(file);
  return obj;
}

void
pic_printf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_str *str;

  va_start(ap, fmt);

  str = pic_str_ptr(pic_car(pic, pic_xvformat(pic, fmt, ap)));

  va_end(ap);

  xprintf("%s", pic_str_cstr(str));
  xfflush(xstdout);
}

static pic_value
pic_write_write(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file);
  return pic_none_value();
}

static pic_value
pic_write_write_simple(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_simple(pic, v, port->file);
  return pic_none_value();
}

static pic_value
pic_write_write_shared(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_shared(pic, v, port->file);
  return pic_none_value();
}

static pic_value
pic_write_display(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  display(pic, v, port->file);
  return pic_none_value();
}

void
pic_init_write(pic_state *pic)
{
  pic_defun(pic, "write", pic_write_write);
  pic_defun(pic, "write-simple", pic_write_write_simple);
  pic_defun(pic, "write-shared", pic_write_write_shared);
  pic_defun(pic, "display", pic_write_display);
}
