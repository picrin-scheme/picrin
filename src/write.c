/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/port.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/number.h"
#include "picrin/macro.h"

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

static struct writer_control *
writer_control_new(pic_state *pic, xFILE *file, int mode)
{
  struct writer_control *p;

  p = (struct writer_control *)pic_alloc(pic, sizeof(struct writer_control));
  p->pic = pic;
  p->file = file;
  p->mode = mode;
  p->cnt = 0;
  xh_init_ptr(&p->labels, sizeof(int));
  xh_init_ptr(&p->visited, sizeof(int));
  return p;
}

static void
writer_control_destroy(struct writer_control *p)
{
  pic_state *pic = p->pic;

  xh_destroy(&p->labels);
  xh_destroy(&p->visited);
  pic_free(pic, p);
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
    e = xh_get(&p->labels, pic_obj_ptr(obj));
    if (e == NULL) {
      c = -1;
      xh_put(&p->labels, pic_obj_ptr(obj), &c);
    }
    else if (xh_val(e, int) == -1) {
      c = p->cnt++;
      xh_put(&p->labels, pic_obj_ptr(obj), &c);
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
    if ((e = xh_get(&p->labels, pic_obj_ptr(pair->cdr))) && xh_val(e, int) != -1) {
      xfprintf(p->file, " . ");

      if ((xh_get(&p->visited, pic_obj_ptr(pair->cdr)))) {
        xfprintf(p->file, "#%d#", xh_val(e, int));
        return;
      }
      else {
        xfprintf(p->file, "#%d=", xh_val(e, int));
        c = 1;
        xh_put(&p->visited, pic_obj_ptr(pair->cdr), &c);
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

  UNUSED(pic);

  for (i = 0; i < pic_strlen(str); ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      xfputc('\\', file);
    }
    xfputc(cstr[i], file);
  }
}

static void
write_core(struct writer_control *p, pic_value obj)
{
  pic_state *pic = p->pic;
  xFILE *file = p->file;
  size_t i;
  xh_entry *e;
  int c;

  /* shared objects */
  if (pic_vtype(obj) == PIC_VTYPE_HEAP
      && (e = xh_get(&p->labels, pic_obj_ptr(obj)))
      && xh_val(e, int) != -1) {
    if ((xh_get(&p->visited, pic_obj_ptr(obj)))) {
      xfprintf(file, "#%d#", xh_val(e, int));
      return;
    }
    else {
      xfprintf(file, "#%d=", xh_val(e, int));
      c = 1;
      xh_put(&p->visited, pic_obj_ptr(obj), &c);
    }
  }

  switch (pic_type(obj)) {
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
  case PIC_TT_INT:
  case PIC_TT_BIGINT:
  case PIC_TT_RATIONAL:
  case PIC_TT_BIGFLOAT:
    xfprintf(file, "%s", pic_str_cstr(pic_str_ptr(pic_number_to_string(pic, obj, 10))));
    break;
  case PIC_TT_EOF:
    xfprintf(file, "#<eof-object>");
    break;
  case PIC_TT_UNDEF:
    xfprintf(file, "#<undef>");
    break;
  case PIC_TT_PROC:
    xfprintf(file, "#<proc %p>", pic_ptr(obj));
    break;
  case PIC_TT_PORT:
    xfprintf(file, "#<port %p>", pic_ptr(obj));
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
  case PIC_TT_ERROR:
    xfprintf(file, "#<error %p>", pic_ptr(obj));
    break;
  case PIC_TT_ENV:
    xfprintf(file, "#<env %p>", pic_ptr(obj));
    break;
  case PIC_TT_CONT:
    xfprintf(file, "#<cont %p>", pic_ptr(obj));
    break;
  case PIC_TT_SENV:
    xfprintf(file, "#<senv %p>", pic_ptr(obj));
    break;
  case PIC_TT_MACRO:
    xfprintf(file, "#<macro %p>", pic_ptr(obj));
    break;
  case PIC_TT_SC:
    xfprintf(file, "#<sc %p: ", pic_ptr(obj));
    write_core(p, pic_sc_ptr(obj)->expr);
    xfprintf(file, ">");
    break;
  case PIC_TT_LIB:
    xfprintf(file, "#<lib %p>", pic_ptr(obj));
    break;
  case PIC_TT_VAR:
    xfprintf(file, "#<var %p>", pic_ptr(obj));
    break;
  case PIC_TT_IREP:
    xfprintf(file, "#<irep %p>", pic_ptr(obj));
    break;
  case PIC_TT_DATA:
    xfprintf(file, "#<data %p>", pic_ptr(obj));
    break;
  case PIC_TT_BOX:
    xfprintf(file, "#<box %p>", pic_ptr(obj));
    break;
  case PIC_TT_DICT:
    xfprintf(file, "#<dict %p>", pic_ptr(obj));
    break;
  }
}

static void
write(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file, WRITE_MODE);

  traverse_shared(p, obj);      /* FIXME */

  write_core(p, obj);

  writer_control_destroy(p);
}

static void
write_simple(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file, WRITE_MODE);

  /* no traverse here! */

  write_core(p, obj);

  writer_control_destroy(p);
}

static void
write_shared(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file, WRITE_MODE);

  traverse_shared(p, obj);

  write_core(p, obj);

  writer_control_destroy(p);
}

static void
display(pic_state *pic, pic_value obj, xFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file, DISPLAY_MODE);

  traverse_shared(p, obj);      /* FIXME */

  write_core(p, obj);

  writer_control_destroy(p);
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

  str = pic_str_ptr(pic_car(pic, pic_vformat(pic, fmt, ap)));

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
  pic_deflibrary ("(scheme write)") {
    pic_defun(pic, "write", pic_write_write);
    pic_defun(pic, "write-simple", pic_write_write_simple);
    pic_defun(pic, "write-shared", pic_write_write_shared);
    pic_defun(pic, "display", pic_write_display);
  }
}
