/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/port.h"
#include "picrin/pair.h"
#include "picrin/blob.h"
#include "picrin/macro.h"

static bool
is_quote(pic_state *pic, pic_value pair)
{
  return pic_pair_p(pic_cdr(pic, pair))
    && pic_nil_p(pic_cddr(pic, pair))
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sQUOTE));
}

static bool
is_unquote(pic_state *pic, pic_value pair)
{
  return pic_pair_p(pic_cdr(pic, pair))
    && pic_nil_p(pic_cddr(pic, pair))
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sUNQUOTE));
}

static bool
is_unquote_splicing(pic_state *pic, pic_value pair)
{
  return pic_pair_p(pic_cdr(pic, pair))
    && pic_nil_p(pic_cddr(pic, pair))
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sUNQUOTE_SPLICING));
}

static bool
is_quasiquote(pic_state *pic, pic_value pair)
{
  return pic_pair_p(pic_cdr(pic, pair))
    && pic_nil_p(pic_cddr(pic, pair))
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sQUASIQUOTE));
}

struct writer_control {
  pic_state *pic;
  XFILE *file;
  xhash *labels;
  xhash *visited;
  int cnt;
};

static struct writer_control *
writer_control_new(pic_state *pic, XFILE *file)
{
  struct writer_control *p;

  p = (struct writer_control *)pic_alloc(pic, sizeof(struct writer_control));
  p->pic = pic;
  p->file = file;
  p->labels = xh_new_ptr();
  p->visited = xh_new_ptr();
  p->cnt = 0;
  return p;
}

static void
traverse_shared(struct writer_control *p, pic_value obj)
{
  xh_entry *e;
  size_t i;

  switch (pic_type(obj)) {
  case PIC_TT_PAIR:
  case PIC_TT_VECTOR:
    e = xh_get(p->labels, pic_obj_ptr(obj));
    if (e == NULL) {
      xh_put(p->labels, pic_obj_ptr(obj), -1);
    }
    else if (e->val == -1) {
      xh_put(p->labels, pic_obj_ptr(obj), p->cnt++);
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

  write_core(p, pair->car);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  else if (pic_pair_p(pair->cdr)) {

    /* shared objects */
    if ((e = xh_get(p->labels, pic_obj_ptr(pair->cdr))) && e->val != -1) {
      xfprintf(p->file, " . ");

      if ((xh_get(p->visited, pic_obj_ptr(pair->cdr)))) {
        xfprintf(p->file, "#%d#", e->val);
        return;
      }
      else {
        xfprintf(p->file, "#%d=", e->val);
        xh_put(p->visited, pic_obj_ptr(pair->cdr), 1);
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
write_str(pic_state *pic, struct pic_string *str, XFILE *file)
{
  size_t i;
  const char *cstr = str->str;

  UNUSED(pic);

  for (i = 0; i < str->len; ++i) {
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
  XFILE *file = p->file;
  size_t i;
  xh_entry *e;

  /* shared objects */
  if (pic_vtype(obj) == PIC_VTYPE_HEAP
      && (e = xh_get(p->labels, pic_obj_ptr(obj)))
      && e->val != -1) {
    if ((xh_get(p->visited, pic_obj_ptr(obj)))) {
      xfprintf(file, "#%d#", e->val);
      return;
    }
    else {
      xfprintf(file, "#%d=", e->val);
      xh_put(p->visited, pic_obj_ptr(obj), 1);
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
    xfprintf(file, "%f", pic_float(obj));
    break;
  case PIC_TT_INT:
    xfprintf(file, "%d", pic_int(obj));
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
    write_core(p, pic_sc(obj)->expr);
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
  }
}

static void
write_simple(pic_state *pic, pic_value obj, XFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file);

  /* no traverse here! */

  write_core(p, obj);
}

static void
write_shared(pic_state *pic, pic_value obj, XFILE *file)
{
  struct writer_control *p;

  p = writer_control_new(pic, file);

  traverse_shared(p, obj);

  write_core(p, obj);
}

pic_value
pic_debug(pic_state *pic, pic_value obj)
{
  return pic_fdebug(pic, obj, xstdout);
}

pic_value
pic_fdebug(pic_state *pic, pic_value obj, XFILE *file)
{
  write_shared(pic, obj, file);
  xfflush(file);
  return obj;
}

static pic_value
pic_port_write_simple(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_simple(pic, v, port->file);
  return pic_none_value();
}

static pic_value
pic_port_write_shared(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write_shared(pic, v, port->file);
  return pic_none_value();
}

void
pic_init_write(pic_state *pic)
{
  pic_deflibrary ("(scheme write)") {
    pic_defun(pic, "write-simple", pic_port_write_simple);
    pic_defun(pic, "write-shared", pic_port_write_shared);
  }
}
