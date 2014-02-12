/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/port.h"
#include "picrin/pair.h"
#include "picrin/blob.h"
#include "picrin/macro.h"

static void write(pic_state *, pic_value, XFILE *file);

static bool
is_quote(pic_state *pic, pic_value pair)
{
  return pic_list_p(pic, pair)
    && pic_length(pic, pair) == 2
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sQUOTE));
}

static bool
is_unquote(pic_state *pic, pic_value pair)
{
  return pic_list_p(pic, pair)
    && pic_length(pic, pair) == 2
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sUNQUOTE));
}

static bool
is_unquote_splicing(pic_state *pic, pic_value pair)
{
  return pic_list_p(pic, pair)
    && pic_length(pic, pair) == 2
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sUNQUOTE_SPLICING));
}

static bool
is_quasiquote(pic_state *pic, pic_value pair)
{
  return pic_list_p(pic, pair)
    && pic_length(pic, pair) == 2
    && pic_eq_p(pic_car(pic, pair), pic_symbol_value(pic->sQUASIQUOTE));
}

static void
write_pair(pic_state *pic, struct pic_pair *pair, XFILE *file)
{
  write(pic, pair->car, file);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  if (pic_pair_p(pair->cdr)) {
    xfprintf(file, " ");
    write_pair(pic, pic_pair_ptr(pair->cdr), file);
    return;
  }
  xfprintf(file, " . ");
  write(pic, pair->cdr, file);
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
write(pic_state *pic, pic_value obj, XFILE *file)
{
  size_t i;

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
      write(pic, pic_list_ref(pic, obj, 1), file);
      break;
    }
    else if (is_unquote(pic, obj)) {
      xfprintf(file, ",");
      write(pic, pic_list_ref(pic, obj, 1), file);
      break;
    }
    else if (is_unquote_splicing(pic, obj)) {
      xfprintf(file, ",@");
      write(pic, pic_list_ref(pic, obj, 1), file);
      break;
    }
    else if (is_quasiquote(pic, obj)) {
      xfprintf(file, "`");
      write(pic, pic_list_ref(pic, obj, 1), file);
      break;
    }
    xfprintf(file, "(");
    write_pair(pic, pic_pair_ptr(obj), file);
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
      write(pic, pic_vec_ptr(obj)->data[i], file);
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
    write(pic, pic_sc(obj)->expr, file);
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

pic_value
pic_debug(pic_state *pic, pic_value obj)
{
  return pic_fdebug(pic, obj, xstdout);
}

pic_value
pic_fdebug(pic_state *pic, pic_value obj, XFILE *file)
{
  write(pic, obj, file);
  xfflush(file);
  return obj;
}

static pic_value
pic_port_write_simple(pic_state *pic)
{
  pic_value v;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "o|p", &v, &port);
  write(pic, v, port->file);
  return pic_none_value();
}

void
pic_init_write(pic_state *pic)
{
  pic_deflibrary ("(scheme write)") {
    pic_defun(pic, "write-simple", pic_port_write_simple);
  }
}
