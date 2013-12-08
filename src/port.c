#include <stdio.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/blob.h"
#include "picrin/macro.h"

static void write_pair(pic_state *pic, struct pic_pair *pair);
static void write_str(pic_state *pic, struct pic_string *str);

static void
write(pic_state *pic, pic_value obj)
{
  int i;

  switch (pic_type(obj)) {
  case PIC_TT_NIL:
    printf("()");
    break;
  case PIC_TT_BOOL:
    if (pic_true_p(obj))
      printf("#t");
    else
      printf("#f");
    break;
  case PIC_TT_PAIR:
    printf("(");
    write_pair(pic, pic_pair_ptr(obj));
    printf(")");
    break;
  case PIC_TT_SYMBOL:
    printf("%s", pic_symbol_name(pic, pic_sym(obj)));
    break;
  case PIC_TT_CHAR:
    switch (pic_char(obj)) {
    default: printf("#\\%c", pic_char(obj)); break;
    case '\a': printf("#\\alarm"); break;
    case '\b': printf("#\\backspace"); break;
    case 0x7f: printf("#\\delete"); break;
    case 0x1b: printf("#\\escape"); break;
    case '\n': printf("#\\newline"); break;
    case '\r': printf("#\\return"); break;
    case ' ': printf("#\\space"); break;
    case '\t': printf("#\\tab"); break;
    }
    break;
  case PIC_TT_FLOAT:
    printf("%f", pic_float(obj));
    break;
  case PIC_TT_INT:
    printf("%d", pic_int(obj));
    break;
  case PIC_TT_EOF:
    printf("#<eof-object>");
    break;
  case PIC_TT_UNDEF:
    printf("#<undef>");
    break;
  case PIC_TT_PROC:
    printf("#<proc %p>", pic_proc_ptr(obj));
    break;
  case PIC_TT_PORT:
    printf("#<port %p>", pic_port_ptr(obj));
    break;
  case PIC_TT_STRING:
    printf("\"");
    write_str(pic, pic_str_ptr(obj));
    printf("\"");
    break;
  case PIC_TT_VECTOR:
    printf("#(");
    for (i = 0; i < pic_vec_ptr(obj)->len; ++i) {
      write(pic, pic_vec_ptr(obj)->data[i]);
      if (i + 1 < pic_vec_ptr(obj)->len) {
	printf(" ");
      }
    }
    printf(")");
    break;
  case PIC_TT_BLOB:
    printf("#u8(");
    for (i = 0; i < pic_blob_ptr(obj)->len; ++i) {
      printf("%d", pic_blob_ptr(obj)->data[i]);
      if (i + 1 < pic_blob_ptr(obj)->len) {
	printf(" ");
      }
    }
    printf(")");
    break;
  case PIC_TT_ERROR:
    printf("#<error %p>", pic_ptr(obj));
    break;
  case PIC_TT_ENV:
    printf("#<env %p>", pic_env_ptr(obj));
    break;
  case PIC_TT_CONT:
    printf("#<cont %p>", pic_ptr(obj));
    break;
  case PIC_TT_SENV:
    printf("#<senv %p>", pic_ptr(obj));
    break;
  case PIC_TT_SYNTAX:
    printf("#<senv %p>", pic_ptr(obj));
    break;
  case PIC_TT_SC:
    printf("#<sc %p: ", pic_ptr(obj));
    write(pic, pic_sc(obj)->expr);
    printf(">");
    break;
  case PIC_TT_LIB:
    printf("#<library %p>", pic_ptr(obj));
    break;
  }
}

static void
write_pair(pic_state *pic, struct pic_pair *pair)
{
  write(pic, pair->car);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  if (pic_pair_p(pair->cdr)) {
    printf(" ");
    write_pair(pic, pic_pair_ptr(pair->cdr));
    return;
  }
  printf(" . ");
  write(pic, pair->cdr);
}

static void
write_str(pic_state *pic, struct pic_string *str)
{
  int i;
  const char *cstr = str->str;

  for (i = 0; i < str->len; ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      putchar('\\');
    }
    putchar(cstr[i]);
  }
}

void
pic_debug(pic_state *pic, pic_value obj)
{
  write(pic, obj);
}

static pic_value
pic_port_input_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(v) && (pic_port_ptr(v)->flags & PIC_PORT_IN) != 0) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_port_output_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(v) && (pic_port_ptr(v)->flags & PIC_PORT_OUT) != 0) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_port_textual_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(v) && (pic_port_ptr(v)->flags & PIC_PORT_TEXT) != 0) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_port_binary_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(v) && (pic_port_ptr(v)->flags & PIC_PORT_BINARY) != 0) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_port_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_port_p(v));
}

static pic_value
pic_port_input_port_open_p(pic_state *pic)
{
  pic_value v;
  struct pic_port *port;

  pic_get_args(pic, "o", &v);

  if (! pic_port_p(v))
    return pic_false_value();
  port = pic_port_ptr(v);
  if ((port->flags & PIC_PORT_IN) == 0)
    return pic_false_value();

  return pic_bool_value(port->status == PIC_PORT_OPEN);
}

static pic_value
pic_port_output_port_open_p(pic_state *pic)
{
  pic_value v;
  struct pic_port *port;

  pic_get_args(pic, "o", &v);

  if (! pic_port_p(v))
    return pic_false_value();
  port = pic_port_ptr(v);
  if ((port->flags & PIC_PORT_OUT) == 0)
    return pic_false_value();

  return pic_bool_value(port->status == PIC_PORT_OPEN);
}

static pic_value
pic_port_write(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);
  write(pic, v);
  return pic_false_value();
}

static pic_value
pic_port_newline(pic_state *pic)
{
  puts("");
  return pic_false_value();
}

static pic_value
pic_port_eof_object_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_vtype(v) == PIC_VTYPE_EOF) {
    return pic_true_value();
  }
  else {
    return pic_false_value();
  }
}

static pic_value
pic_port_eof_object(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "");

  pic_init_value(v, PIC_VTYPE_EOF);

  return v;
}

pic_value
pic_port_close_port(pic_state *pic)
{
  pic_value v;
  struct pic_port *port;

  pic_get_args(pic, "o", &v);

  if (! pic_port_p(v)) {
    pic_error(pic, "close-port: expected port");
  }
  port = pic_port_ptr(v);

  if (fclose(port->file) == EOF) {
    pic_error(pic, "close-port: failure");
  }
  port->status = PIC_PORT_CLOSE;

  return pic_false_value();
}

void
pic_init_port(pic_state *pic)
{
  pic_defun(pic, "input-port?", pic_port_input_port_p);
  pic_defun(pic, "output-port?", pic_port_output_port_p);
  pic_defun(pic, "textual-port?", pic_port_textual_port_p);
  pic_defun(pic, "binary-port?", pic_port_binary_port_p);
  pic_defun(pic, "port?", pic_port_port_p);
  pic_defun(pic, "input-port-open?", pic_port_input_port_open_p);
  pic_defun(pic, "output-port-open?", pic_port_output_port_open_p);
  pic_defun(pic, "eof-object?", pic_port_eof_object_p);
  pic_defun(pic, "eof-object", pic_port_eof_object);
  pic_defun(pic, "close-port", pic_port_close_port);
  pic_defun(pic, "close-input-port", pic_port_close_port);
  pic_defun(pic, "close-output-port", pic_port_close_port);

  DEFLIBRARY(pic, "(scheme write)")
  {
    pic_defun(pic, "write", pic_port_write);
    pic_defun(pic, "newline", pic_port_newline);
  }
  ENDLIBRARY(pic)
}
