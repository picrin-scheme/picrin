/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/string.h"
#include "picrin/blob.h"

pic_value
pic_eof_object()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_EOF);

  return v;
}

struct pic_port *
pic_stdin(pic_state *pic)
{
  pic_value obj;

  obj = pic_funcall(pic, pic->PICRIN_BASE, "current-input-port", pic_nil_value());

  return pic_port_ptr(obj);
}

struct pic_port *
pic_stdout(pic_state *pic)
{
  pic_value obj;

  obj = pic_funcall(pic, pic->PICRIN_BASE, "current-output-port", pic_nil_value());

  return pic_port_ptr(obj);
}

struct pic_port *
pic_make_standard_port(pic_state *pic, xFILE *file, short dir)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = file;
  port->flags = dir | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;
  return port;
}

struct pic_port *
pic_open_input_string(pic_state *pic, const char *str)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port *), PIC_TT_PORT);
  port->file = xmopen();
  port->flags = PIC_PORT_IN | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;

  xfputs(str, port->file);
  xfflush(port->file);
  xrewind(port->file);

  return port;
}

struct pic_port *
pic_open_output_string(pic_state *pic)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port *), PIC_TT_PORT);
  port->file = xmopen();
  port->flags = PIC_PORT_OUT | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;

  return port;
}

struct pic_string *
pic_get_output_string(pic_state *pic, struct pic_port *port)
{
  size_t size;
  char *buf;

  /* get endpos */
  xfflush(port->file);
  size = (size_t)xftell(port->file);
  xrewind(port->file);

  /* copy to buf */
  buf = (char *)pic_alloc(pic, size + 1);
  buf[size] = 0;
  xfread(buf, size, 1, port->file);

  return pic_make_str(pic, buf, size);
}

void
pic_close_port(pic_state *pic, struct pic_port *port)
{
  if (xfclose(port->file) == EOF) {
    pic_errorf(pic, "close-port: failure");
  }
  port->status = PIC_PORT_CLOSE;
}

static pic_value
pic_port_call_with_port(pic_state *pic)
{
  struct pic_port *port;
  struct pic_proc *proc;
  pic_value value;

  pic_get_args(pic, "pl", &port, &proc);

  value = pic_apply1(pic, proc, pic_obj_value(port));

  pic_close_port(pic, port);

  return value;
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
  pic_get_args(pic, "");

  return pic_eof_object();
}

static pic_value
pic_port_port_open_p(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  return pic_bool_value(port->status == PIC_PORT_OPEN);
}

static pic_value
pic_port_close_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  pic_close_port(pic, port);

  return pic_none_value();
}

#define assert_port_profile(port, flgs, stat, caller) do {              \
    if ((port->flags & (flgs)) != (flgs)) {                             \
      switch (flgs) {                                                   \
      case PIC_PORT_IN:                                                 \
        pic_errorf(pic, caller ": expected output port");               \
      case PIC_PORT_OUT:                                                \
        pic_errorf(pic, caller ": expected input port");                \
      case PIC_PORT_IN | PIC_PORT_TEXT:                                 \
        pic_errorf(pic, caller ": expected input/textual port");        \
      case PIC_PORT_IN | PIC_PORT_BINARY:                               \
        pic_errorf(pic, caller ": expected input/binary port");         \
      case PIC_PORT_OUT | PIC_PORT_TEXT:                                \
        pic_errorf(pic, caller ": expected output/textual port");       \
      case PIC_PORT_OUT | PIC_PORT_BINARY:                              \
        pic_errorf(pic, caller ": expected output/binary port");        \
      }                                                                 \
    }                                                                   \
    if (port->status != stat) {                                         \
      switch (stat) {                                                   \
      case PIC_PORT_OPEN:                                               \
        pic_errorf(pic, caller ": expected open port");                 \
      case PIC_PORT_CLOSE:                                              \
        pic_errorf(pic, caller ": expected close port");                \
      }                                                                 \
    }                                                                   \
  } while (0)

static pic_value
pic_port_open_input_string(pic_state *pic)
{
  struct pic_port *port;
  char *str;

  pic_get_args(pic, "z", &str);

  port = pic_open_input_string(pic, str);

  return pic_obj_value(port);
}

static pic_value
pic_port_open_output_string(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "");

  port = pic_open_output_string(pic);

  return pic_obj_value(port);
}

static pic_value
pic_port_get_output_string(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, PIC_PORT_OPEN, "get-output-string");

  return pic_obj_value(pic_get_output_string(pic, port));
}

static pic_value
pic_port_open_input_blob(pic_state *pic)
{
  struct pic_port *port;
  struct pic_blob *blob;

  pic_get_args(pic, "b", &blob);

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port *), PIC_TT_PORT);
  port->file = xmopen();
  port->flags = PIC_PORT_IN | PIC_PORT_BINARY;
  port->status = PIC_PORT_OPEN;

  xfwrite(blob->data, 1, blob->len, port->file);
  xfflush(port->file);
  xrewind(port->file);

  return pic_obj_value(port);
}

static pic_value
pic_port_open_output_bytevector(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "");

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port *), PIC_TT_PORT);
  port->file = xmopen();
  port->flags = PIC_PORT_OUT | PIC_PORT_BINARY;
  port->status = PIC_PORT_OPEN;

  return pic_obj_value(port);
}

static pic_value
pic_port_get_output_bytevector(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);
  pic_blob *blob;
  size_t size;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, PIC_PORT_OPEN, "get-output-bytevector");

  /* get endpos */
  xfflush(port->file);
  size = (size_t)xftell(port->file);
  xrewind(port->file);

  /* copy to buf */
  blob = pic_make_blob(pic, size);
  xfread(blob->data, 1, size, port->file);

  return pic_obj_value(blob);
}

static pic_value
pic_port_read_char(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "read-char");

  if ((c = xfgetc(port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    return pic_char_value((char)c);
  }
}

static pic_value
pic_port_peek_char(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "peek-char");

  if ((c = xfgetc(port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    xungetc(c, port->file);
    return pic_char_value((char)c);
  }
}

static pic_value
pic_port_read_line(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic), *buf;
  struct pic_string *str;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "read-line");

  buf = pic_open_output_string(pic);
  while ((c = xfgetc(port->file)) != EOF && c != '\n') {
    xfputc(c, buf->file);
  }

  str = pic_get_output_string(pic, buf);
  if (pic_strlen(str) == 0 && c == EOF) {
    return pic_eof_object();
  }
  else {
    return pic_obj_value(str);
  }
}

static pic_value
pic_port_char_ready_p(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "char-ready?");

  pic_get_args(pic, "|p", &port);

  return pic_true_value();      /* FIXME: always returns #t */
}

static pic_value
pic_port_read_string(pic_state *pic){
  struct pic_port *port = pic_stdin(pic), *buf;
  pic_str *str;
  int k, i;
  int c;

  pic_get_args(pic, "i|p", &k,  &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "read-stritg");

  c = EOF;
  buf = pic_open_output_string(pic);
  for(i = 0; i < k; ++i) {
    if((c = xfgetc(port->file)) == EOF){
      break;
    }
    xfputc(c, buf->file);
  }

  str = pic_get_output_string(pic, buf);
  if (pic_strlen(str) == 0 && c == EOF) {
    return pic_eof_object();
  }
  else {
    return pic_obj_value(str);
  }

}

static pic_value
pic_port_read_byte(pic_state *pic){
  struct pic_port *port = pic_stdin(pic);
  int c;
  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "read-u8");
  if ((c = xfgetc(port->file)) == EOF) {
    return pic_eof_object();
  }

  return pic_int_value(c);
}

static pic_value
pic_port_peek_byte(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "peek-u8");

  c = xfgetc(port->file);
  if (c == EOF) {
    return pic_eof_object();
  }
  else {
    xungetc(c, port->file);
    return pic_int_value(c);
  }
}

static pic_value
pic_port_byte_ready_p(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "u8-ready?");

  return pic_true_value();      /* FIXME: always returns #t */
}


static pic_value
pic_port_read_blob(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);
  pic_blob *blob;
  size_t k, i;

  pic_get_args(pic, "k|p", &k, &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "read-bytevector");

  blob = pic_make_blob(pic, k);

  i = xfread(blob->data, sizeof(char), k, port->file);
  if (i == 0) {
    return pic_eof_object();
  }
  else {
    pic_realloc(pic, blob->data, i);
    blob->len = i;
    return pic_obj_value(blob);
  }
}

static pic_value
pic_port_read_blob_ip(pic_state *pic)
{
  struct pic_port *port;
  struct pic_blob *bv;
  int n;
  char *buf;
  size_t start, end, i, len;

  n = pic_get_args(pic, "b|pkk", &bv, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdin(pic);
  case 2:
    start = 0;
  case 3:
    end = bv->len;
  }

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "read-bytevector!");

  if (end < start) {
    pic_errorf(pic, "read-bytevector!: end index must be greater than or equal to start index");
  }

  len = end - start;

  buf = pic_calloc(pic, len, sizeof(char));
  i = xfread(buf, sizeof(char), len, port->file);
  memcpy(bv->data + start, buf, i);
  pic_free(pic, buf);

  if (i == 0) {
    return pic_eof_object();
  }
  else {
    return pic_size_value(i);
  }
}

static pic_value
pic_port_newline(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, PIC_PORT_OPEN, "newline");

  xfputs("\n", port->file);
  return pic_none_value();
}

static pic_value
pic_port_write_char(pic_state *pic)
{
  char c;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "c|p", &c, &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, PIC_PORT_OPEN, "write-char");

  xfputc(c, port->file);
  return pic_none_value();
}

static pic_value
pic_port_write_string(pic_state *pic)
{
  char *str;
  struct pic_port *port;
  int start, end, n, i;

  n = pic_get_args(pic, "z|pii", &str, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdout(pic);
  case 2:
    start = 0;
  case 3:
    end = INT_MAX;
  }

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, PIC_PORT_OPEN, "write-string");

  for (i = start; i < end && str[i] != '\0'; ++i) {
    xfputc(str[i], port->file);
  }
  return pic_none_value();
}

static pic_value
pic_port_write_byte(pic_state *pic)
{
  int i;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "i|p", &i, &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, PIC_PORT_OPEN, "write-u8");

  xfputc(i, port->file);
  return pic_none_value();
}

static pic_value
pic_port_write_blob(pic_state *pic)
{
  struct pic_blob *blob;
  struct pic_port *port;
  int n;
  size_t start, end, i;

  n = pic_get_args(pic, "b|pkk", &blob, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdout(pic);
  case 2:
    start = 0;
  case 3:
    end = blob->len;
  }

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, PIC_PORT_OPEN, "write-bytevector");

  for (i = start; i < end; ++i) {
    xfputc(blob->data[i], port->file);
  }
  return pic_none_value();
}

static pic_value
pic_port_flush(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT, PIC_PORT_OPEN, "flush-output-port");

  xfflush(port->file);
  return pic_none_value();
}

void
pic_init_port(pic_state *pic)
{
  pic_defvar(pic, "current-input-port", pic_obj_value(pic->xSTDIN), NULL);
  pic_defvar(pic, "current-output-port", pic_obj_value(pic->xSTDOUT), NULL);
  pic_defvar(pic, "current-error-port", pic_obj_value(pic->xSTDERR), NULL);

  pic_defun(pic, "call-with-port", pic_port_call_with_port);

  pic_defun(pic, "input-port?", pic_port_input_port_p);
  pic_defun(pic, "output-port?", pic_port_output_port_p);
  pic_defun(pic, "textual-port?", pic_port_textual_port_p);
  pic_defun(pic, "binary-port?", pic_port_binary_port_p);
  pic_defun(pic, "port?", pic_port_port_p);

  pic_defun(pic, "port-open?", pic_port_port_open_p);
  pic_defun(pic, "close-port", pic_port_close_port);

  /* string I/O */
  pic_defun(pic, "open-input-string", pic_port_open_input_string);
  pic_defun(pic, "open-output-string", pic_port_open_output_string);
  pic_defun(pic, "get-output-string", pic_port_get_output_string);
  pic_defun(pic, "open-input-bytevector", pic_port_open_input_blob);
  pic_defun(pic, "open-output-bytevector", pic_port_open_output_bytevector);
  pic_defun(pic, "get-output-bytevector", pic_port_get_output_bytevector);

  /* input */
  pic_defun(pic, "read-char", pic_port_read_char);
  pic_defun(pic, "peek-char", pic_port_peek_char);
  pic_defun(pic, "read-line", pic_port_read_line);
  pic_defun(pic, "eof-object?", pic_port_eof_object_p);
  pic_defun(pic, "eof-object", pic_port_eof_object);
  pic_defun(pic, "char-ready?", pic_port_char_ready_p);
  pic_defun(pic, "read-string", pic_port_read_string);
  pic_defun(pic, "read-u8", pic_port_read_byte);
  pic_defun(pic, "peek-u8", pic_port_peek_byte);
  pic_defun(pic, "u8-ready?", pic_port_byte_ready_p);
  pic_defun(pic, "read-bytevector", pic_port_read_blob);
  pic_defun(pic, "read-bytevector!", pic_port_read_blob_ip);

  /* output */
  pic_defun(pic, "newline", pic_port_newline);
  pic_defun(pic, "write-char", pic_port_write_char);
  pic_defun(pic, "write-string", pic_port_write_string);
  pic_defun(pic, "write-u8", pic_port_write_byte);
  pic_defun(pic, "write-bytevector", pic_port_write_blob);
  pic_defun(pic, "flush-output-port", pic_port_flush);
}
