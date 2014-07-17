/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>
#include <string.h>
#include <limits.h>

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
  struct pic_proc *proc;

  proc = pic_proc_ptr(pic_ref(pic, "current-input-port"));

  return pic_port_ptr(pic_apply(pic, proc, pic_nil_value()));
}

struct pic_port *
pic_stdout(pic_state *pic)
{
  struct pic_proc *proc;

  proc = pic_proc_ptr(pic_ref(pic, "current-output-port"));

  return pic_port_ptr(pic_apply(pic, proc, pic_nil_value()));
}

static pic_value
port_new_stdport(pic_state *pic, xFILE *file, short dir)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = file;
  port->flags = dir | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;
  return pic_obj_value(port);
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
  long size;
  char *buf;

  /* get endpos */
  xfflush(port->file);
  size = xftell(port->file);
  xrewind(port->file);

  /* copy to buf */
  buf = (char *)pic_alloc(pic, size + 1);
  buf[size] = 0;
  xfread(buf, size, 1, port->file);

  return pic_str_new(pic, buf, size);
}

void
pic_close_port(pic_state *pic, struct pic_port *port)
{
  if (xfclose(port->file) == EOF) {
    pic_error(pic, "close-port: failure");
  }
  port->status = PIC_PORT_CLOSE;
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
pic_port_seekable_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(v) && pic_port_ptr(v)->file->vtable.seek != NULL) {
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
pic_port_close_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  pic_close_port(pic, port);

  return pic_none_value();
}

static pic_value
pic_port_seek_port(pic_state *pic)
{
  int i;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "pi", &port, &i);

  xfseek(port->file, (long) i, SEEK_SET);

  return pic_nil_value();
}

static pic_value
pic_port_current_seeker_position(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "p", &port);

  return pic_int_value(xftell(port->file));
}

#define assert_port_profile(port, flgs, stat, caller) do {              \
    if ((port->flags & (flgs)) != (flgs)) {                             \
      switch (flgs) {                                                   \
      case PIC_PORT_IN:                                                 \
        pic_error(pic, caller ": expected output port");                \
      case PIC_PORT_OUT:                                                \
        pic_error(pic, caller ": expected input port");                 \
      case PIC_PORT_IN | PIC_PORT_TEXT:                                 \
        pic_error(pic, caller ": expected input/textual port");         \
      case PIC_PORT_IN | PIC_PORT_BINARY:                               \
        pic_error(pic, caller ": expected input/binary port");          \
      case PIC_PORT_OUT | PIC_PORT_TEXT:                                \
        pic_error(pic, caller ": expected output/textual port");        \
      case PIC_PORT_OUT | PIC_PORT_BINARY:                              \
        pic_error(pic, caller ": expected output/binary port");         \
      }                                                                 \
    }                                                                   \
    if (port->status != stat) {                                         \
      switch (stat) {                                                   \
      case PIC_PORT_OPEN:                                               \
        pic_error(pic, caller ": expected open port");                  \
      case PIC_PORT_CLOSE:                                              \
        pic_error(pic, caller ": expected close port");                 \
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
  long endpos;
  char *buf;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, PIC_PORT_OPEN, "get-output-bytevector");

  /* get endpos */
  xfflush(port->file);
  endpos = xftell(port->file);
  xrewind(port->file);

  /* copy to buf */
  buf = (char *)pic_alloc(pic, endpos);
  xfread(buf, 1, endpos, port->file);

  return pic_obj_value(pic_blob_new(pic, buf, endpos));
}

static pic_value
pic_port_read_char(pic_state *pic)
{
  char c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "read-char");

  if ((c = xfgetc(port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    return pic_char_value(c);
  }
}

static pic_value
pic_port_peek_char(pic_state *pic)
{
  char c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "peek-char");

  if ((c = xfgetc(port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    xungetc(c, port->file);
    return pic_char_value(c);
  }
}

static pic_value
pic_port_read_line(pic_state *pic)
{
  char c;
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
  char c;

  pic_get_args(pic, "i|p", &k,  &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, PIC_PORT_OPEN, "read-stritg");

  buf = pic_open_output_string(pic);
  for(i = 0; i < k; ++i) {
    c = xfgetc(port->file);
    if( c == EOF){
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
  char c;
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
  char c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "peek-u8");

  if ((c = xfgetc(port->file)) == EOF) {
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
pic_port_read_blob(pic_state *pic){
  struct pic_port *port = pic_stdin(pic);
  int k, i;
  char *buf;

  pic_get_args(pic, "i|p", &k,  &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "read-bytevector");

  buf = pic_calloc(pic, k, sizeof(char));
  i = xfread(buf, sizeof(char), k, port->file);
  if ( i == 0 ) {
    return pic_eof_object();
  }
  else {
    pic_realloc(pic, buf, i);
    return pic_obj_value(pic_blob_new(pic, buf, i));
  }
}

static pic_value
pic_port_read_blob_ip(pic_state *pic){
  struct pic_port *port;
  struct pic_blob *bv;
  int i, n, start, end, len;
  char *buf;

  n = pic_get_args(pic, "b|pii", &bv, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdin(pic);
  case 2:
    start = 0;
  case 3:
    end = bv->len;
  }

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, PIC_PORT_OPEN, "read-bytevector!");
  len = end - start;

  buf = pic_calloc(pic, len, sizeof(char));
  i = xfread(buf, sizeof(char), len, port->file);
  memcpy(bv->data + start, buf, i);
  pic_free(pic, buf);
  
  if ( i == 0) {
    return pic_eof_object();
  }
  else {
    return pic_int_value(i);
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
  int start, end, n, i;

  n = pic_get_args(pic, "b|pii", &blob, &port, &start, &end);
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
  pic_deflibrary ("(picrin port)") {
    pic_define(pic, "standard-input-port", port_new_stdport(pic, xstdin, PIC_PORT_IN));
    pic_define(pic, "standard-output-port", port_new_stdport(pic, xstdout, PIC_PORT_OUT));
    pic_define(pic, "standard-error-port", port_new_stdport(pic, xstderr, PIC_PORT_OUT));
  }

  pic_deflibrary ("(picrin port seek)"){
    pic_defun(pic, "seek-port", pic_port_seek_port);
    pic_defun(pic, "port-seekable?", pic_port_seekable_port_p);
    pic_defun(pic, "port-current-seeker-position", pic_port_current_seeker_position);
  }

  pic_defun(pic, "input-port?", pic_port_input_port_p);
  pic_defun(pic, "output-port?", pic_port_output_port_p);
  pic_defun(pic, "textual-port?", pic_port_textual_port_p);
  pic_defun(pic, "binary-port?", pic_port_binary_port_p);
  pic_defun(pic, "port?", pic_port_port_p);
  pic_defun(pic, "input-port-open?", pic_port_input_port_open_p);
  pic_defun(pic, "output-port-open?", pic_port_output_port_open_p);
  pic_defun(pic, "close-port", pic_port_close_port);
  pic_defun(pic, "close-input-port", pic_port_close_port);
  pic_defun(pic, "close-output-port", pic_port_close_port);

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
