/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

#undef EOF
#define EOF (-1)

struct pic_port *
pic_make_port(pic_state *pic, xFILE *file)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TYPE_PORT);
  port->file = file;
  return port;
}

void
pic_close_port(pic_state *pic, struct pic_port *port)
{
  if (port->file->flag == 0) {
    return;
  }
  if (xfclose(pic, port->file) == EOF) {
    pic_errorf(pic, "close-port: failure");
  }
}

static pic_value
pic_port_input_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->file->flag & X_READ) != 0) {
    return pic_true_value(pic);
  } else {
    return pic_false_value(pic);
  }
}

static pic_value
pic_port_output_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->file->flag & X_WRITE) != 0) {
    return pic_true_value(pic);
  }
  else {
    return pic_false_value(pic);
  }
}

static pic_value
pic_port_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_port_p(pic, v));
}

static pic_value
pic_port_eof_object_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_eof_p(pic, v));
}

static pic_value
pic_port_eof_object(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_eof_object(pic);
}

static pic_value
pic_port_port_open_p(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  return pic_bool_value(pic, port->file->flag != 0);
}

static pic_value
pic_port_close_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  pic_close_port(pic, port);

  return pic_undef_value(pic);
}

#define assert_port_profile(port, flags, caller) do {                   \
    if ((port->file->flag & (flags)) != (flags)) {                      \
      switch (flags) {                                                  \
      case X_WRITE:                                                     \
        pic_errorf(pic, caller ": expected output port");               \
      case X_READ:                                                      \
        pic_errorf(pic, caller ": expected input port");                \
      }                                                                 \
    }                                                                   \
    if (port->file->flag == 0) {                                        \
      pic_errorf(pic, caller ": expected open port");                   \
    }                                                                   \
  } while (0)

static pic_value
pic_port_open_input_bytevector(pic_state *pic)
{
  pic_value blob;
  unsigned char *buf;
  int len;

  pic_get_args(pic, "b", &blob);

  buf = pic_blob(pic, blob, &len);

  return pic_obj_value(pic_make_port(pic, xfopen_buf(pic, (char *)buf, len, "r")));
}

static pic_value
pic_port_open_output_bytevector(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_obj_value(pic_make_port(pic, xfopen_buf(pic, NULL, 0, "w")));
}

static pic_value
pic_port_get_output_bytevector(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);
  const char *buf;
  int len;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, X_WRITE, "get-output-bytevector");

  if (xfget_buf(pic, port->file, &buf, &len) < 0) {
    pic_errorf(pic, "port was not created by open-output-bytevector");
  }
  return pic_blob_value(pic, (unsigned char *)buf, len);
}

static pic_value
pic_port_read_u8(pic_state *pic){
  struct pic_port *port = pic_stdin(pic);
  int c;
  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, X_READ, "read-u8");
  if ((c = xfgetc(pic, port->file)) == EOF) {
    return pic_eof_object(pic);
  }

  return pic_int_value(pic, c);
}

static pic_value
pic_port_peek_u8(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, X_READ, "peek-u8");

  c = xfgetc(pic, port->file);
  if (c == EOF) {
    return pic_eof_object(pic);
  }
  else {
    xungetc(c, port->file);
    return pic_int_value(pic, c);
  }
}

static pic_value
pic_port_u8_ready_p(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, X_READ, "u8-ready?");

  return pic_true_value(pic);   /* FIXME: always returns #t */
}


static pic_value
pic_port_read_bytevector(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);
  unsigned char *buf;
  int k, i;

  pic_get_args(pic, "i|p", &k, &port);

  assert_port_profile(port, X_READ, "read-bytevector");

  buf = pic_blob(pic, pic_blob_value(pic, NULL, k), NULL);

  i = xfread(pic, buf, sizeof(char), k, port->file);
  if (i == 0) {
    return pic_eof_object(pic);
  }
  return pic_blob_value(pic, buf, i);
}

static pic_value
pic_port_read_bytevector_ip(pic_state *pic)
{
  struct pic_port *port;
  pic_value bv;
  unsigned char *buf;
  int n, start, end, i, len;

  n = pic_get_args(pic, "b|pii", &bv, &port, &start, &end);

  buf = pic_blob(pic, bv, &len);

  switch (n) {
  case 1:
    port = pic_stdin(pic);
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);
  assert_port_profile(port, X_READ, "read-bytevector!");

  i = xfread(pic, buf + start, 1, end - start, port->file);
  if (i == 0) {
    return pic_eof_object(pic);
  }
  return pic_int_value(pic, i);
}

static pic_value
pic_port_write_u8(pic_state *pic)
{
  int i;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "i|p", &i, &port);

  assert_port_profile(port, X_WRITE, "write-u8");

  xfputc(pic, i, port->file);
  return pic_undef_value(pic);
}

static pic_value
pic_port_write_bytevector(pic_state *pic)
{
  pic_value blob;
  struct pic_port *port;
  unsigned char *buf;
  int n, start, end, len, done;

  n = pic_get_args(pic, "b|pii", &blob, &port, &start, &end);

  buf = pic_blob(pic, blob, &len);

  switch (n) {
  case 1:
    port = pic_stdout(pic);
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);
  assert_port_profile(port, X_WRITE, "write-bytevector");

  done = 0;
  while (done < end - start) {
    done += xfwrite(pic, buf + start + done, 1, end - start - done, port->file);
    /* FIXME: error check... */
  }
  return pic_undef_value(pic);
}

static pic_value
pic_port_flush(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, X_WRITE, "flush-output-port");

  xfflush(pic, port->file);
  return pic_undef_value(pic);
}

static pic_value
coerce_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  return pic_obj_value(port);
}

#define DEFINE_PORT(pic, name, file)                                    \
  pic_defvar(pic, name, pic_obj_value(pic_make_port(pic, file)), coerce)

void
pic_init_port(pic_state *pic)
{
  struct pic_proc *coerce = pic_lambda(pic, coerce_port, 0);

  DEFINE_PORT(pic, "current-input-port", xstdin);
  DEFINE_PORT(pic, "current-output-port", xstdout);
  DEFINE_PORT(pic, "current-error-port", xstderr);

  pic_defun(pic, "port?", pic_port_port_p);
  pic_defun(pic, "input-port?", pic_port_input_port_p);
  pic_defun(pic, "output-port?", pic_port_output_port_p);
  pic_defun(pic, "port-open?", pic_port_port_open_p);
  pic_defun(pic, "close-port", pic_port_close_port);

  pic_defun(pic, "eof-object?", pic_port_eof_object_p);
  pic_defun(pic, "eof-object", pic_port_eof_object);

  /* input */
  pic_defun(pic, "read-u8", pic_port_read_u8);
  pic_defun(pic, "peek-u8", pic_port_peek_u8);
  pic_defun(pic, "u8-ready?", pic_port_u8_ready_p);
  pic_defun(pic, "read-bytevector", pic_port_read_bytevector);
  pic_defun(pic, "read-bytevector!", pic_port_read_bytevector_ip);

  /* output */
  pic_defun(pic, "write-u8", pic_port_write_u8);
  pic_defun(pic, "write-bytevector", pic_port_write_bytevector);
  pic_defun(pic, "flush-output-port", pic_port_flush);

  /* string I/O */
  pic_defun(pic, "open-input-bytevector", pic_port_open_input_bytevector);
  pic_defun(pic, "open-output-bytevector", pic_port_open_output_bytevector);
  pic_defun(pic, "get-output-bytevector", pic_port_get_output_bytevector);
}
