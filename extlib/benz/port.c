/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

pic_value
pic_eof_object()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_EOF);

  return v;
}

static pic_value
pic_assert_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  return pic_obj_value(port);
}

/* current-(input|output|error)-port */

#if PIC_ENABLE_STDIO

static int
file_read(pic_state PIC_UNUSED(*pic), void *cookie, char *ptr, int size) {
  FILE *file = cookie;
  int r;

  size = 1;                     /* override size */

  r = (int)fread(ptr, 1, (size_t)size, file);
  if (r < size && ferror(file)) {
    return -1;
  }
  if (r == 0 && feof(file)) {
    clearerr(file);
  }
  return r;
}

static int
file_write(pic_state PIC_UNUSED(*pic), void *cookie, const char *ptr, int size) {
  FILE *file = cookie;
  int r;

  r = (int)fwrite(ptr, 1, (size_t)size, file);
  if (r < size) {
    return -1;
  }
  fflush(cookie);
  return r;
}

static long
file_seek(pic_state PIC_UNUSED(*pic), void *cookie, long pos, int whence) {
  switch (whence) {
  case XSEEK_CUR:
    whence = SEEK_CUR;
    break;
  case XSEEK_SET:
    whence = SEEK_SET;
    break;
  case XSEEK_END:
    whence = SEEK_END;
    break;
  }
  if (fseek(cookie, pos, whence) == 0) {
    return ftell(cookie);
  }
  return -1;
}

static int
file_close(pic_state PIC_UNUSED(*pic), void *cookie) {
  return fclose(cookie);
}

static xFILE *
file_open(pic_state *pic, const char *name, const char *mode) {
  FILE *fp;

  if ((fp = fopen(name, mode)) == NULL) {
    return NULL;
  }

  switch (*mode) {
  case 'r':
    return xfunopen(pic, fp, file_read, NULL, file_seek, file_close);
  default:
    return xfunopen(pic, fp, NULL, file_write, file_seek, file_close);
  }
}

PIC_NORETURN static void
file_error(pic_state *pic, const char *msg)
{
  struct pic_error *e;

  e = pic_make_error(pic, pic_intern_lit(pic, "file"), msg, pic_nil_value(pic));

  pic_raise(pic, pic_obj_value(e));
}

struct pic_port *
pic_open_file(pic_state *pic, const char *name, int flags) {
  struct pic_port *port;
  xFILE *file;
  char mode = 'r';

  if ((flags & PIC_PORT_IN) == 0) {
    mode = 'w';
  }
  if ((file = file_open(pic, name, &mode)) == NULL) {
    file_error(pic, pic_str_cstr(pic, pic_format(pic, "could not open file '%s'", name)));
  }

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = file;
  port->flags = flags | PIC_PORT_OPEN;

  return port;
}

#else

/* null file */

static int
null_read(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie), char PIC_UNUSED(*ptr), int PIC_UNUSED(size)) {
  return 0;
}

static int
null_write(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie), const char PIC_UNUSED(*ptr), int size) {
  return size;
}

static long
null_seek(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie), long PIC_UNUSED(pos), int PIC_UNUSED(whence)) {
  return 0;
}

static int
null_close(pic_state PIC_UNUSED(*pic), void PIC_UNUSED(*cookie)) {
  return 0;
}

#endif

static void
pic_define_standard_port(pic_state *pic, const char *name, xFILE *file, int dir)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = file;
  port->flags = dir | PIC_PORT_TEXT | PIC_PORT_OPEN;

  pic_defvar(pic, name, pic_obj_value(port), pic_make_proc(pic, pic_assert_port, 0, NULL));
}

#define DEFINE_STANDARD_PORT_ACCESSOR(name, var)        \
  struct pic_port *                                     \
  name(pic_state *pic)                                  \
  {                                                     \
    pic_value obj;                                      \
                                                        \
    obj = pic_funcall(pic, "picrin.base", var, 0);      \
                                                        \
    return pic_port_ptr(obj);                           \
  }

DEFINE_STANDARD_PORT_ACCESSOR(pic_stdin, "current-input-port")
DEFINE_STANDARD_PORT_ACCESSOR(pic_stdout, "current-output-port")
DEFINE_STANDARD_PORT_ACCESSOR(pic_stderr, "current-error-port")

struct strfile {
  char *buf;
  long pos, end, capa;
};

static int
string_read(pic_state PIC_UNUSED(*pic), void *cookie, char *ptr, int size)
{
  struct strfile *m = cookie;

  if (size > (int)(m->end - m->pos))
    size = (int)(m->end - m->pos);
  memcpy(ptr, m->buf + m->pos, size);
  m->pos += size;
  return size;
}

static int
string_write(pic_state *pic, void *cookie, const char *ptr, int size)
{
  struct strfile *m = cookie;

  if (m->pos + size >= m->capa) {
    m->capa = (m->pos + size) * 2;
    m->buf = pic_realloc(pic, m->buf, m->capa);
  }
  memcpy(m->buf + m->pos, ptr, size);
  m->pos += size;
  if (m->end < m->pos)
    m->end = m->pos;
  return size;
}

static long
string_seek(pic_state PIC_UNUSED(*pic), void *cookie, long pos, int whence)
{
  struct strfile *m = cookie;

  switch (whence) {
  case XSEEK_SET:
    m->pos = pos;
    break;
  case XSEEK_CUR:
    m->pos += pos;
    break;
  case XSEEK_END:
    m->pos = m->end + pos;
    break;
  }

  return m->pos;
}

static int
string_close(pic_state *pic, void *cookie)
{
  struct strfile *m = cookie;

  pic_free(pic, m->buf);
  pic_free(pic, m);
  return 0;
}

static xFILE *
string_open(pic_state *pic, const char *data, size_t size)
{
  struct strfile *m;
  xFILE *file;

  m = pic_malloc(pic, sizeof(struct strfile));
  m->buf = pic_malloc(pic, size);
  m->pos = 0;
  m->end = size;
  m->capa = size;


  if (data != NULL) {
    memcpy(m->buf, data, size);
    file = xfunopen(pic, m, string_read, NULL, string_seek, string_close);
  } else {
    file = xfunopen(pic, m, NULL, string_write, string_seek, string_close);
  }

  if (file == NULL) {
    string_close(pic, m);
    pic_error(pic, "could not open new output string/bytevector port", pic_nil_value(pic));
  }
  return file;
}

struct pic_port *
pic_open_input_string(pic_state *pic, const char *str)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = string_open(pic, str, strlen(str));
  port->flags = PIC_PORT_IN | PIC_PORT_TEXT | PIC_PORT_OPEN;

  return port;
}

struct pic_port *
pic_open_output_string(pic_state *pic)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = string_open(pic, NULL, 0);
  port->flags = PIC_PORT_OUT | PIC_PORT_TEXT | PIC_PORT_OPEN;

  return port;
}

struct pic_string *
pic_get_output_string(pic_state *pic, struct pic_port *port)
{
  struct strfile *s;

  if (port->file->vtable.write != string_write) {
    pic_errorf(pic, "get-output-string: port is not made by open-output-string");
  }

  xfflush(pic, port->file);

  s = port->file->vtable.cookie;

  return pic_make_str(pic, s->buf, s->end);
}

void
pic_close_port(pic_state *pic, struct pic_port *port)
{
  if ((port->flags & PIC_PORT_OPEN) == 0) {
    return;
  }
  if (xfclose(pic, port->file) == EOF) {
    pic_errorf(pic, "close-port: failure");
  }
  port->flags &= ~PIC_PORT_OPEN;
}

static pic_value
pic_port_call_with_port(pic_state *pic)
{
  struct pic_port *port;
  struct pic_proc *proc;
  pic_value value;

  pic_get_args(pic, "pl", &port, &proc);

  value = pic_call(pic, proc, 1, pic_obj_value(port));

  pic_close_port(pic, port);

  return value;
}

static pic_value
pic_port_input_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->flags & PIC_PORT_IN) != 0) {
    return pic_true_value(pic);
  }
  else {
    return pic_false_value(pic);
  }
}

static pic_value
pic_port_output_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->flags & PIC_PORT_OUT) != 0) {
    return pic_true_value(pic);
  }
  else {
    return pic_false_value(pic);
  }
}

static pic_value
pic_port_textual_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->flags & PIC_PORT_TEXT) != 0) {
    return pic_true_value(pic);
  }
  else {
    return pic_false_value(pic);
  }
}

static pic_value
pic_port_binary_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v) && (pic_port_ptr(v)->flags & PIC_PORT_BINARY) != 0) {
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

  return pic_eof_object();
}

static pic_value
pic_port_port_open_p(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  return pic_bool_value(pic, port->flags & PIC_PORT_OPEN);
}

static pic_value
pic_port_close_port(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "p", &port);

  pic_close_port(pic, port);

  return pic_undef_value(pic);
}

#define assert_port_profile(port, flgs, caller) do {                    \
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
    if ((port->flags & PIC_PORT_OPEN) == 0) {                           \
      pic_errorf(pic, caller ": expected open port");                   \
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

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, "get-output-string");

  return pic_obj_value(pic_get_output_string(pic, port));
}

static pic_value
pic_port_open_input_blob(pic_state *pic)
{
  struct pic_port *port;
  struct pic_blob *blob;

  pic_get_args(pic, "b", &blob);

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = string_open(pic, (const char *)blob->data, blob->len);
  port->flags = PIC_PORT_IN | PIC_PORT_BINARY | PIC_PORT_OPEN;

  return pic_obj_value(port);
}

static pic_value
pic_port_open_output_bytevector(pic_state *pic)
{
  struct pic_port *port;

  pic_get_args(pic, "");

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port), PIC_TT_PORT);
  port->file = string_open(pic, NULL, 0);
  port->flags = PIC_PORT_OUT | PIC_PORT_BINARY | PIC_PORT_OPEN;

  return pic_obj_value(port);
}

static pic_value
pic_port_get_output_bytevector(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);
  struct pic_blob *blob;
  struct strfile *s;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, "get-output-bytevector");

  if (port->file->vtable.write != string_write) {
    pic_errorf(pic, "get-output-bytevector: port is not made by open-output-bytevector");
  }

  xfflush(pic, port->file);

  s = port->file->vtable.cookie;

  blob = pic_make_blob(pic, s->end);
  memcpy(blob->data, s->buf, s->end);

  return pic_obj_value(blob);
}

static pic_value
pic_port_read_char(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, "read-char");

  if ((c = xfgetc(pic, port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    return pic_char_value(pic, (char)c);
  }
}

static pic_value
pic_port_peek_char(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, "peek-char");

  if ((c = xfgetc(pic, port->file)) == EOF) {
    return pic_eof_object();
  }
  else {
    xungetc(c, port->file);
    return pic_char_value(pic, (char)c);
  }
}

static pic_value
pic_port_read_line(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic), *buf;
  struct pic_string *str;
  pic_value res = pic_eof_object();

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, "read-line");

  buf = pic_open_output_string(pic);
  while ((c = xfgetc(pic, port->file)) != EOF && c != '\n') {
    xfputc(pic, c, buf->file);
  }

  str = pic_get_output_string(pic, buf);
  if (pic_str_len(pic, str) == 0 && c == EOF) {
    /* EOF */
  } else {
    res = pic_obj_value(str);
  }
  pic_close_port(pic, buf);
  return res;
}

static pic_value
pic_port_char_ready_p(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, "char-ready?");

  pic_get_args(pic, "|p", &port);

  return pic_true_value(pic);      /* FIXME: always returns #t */
}

static pic_value
pic_port_read_string(pic_state *pic){
  struct pic_port *port = pic_stdin(pic), *buf;
  struct pic_string *str;
  int k, i;
  int c;
  pic_value res = pic_eof_object();

  pic_get_args(pic, "i|p", &k,  &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_TEXT, "read-stritg");

  c = EOF;
  buf = pic_open_output_string(pic);
  for(i = 0; i < k; ++i) {
    if((c = xfgetc(pic, port->file)) == EOF){
      break;
    }
    xfputc(pic, c, buf->file);
  }

  str = pic_get_output_string(pic, buf);
  if (pic_str_len(pic, str) == 0 && c == EOF) {
    /* EOF */
  } else {
    res = pic_obj_value(str);
  }
  pic_close_port(pic, buf);
  return res;
}

static pic_value
pic_port_read_byte(pic_state *pic){
  struct pic_port *port = pic_stdin(pic);
  int c;
  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, "read-u8");
  if ((c = xfgetc(pic, port->file)) == EOF) {
    return pic_eof_object();
  }

  return pic_int_value(pic, c);
}

static pic_value
pic_port_peek_byte(pic_state *pic)
{
  int c;
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, "peek-u8");

  c = xfgetc(pic, port->file);
  if (c == EOF) {
    return pic_eof_object();
  }
  else {
    xungetc(c, port->file);
    return pic_int_value(pic, c);
  }
}

static pic_value
pic_port_byte_ready_p(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, "u8-ready?");

  return pic_true_value(pic);      /* FIXME: always returns #t */
}


static pic_value
pic_port_read_blob(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);
  struct pic_blob *blob;
  int k, i;

  pic_get_args(pic, "i|p", &k, &port);

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, "read-bytevector");

  blob = pic_make_blob(pic, k);

  i = xfread(pic, blob->data, sizeof(char), k, port->file);
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
  char *buf;
  int n, start, end, i, len;

  n = pic_get_args(pic, "b|pii", &bv, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdin(pic);
  case 2:
    start = 0;
  case 3:
    end = bv->len;
  }

  assert_port_profile(port, PIC_PORT_IN | PIC_PORT_BINARY, "read-bytevector!");

  if (end < start) {
    pic_errorf(pic, "read-bytevector!: end index must be greater than or equal to start index");
  }

  len = end - start;

  buf = pic_calloc(pic, len, sizeof(char));
  i = xfread(pic, buf, sizeof(char), len, port->file);
  memcpy(bv->data + start, buf, i);
  pic_free(pic, buf);

  if (i == 0) {
    return pic_eof_object();
  }
  else {
    return pic_int_value(pic, i);
  }
}

static pic_value
pic_port_newline(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, "newline");

  xfputs(pic, "\n", port->file);
  return pic_undef_value(pic);
}

static pic_value
pic_port_write_char(pic_state *pic)
{
  char c;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "c|p", &c, &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, "write-char");

  xfputc(pic, c, port->file);
  return pic_undef_value(pic);
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

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_TEXT, "write-string");

  for (i = start; i < end && str[i] != '\0'; ++i) {
    xfputc(pic, str[i], port->file);
  }
  return pic_undef_value(pic);
}

static pic_value
pic_port_write_byte(pic_state *pic)
{
  int i;
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "i|p", &i, &port);

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, "write-u8");

  xfputc(pic, i, port->file);
  return pic_undef_value(pic);
}

static pic_value
pic_port_write_blob(pic_state *pic)
{
  struct pic_blob *blob;
  struct pic_port *port;
  int n, start, end, i;

  n = pic_get_args(pic, "b|pii", &blob, &port, &start, &end);
  switch (n) {
  case 1:
    port = pic_stdout(pic);
  case 2:
    start = 0;
  case 3:
    end = blob->len;
  }

  assert_port_profile(port, PIC_PORT_OUT | PIC_PORT_BINARY, "write-bytevector");

  for (i = start; i < end; ++i) {
    xfputc(pic, blob->data[i], port->file);
  }
  return pic_undef_value(pic);
}

static pic_value
pic_port_flush(pic_state *pic)
{
  struct pic_port *port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, PIC_PORT_OUT, "flush-output-port");

  xfflush(pic, port->file);
  return pic_undef_value(pic);
}

void
pic_init_port(pic_state *pic)
{
#if PIC_ENABLE_STDIO
# define FILE_VTABLE { 0, file_read, file_write, file_seek, file_close }
#else
# define FILE_VTABLE { 0, null_read, null_write, null_seek, null_close }
#endif

  static const xFILE skel[3] = {
    { { 0 }, 0, NULL, NULL, FILE_VTABLE, X_READ },
    { { 0 }, 0, NULL, NULL, FILE_VTABLE, X_WRITE | X_LNBUF },
    { { 0 }, 0, NULL, NULL, FILE_VTABLE, X_WRITE | X_UNBUF }
  };

  pic->files[0] = skel[0];
  pic->files[1] = skel[1];
  pic->files[2] = skel[2];

#if PIC_ENABLE_STDIO
  pic->files[0].vtable.cookie = stdin;
  pic->files[1].vtable.cookie = stdout;
  pic->files[2].vtable.cookie = stderr;
#endif

  pic_define_standard_port(pic, "current-input-port", xstdin, PIC_PORT_IN);
  pic_define_standard_port(pic, "current-output-port", xstdout, PIC_PORT_OUT);
  pic_define_standard_port(pic, "current-error-port", xstderr, PIC_PORT_OUT);

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
