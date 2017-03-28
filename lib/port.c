/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

#ifndef EOF
# define EOF (-1)
#endif

bool
pic_port_p(pic_state *pic, pic_value obj, const pic_port_type *type)
{
  if (pic_type(pic, obj) != PIC_TYPE_PORT) {
    return false;
  }
  return type == NULL || pic_port_ptr(pic, obj)->file.vtable == type;
}

pic_value
pic_funopen(pic_state *pic, void *cookie, const pic_port_type *type)
{
  struct port *port;

  port = (struct port *)pic_obj_alloc(pic, sizeof(struct port), PIC_TYPE_PORT);
  port->file.cnt = 0;
  port->file.base = NULL;
  port->file.flag = type->read ? FILE_READ : FILE_WRITE;
  port->file.cookie = cookie;
  port->file.vtable = type;

  return obj_value(pic, port);
}

int
pic_fclose(pic_state *pic, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  if (fp->flag == 0)
    return 0;
  pic_fflush(pic, port);
  fp->flag = 0;
  if (fp->base != fp->buf)
    pic_free(pic, fp->base);
  return fp->vtable->close(pic, fp->cookie);
}

void
pic_clearerr(pic_state *PIC_UNUSED(pic), pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  fp->flag &= ~(FILE_EOF | FILE_ERR);
}

int
pic_feof(pic_state *PIC_UNUSED(pic), pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  return (fp->flag & FILE_EOF) != 0;
}

int
pic_ferror(pic_state *PIC_UNUSED(pic), pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  return (fp->flag & FILE_ERR) != 0;
}

static int
fillbuf(pic_state *pic, struct file *fp)
{
  int bufsize;

  if ((fp->flag & (FILE_READ|FILE_EOF|FILE_ERR)) != FILE_READ)
    return EOF;
  if (fp->base == NULL) {
    if ((fp->flag & FILE_UNBUF) == 0) {
      /* no buffer yet */
      if ((fp->base = pic_malloc(pic, PIC_BUFSIZ)) == NULL) {
        /* can't get buffer, try unbuffered */
        fp->flag |= FILE_UNBUF;
      }
    }
    if (fp->flag & FILE_UNBUF) {
      fp->base = fp->buf;
    }
  }
  bufsize = (fp->flag & FILE_UNBUF) ? sizeof(fp->buf) : PIC_BUFSIZ;

  fp->ptr = fp->base;
  fp->cnt = fp->vtable->read(pic, fp->cookie, fp->ptr, bufsize);

  if (--fp->cnt < 0) {
    if (fp->cnt == -1)
      fp->flag |= FILE_EOF;
    else
      fp->flag |= FILE_ERR;
    fp->cnt = 0;
    return EOF;
  }

  return (unsigned char) *fp->ptr++;
}

static int
flushbuf(pic_state *pic, int x, struct file *fp)
{
  int num_written=0, bufsize=0;
  char c = x;

  if ((fp->flag & (FILE_WRITE|FILE_EOF|FILE_ERR)) != FILE_WRITE)
    return EOF;
  if (fp->base == NULL && ((fp->flag & FILE_UNBUF) == 0)) {
    /* no buffer yet */
    if ((fp->base = pic_malloc(pic, PIC_BUFSIZ)) == NULL) {
      /* couldn't allocate a buffer, so try unbuffered */
      fp->flag |= FILE_UNBUF;
    } else {
      fp->ptr = fp->base;
      fp->cnt = PIC_BUFSIZ - 1;
    }
  }
  if (fp->flag & FILE_UNBUF) {
    /* unbuffered write */
    fp->ptr = fp->base = NULL;
    fp->cnt = 0;
    if (x == EOF)
      return EOF;
    num_written = fp->vtable->write(pic, fp->cookie, (const char *) &c, 1);
    bufsize = 1;
  } else {
    /* buffered write */
    assert(fp->ptr);
    if (x != EOF) {
      *fp->ptr++ = (unsigned char) c;
    }
    bufsize = (int)(fp->ptr - fp->base);
    while(bufsize - num_written > 0) {
      int t;
      t = fp->vtable->write(pic, fp->cookie, fp->base + num_written, bufsize - num_written);
      if (t < 0)
        break;
      num_written += t;
    }

    fp->ptr = fp->base;
    fp->cnt = PIC_BUFSIZ - 1;
  }

  if (num_written == bufsize) {
    return x;
  } else {
    fp->flag |= FILE_ERR;
    return EOF;
  }
}

int
pic_fflush(pic_state *pic, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  int retval;

  retval = 0;
  if ((fp->flag & FILE_WRITE) == 0)
    return -1;
  flushbuf(pic, EOF, fp);
  if (fp->flag & FILE_ERR)
    retval = -1;
  return retval;
}

#define getc_(pic, p)                           \
  ((--(p)->cnt >= 0)                            \
   ? (unsigned char) *(p)->ptr++                \
   : fillbuf((pic), p))
#define putc_(pic, x, p)                                                \
  ((--(p)->cnt >= 0 && !(((p)->flag & FILE_LNBUF) && (x) == '\n'))      \
   ? *(p)->ptr++ = (x)                                                  \
   : flushbuf((pic), (x), (p)))

int
pic_fputc(pic_state *pic, int x, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  return putc_(pic, x, fp);
}

int
pic_fgetc(pic_state *pic, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  return getc_(pic, fp);
}

int
pic_fputs(pic_state *pic, const char *s, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;

  const char *ptr = s;
  while(*ptr != '\0') {
    if (putc_(pic, *ptr, fp) == EOF)
      return EOF;
    ++ptr;
  }
  return (int)(ptr - s);
}

char *
pic_fgets(pic_state *pic, char *s, int size, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  int c = 0;
  char *buf;

  pic_fflush(pic, port);

  if (size == 0) {
    return NULL;
  }
  buf = s;
  while (--size > 0 && (c = getc_(pic, fp)) != EOF) {
    if ((*buf++ = c) == '\n')
      break;
  }
  *buf = '\0';

  return (c == EOF && buf == s) ? NULL : s;
}

int
pic_ungetc(pic_state *PIC_UNUSED(pic), int c, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  unsigned char uc = c;

  if (c == EOF || fp->base == fp->ptr) {
    return EOF;
  }
  fp->cnt++;
  return *--fp->ptr = uc;
}

size_t
pic_fread(pic_state *pic, void *ptr, size_t size, size_t count, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  char *bptr = ptr;
  long nbytes;
  int c;

  nbytes = size * count;
  while (nbytes > fp->cnt) {
    memcpy(bptr, fp->ptr, fp->cnt);
    fp->ptr += fp->cnt;
    bptr += fp->cnt;
    nbytes -= fp->cnt;
    if ((c = fillbuf(pic, fp)) == EOF) {
      return (size * count - nbytes) / size;
    } else {
      pic_ungetc(pic, c, port);
    }
  }
  memcpy(bptr, fp->ptr, nbytes);
  fp->ptr += nbytes;
  fp->cnt -= nbytes;
  return count;
}

size_t
pic_fwrite(pic_state *pic, const void *ptr, size_t size, size_t count, pic_value port)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  const char *bptr = ptr;
  long nbytes;

  nbytes = size * count;
  while (nbytes > fp->cnt) {
    memcpy(fp->ptr, bptr, fp->cnt);
    fp->ptr += fp->cnt;
    bptr += fp->cnt;
    nbytes -= fp->cnt;
    if (flushbuf(pic, EOF, fp) == EOF) {
      return (size * count - nbytes) / size;
    }
  }
  memcpy(fp->ptr, bptr, nbytes);
  fp->ptr += nbytes;
  fp->cnt -= nbytes;
  return count;
}

long
pic_fseek(pic_state *pic, pic_value port, long offset, int whence)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  long s;

  pic_fflush(pic, port);

  fp->ptr = fp->base;
  fp->cnt = 0;

  if ((s = fp->vtable->seek(pic, fp->cookie, offset, whence)) != 0)
    return s;
  fp->flag &= ~FILE_EOF;
  return 0;
}

#if PIC_USE_STDIO

static int
file_read(pic_state *PIC_UNUSED(pic), void *cookie, char *ptr, int size) {
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
file_write(pic_state *PIC_UNUSED(pic), void *cookie, const char *ptr, int size) {
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
file_seek(pic_state *PIC_UNUSED(pic), void *cookie, long pos, int whence) {
  switch (whence) {
  case PIC_SEEK_CUR:
    whence = SEEK_CUR;
    break;
  case PIC_SEEK_SET:
    whence = SEEK_SET;
    break;
  case PIC_SEEK_END:
    whence = SEEK_END;
    break;
  }
  if (fseek(cookie, pos, whence) == 0) {
    return ftell(cookie);
  }
  return -1;
}

static int
file_close(pic_state *PIC_UNUSED(pic), void *cookie) {
  return fclose(cookie);
}

static const pic_port_type file_rd = {
  file_read, 0, file_seek, file_close
};
static const pic_port_type file_wr = {
  0, file_write, file_seek, file_close
};

pic_value
pic_fopen(pic_state *pic, FILE *fp, const char *mode) {
  if (*mode == 'r') {
    return pic_funopen(pic, fp, &file_rd);
  } else {
    return pic_funopen(pic, fp, &file_wr);
  }
}

#else

static int
null_read(pic_state *PIC_UNUSED(pic), void *PIC_UNUSED(cookie), char *PIC_UNUSED(ptr), int PIC_UNUSED(size)) {
  return 0;
}

static int
null_write(pic_state *PIC_UNUSED(pic), void *PIC_UNUSED(cookie), const char *PIC_UNUSED(ptr), int size) {
  return size;
}

static long
null_seek(pic_state *PIC_UNUSED(pic), void *PIC_UNUSED(cookie), long PIC_UNUSED(pos), int PIC_UNUSED(whence)) {
  return 0;
}

static int
null_close(pic_state *PIC_UNUSED(pic), void *PIC_UNUSED(cookie)) {
  return 0;
}


static const pic_port_type null_rd = {
  null_read, 0, null_seek, null_close
};
static const pic_port_type null_wr = {
  0, null_write, null_seek, null_close
};

static pic_value
pic_fopen_null(pic_state *PIC_UNUSED(pic), const char *mode)
{
  switch (*mode) {
  case 'r':
    return pic_funopen(pic, 0, &null_rd);
  default:
    return pic_funopen(pic, 0, &null_wr);
  }
}

#endif

typedef struct { char *buf; long pos, end, capa; } xbuf_t;

static int
string_read(pic_state *PIC_UNUSED(pic), void *cookie, char *ptr, int size)
{
  xbuf_t *m = cookie;

  if (size > (int)(m->end - m->pos))
    size = (int)(m->end - m->pos);
  memcpy(ptr, m->buf + m->pos, size);
  m->pos += size;
  return size;
}

static int
string_write(pic_state *pic, void *cookie, const char *ptr, int size)
{
  xbuf_t *m = cookie;

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
string_seek(pic_state *PIC_UNUSED(pic), void *cookie, long pos, int whence)
{
  xbuf_t *m = cookie;

  switch (whence) {
  case PIC_SEEK_SET:
    m->pos = pos;
    break;
  case PIC_SEEK_CUR:
    m->pos += pos;
    break;
  case PIC_SEEK_END:
    m->pos = m->end + pos;
    break;
  }

  return m->pos;
}

static int
string_close(pic_state *pic, void *cookie)
{
  xbuf_t *m = cookie;

  pic_free(pic, m->buf);
  pic_free(pic, m);
  return 0;
}

static const pic_port_type string_rd = {
  string_read, 0, string_seek, string_close
};
static const pic_port_type string_wr = {
  0, string_write, string_seek, string_close
};

pic_value
pic_fmemopen(pic_state *pic, const char *data, int size, const char *mode)
{
  xbuf_t *m;

  m = pic_malloc(pic, sizeof(xbuf_t));
  m->buf = pic_malloc(pic, size);
  m->pos = 0;
  m->end = size;
  m->capa = size;

  if (*mode == 'r') {
    memcpy(m->buf, data, size);
    return pic_funopen(pic, m, &string_rd);
  } else {
    return pic_funopen(pic, m, &string_wr);
  }
}

int
pic_fgetbuf(pic_state *pic, pic_value port, const char **buf, int *len)
{
  struct file *fp = &pic_port_ptr(pic, port)->file;
  xbuf_t *s;

  pic_fflush(pic, port);

  if (fp->vtable->write != string_write) {
    return -1;
  }
  s = fp->cookie;
  *len = s->end;
  *buf = s->buf;
  return 0;
}

static pic_value
pic_port_input_port_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (pic_port_p(pic, v, NULL) && (pic_port_ptr(pic, v)->file.flag & FILE_READ) != 0) {
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

  if (pic_port_p(pic, v, NULL) && (pic_port_ptr(pic, v)->file.flag & FILE_WRITE) != 0) {
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

  return pic_bool_value(pic, pic_port_p(pic, v, NULL));
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
  pic_value port;

  pic_get_args(pic, "p", &port);

  return pic_bool_value(pic, pic_port_ptr(pic, port)->file.flag != 0);
}

static pic_value
pic_port_close_port(pic_state *pic)
{
  pic_value port;

  pic_get_args(pic, "p", &port);

  pic_fclose(pic, port);

  return pic_undef_value(pic);
}

#define assert_port_profile(port, flags, caller) do {           \
    int flag = pic_port_ptr(pic, port)->file.flag;              \
    if ((flag & (flags)) != (flags)) {                          \
      switch (flags) {                                          \
      case FILE_WRITE:                                          \
        pic_error(pic, caller ": output port required", 0);     \
      case FILE_READ:                                           \
        pic_error(pic, caller ": input port required", 0);      \
      }                                                         \
    }                                                           \
    if (flag == 0) {                                            \
      pic_error(pic, caller ": open port required", 0);         \
    }                                                           \
  } while (0)

static pic_value
pic_port_open_input_bytevector(pic_state *pic)
{
  unsigned char *buf;
  int len;

  pic_get_args(pic, "b", &buf, &len);

  return pic_fmemopen(pic, (char *)buf, len, "r");
}

static pic_value
pic_port_open_output_bytevector(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_fmemopen(pic, NULL, 0, "w");
}

static pic_value
pic_port_get_output_bytevector(pic_state *pic)
{
  pic_value port = pic_stdout(pic);
  const char *buf;
  int len;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, FILE_WRITE, "get-output-bytevector");

  if (pic_fgetbuf(pic, port, &buf, &len) < 0) {
    pic_error(pic, "port was not created by open-output-bytevector", 0);
  }
  return pic_blob_value(pic, (unsigned char *)buf, len);
}

static pic_value
pic_port_read_u8(pic_state *pic)
{
  pic_value port = pic_stdin(pic);
  int c;

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, FILE_READ, "read-u8");
  if ((c = pic_fgetc(pic, port)) == EOF) {
    return pic_eof_object(pic);
  }

  return pic_int_value(pic, c);
}

static pic_value
pic_port_peek_u8(pic_state *pic)
{
  int c;
  pic_value port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, FILE_READ, "peek-u8");

  c = pic_fgetc(pic, port);
  if (c == EOF) {
    return pic_eof_object(pic);
  }
  else {
    pic_ungetc(pic, c, port);
    return pic_int_value(pic, c);
  }
}

static pic_value
pic_port_read_bytevector_ip(pic_state *pic)
{
  pic_value port;
  unsigned char *buf;
  int n, start, end, i, len;

  n = pic_get_args(pic, "b|pii", &buf, &len, &port, &start, &end);

  switch (n) {
  case 1:
    port = pic_stdin(pic);
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);
  assert_port_profile(port, FILE_READ, "read-bytevector!");

  i = pic_fread(pic, buf + start, 1, end - start, port);
  if (i == 0) {
    return pic_eof_object(pic);
  }
  return pic_int_value(pic, i);
}

static pic_value
pic_port_write_u8(pic_state *pic)
{
  int i;
  pic_value port = pic_stdout(pic);

  pic_get_args(pic, "i|p", &i, &port);

  assert_port_profile(port, FILE_WRITE, "write-u8");

  pic_fputc(pic, i, port);
  return pic_undef_value(pic);
}

static pic_value
pic_port_write_bytevector(pic_state *pic)
{
  pic_value port;
  unsigned char *buf;
  int n, start, end, len, done;

  n = pic_get_args(pic, "b|pii", &buf, &len, &port, &start, &end);

  switch (n) {
  case 1:
    port = pic_stdout(pic);
  case 2:
    start = 0;
  case 3:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);
  assert_port_profile(port, FILE_WRITE, "write-bytevector");

  done = 0;
  while (done < end - start) {
    done += pic_fwrite(pic, buf + start + done, 1, end - start - done, port);
    /* FIXME: error check... */
  }
  return pic_undef_value(pic);
}

static pic_value
pic_port_flush(pic_state *pic)
{
  pic_value port = pic_stdout(pic);

  pic_get_args(pic, "|p", &port);

  assert_port_profile(port, FILE_WRITE, "flush-output-port");

  pic_fflush(pic, port);
  return pic_undef_value(pic);
}

void
pic_init_port(pic_state *pic)
{
#if PIC_USE_STDIO
  pic_defvar(pic, "current-input-port", pic_fopen(pic, stdin, "r"));
  pic_defvar(pic, "current-output-port", pic_fopen(pic, stdout, "w"));
  pic_defvar(pic, "current-error-port", pic_fopen(pic, stdout, "w"));
#else
  pic_defvar(pic, "current-input-port", pic_fopen_null(pic, "r"));
  pic_defvar(pic, "current-output-port", pic_fopen_null(pic, "w"));
  pic_defvar(pic, "current-error-port", pic_fopen_null(pic, "w"));
#endif

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
