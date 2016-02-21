/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/state.h"

#ifndef EOF
# define EOF (-1)
#endif

xFILE *xfunopen(pic_state *pic, void *cookie, int (*read)(pic_state *, void *, char *, int), int (*write)(pic_state *, void *, const char *, int), long (*seek)(pic_state *, void *, long, int), int (*close)(pic_state *, void *)) {
  xFILE *fp;

  for (fp = pic->files; fp < pic->files + XOPEN_MAX; fp++)
    if ((fp->flag & (X_READ | X_WRITE)) == 0)
      break;  /* found free slot */

  if (fp >= pic->files + XOPEN_MAX)  /* no free slots */
    return NULL;

  fp->cnt = 0;
  fp->base = NULL;
  fp->flag = read? X_READ : X_WRITE;

  fp->vtable.cookie = cookie;
  fp->vtable.read = read;
  fp->vtable.write = write;
  fp->vtable.seek = seek;
  fp->vtable.close = close;

  return fp;
}

int xfclose(pic_state *pic, xFILE *fp) {
  xfflush(pic, fp);
  fp->flag = 0;
  if (fp->base != fp->buf)
    pic_free(pic, fp->base);
  return fp->vtable.close(pic, fp->vtable.cookie);
}

void xclearerr(pic_state *PIC_UNUSED(pic), xFILE *fp) {
  fp->flag &= ~(X_EOF | X_ERR);
}

int xfeof(pic_state *PIC_UNUSED(pic), xFILE *fp) {
  return (fp->flag & X_EOF) != 0;
}

int xferror(pic_state *PIC_UNUSED(pic), xFILE *fp) {
  return (fp->flag & X_ERR) != 0;
}

int x_fillbuf(pic_state *pic, xFILE *fp) {
  int bufsize;

  if ((fp->flag & (X_READ|X_EOF|X_ERR)) != X_READ)
    return EOF;
  if (fp->base == NULL) {
    if ((fp->flag & X_UNBUF) == 0) {
      /* no buffer yet */
      if ((fp->base = pic_malloc(pic, XBUFSIZ)) == NULL) {
        /* can't get buffer, try unbuffered */
        fp->flag |= X_UNBUF;
      }
    }
    if (fp->flag & X_UNBUF) {
      fp->base = fp->buf;
    }
  }
  bufsize = (fp->flag & X_UNBUF) ? sizeof(fp->buf) : XBUFSIZ;

  fp->ptr = fp->base;
  fp->cnt = fp->vtable.read(pic, fp->vtable.cookie, fp->ptr, bufsize);

  if (--fp->cnt < 0) {
    if (fp->cnt == -1)
      fp->flag |= X_EOF;
    else
      fp->flag |= X_ERR;
    fp->cnt = 0;
    return EOF;
  }

  return (unsigned char) *fp->ptr++;
}

int x_flushbuf(pic_state *pic, int x, xFILE *fp) {
  int num_written=0, bufsize=0;
  char c = x;

  if ((fp->flag & (X_WRITE|X_EOF|X_ERR)) != X_WRITE)
    return EOF;
  if (fp->base == NULL && ((fp->flag & X_UNBUF) == 0)) {
    /* no buffer yet */
    if ((fp->base = pic_malloc(pic, XBUFSIZ)) == NULL) {
      /* couldn't allocate a buffer, so try unbuffered */
      fp->flag |= X_UNBUF;
    } else {
      fp->ptr = fp->base;
      fp->cnt = XBUFSIZ - 1;
    }
  }
  if (fp->flag & X_UNBUF) {
    /* unbuffered write */
    fp->ptr = fp->base = NULL;
    fp->cnt = 0;
    if (x == EOF)
      return EOF;
    num_written = fp->vtable.write(pic, fp->vtable.cookie, (const char *) &c, 1);
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
      t = fp->vtable.write(pic, fp->vtable.cookie, fp->base + num_written, bufsize - num_written);
      if (t < 0)
        break;
      num_written += t;
    }

    fp->ptr = fp->base;
    fp->cnt = XBUFSIZ - 1;
  }

  if (num_written == bufsize) {
    return x;
  } else {
    fp->flag |= X_ERR;
    return EOF;
  }
}

int xfflush(pic_state *pic, xFILE *f) {
  int retval;
  int i;

  retval = 0;
  if (f == NULL) {
    /* flush all output streams */
    for (i = 0; i < XOPEN_MAX; i++) {
      if ((pic->files[i].flag & X_WRITE) && (xfflush(pic, &pic->files[i]) == -1))
        retval = -1;
    }
  } else {
    if ((f->flag & X_WRITE) == 0)
      return -1;
    x_flushbuf(pic, EOF, f);
    if (f->flag & X_ERR)
      retval = -1;
  }
  return retval;
}

#define xgetc(pic, p)                                           \
  ((--(p)->cnt >= 0)                                            \
   ? (unsigned char) *(p)->ptr++                                \
   : x_fillbuf((pic), p))
#define xputc(pic, x, p)                                        \
  ((--(p)->cnt >= 0 && !(((p)->flag & X_LNBUF) && (x) == '\n')) \
   ? *(p)->ptr++ = (x)                                          \
   : x_flushbuf((pic), (x), (p)))

int xfputc(pic_state *pic, int x, xFILE *fp) {
  return xputc(pic, x, fp);
}

int xfgetc(pic_state *pic, xFILE *fp) {
  return xgetc(pic, fp);
}

int xfputs(pic_state *pic, const char *s, xFILE *stream) {
  const char *ptr = s;
  while(*ptr != '\0') {
    if (xputc(pic, *ptr, stream) == EOF)
      return EOF;
    ++ptr;
  }
  return (int)(ptr - s);
}

char *xfgets(pic_state *pic, char *s, int size, xFILE *stream) {
  int c = 0;
  char *buf;

  xfflush(pic, NULL);

  if (size == 0) {
    return NULL;
  }
  buf = s;
  while (--size > 0 && (c = xgetc(pic, stream)) != EOF) {
    if ((*buf++ = c) == '\n')
      break;
  }
  *buf = '\0';

  return (c == EOF && buf == s) ? NULL : s;
}

int xungetc(pic_state *PIC_UNUSED(pic), int c, xFILE *fp) {
  unsigned char uc = c;

  if (c == EOF || fp->base == fp->ptr) {
    return EOF;
  }
  fp->cnt++;
  return *--fp->ptr = uc;
}

size_t xfread(pic_state *pic, void *ptr, size_t size, size_t count, xFILE *fp) {
  char *bptr = ptr;
  long nbytes;
  int c;

  nbytes = size * count;
  while (nbytes > fp->cnt) {
    memcpy(bptr, fp->ptr, fp->cnt);
    fp->ptr += fp->cnt;
    bptr += fp->cnt;
    nbytes -= fp->cnt;
    if ((c = x_fillbuf(pic, fp)) == EOF) {
      return (size * count - nbytes) / size;
    } else {
      xungetc(pic, c, fp);
    }
  }
  memcpy(bptr, fp->ptr, nbytes);
  fp->ptr += nbytes;
  fp->cnt -= nbytes;
  return count;
}

size_t xfwrite(pic_state *pic, const void *ptr, size_t size, size_t count, xFILE *fp) {
  const char *bptr = ptr;
  long nbytes;

  nbytes = size * count;
  while (nbytes > fp->cnt) {
    memcpy(fp->ptr, bptr, fp->cnt);
    fp->ptr += fp->cnt;
    bptr += fp->cnt;
    nbytes -= fp->cnt;
    if (x_flushbuf(pic, EOF, fp) == EOF) {
      return (size * count - nbytes) / size;
    }
  }
  memcpy(fp->ptr, bptr, nbytes);
  fp->ptr += nbytes;
  fp->cnt -= nbytes;
  return count;
}

long xfseek(pic_state *pic, xFILE *fp, long offset, int whence) {
  long s;

  xfflush(pic, fp);

  fp->ptr = fp->base;
  fp->cnt = 0;

  if ((s = fp->vtable.seek(pic, fp->vtable.cookie, offset, whence)) != 0)
    return s;
  fp->flag &= ~X_EOF;
  return 0;
}

int xfprintf(pic_state *pic, xFILE *stream, const char *fmt, ...) {
  va_list ap;
  int n;

  va_start(ap, fmt);
  n = xvfprintf(pic, stream, fmt, ap);
  va_end(ap);
  return n;
}

static int print_int(pic_state *pic, xFILE *stream, long x, int base) {
  static const char digits[] = "0123456789abcdef";
  char buf[20];
  int i, c, neg;

  neg = 0;
  if (x < 0) {
    neg = 1;
    x = -x;
  }

  i = 0;
  do {
    buf[i++] = digits[x % base];
  } while ((x /= base) != 0);

  if (neg) {
    buf[i++] = '-';
  }

  c = i;
  while (i-- > 0) {
    xputc(pic, buf[i], stream);
  }
  return c;
}

int xvfprintf(pic_state *pic, xFILE *stream, const char *fmt, va_list ap) {
  const char *p;
  char *sval;
  int ival;
  void *vp;
  int cnt = 0;

  for (p = fmt; *p; p++) {
    if (*p != '%') {
      xputc(pic, *p, stream);
      cnt++;
      continue;
    }
    switch (*++p) {
    case 'd':
    case 'i':
      ival = va_arg(ap, int);
      cnt += print_int(pic, stream, ival, 10);
      break;
    case 'f': {
      char buf[64];
      PIC_DOUBLE_TO_CSTRING(va_arg(ap, double), buf);
      cnt += xfputs(pic, buf, stream);
      break;
    }
    case 'c':
      ival = va_arg(ap, int);
      cnt += xfputc(pic, ival, stream);
      break;
    case 's':
      sval = va_arg(ap, char*);
      cnt += xfputs(pic, sval, stream);
      break;
    case 'p':
      vp = va_arg(ap, void*);
      cnt += xfputs(pic, "0x", stream);
      cnt += print_int(pic, stream, (long)vp, 16);
      break;
    case '%':
      xputc(pic, *(p-1), stream);
      cnt++;
      break;
    default:
      xputc(pic, '%', stream);
      xputc(pic, *(p-1), stream);
      cnt += 2;
      break;
    }
  }
  return cnt;
}

xFILE *xfile_xstdin(pic_state *pic) { return &pic->files[0]; }
xFILE *xfile_xstdout(pic_state *pic) { return &pic->files[1]; }
xFILE *xfile_xstderr(pic_state *pic) { return &pic->files[2]; }

#if PIC_ENABLE_STDIO

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
file_close(pic_state *PIC_UNUSED(pic), void *cookie) {
  return fclose(cookie);
}

xFILE *xfopen_file(pic_state *pic, FILE *fp, const char *mode) {
  xFILE *f;
  if (*mode == 'r') {
    f = xfunopen(pic, fp, file_read, 0, file_seek, file_close);
  } else {
    f = xfunopen(pic, fp, 0, file_write, file_seek, file_close);
  }
  return f;
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
  xbuf_t *m = cookie;

  pic_free(pic, m->buf);
  pic_free(pic, m);
  return 0;
}

xFILE *xfopen_buf(pic_state *pic, const char *data, int size, const char *mode) {
  xbuf_t *m;
  xFILE *file;

  m = pic_malloc(pic, sizeof(xbuf_t));
  m->buf = pic_malloc(pic, size);
  m->pos = 0;
  m->end = size;
  m->capa = size;

  if (*mode == 'r') {
    memcpy(m->buf, data, size);
    file = xfunopen(pic, m, string_read, NULL, string_seek, string_close);
  } else {
    file = xfunopen(pic, m, NULL, string_write, string_seek, string_close);
  }
  if (file == NULL) {
    string_close(pic, m);
  }
  return file;
}

int xfget_buf(pic_state *pic, xFILE *file, const char **buf, int *len) {
  xbuf_t *s;

  xfflush(pic, file);

  if (file->vtable.write != string_write) {
    return -1;
  }
  s = file->vtable.cookie;
  *len = s->end;
  *buf = s->buf;
  return 0;
}

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

xFILE *xfopen_null(pic_state *PIC_UNUSED(pic), const char *mode) {
  switch (*mode) {
  case 'r':
    return xfunopen(pic, 0, null_read, 0, null_seek, null_close);
  default:
    return xfunopen(pic, 0, 0, null_write, null_seek, null_close);
  }
}
