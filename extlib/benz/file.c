#include "picrin.h"

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
    fp->cnt = BUFSIZ - 1;
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
  int c;
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

int xputs(pic_state *pic, const char *s) {
  int i = 1;

  while(*s != '\0') {
    if (xputchar(pic, *s++) == EOF)
      return EOF;
    i++;
  }
  if (xputchar(pic, '\n') == EOF) {
    return EOF;
  }
  return i;
}

char *xgets(pic_state *pic, char *s) {
  int c;
  char *buf;

  xfflush(pic, NULL);

  buf = s;
  while ((c = xgetchar(pic)) != EOF && c != '\n') {
    *buf++ = c;
  }
  *buf = '\0';

  return (c == EOF && buf == s) ? NULL : s;
}

int xungetc(int c, xFILE *fp) {
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
      xungetc(c, fp);
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

long xftell(pic_state *pic, xFILE *fp) {
  return xfseek(pic, fp, 0, XSEEK_CUR);
}

void xrewind(pic_state *pic, xFILE *fp) {
  xfseek(pic, fp, 0, XSEEK_SET);
  xclearerr(fp);
}

int xprintf(pic_state *pic, const char *fmt, ...) {
  va_list ap;
  int n;

  va_start(ap, fmt);
  n = xvfprintf(pic, xstdout, fmt, ap);
  va_end(ap);
  return n;
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
#if PIC_ENABLE_FLOAT
  double dval;
#endif
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
#if PIC_ENABLE_FLOAT
    case 'f':
      dval = va_arg(ap, double);
      cnt += print_int(pic, stream, dval, 10);
      xputc(pic, '.', stream);
      cnt++;
      if ((ival = fabs((dval - floor(dval)) * 1e4) + 0.5) == 0) {
        cnt += xfputs(pic, "0000", stream);
      } else {
        int i;
        for (i = 0; i < 3 - (int)log10(ival); ++i) {
          xputc(pic, '0', stream);
          cnt++;
        }
        cnt += print_int(pic, stream, ival, 10);
      }
      break;
#endif
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

#if 0
int main()
{
  char buf[256];

  xgets(buf);

  xprintf("%s\n", buf);
  xprintf("hello\n");
  xprintf("hello\n");
  //  xfflush(0);
}
#endif
