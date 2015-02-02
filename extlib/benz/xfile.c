#include "picrin.h"

#define XF_EOF 1
#define XF_ERR 2

static xFILE pool[XFOPEN_MAX];

xFILE *
xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*flush)(void *), int (*close)(void *))
{
  xFILE *file;
  static int c = 0;

  for (file = pool; file < pool + XFOPEN_MAX; file++) {
    if (file->vtable.read == 0 && file->vtable.write == 0) {
      break;
    }
  }
  if (file >= pool + XFOPEN_MAX) {
    return NULL;
  }

  file->ungot = -1;
  file->err = 0;
  /* set vtable */
  file->vtable.cookie = cookie;
  file->vtable.read = read;
  file->vtable.write = write;
  file->vtable.seek = seek;
  file->vtable.flush = flush;
  file->vtable.close = close;

  return file;
}

int
xfclose(xFILE *file)
{
  int r;

  r = xfflush(file);
  if (r == EOF) {
    return -1;
  }
  r = file->vtable.close(file->vtable.cookie);
  if (r == EOF) {
    return -1;
  }
  file->vtable.read = NULL;
  file->vtable.write = NULL;
  return 0;
}

int
xfflush(xFILE *file)
{
  return file->vtable.flush(file->vtable.cookie);
}

#define min(a,b) ((a) < (b) ? (a) : (b))

size_t
xfread(void *ptr, size_t block, size_t nitems, xFILE *file)
{
  char buf[256];
  char *dst = ptr;
  size_t i;
  int n, rest;

  for (i = 0; i < nitems; ++i) {
    rest = (int)block;
    if (file->ungot != -1 && block > 0) {
      *dst++ = file->ungot;
      rest--;
      file->ungot = -1;
    }
    for (; rest > 0; rest -= n) {
      n = file->vtable.read(file->vtable.cookie, buf, min((int)sizeof buf, rest));
      if (n < 0) {
        file->err |= XF_ERR;
        goto exit;
      }
      if (n == 0) {
        file->err |= XF_EOF;
        goto exit;
      }
      memcpy(dst, buf, n);
      dst += (unsigned)n;
    }
  }

 exit:
  return i;
}

size_t
xfwrite(const void *ptr, size_t block, size_t nitems, xFILE *file)
{
  char *dst = (char *)ptr;
  size_t i, offset;
  int n;

  for (i = 0; i < nitems; ++i) {
    offset = 0;
    while (offset < block) {
      n = file->vtable.write(file->vtable.cookie, dst + offset, (int)(block - offset));
      if (n < 0) {
        file->err |= XF_ERR;
        goto exit;
      }
      offset += (unsigned)n;
    }
    dst += block;
  }

 exit:
  return i;
}

long
xfseek(xFILE *file, long offset, int whence)
{
  file->ungot = -1;
  return file->vtable.seek(file->vtable.cookie, offset, whence);
}

long
xftell(xFILE *file)
{
  return xfseek(file, 0, XF_SEEK_CUR);
}

void
xrewind(xFILE *file)
{
  xfseek(file, 0, XF_SEEK_SET);
}

void
xclearerr(xFILE *file)
{
  file->err = 0;
}

int
xfeof(xFILE *file)
{
  return file->err & XF_EOF;
}

int
xferror(xFILE *file)
{
  return file->err & XF_ERR;
}

int
xfgetc(xFILE *file)
{
  char buf[1];

  xfread(buf, 1, 1, file);

  if (xfeof(file) || xferror(file)) {
    return EOF;
  }

  return buf[0];
}

int
xgetc(xFILE *file)
{
  return xfgetc(file);
}

char *
xfgets(char *str, int size, xFILE *file)
{
  int c = EOF, i;

  for (i = 0; i < size - 1 && c != '\n'; ++i) {
    if ((c = xfgetc(file)) == EOF) {
      break;
    }
    str[i] = (char)c;
  }
  if (i == 0 && c == EOF) {
    return NULL;
  }
  if (xferror(file)) {
    return NULL;
  }
  str[i] = '\0';

  return str;
}

int
xungetc(int c, xFILE *file)
{
  file->ungot = c;
  if (c != EOF) {
    file->err &= ~XF_EOF;
  }
  return c;
}

int
xfputc(int c, xFILE *file)
{
  char buf[1];

  buf[0] = (char)c;
  xfwrite(buf, 1, 1, file);

  if (xferror(file)) {
    return EOF;
  }
  return buf[0];
}

int
xputc(int c, xFILE *file)
{
  return xfputc(c, file);
}

int
xfputs(const char *str, xFILE *file)
{
  xfwrite(str, strlen(str), 1, file);

  if (xferror(file)) {
    return EOF;
  }
  return 0;
}

int
xfprintf(xFILE *stream, const char *fmt, ...)
{
  va_list ap;
  int n;

  va_start(ap, fmt);
  n = xvfprintf(stream, fmt, ap);
  va_end(ap);
  return n;
}

static void
print_int(xFILE *stream, long x, int base)
{
  static char digits[] = "0123456789abcdef";
  char buf[20];
  int i, neg;

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

  while (i-- > 0) {
    xputc(buf[i], stream);
  }
}

#define ABS(x) ((x) < 0 ? -(x) : (x))

int
xvfprintf(xFILE *stream, const char *fmt, va_list ap)
{
  const char *p;
  char *sval;
  int ival;
#if PIC_ENABLE_FLOAT
  double dval;
#endif
  void *vp;
  long seekr = xftell(stream);

  for (p = fmt; *p; p++) {
    if (*p != '%') {
      xputc(*p, stream);
      continue;
    }
    switch (*++p) {
    case 'd':
    case 'i':
      ival = va_arg(ap, int);
      print_int(stream, ival, 10);
      break;
#if PIC_ENABLE_FLOAT
    case 'f':
      dval = va_arg(ap, double);
      print_int(stream, dval, 10);
      xputc('.', stream);
      if ((ival = ABS((dval - floor(dval)) * 1e4) + 0.5) == 0) {
        xfputs("0000", stream);
      } else {
        int i;
        for (i = 0; i < 3 - (int)log10(ival); ++i) {
          xputc('0', stream);
        }
        print_int(stream, ival, 10);
      }
      break;
#endif
    case 's':
      sval = va_arg(ap, char*);
      xfputs(sval, stream);
      break;
    case 'p':
      vp = va_arg(ap, void*);
      xfputs("0x", stream);
      print_int(stream, (long)vp, 16);
      break;
    case '%':
      xputc(*(p-1), stream);
      break;
    default:
      xputc('%', stream);
      xputc(*(p-1), stream);
      break;
    }
  }
  return xftell(stream) - seekr;
}
