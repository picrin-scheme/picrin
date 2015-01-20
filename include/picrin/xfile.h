#ifndef XFILE_H
#define XFILE_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct {
  int ungot;
  int flags;
  /* operators */
  struct {
    void *cookie;
    int (*read)(void *, char *, int);
    int (*write)(void *, const char *, int);
    long (*seek)(void *, long, int);
    int (*flush)(void *);
    int (*close)(void *);
  } vtable;
} xFILE;

/* generic file constructor */
static inline xFILE *xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*flush)(void *), int (*close)(void *));

/* resource aquisition */
static inline xFILE *xfpopen(FILE *);
static inline xFILE *xmopen();
static inline xFILE *xfopen(const char *, const char *);
static inline int xfclose(xFILE *);

/* buffer management */
static inline int xfflush(xFILE *);

/* direct IO with buffering */
static inline size_t xfread(void *, size_t, size_t, xFILE *);
static inline size_t xfwrite(const void *, size_t, size_t, xFILE *);

/* indicator positioning */
static inline long xfseek(xFILE *, long offset, int whence);
static inline long xftell(xFILE *);
static inline void xrewind(xFILE *);

/* stream status */
static inline void xclearerr(xFILE *);
static inline int xfeof(xFILE *);
static inline int xferror(xFILE *);

/* character IO */
static inline int xfgetc(xFILE *);
static inline char *xfgets(char *, int, xFILE *);
static inline int xfputc(int, xFILE *);
static inline int xfputs(const char *, xFILE *);
static inline int xgetc(xFILE *);
static inline int xgetchar(void);
static inline int xputc(int, xFILE *);
static inline int xputchar(int);
static inline int xputs(const char *);
static inline int xungetc(int, xFILE *);

/* formatted I/O */
static inline int xprintf(const char *, ...);
static inline int xfprintf(xFILE *, const char *, ...);
static inline int xvfprintf(xFILE *, const char *, va_list);

/* standard I/O */
#define xstdin (xstdin_())
#define xstdout (xstdout_())
#define xstderr (xstderr_())


/* private */

#define XF_EOF 1
#define XF_ERR 2

static inline xFILE *
xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*flush)(void *), int (*close)(void *))
{
  xFILE *file;

  file = (xFILE *)malloc(sizeof(xFILE));
  if (! file) {
    return NULL;
  }
  file->ungot = -1;
  file->flags = 0;
  /* set vtable */
  file->vtable.cookie = cookie;
  file->vtable.read = read;
  file->vtable.write = write;
  file->vtable.seek = seek;
  file->vtable.flush = flush;
  file->vtable.close = close;

  return file;
}

/*
 * Derieved xFILE Classes
 */

static inline int
xf_file_read(void *cookie, char *ptr, int size)
{
  FILE *file = cookie;
  int r;

  r = (int)fread(ptr, 1, (size_t)size, file);
  if (r < size && ferror(file)) {
    return -1;
  }
  if (r == 0 && feof(file)) {
    clearerr(file);
  }
  return r;
}

static inline int
xf_file_write(void *cookie, const char *ptr, int size)
{
  FILE *file = cookie;
  int r;

  r = (int)fwrite(ptr, 1, (size_t)size, file);
  if (r < size) {
    return -1;
  }
  return r;
}

static inline long
xf_file_seek(void *cookie, long pos, int whence)
{
  return fseek(cookie, pos, whence);
}

static inline int
xf_file_flush(void *cookie)
{
  return fflush(cookie);
}

static inline int
xf_file_close(void *cookie)
{
  return fclose(cookie);
}

static inline xFILE *
xfpopen(FILE *fp)
{
  xFILE *file;

  file = xfunopen(fp, xf_file_read, xf_file_write, xf_file_seek, xf_file_flush, xf_file_close);
  if (! file) {
    return NULL;
  }

  return file;
}

#define XF_FILE_VTABLE xf_file_read, xf_file_write, xf_file_seek, xf_file_flush, xf_file_close

static inline xFILE *
xstdin_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stdin;
  }
  return &x;
}

static inline xFILE *
xstdout_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stdout;
  }
  return &x;
}

static inline xFILE *
xstderr_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stderr;
  }
  return &x;
}

struct xf_membuf {
  char *buf;
  long pos, end, capa;
};

static inline int
xf_mem_read(void *cookie, char *ptr, int size)
{
  struct xf_membuf *mem;

  mem = (struct xf_membuf *)cookie;

  if (size > (int)(mem->end - mem->pos))
    size = (int)(mem->end - mem->pos);
  memcpy(ptr, mem->buf + mem->pos, size);
  mem->pos += size;
  return size;
}

static inline int
xf_mem_write(void *cookie, const char *ptr, int size)
{
  struct xf_membuf *mem;

  mem = (struct xf_membuf *)cookie;

  if (mem->pos + size >= mem->capa) {
    mem->capa = (mem->pos + size) * 2;
    mem->buf = realloc(mem->buf, (size_t)mem->capa);
  }
  memcpy(mem->buf + mem->pos, ptr, size);
  mem->pos += size;
  if (mem->end < mem->pos)
    mem->end = mem->pos;
  return size;
}

static inline long
xf_mem_seek(void *cookie, long pos, int whence)
{
  struct xf_membuf *mem;

  mem = (struct xf_membuf *)cookie;

  switch (whence) {
  case SEEK_SET:
    mem->pos = pos;
    break;
  case SEEK_CUR:
    mem->pos += pos;
    break;
  case SEEK_END:
    mem->pos = mem->end + pos;
    break;
  }

  return mem->pos;
}

static inline int
xf_mem_flush(void *cookie)
{
  (void)cookie;

  return 0;
}

static inline int
xf_mem_close(void *cookie)
{
  struct xf_membuf *mem;

  mem = (struct xf_membuf *)cookie;
  free(mem->buf);
  free(mem);
  return 0;
}

static inline xFILE *
xmopen()
{
  struct xf_membuf *mem;

  mem = (struct xf_membuf *)malloc(sizeof(struct xf_membuf));
  mem->buf = (char *)malloc(BUFSIZ);
  mem->pos = 0;
  mem->end = 0;
  mem->capa = BUFSIZ;

  return xfunopen(mem, xf_mem_read, xf_mem_write, xf_mem_seek, xf_mem_flush, xf_mem_close);
}

#undef XF_FILE_VTABLE

static inline xFILE *
xfopen(const char *filename, const char *mode)
{
  FILE *fp;
  xFILE *file;

  fp = fopen(filename, mode);
  if (! fp) {
    return NULL;
  }

  file = xfpopen(fp);
  if (! file) {
    return NULL;
  }

  return file;
}

static inline int
xfclose(xFILE *file)
{
  int r;

  r = file->vtable.close(file->vtable.cookie);
  if (r == EOF) {
    return -1;
  }

  free(file);
  return 0;
}

static inline int
xfflush(xFILE *file)
{
  return file->vtable.flush(file->vtable.cookie);
}

static inline size_t
xfread(void *ptr, size_t block, size_t nitems, xFILE *file)
{
  char *dst = (char *)ptr;
  char buf[block];
  size_t i, offset;
  int n;

  for (i = 0; i < nitems; ++i) {
    offset = 0;
    if (file->ungot != -1 && block > 0) {
      buf[0] = (char)file->ungot;
      offset += 1;
      file->ungot = -1;
    }
    while (offset < block) {
      n = file->vtable.read(file->vtable.cookie, buf + offset, (int)(block - offset));
      if (n < 0) {
        file->flags |= XF_ERR;
        goto exit;
      }
      if (n == 0) {
        file->flags |= XF_EOF;
        goto exit;
      }
      offset += (unsigned)n;
    }
    memcpy(dst, buf, block);
    dst += block;
  }

 exit:
  return i;
}

static inline size_t
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
        file->flags |= XF_ERR;
        goto exit;
      }
      offset += (unsigned)n;
    }
    dst += block;
  }

 exit:
  return i;
}

static inline long
xfseek(xFILE *file, long offset, int whence)
{
  file->ungot = -1;
  return file->vtable.seek(file->vtable.cookie, offset, whence);
}

static inline long
xftell(xFILE *file)
{
  return xfseek(file, 0, SEEK_CUR);
}

static inline void
xrewind(xFILE *file)
{
  xfseek(file, 0, SEEK_SET);
}

static inline void
xclearerr(xFILE *file)
{
  file->flags = 0;
}

static inline int
xfeof(xFILE *file)
{
  return file->flags & XF_EOF;
}

static inline int
xferror(xFILE *file)
{
  return file->flags & XF_ERR;
}

static inline int
xfgetc(xFILE *file)
{
  char buf[1];

  xfread(buf, 1, 1, file);

  if (xfeof(file) || xferror(file)) {
    return EOF;
  }

  return buf[0];
}

static inline int
xgetc(xFILE *file)
{
  return xfgetc(file);
}

static inline char *
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

static inline int
xungetc(int c, xFILE *file)
{
  file->ungot = c;
  if (c != EOF) {
    file->flags &= ~XF_EOF;
  }
  return c;
}

static inline int
xgetchar(void)
{
  return xfgetc(xstdin);
}

static inline int
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

static inline int
xputc(int c, xFILE *file)
{
  return xfputc(c, file);
}

static inline int
xputchar(int c)
{
  return xfputc(c, xstdout);
}

static inline int
xfputs(const char *str, xFILE *file)
{
  size_t len;

  len = strlen(str);
  xfwrite(str, len, 1, file);

  if (xferror(file)) {
    return EOF;
  }
  return 0;
}

static inline int
xputs(const char *s)
{
  return xfputs(s, xstdout);
}

static inline int
xprintf(const char *fmt, ...)
{
  va_list ap;
  int n;

  va_start(ap, fmt);
  n = xvfprintf(xstdout, fmt, ap);
  va_end(ap);
  return n;
}

static inline int
xfprintf(xFILE *stream, const char *fmt, ...)
{
  va_list ap;
  int n;

  va_start(ap, fmt);
  n = xvfprintf(stream, fmt, ap);
  va_end(ap);
  return n;
}

static inline int
xvfprintf(xFILE *stream, const char *fmt, va_list ap)
{
  va_list ap2;

  va_copy(ap2, ap);
  {
    char buf[vsnprintf(NULL, 0, fmt, ap2)];

    vsnprintf(buf, sizeof buf + 1, fmt, ap);

    if (xfwrite(buf, sizeof buf, 1, stream) < 1) {
      return -1;
    }

    va_end(ap2);
    return (int)(sizeof buf);
  }
}

#if defined(__cplusplus)
}
#endif

#endif
