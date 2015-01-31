#include "picrin.h"
#include "picrin/xfile.h"

#include <stdio.h>

PIC_INLINE xFILE *xfpopen(FILE *);
PIC_INLINE xFILE *xfopen(const char *, const char *);

/* standard I/O */
#define xstdin (xstdin_())
#define xstdout (xstdout_())
#define xstderr (xstderr_())

PIC_INLINE int
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

PIC_INLINE int
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

PIC_INLINE long
xf_file_seek(void *cookie, long pos, int whence)
{
  return fseek(cookie, pos, whence);
}

PIC_INLINE int
xf_file_flush(void *cookie)
{
  return fflush(cookie);
}

PIC_INLINE int
xf_file_close(void *cookie)
{
  return fclose(cookie);
}

PIC_INLINE xFILE *
xfpopen(FILE *fp)
{
  xFILE *file;

  file = xfunopen(fp, xf_file_read, xf_file_write, xf_file_seek, xf_file_flush, xf_file_close);
  if (! file) {
    return NULL;
  }

  return file;
}

PIC_INLINE xFILE *
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

#define XF_FILE_VTABLE xf_file_read, xf_file_write, xf_file_seek, xf_file_flush, xf_file_close

PIC_INLINE xFILE *
xstdin_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stdin;
  }
  return &x;
}

PIC_INLINE xFILE *
xstdout_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stdout;
  }
  return &x;
}

PIC_INLINE xFILE *
xstderr_()
{
  static xFILE x = { -1, 0, { NULL, XF_FILE_VTABLE } };

  if (! x.vtable.cookie) {
    x.vtable.cookie = stderr;
  }
  return &x;
}
