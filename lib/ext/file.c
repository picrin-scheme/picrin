/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>

#include "picrin.h"

#if PIC_USE_LIBC

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

pic_value
pic_fopen(pic_state *pic, FILE *fp, const char *mode) {
  static const pic_port_type file_rd = { file_read, 0, file_seek, file_close };
  static const pic_port_type file_wr = { 0, file_write, file_seek, file_close };

  if (*mode == 'r') {
    return pic_funopen(pic, fp, &file_rd);
  } else {
    return pic_funopen(pic, fp, &file_wr);
  }
}

void
pic_init_file(pic_state *PIC_UNUSED(pic))
{
#if PIC_USE_STDIO
  pic_defvar(pic, "current-input-port", pic_fopen(pic, stdin, "r"));
  pic_defvar(pic, "current-output-port", pic_fopen(pic, stdout, "w"));
  pic_defvar(pic, "current-error-port", pic_fopen(pic, stdout, "w"));
#endif
}

#endif
