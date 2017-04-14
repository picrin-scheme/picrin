/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>

#include "picrin.h"

#if PIC_USE_FILE

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

PIC_NORETURN static void
file_error(pic_state *pic, const char *msg, const char *fname)
{
  pic_value fn = pic_cstr_value(pic, fname);

  pic_raise(pic, pic_make_error(pic, "file", msg, pic_list(pic, 1, fn)));
}

pic_value
pic_file_open_input_file(pic_state *pic)
{
  const char *fname;
  FILE *fp;

  pic_get_args(pic, "z", &fname);

  if ((fp = fopen(fname, "r")) == NULL) {
    file_error(pic, "could not open file", fname);
  }
  return pic_fopen(pic, fp, "r");
}

pic_value
pic_file_open_output_file(pic_state *pic)
{
  const char *fname;
  FILE *fp;

  pic_get_args(pic, "z", &fname);

  if ((fp = fopen(fname, "w")) == NULL) {
    file_error(pic, "could not open file", fname);
  }
  return pic_fopen(pic, fp, "w");
}

pic_value
pic_file_exists_p(pic_state *pic)
{
  const char *fname;
  FILE *fp;

  pic_get_args(pic, "z", &fname);

  fp = fopen(fname, "r");
  if (fp) {
    fclose(fp);
  }
  return pic_bool_value(pic, fp != NULL);
}

pic_value
pic_file_delete(pic_state *pic)
{
  const char *fname;

  pic_get_args(pic, "z", &fname);

  if (remove(fname) != 0) {
    file_error(pic, "file cannot be deleted", fname);
  }
  return pic_undef_value(pic);
}

void
pic_init_file(pic_state *pic)
{
  pic_defvar(pic, "current-input-port", pic_fopen(pic, stdin, "r"));
  pic_defvar(pic, "current-output-port", pic_fopen(pic, stdout, "w"));
  pic_defvar(pic, "current-error-port", pic_fopen(pic, stdout, "w"));
  pic_defun(pic, "open-binary-input-file", pic_file_open_input_file);
  pic_defun(pic, "open-binary-output-file", pic_file_open_output_file);
  pic_defun(pic, "file-exists?", pic_file_exists_p);
  pic_defun(pic, "delete-file", pic_file_delete);
}

#endif
