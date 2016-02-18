/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

#include <stdio.h>

PIC_NORETURN static void
file_error(pic_state *pic, const char *msg)
{
  pic_error(pic, "file", msg, pic_nil_value(pic));
}

pic_value
pic_file_open_input_file(pic_state *pic)
{
  static const short flags = PIC_PORT_IN;
  char *fname;

  pic_get_args(pic, "z", &fname);

  return pic_obj_value(pic_open_file(pic, fname, flags));
}

pic_value
pic_file_open_binary_input_file(pic_state *pic)
{
  static const short flags = PIC_PORT_IN;
  char *fname;

  pic_get_args(pic, "z", &fname);

  return pic_obj_value(pic_open_file(pic, fname, flags));
}

pic_value
pic_file_open_output_file(pic_state *pic)
{
  static const short flags = PIC_PORT_OUT;
  char *fname;

  pic_get_args(pic, "z", &fname);

  return pic_obj_value(pic_open_file(pic, fname, flags));
}

pic_value
pic_file_open_binary_output_file(pic_state *pic)
{
  static const short flags = PIC_PORT_OUT;
  char *fname;

  pic_get_args(pic, "z", &fname);

  return pic_obj_value(pic_open_file(pic, fname, flags));
}

pic_value
pic_file_exists_p(pic_state *pic)
{
  char *fname;
  FILE *fp;

  pic_get_args(pic, "z", &fname);

  fp = fopen(fname, "r");
  if (fp) {
    fclose(fp);
    return pic_true_value(pic);
  } else {
    return pic_false_value(pic);
  }
}

pic_value
pic_file_delete(pic_state *pic)
{
  char *fname;

  pic_get_args(pic, "z", &fname);

  if (remove(fname) != 0) {
    file_error(pic, "file cannot be deleted");
  }
  return pic_undef_value(pic);
}

void
pic_init_file(pic_state *pic)
{
  pic_deflibrary(pic, "scheme.file");

  pic_defun(pic, "open-input-file", pic_file_open_input_file);
  pic_defun(pic, "open-binary-input-file", pic_file_open_binary_input_file);
  pic_defun(pic, "open-output-file", pic_file_open_output_file);
  pic_defun(pic, "open-binary-output-file", pic_file_open_binary_output_file);
  pic_defun(pic, "file-exists?", pic_file_exists_p);
  pic_defun(pic, "delete-file", pic_file_delete);
}
