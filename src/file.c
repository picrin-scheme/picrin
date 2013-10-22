#include <stdio.h>

#include "picrin.h"

pic_value
pic_file_exists_p(pic_state *pic)
{
  char *fname;
  size_t size;
  FILE *fp;

  pic_get_args(pic, "s", &fname, &size);

  fp = fopen(fname, "r");
  if (fp) {
    fclose(fp);
    return pic_true_value();
  } else {
    return pic_false_value();
  }
}

pic_value
pic_file_delete(pic_state *pic)
{
  char *fname;
  size_t size;

  pic_get_args(pic, "s", &fname, &size);

  if (remove(fname) != 0) {
    pic_error(pic, "file cannot be deleted");
  }
  return pic_true_value();
}

void
pic_init_file(pic_state *pic)
{
  pic_defun(pic, "file-exists?", pic_file_exists_p);
  pic_defun(pic, "delete-file", pic_file_delete);
}
