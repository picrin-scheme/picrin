/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/error.h"
#include "picrin/string.h"
#include "xfile_stdio.h"

void pic_init_contrib(pic_state *);
void pic_load_piclib(pic_state *);

static pic_value
pic_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->features;
}

static pic_value
pic_libraries(pic_state *pic)
{
  pic_value libs = pic_nil_value(), lib, it;

  pic_get_args(pic, "");

  pic_for_each (lib, pic->libs, it) {
    libs = pic_cons(pic, pic_car(pic, lib), libs);
  }

  return libs;
}

void
pic_init_picrin(pic_state *pic)
{
  pic_add_feature(pic, "r7rs");

  pic_deflibrary (pic, "(picrin library)") {
    pic_defun(pic, "libraries", pic_libraries);
  }

  pic_deflibrary (pic, "(scheme base)") {
    pic_defun(pic, "features", pic_features);
  }

  pic_init_contrib(pic);
  pic_load_piclib(pic);
}

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  struct pic_lib *PICRIN_MAIN;
  int status = 0;

  pic = pic_open(argc, argv, envp, xstdin, xstdout, xstdout);

  pic_init_picrin(pic);

  PICRIN_MAIN = pic_find_library(pic, pic_read_cstr(pic, "(picrin main)"));

  pic_try {
    pic_funcall(pic, PICRIN_MAIN, "main", pic_nil_value());
  }
  pic_catch {
    fputs(pic_str_cstr(pic_format_error(pic)), stderr);
    status = 1;
  }

  pic_close(pic);

  return status;
}
