/**
 * See Copyright Notice in picrin.h
 */

#include "config.h"

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/error.h"

void pic_init_contrib(pic_state *);
void pic_load_piclib(pic_state *);

static pic_value
pic_features(pic_state *pic)
{
  pic_value features = pic_nil_value();

  pic_get_args(pic, "");

  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "r7rs")), features);
  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "ieee-float")), features);
  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "picrin")), features);

  return features;
}

static pic_value
pic_libraries(pic_state *pic)
{
  pic_value libs = pic_nil_value(), lib;

  pic_get_args(pic, "");

  pic_for_each (lib, pic->libs) {
    libs = pic_cons(pic, pic_car(pic, lib), libs);
  }

  return libs;
}

void
pic_init_picrin(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin library)") {
    pic_defun(pic, "libraries", pic_libraries);
  }

  pic_deflibrary (pic, "(scheme base)") {
    pic_defun(pic, "features", pic_features);

    pic_init_contrib(pic);
    pic_load_piclib(pic);
  }
}

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  int status = 0;

  pic = pic_open(argc, argv, envp);

  pic_init_picrin(pic);

  pic_try {
    pic_import(pic, pic_read_cstr(pic, "(picrin main)"));
    pic_funcall(pic, "main", pic_nil_value());
  }
  pic_catch {
    pic_print_backtrace(pic, pic->err);
    status = 1;
  }

  pic_close(pic);

  return status;
}
