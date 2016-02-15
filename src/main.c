/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void pic_init_contrib(pic_state *);
void pic_load_piclib(pic_state *);

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

  pic_deflibrary(pic, "(picrin library)");
  pic_defun(pic, "libraries", pic_libraries);

  pic_init_contrib(pic);
  pic_load_piclib(pic);
}

int picrin_argc;
char **picrin_argv;
char **picrin_envp;

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  struct pic_lib *PICRIN_MAIN;
  int status;

  pic = pic_open(pic_default_allocf, NULL);

  picrin_argc = argc;
  picrin_argv = argv;
  picrin_envp = envp;

  pic_try {
    pic_init_picrin(pic);

    PICRIN_MAIN = pic_find_library(pic, pic_read_cstr(pic, "(picrin main)"));

    pic_funcall(pic, PICRIN_MAIN, "main", 0);

    status = 0;
  }
  pic_catch {
    pic_print_backtrace(pic, xstderr);
    status = 1;
  }

  pic_close(pic);

  return status;
}
