/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/error.h"
#include "picrin/string.h"
#include "xfile_stdio.h"

#include <stdlib.h>
#include <setjmp.h>

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

static void *
pic_default_allocf(void *ptr, size_t size)
{
  if (size == 0) {
    if (ptr) {
      free(ptr);
    }
    return NULL;
  }
  if (ptr) {
    return realloc(ptr, size);
  } else {
    return malloc(size);
  }
}

static int
pic_default_setjmpf(void *buf)
{
  return setjmp(*(jmp_buf *)buf);
}

static void
pic_default_longjmpf(void *buf, int val)
{
  if (buf == NULL) {
    abort();
  }
  longjmp(*(jmp_buf *)buf, val);
}

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  struct pic_lib *PICRIN_MAIN;
  int status = 0;

  pic = pic_open(pic_default_allocf, pic_default_setjmpf, pic_default_longjmpf, sizeof(jmp_buf), argc, argv, envp, xstdin, xstdout, xstdout);

  pic_init_picrin(pic);

  PICRIN_MAIN = pic_find_library(pic, pic_read_cstr(pic, "(picrin main)"));

  pic_try {
    pic_funcall(pic, PICRIN_MAIN, "main", pic_nil_value());
  }
  pic_catch {
    pic_print_backtrace(pic, xstderr);
    status = 1;
  }

  pic_close(pic);

  return status;
}
