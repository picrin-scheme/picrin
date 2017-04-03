/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

#if PIC_USE_LIBRARY

void
pic_deflibrary(pic_state *pic, const char *lib)
{
  pic_value name = pic_intern_cstr(pic, lib), v;

  v = pic_funcall(pic, "find-library", 1, name);
  if (! pic_bool(pic, v)) {
    pic_funcall(pic, "make-library", 1, name);
  }
}

void
pic_in_library(pic_state *pic, const char *lib)
{
  pic_value name = pic_intern_cstr(pic, lib);

  pic_funcall(pic, "current-library", 1, name);
}

void
pic_export(pic_state *pic, int n, ...)
{
  size_t ai = pic_enter(pic);
  va_list ap;

  va_start(ap, n);
  while (n--) {
    pic_value var = pic_intern_cstr(pic, va_arg(ap, const char *));
    pic_funcall(pic, "library-export", 2, var, var);
  }
  va_end(ap);
  pic_leave(pic, ai);
}

#endif
