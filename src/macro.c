/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
pic_macro_gensym(pic_state *pic)
{
  static const char skel[] = ".g";
  pic_sym uniq;

  pic_get_args(pic, "");

  uniq = pic_gensym(pic, pic_intern_cstr(pic, skel));
  return pic_sym_value(uniq);
}

static pic_value
pic_macro_ungensym(pic_state *pic)
{
  pic_sym sym;

  pic_get_args(pic, "m", &sym);

  return pic_sym_value(pic_ungensym(pic, sym));
}

static pic_value
pic_macro_macroexpand(pic_state *pic)
{
  pic_value expr;

  pic_get_args(pic, "o", &expr);

  return pic_macroexpand(pic, expr, pic->lib);
}

void
pic_init_macro2(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin macro)") {
    pic_defun(pic, "gensym", pic_macro_gensym);
    pic_defun(pic, "ungensym", pic_macro_ungensym);
    pic_defun(pic, "macroexpand", pic_macro_macroexpand);
  }
}
