/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/lib.h"
#include "picrin/pair.h"
#include "picrin/macro.h"

struct pic_lib *
pic_make_library(pic_state *pic, pic_value name)
{
  struct pic_lib *lib;
  struct pic_senv *senv;

  if ((lib = pic_find_library(pic, name)) != NULL) {

#if DEBUG
    printf("* reopen library: ");
    pic_debug(pic, name);
    puts("");
#endif

    return lib;
  }

  senv = pic_null_syntactic_environment(pic);

  lib = (struct pic_lib *)pic_obj_alloc(pic, sizeof(struct pic_lib), PIC_TT_LIB);
  lib->senv = senv;
  lib->name = name;
  xh_init_int(&lib->exports, sizeof(pic_sym));

  /* register! */
  pic->lib_tbl = pic_acons(pic, name, pic_obj_value(lib), pic->lib_tbl);

  return lib;
}

void
pic_in_library(pic_state *pic, pic_value spec)
{
  struct pic_lib *lib;

  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_errorf(pic, "library not found: ~a", spec);
  }
  pic->lib = lib;
}

struct pic_lib *
pic_find_library(pic_state *pic, pic_value spec)
{
  pic_value v;

  v = pic_assoc(pic, spec, pic->lib_tbl);
  if (pic_false_p(v)) {
    return NULL;
  }
  return pic_lib_ptr(pic_cdr(pic, v));
}

void
pic_import(pic_state *pic, pic_value spec)
{
  struct pic_lib *lib;
  xh_iter it;

  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_errorf(pic, "library not found: ~a", spec);
  }
  xh_begin(&it, &lib->exports);
  while (xh_next(&it)) {

#if DEBUG
    printf("* importing %s as %s\n", pic_symbol_name(pic, xh_key(it.e, pic_sym)), pic_symbol_name(pic, xh_val(it.e, pic_sym)));
#endif

    pic_put_rename(pic, pic->lib->senv, xh_key(it.e, pic_sym), xh_val(it.e, pic_sym));
  }
}

void
pic_export(pic_state *pic, pic_sym sym)
{
  pic_sym rename;

  if (! pic_find_rename(pic, pic->lib->senv, sym, &rename)) {
    pic_errorf(pic, "export: symbol not defined %s", pic_symbol_name(pic, sym));
  }

#if DEBUG
  printf("* exporting %s as %s\n", pic_symbol_name(pic, sym), pic_symbol_name(pic, rename));
#endif

  xh_put_int(&pic->lib->exports, sym, &rename);
}

void
pic_export_as(pic_state *pic, pic_sym sym, pic_sym as)
{
  pic_sym rename;

  if (! pic_find_rename(pic, pic->lib->senv, sym, &rename)) {
    pic_errorf(pic, "export: symbol not defined %s", pic_symbol_name(pic, sym));
  }

#if DEBUG
  printf("* exporting %s as %s\n", pic_symbol_name(pic, as), pic_symbol_name(pic, rename));
#endif

  xh_put_int(&pic->lib->exports, as, &rename);
}
