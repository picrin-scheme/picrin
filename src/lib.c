/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/lib.h"
#include "picrin/pair.h"
#include "picrin/macro.h"
#include "picrin/error.h"

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
  lib->env = senv;
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

  v = pic_assoc(pic, spec, pic->lib_tbl, NULL);
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

    pic_put_rename(pic, pic->lib->env, xh_key(it.e, pic_sym), xh_val(it.e, pic_sym));
  }
}

void
pic_export(pic_state *pic, pic_sym sym)
{
  pic_sym rename;

  if (! pic_find_rename(pic, pic->lib->env, sym, &rename)) {
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

  if (! pic_find_rename(pic, pic->lib->env, sym, &rename)) {
    pic_errorf(pic, "export: symbol not defined %s", pic_symbol_name(pic, sym));
  }

#if DEBUG
  printf("* exporting %s as %s\n", pic_symbol_name(pic, as), pic_symbol_name(pic, rename));
#endif

  xh_put_int(&pic->lib->exports, as, &rename);
}

static pic_value
pic_lib_import(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    pic_import(pic, argv[i]);
  }

  return pic_none_value();
}

static pic_value
pic_lib_export(pic_state *pic)
{
  const pic_sym sRENAME = pic_intern_cstr(pic, "rename");
  size_t argc, i;
  pic_value *argv, spec, a, b;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    spec = argv[i];
    if (pic_sym_p(spec)) {      /* (export a) */
      pic_export(pic, pic_sym(spec));
    }
    else {                      /* (export (rename a b)) */
      if (! pic_list_p(spec))
        goto fail;
      if (! pic_length(pic, spec) == 3)
        goto fail;
      if (! pic_eq_p(pic_car(pic, spec), pic_sym_value(sRENAME)))
        goto fail;
      if (! pic_sym_p(a = pic_list_ref(pic, spec, 1)))
        goto fail;
      if (! pic_sym_p(b = pic_list_ref(pic, spec, 2)))
        goto fail;
      pic_export_as(pic, pic_sym(a), pic_sym(b));
    }
  }

  return pic_none_value();

 fail:
  pic_errorf(pic, "illegal export spec: ~s", spec);
}

static pic_value
pic_lib_define_library(pic_state *pic)
{
  struct pic_lib *prev = pic->lib;
  size_t argc, i;
  pic_value spec, *argv;

  pic_get_args(pic, "o*", &spec, &argc, &argv);

  pic_make_library(pic, spec);

  pic_try {
    pic_in_library(pic, spec);

    for (i = 0; i < argc; ++i) {
      pic_void(pic_eval(pic, argv[i], pic->lib));
    }

    pic_in_library(pic, prev->name);
  }
  pic_catch {
    pic_in_library(pic, prev->name); /* restores pic->lib even if an error occurs */
    pic_throw_error(pic, pic->err);
  }

  return pic_none_value();
}

void
pic_init_lib(pic_state *pic)
{
  void pic_defmacro(pic_state *, pic_sym, pic_sym, pic_func_t);

  pic_defmacro(pic, pic->sIMPORT, pic->rIMPORT, pic_lib_import);
  pic_defmacro(pic, pic->sEXPORT, pic->rEXPORT, pic_lib_export);
  pic_defmacro(pic, pic->sDEFINE_LIBRARY, pic->rDEFINE_LIBRARY, pic_lib_define_library);
}
