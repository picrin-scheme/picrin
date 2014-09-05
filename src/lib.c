/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/lib.h"
#include "picrin/pair.h"
#include "picrin/macro.h"
#include "picrin/error.h"
#include "picrin/dict.h"
#include "picrin/string.h"

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
  pic->libs = pic_acons(pic, name, pic_obj_value(lib), pic->libs);

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

  v = pic_assoc(pic, spec, pic->libs, NULL);
  if (pic_false_p(v)) {
    return NULL;
  }
  return pic_lib_ptr(pic_cdr(pic, v));
}

static struct pic_dict *
import_table(pic_state *pic, pic_value spec)
{
  const pic_sym sONLY = pic_intern_cstr(pic, "only");
  const pic_sym sRENAME = pic_intern_cstr(pic, "rename");
  const pic_sym sPREFIX = pic_intern_cstr(pic, "prefix");
  const pic_sym sEXCEPT = pic_intern_cstr(pic, "except");
  struct pic_lib *lib;
  struct pic_dict *imports, *dict;
  pic_value val, id;
  xh_iter it;

  imports = pic_dict_new(pic);

  if (pic_list_p(spec)) {
    if (pic_eq_p(pic_car(pic, spec), pic_sym_value(sONLY))) {
      dict = import_table(pic, pic_cadr(pic, spec));
      pic_for_each (val, pic_cddr(pic, spec)) {
        pic_dict_set(pic, imports, pic_sym(val), pic_dict_ref(pic, dict, pic_sym(val)));
      }
      return imports;
    }
    if (pic_eq_p(pic_car(pic, spec), pic_sym_value(sRENAME))) {
      imports = import_table(pic, pic_cadr(pic, spec));
      pic_for_each (val, pic_cddr(pic, spec)) {
        id = pic_dict_ref(pic, imports, pic_sym(pic_car(pic, val)));
        pic_dict_del(pic, imports, pic_sym(pic_car(pic, val)));
        pic_dict_set(pic, imports, pic_sym(pic_cadr(pic, val)), id);
      }
      return imports;
    }
    if (pic_eq_p(pic_car(pic, spec), pic_sym_value(sPREFIX))) {
      dict = import_table(pic, pic_cadr(pic, spec));
      xh_begin(&it, &dict->hash);
      while (xh_next(&it)) {
        pic_dict_set(pic, imports, pic_intern_cstr(pic, pic_str_cstr(pic_strcat(pic, pic_str_new_cstr(pic, pic_symbol_name(pic, pic_sym(pic_car(pic, pic_cddr(pic, spec))))), pic_str_new_cstr(pic, pic_symbol_name(pic, xh_key(it.e, pic_sym)))))), xh_val(it.e, pic_value));
      }
      return imports;
    }
    if (pic_eq_p(pic_car(pic, spec), pic_sym_value(sEXCEPT))) {
      imports = import_table(pic, pic_cadr(pic, spec));
      pic_for_each (val, pic_cddr(pic, spec)) {
        pic_dict_del(pic, imports, pic_sym(val));
      }
      return imports;
    }
  }
  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_errorf(pic, "library not found: ~a", spec);
  }
  xh_begin(&it, &lib->exports);
  while (xh_next(&it)) {
    pic_dict_set(pic, imports, xh_key(it.e, pic_sym), pic_sym_value(xh_val(it.e, pic_sym)));
  }
  return imports;
}

static void
import(pic_state *pic, pic_value spec)
{
  struct pic_dict *imports;
  xh_iter it;

  imports = import_table(pic, spec);

  xh_begin(&it, &imports->hash);
  while (xh_next(&it)) {

#if DEBUG
    printf("* importing %s as %s\n", pic_symbol_name(pic, xh_key(it.e, pic_sym)), pic_symbol_name(pic, pic_sym(xh_val(it.e, pic_value))));
#endif

    pic_put_rename(pic, pic->lib->env, xh_key(it.e, pic_sym), pic_sym(xh_val(it.e, pic_value)));
  }
}

static void
export(pic_state *pic, pic_value spec)
{
  const pic_sym sRENAME = pic_intern_cstr(pic, "rename");
  pic_value a, b;
  pic_sym rename;

  if (pic_sym_p(spec)) {        /* (export a) */
    a = b = spec;
  } else {                      /* (export (rename a b)) */
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
  }

  if (! pic_find_rename(pic, pic->lib->env, pic_sym(a), &rename)) {
    pic_errorf(pic, "export: symbol not defined %s", pic_symbol_name(pic, pic_sym(a)));
  }

#if DEBUG
  printf("* exporting %s as %s\n", pic_symbol_name(pic, pic_sym(b)), pic_symbol_name(pic, rename));
#endif

  xh_put_int(&pic->lib->exports, pic_sym(b), &rename);

  return;

 fail:
  pic_errorf(pic, "illegal export spec: ~s", spec);
}

void
pic_import(pic_state *pic, pic_value spec)
{
  import(pic, spec);
}

void
pic_export(pic_state *pic, pic_sym sym)
{
  export(pic, pic_sym_value(sym));
}

static pic_value
pic_lib_import(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    import(pic, argv[i]);
  }

  return pic_none_value();
}

static pic_value
pic_lib_export(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    export(pic, argv[i]);
  }

  return pic_none_value();
}

static pic_value
pic_lib_define_library(pic_state *pic)
{
  struct pic_lib *prev = pic->lib;
  size_t argc, i;
  pic_value spec, *argv, var, val;
  const pic_sym sCOND_EXPAND = pic_intern_cstr(pic, "cond-expand");

  pic_get_args(pic, "o*", &spec, &argc, &argv);

  pic_make_library(pic, spec);

  pic_try {
    pic_in_library(pic, spec);

    for (i = 0; i < argc; ++i) {
      if (pic_pair_p(argv[i]) &&
          pic_eq_p(pic_car(pic, argv[i]), pic_sym_value(sCOND_EXPAND)))
        pic_for_each(var, pic_cdr(pic, argv[i])){
          if(pic_condexpand_clause(pic, pic_car(pic, var))){
            pic_for_each(val, pic_cdr(pic, var)){
              pic_void(pic_eval(pic, val, pic->lib));              
            }
          }
        }
      else
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

static pic_value
pic_lib_in_library(pic_state *pic)
{
  pic_value spec;

  pic_get_args(pic, "o", &spec);

  pic_in_library(pic, spec);

  return pic_none_value();
}

void
pic_init_lib(pic_state *pic)
{
  void pic_defmacro(pic_state *, pic_sym, pic_sym, pic_func_t);

  pic_defmacro(pic, pic->sIMPORT, pic->rIMPORT, pic_lib_import);
  pic_defmacro(pic, pic->sEXPORT, pic->rEXPORT, pic_lib_export);
  pic_defmacro(pic, pic->sDEFINE_LIBRARY, pic->rDEFINE_LIBRARY, pic_lib_define_library);
  pic_defmacro(pic, pic->sIN_LIBRARY, pic->rIN_LIBRARY, pic_lib_in_library);
}
