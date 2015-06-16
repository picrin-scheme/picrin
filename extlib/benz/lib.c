/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static void
setup_default_env(pic_state *pic, struct pic_env *env)
{
  void pic_define_syntactic_keyword(pic_state *, struct pic_env *, pic_sym *, pic_sym *);

  pic_define_syntactic_keyword(pic, env, pic->sDEFINE_LIBRARY, pic->uDEFINE_LIBRARY);
  pic_define_syntactic_keyword(pic, env, pic->sIMPORT, pic->uIMPORT);
  pic_define_syntactic_keyword(pic, env, pic->sEXPORT, pic->uEXPORT);
  pic_define_syntactic_keyword(pic, env, pic->sCOND_EXPAND, pic->uCOND_EXPAND);
}

struct pic_lib *
pic_make_library(pic_state *pic, pic_value name)
{
  struct pic_lib *lib;
  struct pic_env *env;
  struct pic_dict *exports;

  if ((lib = pic_find_library(pic, name)) != NULL) {
    pic_errorf(pic, "library name already in use: ~s", name);
  }

  env = pic_make_env(pic, NULL);
  exports = pic_make_dict(pic);

  setup_default_env(pic, env);

  lib = (struct pic_lib *)pic_obj_alloc(pic, sizeof(struct pic_lib), PIC_TT_LIB);
  lib->name = name;
  lib->env = env;
  lib->exports = exports;

  /* register! */
  pic->libs = pic_acons(pic, name, pic_obj_value(lib), pic->libs);

  return lib;
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

static void
import_table(pic_state *pic, pic_value spec, struct pic_dict *imports)
{
  struct pic_lib *lib;
  struct pic_dict *table;
  pic_value val, tmp, prefix, it;
  pic_sym *sym, *id, *tag, *nick;
  xh_entry *iter;

  table = pic_make_dict(pic);

  if (pic_pair_p(spec) && pic_sym_p(pic_car(pic, spec))) {

    tag = pic_sym_ptr(pic_car(pic, spec));

    if (tag == pic->sONLY) {
      import_table(pic, pic_cadr(pic, spec), table);

      pic_for_each (val, pic_cddr(pic, spec), it) {
        pic_dict_set(pic, imports, pic_sym_ptr(val), pic_dict_ref(pic, table, pic_sym_ptr(val)));
      }
      return;
    }
    if (tag == pic->sRENAME) {
      import_table(pic, pic_cadr(pic, spec), imports);

      pic_for_each (val, pic_cddr(pic, spec), it) {
        tmp = pic_dict_ref(pic, imports, pic_sym_ptr(pic_car(pic, val)));
        pic_dict_del(pic, imports, pic_sym_ptr(pic_car(pic, val)));
        pic_dict_set(pic, imports, pic_sym_ptr(pic_cadr(pic, val)), tmp);
      }
      return;
    }
    if (tag == pic->sPREFIX) {
      import_table(pic, pic_cadr(pic, spec), table);

      prefix = pic_list_ref(pic, spec, 2);
      pic_dict_for_each (sym, table, iter) {
        id = pic_intern(pic, pic_format(pic, "~s~s", prefix, pic_obj_value(sym)));
        pic_dict_set(pic, imports, id, pic_dict_ref(pic, table, sym));
      }
      return;
    }
    if (tag == pic->sEXCEPT) {
      import_table(pic, pic_cadr(pic, spec), imports);
      pic_for_each (val, pic_cddr(pic, spec), it) {
        pic_dict_del(pic, imports, pic_sym_ptr(val));
      }
      return;
    }
  }
  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_errorf(pic, "library not found: ~a", spec);
  }
  pic_dict_for_each (nick, lib->exports, iter) {
    pic_sym *realname, *uid;

    realname = pic_sym_ptr(pic_dict_ref(pic, lib->exports, nick));

    if ((uid = pic_find_variable(pic, lib->env, pic_obj_value(realname))) == NULL) {
      pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
    }
    pic_dict_set(pic, imports, nick, pic_obj_value(uid));
  }
}

static void
import(pic_state *pic, pic_value spec)
{
  struct pic_dict *imports;
  pic_sym *sym;
  xh_entry *it;

  imports = pic_make_dict(pic);

  import_table(pic, spec, imports);

  pic_dict_for_each (sym, imports, it) {
    pic_put_variable(pic, pic->lib->env, pic_obj_value(sym), pic_sym_ptr(pic_dict_ref(pic, imports, sym)));
  }
}

static void
export(pic_state *pic, pic_value spec)
{
  pic_sym *sRENAME = pic_intern_cstr(pic, "rename");
  pic_value a, b;

  if (pic_sym_p(spec)) {        /* (export a) */
    a = b = spec;
  } else {                      /* (export (rename a b)) */
    if (! pic_list_p(spec))
      goto fail;
    if (! (pic_length(pic, spec) == 3))
      goto fail;
    if (! pic_eq_p(pic_car(pic, spec), pic_obj_value(sRENAME)))
      goto fail;
    if (! pic_sym_p(a = pic_list_ref(pic, spec, 1)))
      goto fail;
    if (! pic_sym_p(b = pic_list_ref(pic, spec, 2)))
      goto fail;
  }

#if DEBUG
  printf("* exporting %s as %s\n", pic_symbol_name(pic, pic_sym_ptr(b)), pic_symbol_name(pic, pic_sym_ptr(a)));
#endif

  pic_dict_set(pic, pic->lib->exports, pic_sym_ptr(b), a);

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
pic_import_library(pic_state *pic, struct pic_lib *lib)
{
  import(pic, lib->name);
}

void
pic_export(pic_state *pic, pic_sym *sym)
{
  export(pic, pic_obj_value(sym));
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

  return pic_undef_value();
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

  return pic_undef_value();
}

static pic_value
pic_lib_make_library(pic_state *pic)
{
  pic_value name;

  pic_get_args(pic, "o", &name);

  return pic_obj_value(pic_make_library(pic, name));
}

static pic_value
pic_lib_find_library(pic_state *pic)
{
  pic_value name;
  struct pic_lib *lib;

  pic_get_args(pic, "o", &name);

  if ((lib = pic_find_library(pic, name)) == NULL) {
    return pic_false_value();
  }
  return pic_obj_value(lib);
}

static pic_value
pic_lib_current_library(pic_state *pic)
{
  pic_value lib;
  size_t n;

  n = pic_get_args(pic, "|o", &lib);

  if (n == 0) {
    return pic_obj_value(pic->lib);
  }
  else {
    pic_assert_type(pic, lib, lib);

    pic->lib = pic_lib_ptr(lib);

    return pic_undef_value();
  }
}

static pic_value
pic_lib_library_name(pic_state *pic)
{
  pic_value lib;

  pic_get_args(pic, "o", &lib);

  pic_assert_type(pic, lib, lib);

  return pic_lib_ptr(lib)->name;
}

static pic_value
pic_lib_library_exports(pic_state *pic)
{
  pic_value lib, exports = pic_nil_value();
  pic_sym *sym;
  xh_entry *it;

  pic_get_args(pic, "o", &lib);

  pic_assert_type(pic, lib, lib);

  pic_dict_for_each (sym, pic_lib_ptr(lib)->exports, it) {
    pic_push(pic, pic_obj_value(sym), exports);
  }

  return exports;
}

static pic_value
pic_lib_library_environment(pic_state *pic)
{
  pic_value lib;

  pic_get_args(pic, "o", &lib);

  pic_assert_type(pic, lib, lib);

  return pic_obj_value(pic_lib_ptr(lib)->env);
}

void
pic_init_lib(pic_state *pic)
{
  void pic_defmacro(pic_state *, pic_sym *, pic_sym *, pic_func_t);

  pic_defmacro(pic, pic->sIMPORT, pic->uIMPORT, pic_lib_import);
  pic_defmacro(pic, pic->sEXPORT, pic->uEXPORT, pic_lib_export);

  pic_defun(pic, "make-library", pic_lib_make_library);
  pic_defun(pic, "find-library", pic_lib_find_library);
  pic_defun(pic, "current-library", pic_lib_current_library);
  pic_defun(pic, "library-name", pic_lib_library_name);
  pic_defun(pic, "library-exports", pic_lib_library_exports);
  pic_defun(pic, "library-environment", pic_lib_library_environment);
}
