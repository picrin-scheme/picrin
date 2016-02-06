/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static void
setup_default_env(pic_state *pic, struct pic_env *env)
{
  pic_put_variable(pic, env, pic_obj_value(pic->sDEFINE_LIBRARY), pic->uDEFINE_LIBRARY);
  pic_put_variable(pic, env, pic_obj_value(pic->sIMPORT), pic->uIMPORT);
  pic_put_variable(pic, env, pic_obj_value(pic->sEXPORT), pic->uEXPORT);
  pic_put_variable(pic, env, pic_obj_value(pic->sCOND_EXPAND), pic->uCOND_EXPAND);
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

void
pic_import(pic_state *pic, struct pic_lib *lib)
{
  pic_sym *name, *realname, *uid;
  khiter_t it;

  pic_dict_for_each (name, lib->exports, it) {
    realname = pic_sym_ptr(pic_dict_ref(pic, lib->exports, name));

    if ((uid = pic_find_variable(pic, lib->env, pic_obj_value(realname))) == NULL) {
      pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
    }
    pic_put_variable(pic, pic->lib->env, pic_obj_value(name), uid);
  }
}

void
pic_export(pic_state *pic, pic_sym *name)
{
  pic_dict_set(pic, pic->lib->exports, name, pic_obj_value(name));
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
  int n;

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
pic_lib_library_import(pic_state *pic)
{
  pic_value lib_opt;
  pic_sym *name, *realname, *uid, *alias = NULL;
  struct pic_lib *lib;

  pic_get_args(pic, "om|m", &lib_opt, &name, &alias);

  pic_assert_type(pic, lib_opt, lib);

  if (alias == NULL) {
    alias = name;
  }

  lib = pic_lib_ptr(lib_opt);

  if (! pic_dict_has(pic, lib->exports, name)) {
    pic_errorf(pic, "attempted to import undefined variable '~s'", pic_obj_value(name));
  } else {
    realname = pic_sym_ptr(pic_dict_ref(pic, lib->exports, name));
  }

  if ((uid = pic_find_variable(pic, lib->env, pic_obj_value(realname))) == NULL) {
    pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
  } else {
    pic_put_variable(pic, pic->lib->env, pic_obj_value(alias), uid);
  }

  return pic_undef_value();
}

static pic_value
pic_lib_library_export(pic_state *pic)
{
  pic_sym *name, *alias = NULL;

  pic_get_args(pic, "m|m", &name, &alias);

  if (alias == NULL) {
    alias = name;
  }

  pic_dict_set(pic, pic->lib->exports, alias, pic_obj_value(name));

  return pic_undef_value();
}

static pic_value
pic_lib_library_exports(pic_state *pic)
{
  pic_value lib, exports = pic_nil_value();
  pic_sym *sym;
  khiter_t it;

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
  pic_defun(pic, "make-library", pic_lib_make_library);
  pic_defun(pic, "find-library", pic_lib_find_library);
  pic_defun(pic, "library-exports", pic_lib_library_exports);
  pic_defun(pic, "library-environment", pic_lib_library_environment);

  pic_defun(pic, "current-library", pic_lib_current_library);
  pic_defun(pic, "library-import", pic_lib_library_import);
  pic_defun(pic, "library-export", pic_lib_library_export);
}
