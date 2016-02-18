/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

KHASH_DEFINE(ltable, const char *, struct pic_lib, kh_str_hash_func, kh_str_cmp_func)

static struct pic_lib *
get_library_opt(pic_state *pic, const char *lib)
{
  khash_t(ltable) *h = &pic->ltable;
  khiter_t it;

  it = kh_get(ltable, h, lib);
  if (it == kh_end(h)) {
    return NULL;
  }
  return &kh_val(h, it);
}

static struct pic_lib *
get_library(pic_state *pic, const char *lib)
{
  struct pic_lib *libp;

  if ((libp = get_library_opt(pic, lib)) == NULL) {
    pic_errorf(pic, "library not found: %s", lib);
  }
  return libp;
}

static struct pic_env *
make_library_env(pic_state *pic, struct pic_string *name)
{
  struct pic_env *env;

  env = pic_make_topenv(pic, name);

  /* set up default environment */
  pic_put_identifier(pic, (pic_id *)pic->sDEFINE_LIBRARY, pic->sDEFINE_LIBRARY, env);
  pic_put_identifier(pic, (pic_id *)pic->sIMPORT, pic->sIMPORT, env);
  pic_put_identifier(pic, (pic_id *)pic->sEXPORT, pic->sEXPORT, env);
  pic_put_identifier(pic, (pic_id *)pic->sCOND_EXPAND, pic->sCOND_EXPAND, env);

  return env;
}

void
pic_make_library(pic_state *pic, const char *lib)
{
  khash_t(ltable) *h = &pic->ltable;
  const char *old_lib;
  struct pic_string *name;
  struct pic_env *env;
  struct pic_dict *exports;
  khiter_t it;
  int ret;

  if (pic->lib) {
    old_lib = pic_current_library(pic);
  }

  name = pic_cstr_value(pic, lib);
  env = make_library_env(pic, name);
  exports = pic_make_dict(pic);

  it = kh_put(ltable, h, pic_str(pic, name), &ret);
  if (ret == 0) {               /* if exists */
    pic_errorf(pic, "library name already in use: %s", lib);
  }

  kh_val(h, it).name = name;
  kh_val(h, it).env = env;
  kh_val(h, it).exports = exports;

  if (pic->lib) {
    pic->lib = get_library(pic, old_lib); /* ltable might be rehashed */
  }
}

void
pic_in_library(pic_state *pic, const char *lib)
{
  pic->lib = get_library(pic, lib);
}

bool
pic_find_library(pic_state *pic, const char *lib)
{
  return get_library_opt(pic, lib) != NULL;
}

const char *
pic_current_library(pic_state *pic)
{
  return pic_str(pic, pic->lib->name);
}

struct pic_env *
pic_library_environment(pic_state *pic, const char *lib)
{
  return get_library(pic, lib)->env;
}

void
pic_import(pic_state *pic, const char *lib)
{
  pic_sym *name, *realname, *uid;
  int it = 0;
  pic_value val;
  struct pic_lib *libp;

  libp = get_library(pic, lib);

  while (pic_dict_next(pic, libp->exports, &it, &name, &val)) {
    realname = pic_sym_ptr(val);

    if ((uid = pic_find_identifier(pic, (pic_id *)realname, libp->env)) == NULL) {
      pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
    }
    pic_put_identifier(pic, (pic_id *)name, uid, pic->lib->env);
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
  const char *lib;

  pic_get_args(pic, "z", &lib);

  pic_make_library(pic, lib);

  return pic_undef_value(pic);
}

static pic_value
pic_lib_find_library(pic_state *pic)
{
  const char *lib;

  pic_get_args(pic, "z", &lib);

  return pic_bool_value(pic, pic_find_library(pic, lib));
}

static pic_value
pic_lib_current_library(pic_state *pic)
{
  const char *lib;
  int n;

  n = pic_get_args(pic, "|z", &lib);

  if (n == 0) {
    return pic_obj_value(pic->lib->name);
  }
  else {
    pic_in_library(pic, lib);

    return pic_undef_value(pic);
  }
}

static pic_value
pic_lib_library_import(pic_state *pic)
{
  const char *lib;
  pic_sym *name, *realname, *uid, *alias = NULL;
  struct pic_lib *libp;

  pic_get_args(pic, "zm|m", &lib, &name, &alias);

  if (alias == NULL) {
    alias = name;
  }

  libp = get_library(pic, lib);

  if (! pic_dict_has(pic, libp->exports, name)) {
    pic_errorf(pic, "attempted to import undefined variable '~s'", pic_obj_value(name));
  } else {
    realname = pic_sym_ptr(pic_dict_ref(pic, libp->exports, name));
  }

  if ((uid = pic_find_identifier(pic, (pic_id *)realname, libp->env)) == NULL) {
    pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
  } else {
    pic_put_identifier(pic, (pic_id *)alias, uid, pic->lib->env);
  }

  return pic_undef_value(pic);
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

  return pic_undef_value(pic);
}

static pic_value
pic_lib_library_exports(pic_state *pic)
{
  const char *lib;
  pic_value exports = pic_nil_value(pic);
  pic_sym *sym;
  int it = 0;
  struct pic_lib *libp;

  pic_get_args(pic, "z", &lib);

  libp = get_library(pic, lib);

  while (pic_dict_next(pic, libp->exports, &it, &sym, NULL)) {
    pic_push(pic, pic_obj_value(sym), exports);
  }

  return exports;
}

static pic_value
pic_lib_library_environment(pic_state *pic)
{
  const char *lib;

  pic_get_args(pic, "z", &lib);

  return pic_obj_value(get_library(pic, lib)->env);
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
