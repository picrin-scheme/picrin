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
    pic_sym *realname, *rename;

    realname = pic_sym_ptr(pic_dict_ref(pic, lib->exports, nick));

    if ((rename = pic_find_rename(pic, lib->env, realname)) == NULL) {
      pic_errorf(pic, "attempted to export undefined variable '~s'", pic_obj_value(realname));
    }
    pic_dict_set(pic, imports, nick, pic_obj_value(rename));
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
    pic_put_rename(pic, pic->lib->env, sym, pic_sym_ptr(pic_dict_ref(pic, imports, sym)));
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

static bool
condexpand(pic_state *pic, pic_value clause)
{
  pic_sym *tag;
  pic_value c, feature, it;

  if (pic_eq_p(clause, pic_obj_value(pic->sELSE))) {
    return true;
  }
  if (pic_sym_p(clause)) {
    pic_for_each (feature, pic->features, it) {
      if(pic_eq_p(feature, clause))
        return true;
    }
    return false;
  }

  if (! (pic_pair_p(clause) && pic_sym_p(pic_car(pic, clause)))) {
    pic_errorf(pic, "invalid 'cond-expand' clause ~s", clause);
  } else {
    tag = pic_sym_ptr(pic_car(pic, clause));
  }

  if (tag == pic->sLIBRARY) {
    return pic_find_library(pic, pic_list_ref(pic, clause, 1)) != NULL;
  }
  if (tag == pic->sNOT) {
    return ! condexpand(pic, pic_list_ref(pic, clause, 1));
  }
  if (tag == pic->sAND) {
    pic_for_each (c, pic_cdr(pic, clause), it) {
      if (! condexpand(pic, c))
        return false;
    }
    return true;
  }
  if (tag == pic->sOR) {
    pic_for_each (c, pic_cdr(pic, clause), it) {
      if (condexpand(pic, c))
        return true;
    }
    return false;
  }

  pic_errorf(pic, "unknown 'cond-expand' directive ~s", clause);
}

static pic_value
pic_lib_condexpand(pic_state *pic)
{
  pic_value *clauses;
  size_t argc, i;

  pic_get_args(pic, "*", &argc, &clauses);

  for (i = 0; i < argc; i++) {
    if (condexpand(pic, pic_car(pic, clauses[i]))) {
      return pic_cons(pic, pic_obj_value(pic->sBEGIN), pic_cdr(pic, clauses[i]));
    }
  }

  return pic_undef_value();
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
pic_lib_define_library(pic_state *pic)
{
  struct pic_lib *lib, *prev = pic->lib;
  size_t argc, i;
  pic_value spec, *argv;

  pic_get_args(pic, "o*", &spec, &argc, &argv);

  if ((lib = pic_find_library(pic, spec)) == NULL) {
    lib = pic_make_library(pic, spec);
  }

  pic_try {
    pic->lib = lib;

    for (i = 0; i < argc; ++i) {
      pic_void(pic_eval(pic, argv[i], pic->lib));
    }

    pic->lib = prev;
  }
  pic_catch {
    pic->lib = prev;   /* restores pic->lib even if an error occured */
    pic_raise(pic, pic->err);
  }

  return pic_undef_value();
}

void
pic_init_lib(pic_state *pic)
{
  void pic_defmacro(pic_state *, pic_sym *, pic_sym *, pic_func_t);

  pic_defmacro(pic, pic->sCOND_EXPAND, pic->uCOND_EXPAND, pic_lib_condexpand);
  pic_defmacro(pic, pic->sIMPORT, pic->uIMPORT, pic_lib_import);
  pic_defmacro(pic, pic->sEXPORT, pic->uEXPORT, pic_lib_export);
  pic_defmacro(pic, pic->sDEFINE_LIBRARY, pic->uDEFINE_LIBRARY, pic_lib_define_library);
}
