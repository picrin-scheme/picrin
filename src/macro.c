/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/lib.h"
#include "picrin/error.h"
#include "picrin/dict.h"
#include "picrin/cont.h"

pic_sym
pic_add_rename(pic_state *pic, struct pic_senv *senv, pic_sym sym)
{
  pic_sym rename;

  rename = pic_gensym(pic, sym);
  pic_put_rename(pic, senv, sym, rename);
  return rename;
}

void
pic_put_rename(pic_state *pic, struct pic_senv *senv, pic_sym sym, pic_sym rename)
{
  UNUSED(pic);

  xh_put_int(&senv->map, sym, &rename);
}

bool
pic_find_rename(pic_state *pic, struct pic_senv *senv, pic_sym sym, pic_sym *rename)
{
  xh_entry *e;

  UNUSED(pic);

  if ((e = xh_get_int(&senv->map, sym)) == NULL) {
    return false;
  }
  if (rename != NULL) {
    *rename = xh_val(e, pic_sym);
  }
  return true;
}

static void
define_macro(pic_state *pic, pic_sym rename, struct pic_proc *proc, struct pic_senv *senv)
{
  struct pic_macro *mac;

  mac = (struct pic_macro *)pic_obj_alloc(pic, sizeof(struct pic_macro), PIC_TT_MACRO);
  mac->senv = senv;
  mac->proc = proc;

  xh_put_int(&pic->macros, rename, &mac);
}

static struct pic_macro *
find_macro(pic_state *pic, pic_sym rename)
{
  xh_entry *e;

  if ((e = xh_get_int(&pic->macros, rename)) == NULL) {
    return NULL;
  }
  return xh_val(e, struct pic_macro *);
}

static pic_sym
make_identifier(pic_state *pic, pic_sym sym, struct pic_senv *senv)
{
  pic_sym rename;

  while (true) {
    if (pic_find_rename(pic, senv, sym, &rename)) {
      return rename;
    }
    if (! senv->up)
      break;
    senv = senv->up;
  }
  if (! pic_interned_p(pic, sym)) {
    return sym;
  }
  else {
    return pic_gensym(pic, sym);
  }
}

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *);

static pic_value
macroexpand_symbol(pic_state *pic, pic_sym sym, struct pic_senv *senv)
{
  return pic_sym_value(make_identifier(pic, sym, senv));
}

static pic_value
macroexpand_quote(pic_state *pic, pic_value expr)
{
  return pic_cons(pic, pic_sym_value(pic->rQUOTE), pic_cdr(pic, expr));
}

static pic_value
macroexpand_import(pic_state *pic, pic_value expr)
{
  pic_value spec;

  pic_for_each (spec, pic_cdr(pic, expr)) {
    pic_import(pic, spec);
  }

  return pic_none_value();
}

static pic_value
macroexpand_export(pic_state *pic, pic_value expr)
{
  extern pic_value pic_export_as(pic_state *, pic_sym, pic_sym);
  pic_value spec;
  pic_sym sRENAME, sym, as;

  sRENAME = pic_intern_cstr(pic, "rename");

  pic_for_each (spec, pic_cdr(pic, expr)) {
    if (pic_sym_p(spec)) {
      sym = as = pic_sym(spec);
    }
    else if (pic_list_p(spec) && pic_eq_p(pic_car(pic, spec), pic_sym_value(sRENAME))) {
      if (pic_length(pic, spec) != 3) {
        pic_error(pic, "syntax error");
      }
      if (! pic_sym_p(pic_list_ref(pic, spec, 1))) {
        pic_error(pic, "syntax error");
      }
      sym = pic_sym(pic_list_ref(pic, spec, 1));
      if (! pic_sym_p(pic_list_ref(pic, spec, 2))) {
        pic_error(pic, "syntax error");
      }
      as = pic_sym(pic_list_ref(pic, spec, 2));
    }
    else {
      pic_error(pic, "syntax error");
    }
    /* TODO: warn if symbol is shadowed by local variable */
    pic_export_as(pic, sym, as);
  }

  return pic_none_value();
}

static pic_value
macroexpand_deflibrary(pic_state *pic, pic_value expr)
{
  struct pic_lib *prev = pic->lib;
  pic_value v;

  if (pic_length(pic, expr) < 2) {
    pic_error(pic, "syntax error");
  }

  pic_make_library(pic, pic_cadr(pic, expr));

  pic_try {
    pic_in_library(pic, pic_cadr(pic, expr));

    pic_for_each (v, pic_cddr(pic, expr)) {
      pic_void(pic_eval(pic, v, pic->lib));
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
macroexpand_list(pic_state *pic, pic_value obj, struct pic_senv *senv)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value x, head, tail;

  if (pic_pair_p(obj)) {
    head = macroexpand(pic, pic_car(pic, obj), senv);
    tail = macroexpand_list(pic, pic_cdr(pic, obj), senv);
    x = pic_cons(pic, head, tail);
  } else {
    x = macroexpand(pic, obj, senv);
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, x);
  return x;
}

static pic_value
macroexpand_lambda(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  pic_value formal, body;
  struct pic_senv *in;
  pic_value a;

  if (pic_length(pic, expr) < 2) {
    pic_error(pic, "syntax error");
  }

  in = pic_senv_new(pic, senv);

  for (a = pic_cadr(pic, expr); pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_value v = pic_car(pic, a);

    if (! pic_sym_p(v)) {
      pic_error(pic, "syntax error");
    }
    pic_add_rename(pic, in, pic_sym(v));
  }
  if (pic_sym_p(a)) {
    pic_add_rename(pic, in, pic_sym(a));
  }
  else if (! pic_nil_p(a)) {
    pic_error(pic, "syntax error");
  }

  formal = macroexpand_list(pic, pic_cadr(pic, expr), in);
  body = macroexpand_list(pic, pic_cddr(pic, expr), in);

  return pic_cons(pic, pic_sym_value(pic->rLAMBDA), pic_cons(pic, formal, body));
}

static pic_value
macroexpand_define(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  pic_sym sym;
  pic_value formal, body, var, val;

  if (pic_length(pic, expr) < 2) {
    pic_error(pic, "syntax error");
  }

  formal = pic_cadr(pic, expr);
  if (pic_pair_p(formal)) {
    var = pic_car(pic, formal);
  } else {
    if (pic_length(pic, expr) != 3) {
      pic_error(pic, "syntax error");
    }
    var = formal;
  }
  if (! pic_sym_p(var)) {
    pic_error(pic, "binding to non-symbol object");
  }
  sym = pic_sym(var);
  if (! pic_find_rename(pic, senv, sym, NULL)) {
    pic_add_rename(pic, senv, sym);
  }
  body = pic_cddr(pic, expr);
  if (pic_pair_p(formal)) {
    val = macroexpand_lambda(pic, pic_cons(pic, pic_false_value(), pic_cons(pic, pic_cdr(pic, formal), body)), senv);
  } else {
    val = macroexpand(pic, pic_car(pic, body), senv);
  }
  return pic_list3(pic, pic_sym_value(pic->rDEFINE), macroexpand_symbol(pic, sym, senv), val);
}

static pic_value
macroexpand_defsyntax(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  pic_value var, val;
  pic_sym sym, rename;

  if (pic_length(pic, expr) != 3) {
    pic_error(pic, "syntax error");
  }

  var = pic_cadr(pic, expr);
  if (! pic_sym_p(var)) {
    pic_error(pic, "binding to non-symbol object");
  }
  sym = pic_sym(var);
  if (! pic_find_rename(pic, senv, sym, &rename)) {
    rename = pic_add_rename(pic, senv, sym);
  } else {
    pic_warnf(pic, "redefining syntax variable: ~s", pic_sym_value(sym));
  }

  val = pic_cadr(pic, pic_cdr(pic, expr));

  pic_try {
    val = pic_eval(pic, val, pic->lib);
  } pic_catch {
    pic_errorf(pic, "macroexpand error while definition: %s", pic_errmsg(pic));
  }

  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
  }

  define_macro(pic, rename, pic_proc_ptr(val), senv);

  return pic_none_value();
}

static pic_value
macroexpand_macro(pic_state *pic, struct pic_macro *mac, pic_value expr, struct pic_senv *senv)
{
  pic_value v, args;

#if DEBUG
  puts("before expand-1:");
  pic_debug(pic, expr);
  puts("");
#endif

  if (mac->senv == NULL) { /* legacy macro */
    args = pic_cdr(pic, expr);
  }
  else {
    args = pic_list3(pic, expr, pic_obj_value(senv), pic_obj_value(mac->senv));
  }

  pic_try {
    v = pic_apply(pic, mac->proc, args);
  } pic_catch {
    pic_errorf(pic, "macroexpand error while application: %s", pic_errmsg(pic));
  }

#if DEBUG
  puts("after expand-1:");
  pic_debug(pic, v);
  puts("");
#endif

  return macroexpand(pic, v, senv);
}

static pic_value
macroexpand_node(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  switch (pic_type(expr)) {
  case PIC_TT_SYMBOL: {
    return macroexpand_symbol(pic, pic_sym(expr), senv);
  }
  case PIC_TT_PAIR: {
    pic_value car;
    struct pic_macro *mac;

    if (! pic_list_p(expr)) {
      pic_errorf(pic, "cannot macroexpand improper list: ~s", expr);
    }

    car = macroexpand(pic, pic_car(pic, expr), senv);
    if (pic_sym_p(car)) {
      pic_sym tag = pic_sym(car);

      if (tag == pic->rDEFINE_LIBRARY) {
        return macroexpand_deflibrary(pic, expr);
      }
      else if (tag == pic->rIMPORT) {
        return macroexpand_import(pic, expr);
      }
      else if (tag == pic->rEXPORT) {
        return macroexpand_export(pic, expr);
      }
      else if (tag == pic->rDEFINE_SYNTAX) {
        return macroexpand_defsyntax(pic, expr, senv);
      }
      else if (tag == pic->rLAMBDA) {
        return macroexpand_lambda(pic, expr, senv);
      }
      else if (tag == pic->rDEFINE) {
        return macroexpand_define(pic, expr, senv);
      }
      else if (tag == pic->rQUOTE) {
        return macroexpand_quote(pic, expr);
      }

      if ((mac = find_macro(pic, tag)) != NULL) {
        return macroexpand_macro(pic, mac, expr, senv);
      }
    }

    return pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv));
  }
  default:
    return expr;
  }
}

static pic_value
macroexpand(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value v;

#if DEBUG
  printf("[macroexpand] expanding... ");
  pic_debug(pic, expr);
  puts("");
#endif

  v = macroexpand_node(pic, expr, senv);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, v);
  return v;
}

pic_value
pic_macroexpand(pic_state *pic, pic_value expr, struct pic_lib *lib)
{
  struct pic_lib *prev;
  pic_value v;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  /* change library for macro-expansion time processing */
  prev = pic->lib;
  pic->lib = lib;

  v = macroexpand(pic, expr, lib->env);

  pic->lib = prev;

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}

static pic_value
macroexpand_one(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  struct pic_macro *mac;
  pic_value v, args;

  if (pic_sym_p(expr)) {
    pic_sym sym;

    sym = pic_sym(expr);

    if (pic_interned_p(pic, sym)) {
      return pic_sym_value(make_identifier(pic, pic_sym(expr), senv));
    }
  }
  if (pic_pair_p(expr) && pic_sym_p(pic_car(pic, expr))) {
    pic_sym sym;

    sym = make_identifier(pic, pic_sym(pic_car(pic, expr)), senv);

    if ((mac = find_macro(pic, sym)) != NULL) {
      if (mac->senv == NULL) { /* legacy macro */
        args = pic_cdr(pic, expr);
      }
      else {
        args = pic_list3(pic, expr, pic_obj_value(senv), pic_obj_value(mac->senv));
      }

      pic_try {
        v = pic_apply(pic, mac->proc, args);
      } pic_catch {
        pic_errorf(pic, "macroexpand error while application: %s", pic_errmsg(pic));
      }

      return v;
    }
  }

  return pic_undef_value();     /* no expansion occurred */
}

struct pic_senv *
pic_senv_new(pic_state *pic, struct pic_senv *up)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  xh_init_int(&senv->map, sizeof(pic_sym));

  return senv;
}

struct pic_senv *
pic_null_syntactic_environment(pic_state *pic)
{
  struct pic_senv *senv;

  senv = pic_senv_new(pic, NULL);

  pic_define_syntactic_keyword(pic, senv, pic->sDEFINE_LIBRARY, pic->rDEFINE_LIBRARY);
  pic_define_syntactic_keyword(pic, senv, pic->sIMPORT, pic->rIMPORT);
  pic_define_syntactic_keyword(pic, senv, pic->sEXPORT, pic->rEXPORT);

  return senv;
}

void
pic_define_syntactic_keyword(pic_state *pic, struct pic_senv *senv, pic_sym sym, pic_sym rsym)
{
  pic_put_rename(pic, senv, sym, rsym);

  if (pic->lib && pic->lib->env == senv) {
    pic_export(pic, sym);
  }
}

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  pic_sym sym, rename;

  /* symbol registration */
  sym = pic_intern_cstr(pic, name);
  rename = pic_add_rename(pic, pic->lib->env, sym);
  define_macro(pic, rename, macro, NULL);

  /* auto export! */
  pic_export(pic, sym);
}

bool
pic_identifier_p(pic_state *pic, pic_value obj)
{
  return pic_sym_p(obj) && ! pic_interned_p(pic, pic_sym(obj));
}

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

static pic_value
pic_macro_macroexpand_1(pic_state *pic)
{
  pic_value expr, val;

  pic_get_args(pic, "o", &expr);

  val = macroexpand_one(pic, expr, pic->lib->env);
  if (pic_undef_p(val)) {
    return pic_values2(pic, expr, pic_false_value());
  }
  else {
    return pic_values2(pic, val, pic_true_value());
  }
}

static pic_value
pic_macro_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_identifier_p(pic, obj));
}

static pic_value
pic_macro_make_identifier(pic_state *pic)
{
  pic_value obj;
  pic_sym sym;

  pic_get_args(pic, "mo", &sym, &obj);

  pic_assert_type(pic, obj, senv);

  return pic_sym_value(make_identifier(pic, sym, pic_senv_ptr(obj)));
}

void
pic_init_macro(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin macro)") {
    pic_defun(pic, "gensym", pic_macro_gensym);
    pic_defun(pic, "ungensym", pic_macro_ungensym);
    pic_defun(pic, "macroexpand", pic_macro_macroexpand);
    pic_defun(pic, "macroexpand-1", pic_macro_macroexpand_1);
    pic_defun(pic, "identifier?", pic_macro_identifier_p);
    pic_defun(pic, "make-identifier", pic_macro_make_identifier);
  }
}
