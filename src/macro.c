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

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *, struct pic_dict *);

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

  xh_put_int(&senv->renames, sym, &rename);
}

bool
pic_find_rename(pic_state *pic, struct pic_senv *senv, pic_sym sym, pic_sym *rename)
{
  xh_entry *e;

  UNUSED(pic);

  if ((e = xh_get_int(&senv->renames, sym)) == NULL) {
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

static struct pic_senv *
push_scope(pic_state *pic, pic_value formals, struct pic_senv *up, struct pic_dict *cxt)
{
  struct pic_senv *senv;
  pic_value a;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  xh_init_int(&senv->renames, sizeof(pic_sym));

  for (a = formals; pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_value v = pic_car(pic, a);

    if (! pic_sym_p(v)) {
      v = macroexpand(pic, v, up, cxt);
    }
    if (! pic_sym_p(v)) {
      pic_error(pic, "syntax error");
    }
    pic_add_rename(pic, senv, pic_sym(v));
  }
  if (! pic_sym_p(a)) {
    a = macroexpand(pic, a, up, cxt);
  }
  if (pic_sym_p(a)) {
    pic_add_rename(pic, senv, pic_sym(a));
  }
  else if (! pic_nil_p(a)) {
    pic_error(pic, "syntax error");
  }
  return senv;
}

static pic_sym
translate(pic_state *pic, pic_sym sym, struct pic_senv *senv, struct pic_dict *cxt)
{
  pic_sym rename;

  if (! pic_interned_p(pic, sym)) {
    return sym;
  }
  while (true) {
    if (pic_find_rename(pic, senv, sym, &rename)) {
      return rename;
    }
    if (! senv->up)
      break;
    senv = senv->up;
  }
  if (pic_dict_has(pic, cxt, sym)) {
    return pic_sym(pic_dict_ref(pic, cxt, sym));
  } else {
    rename = pic_gensym(pic, sym);
    pic_dict_set(pic, cxt, sym, pic_sym_value(rename));
    return rename;
  }
}

static pic_value
macroexpand_symbol(pic_state *pic, pic_sym sym, struct pic_senv *senv, struct pic_dict *cxt)
{
  return pic_sym_value(translate(pic, sym, senv, cxt));
}

static pic_value
macroexpand_quote(pic_state *pic, pic_value expr)
{
  return pic_cons(pic, pic_sym_value(pic->sQUOTE), pic_cdr(pic, expr));
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
      pic_void(pic_eval(pic, v));
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
macroexpand_defsyntax(pic_state *pic, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
{
  pic_value var, val;
  pic_sym sym, rename;

  if (pic_length(pic, expr) != 3) {
    pic_error(pic, "syntax error");
  }

  var = pic_cadr(pic, expr);
  if (! pic_sym_p(var)) {
    var = macroexpand(pic, var, senv, cxt);
  }
  if (! pic_sym_p(var)) {
    pic_error(pic, "binding to non-symbol object");
  }
  sym = pic_sym(var);
  if (! pic_find_rename(pic, senv, sym, &rename)) {
    rename = pic_add_rename(pic, senv, sym);
  }

  val = pic_cadr(pic, pic_cdr(pic, expr));

  pic_try {
    val = pic_eval(pic, val);
  } pic_catch {
    pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
  }

  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
  }

  define_macro(pic, rename, pic_proc_ptr(val), senv);

  return pic_none_value();
}

static pic_value
macroexpand_defmacro(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  pic_value var, val;
  pic_sym sym, rename;

  if (pic_length(pic, expr) < 2) {
    pic_error(pic, "syntax error");
  }

  var = pic_car(pic, pic_cdr(pic, expr));
  if (pic_pair_p(var)) {
    /* FIXME: unhygienic */
    val = pic_cons(pic, pic_sym_value(pic->sLAMBDA),
                   pic_cons(pic, pic_cdr(pic, var),
                            pic_cdr(pic, pic_cdr(pic, expr))));
    var = pic_car(pic, var);
  }
  else {
    if (pic_length(pic, expr) != 3) {
      pic_error(pic, "syntax_error");
    }
    val = pic_car(pic, pic_cdr(pic, pic_cdr(pic, expr)));
  }
  if (! pic_sym_p(var)) {
    pic_error(pic, "syntax error");
  }
  sym = pic_sym(var);
  if (! pic_find_rename(pic, senv, sym, &rename)) {
    rename = pic_add_rename(pic, senv, sym);
  }

  pic_try {
    val = pic_eval(pic, val);
  } pic_catch {
    pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
  }

  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
  }

  define_macro(pic, rename, pic_proc_ptr(val), NULL);

  return pic_none_value();
}

static pic_value
macroexpand_macro(pic_state *pic, struct pic_macro *mac, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
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
    pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
  }

#if DEBUG
  puts("after expand-1:");
  pic_debug(pic, v);
  puts("");
#endif

  return macroexpand(pic, v, senv, cxt);
}

static pic_value
macroexpand_list(pic_state *pic, pic_value obj, struct pic_senv *senv, struct pic_dict *cxt)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value x, head, tail;

  if (pic_pair_p(obj)) {
    head = macroexpand(pic, pic_car(pic, obj), senv, cxt);
    tail = macroexpand_list(pic, pic_cdr(pic, obj), senv, cxt);
    x = pic_cons(pic, head, tail);
  } else {
    x = macroexpand(pic, obj, senv, cxt);
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, x);
  return x;
}

static pic_value
macroexpand_lambda(pic_state *pic, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
{
  pic_value formal, body;
  struct pic_senv *in;

  in = push_scope(pic, pic_cadr(pic, expr), senv, cxt);

  formal = macroexpand_list(pic, pic_cadr(pic, expr), in, cxt);
  body = macroexpand_list(pic, pic_cddr(pic, expr), in, cxt);

  return pic_cons(pic, pic_sym_value(pic->sLAMBDA), pic_cons(pic, formal, body));
}

static pic_value
macroexpand_define(pic_state *pic, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
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
    var = macroexpand(pic, var, senv, cxt);
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
    val = macroexpand_lambda(pic, pic_cons(pic, pic_false_value(), pic_cons(pic, pic_cdr(pic, formal), body)), senv, cxt);
  } else {
    val = macroexpand(pic, pic_car(pic, body), senv, cxt);
  }
  return pic_list3(pic, pic_sym_value(pic->sDEFINE), macroexpand_symbol(pic, sym, senv, cxt), val);
}

static pic_value
macroexpand_node(pic_state *pic, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
{
#if DEBUG
  printf("[macroexpand] expanding... ");
  pic_debug(pic, expr);
  puts("");
#endif

  switch (pic_type(expr)) {
  case PIC_TT_SC: {
    return macroexpand(pic, pic_sc_ptr(expr)->expr, pic_sc_ptr(expr)->senv, cxt);
  }
  case PIC_TT_SYMBOL: {
    return macroexpand_symbol(pic, pic_sym(expr), senv, cxt);
  }
  case PIC_TT_PAIR: {
    pic_value car;
    struct pic_macro *mac;

    if (! pic_list_p(expr)) {
      pic_errorf(pic, "cannot macroexpand improper list: ~s", expr);
    }

    car = macroexpand(pic, pic_car(pic, expr), senv, cxt);
    if (pic_sym_p(car)) {
      pic_sym tag = pic_sym(car);

      if (tag == pic->sDEFINE_LIBRARY) {
        return macroexpand_deflibrary(pic, expr);
      }
      else if (tag == pic->sIMPORT) {
        return macroexpand_import(pic, expr);
      }
      else if (tag == pic->sEXPORT) {
        return macroexpand_export(pic, expr);
      }
      else if (tag == pic->sDEFINE_SYNTAX) {
        return macroexpand_defsyntax(pic, expr, senv, cxt);
      }
      else if (tag == pic->sDEFINE_MACRO) {
        return macroexpand_defmacro(pic, expr, senv);
      }
      else if (tag == pic->sLAMBDA) {
        return macroexpand_lambda(pic, expr, senv, cxt);
      }
      else if (tag == pic->sDEFINE) {
        return macroexpand_define(pic, expr, senv, cxt);
      }
      else if (tag == pic->sQUOTE) {
        return macroexpand_quote(pic, expr);
      }

      if ((mac = find_macro(pic, tag)) != NULL) {
        return macroexpand_macro(pic, mac, expr, senv, cxt);
      }
    }

    return pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv, cxt));
  }
  case PIC_TT_EOF:
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_CHAR:
  case PIC_TT_STRING:
  case PIC_TT_VECTOR:
  case PIC_TT_BLOB: {
    return expr;
  }
  case PIC_TT_PROC:
  case PIC_TT_PORT:
  case PIC_TT_ERROR:
  case PIC_TT_ENV:
  case PIC_TT_CONT:
  case PIC_TT_UNDEF:
  case PIC_TT_SENV:
  case PIC_TT_MACRO:
  case PIC_TT_LIB:
  case PIC_TT_VAR:
  case PIC_TT_IREP:
  case PIC_TT_DATA:
  case PIC_TT_BOX:
  case PIC_TT_DICT:
    pic_errorf(pic, "unexpected value type: ~s", expr);
  }
  UNREACHABLE();
}

static pic_value
macroexpand(pic_state *pic, pic_value expr, struct pic_senv *senv, struct pic_dict *cxt)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value v;

  v = macroexpand_node(pic, expr, senv, cxt);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, v);
  return v;
}

pic_value
pic_macroexpand(pic_state *pic, pic_value expr)
{
  pic_value v;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  v = macroexpand(pic, expr, pic->lib->senv, pic_dict_new(pic));

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}

struct pic_senv *
pic_null_syntactic_environment(pic_state *pic)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = NULL;
  xh_init_int(&senv->renames, sizeof(pic_sym));

  pic_define_syntactic_keyword(pic, senv, pic->sDEFINE_LIBRARY);
  pic_define_syntactic_keyword(pic, senv, pic->sIMPORT);
  pic_define_syntactic_keyword(pic, senv, pic->sEXPORT);

  return senv;
}

void
pic_define_syntactic_keyword(pic_state *pic, struct pic_senv *senv, pic_sym sym)
{
  pic_put_rename(pic, senv, sym, sym);

  if (pic->lib && pic->lib->senv == senv) {
    pic_export(pic, sym);
  }
}

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  pic_sym sym, rename;

  /* symbol registration */
  sym = pic_intern_cstr(pic, name);
  rename = pic_add_rename(pic, pic->lib->senv, sym);
  define_macro(pic, rename, macro, NULL);

  /* auto export! */
  pic_export(pic, sym);
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
pic_macro_macroexpand(pic_state *pic)
{
  pic_value expr;

  pic_get_args(pic, "o", &expr);

  return pic_macroexpand(pic, expr);
}

static struct pic_sc *
sc_new(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  struct pic_sc *sc;

  sc = (struct pic_sc *)pic_obj_alloc(pic, sizeof(struct pic_sc), PIC_TT_SC);
  sc->expr = expr;
  sc->senv = senv;
  return sc;
}

static bool
sc_identifier_p(pic_value obj)
{
  if (pic_sym_p(obj)) {
    return true;
  }
  if (pic_sc_p(obj)) {
    return sc_identifier_p(pic_sc_ptr(obj)->expr);
  }
  return false;
}

static bool
sc_identifier_eq_p(pic_state *pic, struct pic_senv *e1, pic_value x, struct pic_senv *e2, pic_value y)
{
  struct pic_dict *cxt;

  if (! (sc_identifier_p(x) && sc_identifier_p(y))) {
    return false;
  }

  cxt = pic_dict_new(pic);

  x = macroexpand(pic, x, e1, cxt);
  y = macroexpand(pic, y, e2, cxt);

  return pic_eq_p(x, y);
}

static pic_value
pic_macro_make_sc(pic_state *pic)
{
  pic_value senv, free_vars, expr;
  struct pic_sc *sc;

  pic_get_args(pic, "ooo", &senv, &free_vars, &expr);

  if (! pic_senv_p(senv))
    pic_error(pic, "make-syntactic-closure: senv required");

  /* just ignore free_vars for now */
  sc = sc_new(pic, expr, pic_senv_ptr(senv));

  return pic_obj_value(sc);
}

static pic_value
pic_macro_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(sc_identifier_p(obj));
}

static pic_value
pic_macro_identifier_eq_p(pic_state *pic)
{
  pic_value e, x, f, y;
  struct pic_senv *e1, *e2;

  pic_get_args(pic, "oooo", &e, &x, &f, &y);

  if (! pic_senv_p(e)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  e1 = pic_senv_ptr(e);
  if (! pic_senv_p(f)) {
    pic_error(pic, "unexpected type of argument 3");
  }
  e2 = pic_senv_ptr(f);

  return pic_bool_value(sc_identifier_eq_p(pic, e1, x, e2, y));
}

static pic_value
er_macro_rename(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *mac_env;
  struct pic_dict *cxt;

  pic_get_args(pic, "m", &sym);

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));
  cxt = pic_dict_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 2));

  return pic_sym_value(translate(pic, sym, mac_env, cxt));
}

static pic_value
er_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;
  pic_sym m, n;
  struct pic_dict *cxt;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));
  cxt = pic_dict_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 2));

  m = translate(pic, pic_sym(a), use_env, cxt);
  n = translate(pic, pic_sym(b), use_env, cxt);

  return pic_bool_value(m == n);
}

static pic_value
er_macro_call(pic_state *pic)
{
  pic_value expr, use_env, mac_env;
  struct pic_proc *rename, *compare, *cb;
  struct pic_dict *cxt;

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  cxt = pic_dict_new(pic);

  rename = pic_proc_new(pic, er_macro_rename, "<er-macro-renamer>");
  pic_proc_cv_init(pic, rename, 3);
  pic_proc_cv_set(pic, rename, 0, use_env);
  pic_proc_cv_set(pic, rename, 1, mac_env);
  pic_proc_cv_set(pic, rename, 2, pic_obj_value(cxt));

  compare = pic_proc_new(pic, er_macro_compare, "<er-macro-comparator>");
  pic_proc_cv_init(pic, compare, 3);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);
  pic_proc_cv_set(pic, compare, 2, pic_obj_value(cxt));

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  return pic_apply3(pic, cb, expr, pic_obj_value(rename), pic_obj_value(compare));
}

static pic_value
pic_macro_er_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, er_macro_call, "<er-macro-procedure>");
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

static pic_value
ir_macro_inject(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *use_env;
  struct pic_dict *cxt;

  pic_get_args(pic, "m", &sym);

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));
  cxt = pic_dict_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 2));

  return pic_sym_value(translate(pic, sym, use_env, cxt));
}

static pic_value
ir_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *mac_env;
  pic_sym m, n;
  struct pic_dict *cxt;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));
  cxt = pic_dict_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 2));

  m = translate(pic, pic_sym(a), mac_env, cxt);
  n = translate(pic, pic_sym(b), mac_env, cxt);

  return pic_bool_value(m == n);
}

static pic_value
ir_macro_wrap(pic_state *pic, pic_value expr, struct pic_senv *use_env, struct pic_dict *cxt, pic_value *ir)
{
  if (pic_sym_p(expr)) {
    pic_value r;
    r = pic_sym_value(translate(pic, pic_sym(expr), use_env, cxt));
    *ir = pic_acons(pic, r, expr, *ir);
    return r;
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_wrap(pic, pic_car(pic, expr), use_env, cxt, ir),
                    ir_macro_wrap(pic, pic_cdr(pic, expr), use_env, cxt, ir));
  }
  else {
    return expr;
  }
}

static pic_value
ir_macro_unwrap(pic_state *pic, pic_value expr, struct pic_senv *mac_env, struct pic_dict *cxt, pic_value *ir)
{
  if (pic_sym_p(expr)) {
    pic_value r;
    if (pic_test(r = pic_assq(pic, expr, *ir))) {
      return pic_cdr(pic, r);
    }
    return pic_sym_value(translate(pic, pic_sym(expr), mac_env, cxt));
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_unwrap(pic, pic_car(pic, expr), mac_env, cxt, ir),
                    ir_macro_unwrap(pic, pic_cdr(pic, expr), mac_env, cxt, ir));
  }
  else {
    return expr;
  }
}

static pic_value
ir_macro_call(pic_state *pic)
{
  pic_value expr, use_env, mac_env;
  struct pic_proc *inject, *compare, *cb;
  struct pic_dict *cxt;
  pic_value ir = pic_nil_value();

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  cxt = pic_dict_new(pic);

  inject = pic_proc_new(pic, ir_macro_inject, "<ir-macro-injecter>");
  pic_proc_cv_init(pic, inject, 3);
  pic_proc_cv_set(pic, inject, 0, use_env);
  pic_proc_cv_set(pic, inject, 1, mac_env);
  pic_proc_cv_set(pic, inject, 2, pic_obj_value(cxt));

  compare = pic_proc_new(pic, ir_macro_compare, "<ir-macro-comparator>");
  pic_proc_cv_init(pic, compare, 3);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);
  pic_proc_cv_set(pic, compare, 2, pic_obj_value(cxt));

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  expr = ir_macro_wrap(pic, expr, pic_senv_ptr(use_env), cxt, &ir);
  expr = pic_apply3(pic, cb, expr, pic_obj_value(inject), pic_obj_value(compare));
  expr = ir_macro_unwrap(pic, expr, pic_senv_ptr(mac_env), cxt, &ir);

  return expr;
}

static pic_value
pic_macro_ir_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, ir_macro_call, "<ir-macro-procedure>");
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

void
pic_init_macro(pic_state *pic)
{
  pic_deflibrary ("(picrin macro)") {

    /* export define-macro syntax */
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sDEFINE_MACRO);

    pic_defun(pic, "gensym", pic_macro_gensym);
    pic_defun(pic, "macroexpand", pic_macro_macroexpand);
    pic_defun(pic, "make-syntactic-closure", pic_macro_make_sc);
    pic_defun(pic, "identifier?", pic_macro_identifier_p);
    pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
    pic_defun(pic, "er-macro-transformer", pic_macro_er_macro_transformer);
    pic_defun(pic, "ir-macro-transformer", pic_macro_ir_macro_transformer);
  }
}
