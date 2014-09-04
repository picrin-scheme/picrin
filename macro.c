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
  pic_sym sym, rename;
  pic_value var, val;

  while (pic_length(pic, expr) >= 2 && pic_pair_p(pic_cadr(pic, expr))) {
    var = pic_car(pic, pic_cadr(pic, expr));
    val = pic_cdr(pic, pic_cadr(pic, expr));

    expr = pic_list3(pic, pic_sym_value(pic->rDEFINE), var, pic_cons(pic, pic_sym_value(pic->rLAMBDA), pic_cons(pic, val, pic_cddr(pic, expr))));
  }

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
  }
  val = macroexpand(pic, pic_list_ref(pic, expr, 2), senv);

  return pic_list3(pic, pic_sym_value(pic->rDEFINE), pic_sym_value(rename), val);
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
  } else {
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

  return v;
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

      if (tag == pic->rDEFINE_SYNTAX) {
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
        return macroexpand_node(pic, macroexpand_macro(pic, mac, expr, senv), senv);
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
  pic_define_syntactic_keyword(pic, senv, pic->sIN_LIBRARY, pic->rIN_LIBRARY);

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
pic_defmacro(pic_state *pic, pic_sym name, pic_sym id, pic_func_t func)
{
  pic_put_rename(pic, pic->lib->env, name, id);

  /* symbol registration */
  define_macro(pic, id, pic_proc_new(pic, func, pic_symbol_name(pic, name)), NULL);

  /* auto export! */
  pic_export(pic, name);
}

bool
pic_identifier_p(pic_state *pic, pic_value obj)
{
  return pic_sym_p(obj) && ! pic_interned_p(pic, pic_sym(obj));
}

bool
pic_identifier_eq_p(pic_state *pic, struct pic_senv *env1, pic_sym sym1, struct pic_senv *env2, pic_sym sym2)
{
  pic_sym a, b;

  a = make_identifier(pic, sym1, env1);
  if (a != make_identifier(pic, sym1, env1)) {
    a = sym1;
  }

  b = make_identifier(pic, sym2, env2);
  if (b != make_identifier(pic, sym2, env2)) {
    b = sym2;
  }

  return pic_eq_p(pic_sym_value(a), pic_sym_value(b));
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

static pic_value
pic_macro_identifier_eq_p(pic_state *pic)
{
  pic_sym sym1, sym2;
  pic_value env1, env2;

  pic_get_args(pic, "omom", &env1, &sym1, &env2, &sym2);

  pic_assert_type(pic, env1, senv);
  pic_assert_type(pic, env2, senv);

  return pic_bool_value(pic_identifier_eq_p(pic, pic_senv_ptr(env1), sym1, pic_senv_ptr(env2), sym2));
}

void
pic_init_macro(pic_state *pic)
{
  pic_defun(pic, "identifier?", pic_macro_identifier_p);
  pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
  pic_defun(pic, "make-identifier", pic_macro_make_identifier);
}
