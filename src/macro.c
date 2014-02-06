/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/lib.h"

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *);
static pic_value macroexpand_list(pic_state *, pic_value, struct pic_senv *);

struct pic_senv *
pic_null_syntactic_env(pic_state *pic)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = NULL;
  senv->tbl = xh_new_int();
  senv->stx = (struct pic_syntax **)pic_calloc(pic, PIC_MACROS_SIZE, sizeof(struct pic_syntax *));
  senv->xlen = 0;
  senv->xcapa = PIC_MACROS_SIZE;

  return senv;
}

#define register_core_syntax(pic,senv,kind,name) do {			\
    pic_sym sym = pic_intern_cstr(pic, name);                           \
    senv->stx[senv->xlen] = pic_syntax_new(pic, kind, sym);             \
    xh_put_int(senv->tbl, sym, ~senv->xlen);				\
    senv->xlen++;							\
  } while (0)

struct pic_senv *
pic_minimal_syntactic_env(pic_state *pic)
{
  struct pic_senv *senv = pic_null_syntactic_env(pic);

  register_core_syntax(pic, senv, PIC_STX_DEFLIBRARY, "define-library");
  register_core_syntax(pic, senv, PIC_STX_IMPORT, "import");
  register_core_syntax(pic, senv, PIC_STX_EXPORT, "export");

  return senv;
}

struct pic_senv *
pic_core_syntactic_env(pic_state *pic)
{
  struct pic_senv *senv = pic_minimal_syntactic_env(pic);

  register_core_syntax(pic, senv, PIC_STX_DEFINE, "define");
  register_core_syntax(pic, senv, PIC_STX_SET, "set!");
  register_core_syntax(pic, senv, PIC_STX_QUOTE, "quote");
  register_core_syntax(pic, senv, PIC_STX_LAMBDA, "lambda");
  register_core_syntax(pic, senv, PIC_STX_IF, "if");
  register_core_syntax(pic, senv, PIC_STX_BEGIN, "begin");
  register_core_syntax(pic, senv, PIC_STX_DEFMACRO, "define-macro");
  register_core_syntax(pic, senv, PIC_STX_DEFSYNTAX, "define-syntax");

  return senv;
}

#undef register_core_syntax

static struct pic_senv *
new_global_senv(pic_state *pic)
{
  return pic->lib->senv;
}

static struct pic_senv *
new_local_senv(pic_state *pic, pic_value formals, struct pic_senv *up)
{
  struct pic_senv *senv;
  pic_value a;
  pic_sym sym;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  senv->tbl = xh_new_int();
  senv->stx = NULL;
  senv->xlen = 0;
  senv->xcapa = 0;

  for (a = formals; pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_value v = pic_car(pic, a);

    if (! pic_sym_p(v)) {
      v = macroexpand(pic, v, up);
    }
    if (! pic_sym_p(v)) {
      pic_error(pic, "syntax error");
    }
    sym = pic_sym(v);
    xh_put_int(senv->tbl, sym, pic_gensym(pic, sym));
  }
  if (! pic_sym_p(a)) {
    a = macroexpand(pic, a, up);
  }
  if (pic_sym_p(a)) {
    sym = pic_sym(a);
    xh_put_int(senv->tbl, sym, pic_gensym(pic, sym));
  }
  else if (! pic_nil_p(a)) {
    pic_error(pic, "syntax error");
  }
  return senv;
}

struct pic_syntax *
pic_syntax_new(pic_state *pic, int kind, pic_sym sym)
{
  struct pic_syntax *stx;

  stx = (struct pic_syntax *)pic_obj_alloc(pic, sizeof(struct pic_syntax), PIC_TT_SYNTAX);
  stx->kind = kind;
  stx->sym = sym;
  stx->macro = NULL;
  stx->senv = NULL;
  return stx;
}

struct pic_syntax *
pic_syntax_new_macro(pic_state *pic, pic_sym sym, struct pic_proc *macro, struct pic_senv *mac_env)
{
  struct pic_syntax *stx;

  stx = (struct pic_syntax *)pic_obj_alloc(pic, sizeof(struct pic_syntax), PIC_TT_SYNTAX);
  stx->kind = PIC_STX_MACRO;
  stx->sym = sym;
  stx->macro = macro;
  stx->senv = mac_env;
  return stx;
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
pic_identifier_p(pic_value obj)
{
  if (pic_sym_p(obj)) {
    return true;
  }
  if (pic_sc_p(obj)) {
    return pic_identifier_p(pic_sc(obj)->expr);
  }
  return false;
}

static pic_value
strip(pic_state *pic, pic_value expr)
{
  if (pic_sc_p(expr)) {
    return strip(pic, pic_sc(expr)->expr);
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    strip(pic, pic_car(pic, expr)),
                    strip(pic, pic_cdr(pic, expr)));
  }
  return expr;
}

void
pic_import(pic_state *pic, pic_value spec)
{
  struct pic_lib *lib;
  xh_iter it;

  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_error(pic, "library not found");
  }
  for (xh_begin(lib->exports, &it); ! xh_isend(&it); xh_next(&it)) {
#if DEBUG
    if (it.e->val >= 0) {
      printf("* importing %s as %s\n", pic_symbol_name(pic, (long)it.e->key), pic_symbol_name(pic, it.e->val));
    }
    else {
      printf("* importing %s\n", pic_symbol_name(pic, (long)it.e->key));
    }
#endif
    if (it.e->val >= 0) {
      xh_put_int(pic->lib->senv->tbl, (long)it.e->key, it.e->val);
    }
    else {                /* syntax object */
      size_t idx;
      struct pic_senv *senv = pic->lib->senv;

      idx = senv->xlen;
      if (idx >= senv->xcapa) {
        pic_abort(pic, "macro table overflow");
      }
      /* bring macro object from imported lib */
      senv->stx[idx] = lib->senv->stx[~it.e->val];
      xh_put_int(senv->tbl, (long)it.e->key, ~idx);
      senv->xlen++;
    }
  }
}

void
pic_export(pic_state *pic, pic_sym sym)
{
  xh_entry *e;

  e = xh_get_int(pic->lib->senv->tbl, sym);
  if (! e) {
    pic_error(pic, "symbol not defined");
  }
  xh_put_int(pic->lib->exports, (long)e->key, e->val);
}

static void
pic_defsyntax(pic_state *pic, const char *name, struct pic_proc *macro, struct pic_senv *mac_env)
{
  pic_sym sym;
  struct pic_syntax *stx;
  struct pic_senv *global_senv = pic->lib->senv;
  size_t idx;

  sym = pic_intern_cstr(pic, name);
  stx = pic_syntax_new_macro(pic, sym, macro, mac_env);

  idx = global_senv->xlen;
  if (idx >= global_senv->xcapa) {
    pic_abort(pic, "macro table overflow");
  }
  global_senv->stx[idx] = stx;
  xh_put_int(global_senv->tbl, sym, ~idx);
  global_senv->xlen++;
}

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  pic_defsyntax(pic, name, macro, NULL);
}

static pic_value
macroexpand(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  int ai = pic_gc_arena_preserve(pic);

#if DEBUG
  printf("[macroexpand] expanding... ");
  pic_debug(pic, expr);
  puts("");
#endif

  switch (pic_type(expr)) {
  case PIC_TT_SC: {
    struct pic_sc *sc;

    sc = pic_sc(expr);
    return macroexpand(pic, sc->expr, sc->senv);
  }
  case PIC_TT_SYMBOL: {
    xh_entry *e;
    pic_sym uniq;

    if (! pic_interned_p(pic, pic_sym(expr))) {
      return expr;
    }
    while (true) {
      if ((e = xh_get_int(senv->tbl, pic_sym(expr))) != NULL) {
	if (e->val >= 0)
	  return pic_symbol_value(e->val);
	else
	  return pic_obj_value(senv->stx[~e->val]);
      }
      if (! senv->up)
        break;
      senv = senv->up;
    }
    uniq = pic_gensym(pic, pic_sym(expr));
    xh_put_int(senv->tbl, pic_sym(expr), uniq);
    return pic_symbol_value(uniq);
  }
  case PIC_TT_PAIR: {
    pic_value car, v;

    car = macroexpand(pic, pic_car(pic, expr), senv);
    if (pic_syntax_p(car)) {
      switch (pic_syntax(car)->kind) {
      case PIC_STX_DEFLIBRARY: {
        struct pic_lib *prev = pic->lib;

        if (pic_length(pic, expr) < 2) {
          pic_error(pic, "syntax error");
        }
        pic_make_library(pic, pic_cadr(pic, expr));

        /* proceed expressions in new library */
        pic_in_library(pic, pic_cadr(pic, expr));
        {
          struct pic_proc *proc;

          pic_for_each (v, pic_cddr(pic, expr)) {
            proc = pic_compile(pic, v);
            if (proc == NULL) {
              abort();
            }
            pic_apply_argv(pic, proc, 0);
            if (pic_undef_p(v)) {
              abort();
            }
          }
        }
        pic_in_library(pic, prev->name);

        return pic_none_value();
      }
      case PIC_STX_IMPORT: {
        pic_value spec;
        pic_for_each (spec, pic_cdr(pic, expr)) {
          pic_import(pic, spec);
        }
        return pic_none_value();
      }
      case PIC_STX_EXPORT: {
        pic_value spec;
        pic_for_each (spec, pic_cdr(pic, expr)) {
          if (! pic_sym_p(spec)) {
            pic_error(pic, "syntax error");
          }
          /* TODO: warn if symbol is shadowed by local variable */
          pic_export(pic, pic_sym(spec));
        }
        return pic_none_value();
      }
      case PIC_STX_DEFSYNTAX: {
	pic_value var, val;
	struct pic_proc *proc;

	if (pic_length(pic, expr) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = strip(pic, pic_cadr(pic, expr));
	if (! pic_sym_p(var)) {
	  pic_error(pic, "syntax error");
	}

	val = pic_cadr(pic, pic_cdr(pic, expr));
	proc = pic_compile(pic, val);
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	v = pic_apply(pic, proc, pic_nil_value());
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	assert(pic_proc_p(v));
	pic_defsyntax(pic, pic_symbol_name(pic, pic_sym(var)), pic_proc_ptr(v), senv);

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }
      case PIC_STX_DEFMACRO: {
	pic_value var, val;
	struct pic_proc *proc;

	if (pic_length(pic, expr) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_car(pic, pic_cdr(pic, expr));
	if (pic_pair_p(var)) {
	  /* FIXME: unhygienic */
	  val = pic_cons(pic, pic_symbol_value(pic->sLAMBDA),
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

	proc = pic_compile(pic, val);
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	v = pic_apply(pic, proc, pic_nil_value());
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	assert(pic_proc_p(v));
	pic_defmacro(pic, pic_symbol_name(pic, pic_sym(var)), pic_proc_ptr(v));

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }
      case PIC_STX_MACRO: {
	if (pic_syntax(car)->senv == NULL) { /* legacy macro */
	  v = pic_apply(pic, pic_syntax(car)->macro, pic_cdr(pic, expr));
	  if (pic->errmsg) {
	    printf("macroexpand error: %s\n", pic->errmsg);
	    abort();
	  }
	}
	else {
	  v = pic_apply_argv(pic, pic_syntax(car)->macro, 3, expr, pic_obj_value(senv), pic_obj_value(pic_syntax(car)->senv));
	  if (pic->errmsg) {
	    printf("macroexpand error: %s\n", pic->errmsg);
	    abort();
	  }
	}
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);

#if DEBUG
	puts("after expand-1:");
	pic_debug(pic, v);
	puts("");
#endif

	return macroexpand(pic, v, senv);
      }
      case PIC_STX_LAMBDA: {
	struct pic_senv *in = new_local_senv(pic, pic_cadr(pic, expr), senv);

	v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym),
		     pic_cons(pic,
			      macroexpand_list(pic, pic_cadr(pic, expr), in),
			      macroexpand_list(pic, pic_cddr(pic, expr), in)));

	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }
      case PIC_STX_DEFINE: {
	pic_sym uniq;
	pic_value var;

	if (pic_length(pic, expr) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_cadr(pic, expr);
	if (pic_pair_p(var)) {
	  struct pic_senv *in = new_local_senv(pic, pic_cdr(pic, var), senv);
	  pic_value a;
	  pic_sym sym;

	  /* defined symbol */
	  a = pic_car(pic, var);
          if (! pic_sym_p(a)) {
            a = macroexpand(pic, a, senv);
          }
	  if (! pic_sym_p(a)) {
	    pic_error(pic, "binding to non-symbol object");
	  }
	  sym = pic_sym(a);
	  xh_put_int(senv->tbl, sym, pic_gensym(pic, sym));

	  /* binding value */
	  v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym),
		       pic_cons(pic,
				macroexpand_list(pic, pic_cadr(pic, expr), in),
				macroexpand_list(pic, pic_cddr(pic, expr), in)));

	  pic_gc_arena_restore(pic, ai);
	  pic_gc_protect(pic, v);
	  return v;
	}

        if (! pic_sym_p(var)) {
          var = macroexpand(pic, var, senv);
        }
	if (! pic_sym_p(var)) {
	  pic_error(pic, "binding to non-symbol object");
	}
	uniq = pic_gensym(pic, pic_sym(var));
	xh_put_int(senv->tbl, pic_sym(var), (int)uniq);
      }
	FALLTHROUGH;
      case PIC_STX_SET:
      case PIC_STX_IF:
      case PIC_STX_BEGIN:
	v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym), macroexpand_list(pic, pic_cdr(pic, expr), senv));
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      case PIC_STX_QUOTE:
	v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym), pic_cdr(pic, expr));
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }
    }

    v = pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv));
    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, v);
    return v;
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
  case PIC_TT_SYNTAX:
  case PIC_TT_LIB:
  case PIC_TT_VAR:
  case PIC_TT_IREP:
    pic_error(pic, "unexpected value type");
    return pic_undef_value();	/* unreachable */
  }
  /* suppress warnings, never be called */
  abort();
}

static pic_value
macroexpand_list(pic_state *pic, pic_value list, struct pic_senv *senv)
{
  pic_value v;

  if (! pic_pair_p(list))
    return macroexpand(pic, list, senv);

  v = macroexpand(pic, pic_car(pic, list), senv);
  return pic_cons(pic, v, macroexpand_list(pic, pic_cdr(pic, list), senv));
}

pic_value
pic_macroexpand(pic_state *pic, pic_value expr)
{
  struct pic_senv *senv;
  pic_value v;

  senv = new_global_senv(pic);

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  v = macroexpand(pic, expr, senv);

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
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
  sc = sc_new(pic, expr, pic_senv(senv));

  return pic_obj_value(sc);
}

static pic_value
pic_macro_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_identifier_p(obj));
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
  e1 = pic_senv(e);
  if (! pic_senv_p(f)) {
    pic_error(pic, "unexpected type of argument 3");
  }
  e2 = pic_senv(f);

  if (! (pic_identifier_p(x) && pic_identifier_p(y))) {
    return pic_false_value();
  }

  x = macroexpand(pic, x, e1);
  y = macroexpand(pic, y, e2);

  return pic_bool_value(pic_eq_p(x, y));
}

static pic_value
er_macro_rename(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *mac_env;
  pic_value v;

  pic_get_args(pic, "m", &sym);

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));

  v = macroexpand(pic, pic_symbol_value(sym), mac_env);
  if (pic_syntax_p(v)) {
    return pic_symbol_value(sym);
  }
  else {
    return v;
  }
}

static pic_value
er_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  a = macroexpand(pic, a, use_env);
  b = macroexpand(pic, b, use_env);

  return pic_bool_value(pic_eq_p(a, b));
}

static pic_value
er_macro_call(pic_state *pic)
{
  pic_value expr, use_env, mac_env;
  struct pic_proc *rename, *compare, *cb;

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  rename = pic_proc_new(pic, er_macro_rename);
  pic_proc_cv_init(pic, rename, 2);
  pic_proc_cv_set(pic, rename, 0, use_env);
  pic_proc_cv_set(pic, rename, 1, mac_env);

  compare = pic_proc_new(pic, er_macro_compare);
  pic_proc_cv_init(pic, compare, 2);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  return pic_apply_argv(pic, cb, 3, expr, pic_obj_value(rename), pic_obj_value(compare));
}

static pic_value
pic_macro_er_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, er_macro_call);
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

static pic_value
ir_macro_inject(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *use_env;
  pic_value v;

  pic_get_args(pic, "m", &sym);

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  v = macroexpand(pic, pic_symbol_value(sym), use_env);
  if (pic_syntax_p(v)) {
    return pic_symbol_value(sym);
  }
  else {
    return v;
  }
}

static pic_value
ir_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  a = macroexpand(pic, a, use_env);
  b = macroexpand(pic, b, use_env);

  return pic_bool_value(pic_eq_p(a, b));
}

static pic_value
ir_macro_wrap(pic_state *pic, pic_value expr, struct pic_senv *use_env, pic_value *assoc)
{
  if (pic_sym_p(expr)) {
    pic_value ren;
    ren = macroexpand(pic, expr, use_env);
    *assoc = pic_acons(pic, ren, expr, *assoc);
    return ren;
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_wrap(pic, pic_car(pic, expr), use_env, assoc),
                    ir_macro_wrap(pic, pic_cdr(pic, expr), use_env, assoc));
  }
  else {
    return expr;
  }
}

static pic_value
ir_macro_unwrap(pic_state *pic, pic_value expr, struct pic_senv *mac_env, pic_value *assoc)
{
  if (pic_sym_p(expr) || pic_syntax_p(expr)) {
    pic_value r;
    if (pic_test(r = pic_assq(pic, expr, *assoc))) {
      return pic_cdr(pic, r);
    }
    r = macroexpand(pic, expr, mac_env);
    if (pic_syntax_p(r)) {
      return expr;
    }
    else {
      return r;
    }
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_unwrap(pic, pic_car(pic, expr), mac_env, assoc),
                    ir_macro_unwrap(pic, pic_cdr(pic, expr), mac_env, assoc));
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
  pic_value assoc = pic_nil_value();

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  inject = pic_proc_new(pic, ir_macro_inject);
  pic_proc_cv_init(pic, inject, 2);
  pic_proc_cv_set(pic, inject, 0, use_env);
  pic_proc_cv_set(pic, inject, 1, mac_env);

  compare = pic_proc_new(pic, ir_macro_compare);
  pic_proc_cv_init(pic, compare, 2);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  expr = ir_macro_wrap(pic, expr, pic_senv_ptr(use_env), &assoc);
  expr = pic_apply_argv(pic, cb, 3, expr, pic_obj_value(inject), pic_obj_value(compare));
  expr = ir_macro_unwrap(pic, expr, pic_senv_ptr(mac_env), &assoc);
  return expr;
}

static pic_value
pic_macro_ir_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, ir_macro_call);
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

void
pic_init_macro(pic_state *pic)
{
  pic_deflibrary ("(picrin macro)") {
    pic_defun(pic, "make-syntactic-closure", pic_macro_make_sc);
    pic_defun(pic, "identifier?", pic_macro_identifier_p);
    pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
    pic_defun(pic, "er-macro-transformer", pic_macro_er_macro_transformer);
    pic_defun(pic, "ir-macro-transformer", pic_macro_ir_macro_transformer);
  }
}
