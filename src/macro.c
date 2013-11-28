#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "xhash/xhash.h"

#define FALLTHROUGH ((void)0)

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *);
static pic_value macroexpand_list(pic_state *, pic_value, struct pic_senv *);

static pic_sym
new_uniq_sym(pic_state *pic, pic_sym base)
{
  int s = ++pic->uniq_sym_count;
  char *str;
  pic_sym uniq;

  str = (char *)pic_alloc(pic, strlen(pic_symbol_name(pic, base)) + (int)log10(s) + 3);
  sprintf(str, "%s@%d", pic_symbol_name(pic, base), s);
  uniq = pic_intern_cstr(pic, str);

  pic_free(pic, str);
  return uniq;
}

static struct pic_senv *
new_global_senv(pic_state *pic)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = NULL;
  senv->tbl = pic->var_tbl;
  senv->stx = pic->stx;
  senv->xlen = pic->xlen;
  senv->xcapa = pic->xcapa;
  return senv;
}

static struct pic_senv *
new_local_senv(pic_state *pic, pic_value formals, struct pic_senv *up)
{
  struct pic_senv *senv;
  pic_value a;
  pic_sym sym;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  senv->tbl = xh_new();
  senv->stx = NULL;
  senv->xlen = 0;
  senv->xcapa = 0;

  for (a = formals; pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_value v = pic_car(pic, a);

    if (! pic_symbol_p(v)) {
      v = macroexpand(pic, v, up);
    }
    if (! pic_symbol_p(v)) {
      pic_error(pic, "syntax error");
    }
    sym = pic_sym(v);
    xh_put(senv->tbl, pic_symbol_name(pic, sym), (int)new_uniq_sym(pic, sym));
  }
  if (! pic_symbol_p(a)) {
    a = macroexpand(pic, a, up);
  }
  if (pic_symbol_p(a)) {
    sym = pic_sym(a);
    xh_put(senv->tbl, pic_symbol_name(pic, sym), (int)new_uniq_sym(pic, sym));
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
  if (pic_symbol_p(obj)) {
    return true;
  }
  if (pic_sc_p(obj)) {
    return pic_identifier_p(pic_sc(obj)->expr);
  }
  return false;
}

static void
pic_defsyntax(pic_state *pic, const char *name, struct pic_proc *macro, struct pic_senv *mac_env)
{
  int idx;

  idx = pic->xlen;
  if (idx >= pic->xcapa) {
    pic_abort(pic, "macro table overflow");
  }
  pic->stx[idx] = pic_syntax_new_macro(pic, pic_intern_cstr(pic, name), macro, mac_env);
  xh_put(pic->var_tbl, name, ~idx);
  pic->xlen++;
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

  switch (pic_type(expr)) {
  case PIC_TT_SC: {
    struct pic_sc *sc;

    sc = pic_sc(expr);
    return macroexpand(pic, sc->expr, sc->senv);
  }
  case PIC_TT_SYMBOL: {
    struct xh_entry *e;
    while (senv) {
      if ((e = xh_get(senv->tbl, pic_symbol_name(pic, pic_sym(expr)))) != NULL) {
	if (e->val >= 0)
	  return pic_symbol_value((pic_sym)e->val);
	else
	  return pic_obj_value(senv->stx[~e->val]);
      }
      senv = senv->up;
    }
    return expr;
  }
  case PIC_TT_PAIR: {
    pic_value car, v;

    if (! pic_list_p(pic, expr))
      return expr;

    car = macroexpand(pic, pic_car(pic, expr), senv);
    if (pic_syntax_p(car)) {
      switch (pic_syntax(car)->kind) {
      case PIC_STX_DEFSYNTAX: {
	pic_value var, val;
	struct pic_proc *proc;

	if (pic_length(pic, expr) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = pic_cadr(pic, expr);
	if (! pic_symbol_p(var)) {
	  pic_error(pic, "syntax error");
	}

	val = pic_cadr(pic, pic_cdr(pic, expr));
	proc = pic_codegen(pic, val);
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
	return pic_false_value();
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
	if (! pic_symbol_p(var)) {
	  pic_error(pic, "syntax error");
	}

	proc = pic_codegen(pic, val);
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
	return pic_false_value();
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
	  if (! pic_symbol_p(a)) {
	    pic_error(pic, "binding to non-symbol object");
	  }
	  sym = pic_sym(a);
	  xh_put(senv->tbl, pic_symbol_name(pic, sym), (int)new_uniq_sym(pic, sym));

	  /* binding value */
	  v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym),
		       pic_cons(pic,
				macroexpand_list(pic, pic_cadr(pic, expr), in),
				macroexpand_list(pic, pic_cddr(pic, expr), in)));

	  pic_gc_arena_restore(pic, ai);
	  pic_gc_protect(pic, v);
	  return v;
	}

	if (! pic_symbol_p(var)) {
	  pic_error(pic, "binding to non-symbol object");
	}
	uniq = new_uniq_sym(pic, pic_sym(var));
	xh_put(senv->tbl, pic_symbol_name(pic, pic_sym(var)), (int)uniq);
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

  if (pic_nil_p(list))
    return pic_nil_value();

  if (pic_symbol_p(list))
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
  if (! pic_identifier_p(x)) {
    pic_error(pic, "unexpected type of argument 2");
  }
  if (! pic_senv_p(f)) {
    pic_error(pic, "unexpected type of argument 3");
  }
  e2 = pic_senv(f);
  if (! pic_identifier_p(y)) {
    pic_error(pic, "unexpected type of argument 4");
  }

  x = macroexpand(pic, x, e1);
  y = macroexpand(pic, y, e2);

  return pic_bool_value(pic_eq_p(x, y));
}

void
pic_init_macro(pic_state *pic)
{
  pic_defun(pic, "make-syntactic-closure", pic_macro_make_sc);
  pic_defun(pic, "identifier?", pic_macro_identifier_p);
  pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
}
