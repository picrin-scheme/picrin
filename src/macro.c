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

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  int idx;

  idx = pic->xlen;
  if (idx >= pic->xcapa) {
    pic_abort(pic, "macro table overflow");
  }
  pic->stx[idx] = pic_syntax_new_macro(pic, pic_intern_cstr(pic, name), macro);
  xh_put(pic->var_tbl, name, ~idx);
  pic->xlen++;
}

static pic_sym
new_uniq_sym(pic_state *pic, pic_sym base)
{
  int s = ++pic->uniq_sym_count;
  char *str;
  pic_sym uniq;

  str = (char *)pic_alloc(pic, strlen(pic_symbol_name(pic, base)) + (int)log10(s) + 2);
  sprintf(str, "%s@%d", pic_symbol_name(pic, base), s);
  uniq = pic_intern_cstr(pic, str);

  pic_free(pic, str);
  return uniq;
}

static pic_value macroexpand_list(pic_state *, pic_value, struct pic_senv *);

static pic_value
macroexpand(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  int ai = pic_gc_arena_preserve(pic);

  switch (pic_type(expr)) {
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
	v = pic_apply(pic, pic_syntax(car)->macro, pic_cdr(pic, expr));
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);

	return macroexpand(pic, v, senv);
      }
      case PIC_STX_LAMBDA: {
	struct pic_senv *in;
	pic_value a;

	in = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
	in->up = senv;
	in->tbl = xh_new();
	in->stx = NULL;

	for (a = pic_cadr(pic, expr); pic_pair_p(a); a = pic_cdr(pic, a)) {
	  pic_sym gen, orig;

	  orig = pic_sym(pic_car(pic, a));
	  gen = new_uniq_sym(pic, orig);
	  xh_put(in->tbl, pic_symbol_name(pic, orig), (int)gen);
	}
	if (pic_symbol_p(a)) {
	  xh_put(in->tbl, pic_symbol_name(pic, pic_sym(a)), (int)new_uniq_sym(pic, pic_sym(a)));
	}

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
	struct pic_senv *in = senv;

	if (pic_length(pic, expr) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_cadr(pic, expr);
	if (pic_pair_p(var)) {
	  pic_value a;

	  in = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
	  in->up = senv;
	  in->tbl = xh_new();
	  in->stx = NULL;
	  in->xlen = 0;
	  in->xcapa = 0;

	  /* defined symbol */
	  a = pic_car(pic, var);
	  xh_put(senv->tbl, pic_symbol_name(pic, pic_sym(a)), (int)new_uniq_sym(pic, pic_sym(a)));
	  var = pic_cdr(pic, var);

	  for (a = var; pic_pair_p(a); a = pic_cdr(pic, a)) {
	    pic_sym gen, orig;

	    orig = pic_sym(pic_car(pic, a));
	    gen = new_uniq_sym(pic, orig);
	    xh_put(in->tbl, pic_symbol_name(pic, orig), (int)gen);
	  }
	  if (pic_symbol_p(a)) {
	    xh_put(in->tbl, pic_symbol_name(pic, pic_sym(a)), (int)new_uniq_sym(pic, pic_sym(a)));
	  }

	  v = pic_cons(pic, pic_symbol_value(pic_syntax(car)->sym),
		       pic_cons(pic,
				macroexpand_list(pic, pic_cadr(pic, expr), in),
				macroexpand_list(pic, pic_cddr(pic, expr), in)));
	  pic_gc_arena_restore(pic, ai);
	  pic_gc_protect(pic, v);
	  return v;
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

  if (! pic_pair_p(list))
    return macroexpand(pic, list, senv);

  v = macroexpand(pic, pic_car(pic, list), senv);
  return pic_cons(pic, v, macroexpand_list(pic, pic_cdr(pic, list), senv));
}

struct pic_syntax *
pic_syntax_new(pic_state *pic, int kind, pic_sym sym)
{
  struct pic_syntax *stx;

  stx = (struct pic_syntax *)pic_obj_alloc(pic, sizeof(struct pic_syntax), PIC_TT_SYNTAX);
  stx->kind = kind;
  stx->sym = sym;
  stx->macro = NULL;
  return stx;
}

struct pic_syntax *
pic_syntax_new_macro(pic_state *pic, pic_sym sym, struct pic_proc *macro)
{
  struct pic_syntax *stx;

  stx = (struct pic_syntax *)pic_obj_alloc(pic, sizeof(struct pic_syntax), PIC_TT_SYNTAX);
  stx->kind = PIC_STX_MACRO;
  stx->sym = sym;
  stx->macro = macro;
  return stx;
}

pic_value
pic_macroexpand(pic_state *pic, pic_value expr)
{
  struct pic_senv *senv;
  pic_value v;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = NULL;
  senv->tbl = pic->var_tbl;
  senv->stx = pic->stx;
  senv->xlen = pic->xlen;
  senv->xcapa = pic->xcapa;

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
