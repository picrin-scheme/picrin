#include <stdio.h>
#include <assert.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "xhash/xhash.h"

#define FALLTHROUGH ((void)0)

struct syntactic_env {
  struct syntactic_env *up;

  struct xhash *tbl;
};

static void
define_macro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  int idx;

  idx = pic->mlen++;
  pic->macros[idx] = macro;
  xh_put(pic->global_tbl, name, ~idx);
}

static struct pic_proc *
lookup_macro(pic_state *pic, struct syntactic_env *env, const char *name)
{
  struct xh_entry *e;

  e = xh_get(env->tbl, name);
  if (! e)
    return NULL;

  if (e->val >= 0)
    return NULL;

  return pic->macros[~e->val];
}

pic_value
expand(pic_state *pic, pic_value obj, struct syntactic_env *env)
{
  int ai = pic_gc_arena_preserve(pic);

#if DEBUG
  printf("expanding...");
  pic_debug(pic, obj);
  puts("");
#endif

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    return obj;
  }
  case PIC_TT_PAIR: {
    pic_value v;

    if (! pic_list_p(pic, obj))
      return obj;

    if (pic_symbol_p(pic_car(pic, obj))) {
      struct pic_proc *macro;
      pic_sym sym;

      sym = pic_sym(pic_car(pic, obj));
      if (sym == pic->sDEFINE_MACRO) {
	pic_value var, val;
	struct pic_proc *proc;

	if (pic_length(pic, obj) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_car(pic, pic_cdr(pic, obj));
	if (pic_pair_p(var)) {
	  val = pic_cons(pic, pic_symbol_value(pic->sLAMBDA),
			 pic_cons(pic, pic_cdr(pic, var),
				  pic_cdr(pic, pic_cdr(pic, obj))));
	  var = pic_car(pic, var);
	}
	else {
	  if (pic_length(pic, obj) != 3) {
	    pic_error(pic, "syntax_error");
	  }
	  val = pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj)));
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
	define_macro(pic, pic_symbol_name(pic, pic_sym(var)), pic_proc_ptr(v));

	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return pic_false_value();
      }
      macro = lookup_macro(pic, env, pic_symbol_name(pic, sym));
      if (macro) {
	v = pic_apply(pic, macro, pic_cdr(pic, obj));
	if (pic->errmsg) {
	  printf("macroexpand error: %s\n", pic->errmsg);
	  abort();
	}
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return expand(pic, v, env);
      }
    }

    v = pic_nil_value();
    while (! pic_nil_p(obj)) {
      v = pic_cons(pic, expand(pic, pic_car(pic, obj), env), v);
      obj = pic_cdr(pic, obj);

      pic_gc_arena_restore(pic, ai);
      pic_gc_protect(pic, v);
    }
    v = pic_reverse(pic, v);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, v);
    return v;
  }
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_CHAR:
  case PIC_TT_EOF:
  case PIC_TT_STRING:
  case PIC_TT_VECTOR:
  case PIC_TT_BLOB: {
    return obj;
  }
  case PIC_TT_PROC:
  case PIC_TT_PORT:
  case PIC_TT_ERROR:
  case PIC_TT_ENV:
  case PIC_TT_CONT:
  case PIC_TT_UNDEF:
    pic_error(pic, "unexpected value type");
    return pic_undef_value();	/* unreachable */
  }
  /* logic falw (suppress warnings gcc will emit) */
  abort();
}

pic_value
pic_expand(pic_state *pic, pic_value obj)
{
  struct syntactic_env env;
  pic_value v;

  env.tbl = pic->global_tbl;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, obj);
  puts("");
#endif

  v = expand(pic, obj, &env);

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}
