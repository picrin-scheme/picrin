/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>
#include <assert.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "xhash/xhash.h"

#if PIC_NONE_IS_FALSE
# define OP_PUSHNONE OP_PUSHFALSE
#else
# error enable PIC_NONE_IS_FALSE
#endif

static pic_sym *
analyze_args(pic_state *pic, pic_value args, bool *varg, int *argc, int *localc)
{
  pic_sym *syms = (pic_sym *)pic_alloc(pic, sizeof(pic_sym));
  int i = 1, l = 0;
  pic_value v;

  *varg = false;
  for (v = args; pic_pair_p(v); v = pic_cdr(pic, v)) {
    pic_value sym;

    sym = pic_car(pic, v);
    if (! pic_sym_p(sym)) {
      pic_free(pic, syms);
      return NULL;
    }
    syms = (pic_sym *)pic_realloc(pic, syms, sizeof(pic_sym) * (i + 1));
    syms[i] = pic_sym(sym);
    i++;
  }
  if (pic_nil_p(v)) {
    /* pass */
  }
  else if (pic_sym_p(v)) {
    *varg = true;
    syms = (pic_sym *)pic_realloc(pic, syms, sizeof(pic_sym) * (i + 1));
    syms[i] = pic_sym(v);
    l++;
  }
  else {
    pic_free(pic, syms);
    return NULL;
  }
  *argc = i;
  *localc = l;

  return syms;
}

static bool
valid_formal(pic_state *pic, pic_value formal)
{
  bool varg;
  int argc, localc;
  pic_sym *syms;

  syms = analyze_args(pic, formal, &varg, &argc, &localc);
  if (syms == NULL) {
    return false;
  }
  else {
    pic_free(pic, syms);
    return true;
  }
}

typedef struct analyze_scope {
  /* rest args variable is counted by localc */
  bool varg;
  int argc, localc;
  /* if variable v is captured, then xh_get(var_tbl, v) == 1 */
  struct xhash *var_tbl;
  pic_sym *vars;

  struct analyze_scope *up;
} analyze_scope;

typedef struct analyze_state {
  pic_state *pic;
  analyze_scope *scope;
  pic_sym rCONS, rCAR, rCDR, rNILP;
  pic_sym rADD, rSUB, rMUL, rDIV;
  pic_sym rEQ, rLT, rLE, rGT, rGE, rNOT;
  pic_sym sCALL, sTAILCALL, sREF, sRETURN;
} analyze_state;

static void push_scope(analyze_state *, pic_value);
static void pop_scope(analyze_state *);

#define register_symbol(pic, state, slot, name) do {    \
    state->slot = pic_intern_cstr(pic, name);           \
  } while (0)

#define register_renamed_symbol(pic, state, slot, lib, name) do {       \
    struct xh_entry *e;                                                 \
    if (! (e = xh_get(lib->senv->tbl, name)))                           \
      pic_error(pic, "internal error! native VM procedure not found");  \
    state->slot = e->val;                                               \
  } while (0)

static analyze_state *
new_analyze_state(pic_state *pic)
{
  analyze_state *state;
  struct xhash *global_tbl;
  struct xh_iter it;
  struct pic_lib *stdlib;

  state = (analyze_state *)pic_alloc(pic, sizeof(analyze_state));
  state->pic = pic;
  state->scope = NULL;

  stdlib = pic_find_library(pic, pic_parse(pic, "(scheme base)"));

  /* native VM procedures */
  register_renamed_symbol(pic, state, rCONS, stdlib, "cons");
  register_renamed_symbol(pic, state, rCAR, stdlib, "car");
  register_renamed_symbol(pic, state, rCDR, stdlib, "cdr");
  register_renamed_symbol(pic, state, rNILP, stdlib, "null?");
  register_renamed_symbol(pic, state, rADD, stdlib, "+");
  register_renamed_symbol(pic, state, rSUB, stdlib, "-");
  register_renamed_symbol(pic, state, rMUL, stdlib, "*");
  register_renamed_symbol(pic, state, rDIV, stdlib, "/");
  register_renamed_symbol(pic, state, rEQ, stdlib, "=");
  register_renamed_symbol(pic, state, rLT, stdlib, "<");
  register_renamed_symbol(pic, state, rLE, stdlib, "<=");
  register_renamed_symbol(pic, state, rGT, stdlib, ">");
  register_renamed_symbol(pic, state, rGE, stdlib, ">=");
  register_renamed_symbol(pic, state, rNOT, stdlib, "not");

  register_symbol(pic, state, sCALL, "call");
  register_symbol(pic, state, sTAILCALL, "tail-call");
  register_symbol(pic, state, sREF, "ref");
  register_symbol(pic, state, sRETURN, "return");

  /* push initial scope */
  push_scope(state, pic_nil_value());

  global_tbl = pic->global_tbl;
  for (xh_begin(global_tbl, &it); ! xh_isend(&it); xh_next(&it)) {
    xh_put(state->scope->var_tbl, it.e->key, 0);
  }

  return state;
}

static void
destroy_analyze_state(analyze_state *state)
{
  pop_scope(state);
  pic_free(state->pic, state);
}

static void
push_scope(analyze_state *state, pic_value args)
{
  pic_state *pic = state->pic;
  analyze_scope *scope;
  int i;

  scope = (analyze_scope *)pic_alloc(pic, sizeof(analyze_scope));
  scope->up = state->scope;
  scope->var_tbl = xh_new();
  scope->varg = false;
  scope->vars = analyze_args(pic, args, &scope->varg, &scope->argc, &scope->localc);

  if (scope->vars == NULL) {
    pic_error(pic, "logic flaw");
  }

  for (i = 1; i < scope->argc + scope->localc; ++i) {
    xh_put(scope->var_tbl, pic_symbol_name(pic, scope->vars[i]), 0);
  }

  state->scope = scope;
}

static void
pop_scope(analyze_state *state)
{
  analyze_scope *scope;

  scope = state->scope;
  xh_destroy(scope->var_tbl);
  pic_free(state->pic, scope->vars);

  scope = scope->up;
  pic_free(state->pic, state->scope);
  state->scope = scope;
}

static int
lookup_var(analyze_state *state, pic_sym sym)
{
  analyze_scope *scope = state->scope;
  struct xh_entry *e;
  int depth = 0;
  const char *key = pic_symbol_name(state->pic, sym);

 enter:

  e = xh_get(scope->var_tbl, key);
  if (e) {
    if (depth > 0) {            /* mark dirty */
      xh_put(scope->var_tbl, key, 1);
    }
    return depth;
  }
  if (scope->up) {
    scope = scope->up;
    ++depth;
    goto enter;
  }
  return -1;
}

static void
define_var(analyze_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  analyze_scope *scope = state->scope;
  const char *name = pic_symbol_name(pic, sym);

  xh_put(state->scope->var_tbl, name, 0);

  scope->localc++;
  scope->vars = (pic_sym *)pic_realloc(pic, scope->vars, sizeof(pic_sym) * (scope->argc + scope->localc));
  scope->vars[scope->argc + scope->localc - 1] = sym;
}

static pic_value
new_ref(analyze_state *state, int depth, pic_sym sym)
{
  return pic_list(state->pic, 3,
                  pic_symbol_value(state->sREF),
                  pic_int_value(depth),
                  pic_symbol_value(sym));
}

static pic_value analyze_node(analyze_state *, pic_value, bool);
static pic_value analyze_call(analyze_state *, pic_value, bool);
static pic_value analyze_lambda(analyze_state *, pic_value);

static pic_value
analyze(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  int ai = pic_gc_arena_preserve(pic);
  pic_value res;
  pic_sym tag;

  res = analyze_node(state, obj, tailpos);

  tag = pic_sym(pic_car(pic, res));
  if (tailpos) {
    if (tag == pic->sIF || tag == pic->sBEGIN || tag == state->sTAILCALL || tag == state->sRETURN) {
      /* pass through */
    }
    else {
      res = pic_list(pic, 2, pic_symbol_value(state->sRETURN), res);
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, res);
  return res;
}

static pic_value
analyze_node(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    pic_sym sym = pic_sym(obj);
    int depth;

    depth = lookup_var(state, sym);
    if (depth == -1) {
      pic_error(pic, "symbol: unbound variable");
    }
    /* at this stage, lref/cref/gref are not distinguished */
    return new_ref(state, depth, sym);
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    if (! pic_list_p(pic, obj)) {
      pic_error(pic, "invalid expression given");
    }

    proc = pic_list_ref(pic, obj, 0);
    if (pic_sym_p(proc)) {
      pic_sym sym = pic_sym(proc);

      if (sym == pic->sDEFINE) {
	pic_value var, val;

	if (pic_length(pic, obj) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_list_ref(pic, obj, 1);
	if (pic_pair_p(var)) {
	  val = pic_cons(pic, pic_symbol_value(pic->sLAMBDA),
			 pic_cons(pic, pic_list_tail(pic, var, 1),
				  pic_list_tail(pic, obj, 2)));
	  var = pic_list_ref(pic, var, 0);
	}
	else {
	  if (pic_length(pic, obj) != 3) {
	    pic_error(pic, "syntax error");
	  }
	  val = pic_list_ref(pic, obj, 2);
	}
	if (! pic_sym_p(var)) {
	  pic_error(pic, "syntax error");
	}

        define_var(state, pic_sym(var));
        return pic_list(pic, 3,
                        pic_symbol_value(pic->sSETBANG),
                        analyze(state, var, false),
                        analyze(state, val, false));
      }
      else if (sym == pic->sLAMBDA) {
        return analyze_lambda(state, obj);
      }
      else if (sym == pic->sIF) {
	pic_value if_true, if_false;

	if_false = pic_none_value();
	switch (pic_length(pic, obj)) {
	default:
	  pic_error(pic, "syntax error");
	  break;
	case 4:
	  if_false = pic_list_ref(pic, obj, 3);
	  FALLTHROUGH;
	case 3:
	  if_true = pic_list_ref(pic, obj, 2);
	}

        return pic_list(pic, 4,
                        pic_symbol_value(pic->sIF),
                        analyze(state, pic_list_ref(pic, obj, 1), false),
                        analyze(state, if_true, tailpos),
                        analyze(state, if_false, tailpos));
      }
      else if (sym == pic->sBEGIN) {
	pic_value seq;
        bool tail;

        switch (pic_length(pic, obj)) {
        case 1:
          return analyze(state, pic_none_value(), tailpos);
        case 2:
          return analyze(state, pic_list_ref(pic, obj, 1), tailpos);
        default:
          seq = pic_list(pic, 1, pic_symbol_value(pic->sBEGIN));
          for (obj = pic_cdr(pic, obj); ! pic_nil_p(obj); obj = pic_cdr(pic, obj)) {
            if (pic_nil_p(pic_cdr(pic, obj))) {
              tail = tailpos;
            } else {
              tail = false;
            }
            seq = pic_cons(pic, analyze(state, pic_car(pic, obj), tail), seq);
          }
          return pic_reverse(pic, seq);
        }
      }
      else if (sym == pic->sSETBANG) {
	pic_value var, val;

	if (pic_length(pic, obj) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = pic_list_ref(pic, obj, 1);
	if (! pic_sym_p(var)) {
	  pic_error(pic, "syntax error");
	}

        val = pic_list_ref(pic, obj, 2);

        return pic_list(pic, 3,
                        pic_symbol_value(pic->sSETBANG),
                        analyze(state, var, false),
                        analyze(state, val, false));
      }
      else if (sym == pic->sQUOTE) {
	if (pic_length(pic, obj) != 2) {
	  pic_error(pic, "syntax error");
	}
        return obj;
      }

#define ARGC_ASSERT(n) do {				\
	if (pic_length(pic, obj) != (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

#define CONSTRUCT_OP1(op)                                               \
      pic_list(pic, 2,                                                  \
               pic_symbol_value(op),                                    \
               analyze(state, pic_list_ref(pic, obj, 1), false))

#define CONSTRUCT_OP2(op)                                               \
      pic_list(pic, 3,                                                  \
               pic_symbol_value(op),                                    \
               analyze(state, pic_list_ref(pic, obj, 1), false),        \
               analyze(state, pic_list_ref(pic, obj, 2), false))

      else if (sym == state->rCONS) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sCONS);
      }
      else if (sym == state->rCAR) {
	ARGC_ASSERT(1);
        return CONSTRUCT_OP1(pic->sCAR);
      }
      else if (sym == state->rCDR) {
	ARGC_ASSERT(1);
        return CONSTRUCT_OP1(pic->sCDR);
      }
      else if (sym == state->rNILP) {
	ARGC_ASSERT(1);
        return CONSTRUCT_OP1(pic->sNILP);
      }

#define ARGC_ASSERT_GE(n) do {				\
	if (pic_length(pic, obj) < (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

#define FOLD_ARGS(sym) do {                                             \
        obj = analyze(state, pic_car(pic, args), false);                \
        pic_for_each (arg, pic_cdr(pic, args)) {                        \
          obj = pic_list(pic, 3, pic_symbol_value(sym), obj,            \
                         analyze(state, arg, false));                   \
        }                                                               \
      } while (0)

      else if (sym == state->rADD) {
	pic_value args, arg;

	ARGC_ASSERT_GE(0);
	switch (pic_length(pic, obj)) {
	case 1:
          return pic_int_value(0);
	case 2:
	  return analyze(state, pic_car(pic, pic_cdr(pic, obj)), tailpos);
	default:
	  args = pic_cdr(pic, obj);
          FOLD_ARGS(pic->sADD);
          return obj;
	}
      }
      else if (sym == state->rSUB) {
	pic_value args, arg;

	ARGC_ASSERT_GE(1);
	switch (pic_length(pic, obj)) {
	case 2:
          return pic_list(pic, 2, pic_symbol_value(pic->sMINUS),
                          analyze(state, pic_car(pic, pic_cdr(pic, obj)), false));
	default:
	  args = pic_cdr(pic, obj);
          FOLD_ARGS(pic->sSUB);
          return obj;
	}
      }
      else if (sym == state->rMUL) {
	pic_value args, arg;

	ARGC_ASSERT_GE(0);
	switch (pic_length(pic, obj)) {
	case 1:
          return pic_int_value(1);
	case 2:
	  return analyze(state, pic_car(pic, pic_cdr(pic, obj)), tailpos);
	default:
	  args = pic_cdr(pic, obj);
          FOLD_ARGS(pic->sMUL);
          return obj;
	}
      }
      else if (sym == state->rDIV) {
	pic_value args, arg;

	ARGC_ASSERT_GE(1);
	switch (pic_length(pic, obj)) {
	case 2:
          args = pic_cdr(pic, obj);
          obj = pic_list(pic, 3, proc, pic_float_value(1), pic_car(pic, args));
          return analyze(state, obj, tailpos);
	default:
	  args = pic_cdr(pic, obj);
          FOLD_ARGS(pic->sDIV);
          return obj;
	}
	break;
      }
      else if (sym == state->rEQ) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sEQ);
      }
      else if (sym == state->rLT) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sLT);
      }
      else if (sym == state->rLE) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sLE);
      }
      else if (sym == state->rGT) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sGT);
      }
      else if (sym == state->rGE) {
	ARGC_ASSERT(2);
        return CONSTRUCT_OP2(pic->sGE);
      }
      else if (sym == state->rNOT) {
        ARGC_ASSERT(1);
        return CONSTRUCT_OP1(pic->sNOT);
      }
    }
    return analyze_call(state, obj, tailpos);
  }
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_NIL:
  case PIC_TT_CHAR:
  case PIC_TT_STRING:
  case PIC_TT_VECTOR:
  case PIC_TT_BLOB: {
    return pic_list(pic, 2, pic_symbol_value(pic->sQUOTE), obj);
  }
  case PIC_TT_CONT:
  case PIC_TT_ENV:
  case PIC_TT_PROC:
  case PIC_TT_UNDEF:
  case PIC_TT_EOF:
  case PIC_TT_PORT:
  case PIC_TT_ERROR:
  case PIC_TT_SENV:
  case PIC_TT_SYNTAX:
  case PIC_TT_SC:
  case PIC_TT_LIB:
  case PIC_TT_VAR:
  case PIC_TT_IREP:
    pic_error(pic, "invalid expression given");
  }
  pic_abort(pic, "logic flaw");
}

static pic_value
analyze_call(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  int ai = pic_gc_arena_preserve(pic);
  pic_value seq, elt;
  pic_sym call;

  if (! tailpos) {
    call = state->sCALL;
  } else {
    call = state->sTAILCALL;
  }
  seq = pic_list(pic, 1, pic_symbol_value(call));
  pic_for_each (elt, obj) {
    seq = pic_cons(pic, analyze(state, elt, false), seq);
  }
  seq = pic_reverse(pic, seq);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, seq);
  return seq;
}

static pic_value
analyze_lambda(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  int ai = pic_gc_arena_preserve(pic);
  pic_value args, body, locals, varg, closes;

  if (pic_length(pic, obj) < 2) {
    pic_error(pic, "syntax error");
  }

  /* formal arguments */
  args = pic_car(pic, pic_cdr(pic, obj));
  if (! valid_formal(pic, args)) {
    pic_error(pic, "syntax error");
  }

  push_scope(state, args);
  {
    analyze_scope *scope = state->scope;
    int i;

    /* analyze body in inner environment */
    body = pic_cdr(pic, pic_cdr(pic, obj));
    body = pic_cons(pic, pic_symbol_value(pic->sBEGIN), body);
    body = analyze(state, body, true);

    args = pic_nil_value();
    for (i = 1; i < scope->argc; ++i) {
      args = pic_cons(pic, pic_symbol_value(scope->vars[i]), args);
    }
    args = pic_reverse(pic, args);

    locals = pic_nil_value();
    for (i = 0; i < scope->localc; ++i) {
      locals = pic_cons(pic, pic_symbol_value(scope->vars[scope->argc + i]), locals);
    }
    locals = pic_reverse(pic, locals);

    varg = scope->varg ? pic_true_value() : pic_false_value();

    closes = pic_nil_value();
    for (i = 1; i < scope->argc + scope->localc; ++i) {
      pic_sym var = scope->vars[i];
      if (xh_get(scope->var_tbl, pic_symbol_name(pic, var))->val == 1) {
        closes = pic_cons(pic, pic_symbol_value(var), closes);
      }
    }
    closes = pic_reverse(pic, closes);
  }
  pop_scope(state);

  obj = pic_list(pic, 6, pic_symbol_value(pic->sLAMBDA), args, locals, varg, closes, body);
  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, obj);
  return obj;
}

pic_value
pic_analyze(pic_state *pic, pic_value obj)
{
  analyze_state *state;

  state = new_analyze_state(pic);

  obj = analyze(state, obj, true);

  destroy_analyze_state(state);
  return obj;
}

typedef struct resolver_scope {
  int depth;
  bool varg;
  int argc, localc;
  struct xhash *cvs, *lvs;
  unsigned cv_num;

  struct resolver_scope *up;
} resolver_scope;

typedef struct resolver_state {
  pic_state *pic;
  resolver_scope *scope;
  pic_sym sREF;
  pic_sym sGREF, sCREF, sLREF;
} resolver_state;

static void push_resolver_scope(resolver_state *, pic_value, pic_value, bool, pic_value);
static void pop_resolver_scope(resolver_state *);

static resolver_state *
new_resolver_state(pic_state *pic)
{
  resolver_state *state;

  state = (resolver_state *)pic_alloc(pic, sizeof(resolver_state));
  state->pic = pic;
  state->scope = NULL;

  register_symbol(pic, state, sREF, "ref");
  register_symbol(pic, state, sGREF, "gref");
  register_symbol(pic, state, sLREF, "lref");
  register_symbol(pic, state, sCREF, "cref");

  push_resolver_scope(state, pic_nil_value(), pic_nil_value(), false, pic_nil_value());

  return state;
}

static void
destroy_resolver_state(resolver_state *state)
{
  pop_resolver_scope(state);
  pic_free(state->pic, state);
}

static void
push_resolver_scope(resolver_state *state, pic_value args, pic_value locals, bool varg, pic_value closes)
{
  pic_state *pic = state->pic;
  resolver_scope *scope;
  int i, c;

  scope = (resolver_scope *)pic_alloc(pic, sizeof(resolver_scope));
  scope->up = state->scope;
  scope->depth = scope->up ? scope->up->depth + 1 : 0;
  scope->lvs = xh_new();
  scope->cvs = xh_new();
  scope->argc = pic_length(pic, args) + 1;
  scope->localc = pic_length(pic, locals);
  scope->varg = varg;

  /* arguments */
  for (i = 1; i < scope->argc; ++i) {
    xh_put(scope->lvs, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, args, i - 1))), i);
  }

  /* locals */
  for (i = 0; i < scope->localc; ++i) {
    xh_put(scope->lvs, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, locals, i))), scope->argc + i);
  }

  /* closed variables */
  scope->cv_num = 0;
  for (i = 0, c = pic_length(pic, closes); i < c; ++i) {
    xh_put(scope->cvs, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, closes, i))), scope->cv_num++);
  }

  state->scope = scope;
}

static void
pop_resolver_scope(resolver_state *state)
{
  resolver_scope *scope;

  scope = state->scope;
  xh_destroy(scope->cvs);
  xh_destroy(scope->lvs);

  scope = scope->up;
  pic_free(state->pic, state->scope);
  state->scope = scope;
}

static bool
is_closed(resolver_state *state, pic_sym sym)
{
  return xh_get(state->scope->cvs, pic_symbol_name(state->pic, sym)) != NULL;
}

static pic_value
resolve_gref(resolver_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  const char *name = pic_symbol_name(pic, sym);
  struct xh_entry *e;
  size_t i;

  if ((e = xh_get(pic->global_tbl, name))) {
    i = e->val;
  }
  else {
    i = pic->glen++;
    if (i >= pic->gcapa) {
      pic_error(pic, "global table overflow");
    }
    xh_put(pic->global_tbl, name, i);
  }
  return pic_list(pic, 2, pic_symbol_value(state->sGREF), pic_int_value(i));
}

static pic_value
resolve_lref(resolver_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  int i;

  i = xh_get(state->scope->lvs, pic_symbol_name(pic, sym))->val;

  return pic_list(pic, 2, pic_symbol_value(state->sLREF), pic_int_value(i));
}

static pic_value
resolve_cref(resolver_state *state, int depth, pic_sym sym)
{
  pic_state *pic = state->pic;
  resolver_scope *scope = state->scope;
  int i, d;

  d = depth;
  while (d-- > 0) {
    scope = scope->up;
  }

  i = xh_get(scope->cvs, pic_symbol_name(pic, sym))->val;

  return pic_list(pic, 3,
                  pic_symbol_value(state->sCREF),
                  pic_int_value(depth),
                  pic_int_value(i));
}

static pic_value resolve_reference_node(resolver_state *state, pic_value obj);

static pic_value
resolve_reference(resolver_state *state, pic_value obj)
{
  int ai = pic_gc_arena_preserve(state->pic);

  obj = resolve_reference_node(state, obj);

  pic_gc_arena_restore(state->pic, ai);
  pic_gc_protect(state->pic, obj);
  return obj;
}

static pic_value
resolve_reference_node(resolver_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  resolver_scope *scope = state->scope;
  pic_sym tag;

  if (! pic_pair_p(obj))
    return obj;

  tag = pic_sym(pic_car(pic, obj));
  if (tag == state->sREF) {
    int depth;
    pic_sym sym;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    sym = pic_sym(pic_list_ref(pic, obj, 2));
    if (depth == scope->depth) {
      return resolve_gref(state, sym);
    }
    else if (depth == 0 && ! is_closed(state, sym)) {
      return resolve_lref(state, sym);
    }
    else {
      return resolve_cref(state, depth, sym);
    }
  }
  else if (tag == pic->sLAMBDA) {
    pic_value args, locals, closes, body;
    bool varg;

    args = pic_list_ref(pic, obj, 1);
    locals = pic_list_ref(pic, obj, 2);
    varg = pic_true_p(pic_list_ref(pic, obj, 3));
    closes = pic_list_ref(pic, obj, 4);
    body = pic_list_ref(pic, obj, 5);

    push_resolver_scope(state, args, locals, varg, closes);
    {
      body = resolve_reference(state, body);
    }
    pop_resolver_scope(state);

    return pic_list(pic, 6, pic_symbol_value(pic->sLAMBDA), args, locals, pic_bool_value(varg), closes, body);
  }
  else if (tag == pic->sQUOTE) {
    return obj;
  }
  else {
    int ai = pic_gc_arena_preserve(pic);
    pic_value seq = pic_list(pic, 1, pic_symbol_value(tag)), elt;

    pic_for_each (elt, pic_cdr(pic, obj)) {
      seq = pic_cons(pic, resolve_reference(state, elt), seq);

      pic_gc_arena_restore(pic, ai);
      pic_gc_protect(pic, seq);
    }
    return pic_reverse(pic, seq);
  }
}

static pic_value
pic_resolve(pic_state *pic, pic_value obj)
{
  resolver_state *state;

  state = new_resolver_state(pic);

  obj = resolve_reference(state, obj);

  destroy_resolver_state(state);
  return obj;
}

/**
 * scope object
 */

typedef struct codegen_context {
  bool varg;
  /* rest args variable is counted by localc */
  int argc, localc;
  /* closed variable table */
  unsigned *cv_tbl, cv_num;
  /* actual bit code sequence */
  struct pic_code *code;
  size_t clen, ccapa;
  /* child ireps */
  struct pic_irep **irep;
  size_t ilen, icapa;
  /* constant object pool */
  pic_value *pool;
  size_t plen, pcapa;

  struct codegen_context *up;
} codegen_context;

/**
 * global codegen state
 */

typedef struct codegen_state {
  pic_state *pic;
  codegen_context *cxt;
  pic_sym sGREF, sCREF, sLREF;
  pic_sym sCALL, sTAILCALL, sRETURN;
  unsigned *cv_tbl, cv_num;
} codegen_state;

static void push_codegen_context(codegen_state *, pic_value, pic_value, bool, pic_value);
static struct pic_irep *pop_codegen_context(codegen_state *);

static codegen_state *
new_codegen_state(pic_state *pic)
{
  codegen_state *state;

  state = (codegen_state *)pic_alloc(pic, sizeof(codegen_state));
  state->pic = pic;
  state->cxt = NULL;

  register_symbol(pic, state, sCALL, "call");
  register_symbol(pic, state, sTAILCALL, "tail-call");
  register_symbol(pic, state, sGREF, "gref");
  register_symbol(pic, state, sLREF, "lref");
  register_symbol(pic, state, sCREF, "cref");
  register_symbol(pic, state, sRETURN, "return");

  push_codegen_context(state, pic_nil_value(), pic_nil_value(), false, pic_nil_value());

  return state;
}

static struct pic_irep *
destroy_codegen_state(codegen_state *state)
{
  pic_state *pic = state->pic;
  struct pic_irep *irep;

  irep = pop_codegen_context(state);
  pic_free(pic, state);

  return irep;
}

static void
push_codegen_context(codegen_state *state, pic_value args, pic_value locals, bool varg, pic_value closes)
{
  pic_state *pic = state->pic;
  codegen_context *cxt;
  int i, c;
  struct xhash *vars;

  cxt = (codegen_context *)pic_alloc(pic, sizeof(codegen_context));
  cxt->up = state->cxt;
  cxt->argc = pic_length(pic, args) + 1;
  cxt->localc = pic_length(pic, locals);
  cxt->varg = varg;

  /* number local variables */
  vars = xh_new();
  for (i = 1; i < cxt->argc; ++i) {
    xh_put(vars, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, args, i - 1))), i);
  }
  for (i = 0; i < cxt->localc; ++i) {
    xh_put(vars, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, locals, i))), cxt->argc + i);
  }

  /* closed variables */
  cxt->cv_tbl = NULL;
  cxt->cv_num = 0;
  for (i = 0, c = pic_length(pic, closes); i < c; ++i) {
    i = cxt->cv_num++;
    cxt->cv_tbl = (unsigned *)pic_realloc(pic, cxt->cv_tbl, sizeof(unsigned) * cxt->cv_num);
    cxt->cv_tbl[i] = xh_get(vars, pic_symbol_name(pic, pic_sym(pic_list_ref(pic, closes, i))))->val;
  }

  xh_destroy(vars);

  cxt->code = (struct pic_code *)pic_calloc(pic, PIC_ISEQ_SIZE, sizeof(struct pic_code));
  cxt->clen = 0;
  cxt->ccapa = PIC_ISEQ_SIZE;

  cxt->irep = (struct pic_irep **)pic_calloc(pic, PIC_IREP_SIZE, sizeof(struct pic_irep *));
  cxt->ilen = 0;
  cxt->icapa = PIC_IREP_SIZE;

  cxt->pool = (pic_value *)pic_calloc(pic, PIC_POOL_SIZE, sizeof(pic_value));
  cxt->plen = 0;
  cxt->pcapa = PIC_POOL_SIZE;

  state->cxt = cxt;
}

static struct pic_irep *
pop_codegen_context(codegen_state *state)
{
  pic_state *pic = state->pic;
  codegen_context *cxt = state->cxt;
  struct pic_irep *irep;

  /* create irep */
  irep = (struct pic_irep *)pic_obj_alloc(pic, sizeof(struct pic_irep), PIC_TT_IREP);
  irep->varg = state->cxt->varg;
  irep->argc = state->cxt->argc;
  irep->localc = state->cxt->localc;
  irep->cv_tbl = state->cxt->cv_tbl;
  irep->cv_num = state->cxt->cv_num;
  irep->code = pic_realloc(pic, state->cxt->code, sizeof(struct pic_code) * state->cxt->clen);
  irep->clen = state->cxt->clen;
  irep->irep = pic_realloc(pic, state->cxt->irep, sizeof(struct pic_irep *) * state->cxt->ilen);
  irep->ilen = state->cxt->ilen;
  irep->pool = pic_realloc(pic, state->cxt->pool, sizeof(pic_value) * state->cxt->plen);
  irep->plen = state->cxt->plen;

  /* destroy context */
  cxt = cxt->up;
  pic_free(pic, state->cxt);
  state->cxt = cxt;

  return irep;
}

static struct pic_irep *codegen_lambda(codegen_state *, pic_value);

static void
codegen(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  codegen_context *cxt = state->cxt;
  pic_sym sym;

  sym = pic_sym(pic_car(pic, obj));
  if (sym == state->sGREF) {
    cxt->code[cxt->clen].insn = OP_GREF;
    cxt->code[cxt->clen].u.i = pic_int(pic_list_ref(pic, obj, 1));
    cxt->clen++;
    return;
  } else if (sym == state->sCREF) {
    cxt->code[cxt->clen].insn = OP_CREF;
    cxt->code[cxt->clen].u.r.depth = pic_int(pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].u.r.idx = pic_int(pic_list_ref(pic, obj, 2));
    cxt->clen++;
    return;
  } else if (sym == state->sLREF) {
    cxt->code[cxt->clen].insn = OP_LREF;
    cxt->code[cxt->clen].u.i = pic_int(pic_list_ref(pic, obj, 1));
    cxt->clen++;
    return;
  } else if (sym == pic->sSETBANG) {
    pic_value var, val;
    pic_sym type;

    val = pic_list_ref(pic, obj, 2);
    codegen(state, val);

    var = pic_list_ref(pic, obj, 1);
    type = pic_sym(pic_list_ref(pic, var, 0));
    if (type == state->sGREF) {
      cxt->code[cxt->clen].insn = OP_GSET;
      cxt->code[cxt->clen].u.i = pic_int(pic_list_ref(pic, var, 1));
      cxt->clen++;
      cxt->code[cxt->clen].insn = OP_PUSHNONE;
      cxt->clen++;
      return;
    }
    else if (type == state->sCREF) {
      cxt->code[cxt->clen].insn = OP_CSET;
      cxt->code[cxt->clen].u.r.depth = pic_int(pic_list_ref(pic, var, 1));
      cxt->code[cxt->clen].u.r.idx = pic_int(pic_list_ref(pic, var, 2));
      cxt->clen++;
      cxt->code[cxt->clen].insn = OP_PUSHNONE;
      cxt->clen++;
      return;
    }
    else if (type == state->sLREF) {
      cxt->code[cxt->clen].insn = OP_LSET;
      cxt->code[cxt->clen].u.i = pic_int(pic_list_ref(pic, var, 1));
      cxt->clen++;
      cxt->code[cxt->clen].insn = OP_PUSHNONE;
      cxt->clen++;
      return;
    }
  }
  else if (sym == pic->sLAMBDA) {
    int k;

    if (cxt->ilen >= cxt->icapa) {
      cxt->icapa *= 2;
      cxt->irep = (struct pic_irep **)pic_realloc(pic, cxt->irep, sizeof(struct pic_irep *) * cxt->icapa);
    }
    k = cxt->ilen++;
    cxt->code[cxt->clen].insn = OP_LAMBDA;
    cxt->code[cxt->clen].u.i = k;
    cxt->clen++;

    cxt->irep[k] = codegen_lambda(state, obj);
    return;
  }
  else if (sym == pic->sIF) {
    int s, t;

    codegen(state, pic_list_ref(pic, obj, 1));

    cxt->code[cxt->clen].insn = OP_JMPIF;
    s = cxt->clen++;

    /* if false branch */
    codegen(state, pic_list_ref(pic, obj, 3));
    cxt->code[cxt->clen].insn = OP_JMP;
    t = cxt->clen++;

    cxt->code[s].u.i = cxt->clen - s;

    /* if true branch */
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[t].u.i = cxt->clen - t;
    return;
  }
  else if (sym == pic->sBEGIN) {
    pic_value elt;
    pic_for_each (elt, pic_cdr(pic, obj)) {
      codegen(state, elt);
    }
    return;
  }
  else if (sym == pic->sQUOTE) {
    int pidx;

    obj = pic_list_ref(pic, obj, 1);
    switch (pic_type(obj)) {
    case PIC_TT_BOOL:
      if (pic_true_p(obj)) {
        cxt->code[cxt->clen].insn = OP_PUSHTRUE;
      } else {
        cxt->code[cxt->clen].insn = OP_PUSHFALSE;
      }
      cxt->clen++;
      return;
    case PIC_TT_INT:
      cxt->code[cxt->clen].insn = OP_PUSHINT;
      cxt->code[cxt->clen].u.i = pic_int(obj);
      cxt->clen++;
      return;
    case PIC_TT_NIL:
      cxt->code[cxt->clen].insn = OP_PUSHNIL;
      cxt->clen++;
      return;
    case PIC_TT_CHAR:
      cxt->code[cxt->clen].insn = OP_PUSHCHAR;
      cxt->code[cxt->clen].u.c = pic_char(obj);
      cxt->clen++;
      return;
    default:
      if (cxt->plen >= cxt->pcapa) {
        cxt->pcapa *= 2;
        cxt->pool = (pic_value *)pic_realloc(pic, cxt->pool, sizeof(pic_value) * cxt->pcapa);
      }
      pidx = cxt->plen++;
      cxt->pool[pidx] = obj;
      cxt->code[cxt->clen].insn = OP_PUSHCONST;
      cxt->code[cxt->clen].u.i = pidx;
      cxt->clen++;
      return;
    }
  }
  else if (sym == pic->sCONS) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_CONS;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sCAR) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_CAR;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sCDR) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_CDR;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sNILP) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_NILP;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sADD) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_ADD;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sSUB) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_SUB;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sMUL) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_MUL;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sDIV) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_DIV;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sMINUS) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_MINUS;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sEQ) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_EQ;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sLT) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_LT;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sLE) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_LE;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sGT) {
    codegen(state, pic_list_ref(pic, obj, 2));
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_LT;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sGE) {
    codegen(state, pic_list_ref(pic, obj, 2));
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_LE;
    cxt->clen++;
    return;
  }
  else if (sym == pic->sNOT) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_NOT;
    cxt->clen++;
    return;
  }
  else if (sym == state->sCALL || sym == state->sTAILCALL) {
    int len = pic_length(pic, obj);
    pic_value elt;

    pic_for_each (elt, pic_cdr(pic, obj)) {
      codegen(state, elt);
    }
    cxt->code[cxt->clen].insn = (sym == state->sCALL) ? OP_CALL : OP_TAILCALL;
    cxt->code[cxt->clen].u.i = len - 1;
    cxt->clen++;
    return;
  }
  else if (sym == state->sRETURN) {
    codegen(state, pic_list_ref(pic, obj, 1));
    cxt->code[cxt->clen].insn = OP_RET;
    cxt->clen++;
    return;
  }
  pic_error(pic, "codegen: unknown AST type");
}

static struct pic_irep *
codegen_lambda(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value args, locals, closes, body;
  bool varg;

  args = pic_list_ref(pic, obj, 1);
  locals = pic_list_ref(pic, obj, 2);
  varg = pic_true_p(pic_list_ref(pic, obj, 3));
  closes = pic_list_ref(pic, obj, 4);
  body = pic_list_ref(pic, obj, 5);

  /* inner environment */
  push_codegen_context(state, args, locals, varg, closes);
  {
    /* body */
    codegen(state, body);
  }
  return pop_codegen_context(state);
}

struct pic_irep *
pic_codegen(pic_state *pic, pic_value obj)
{
  codegen_state *state;

  state = new_codegen_state(pic);

  codegen(state, obj);

  return destroy_codegen_state(state);
}

struct pic_irep *
compile(pic_state *pic, pic_value obj)
{
  struct pic_irep *irep;
  int ai = pic_gc_arena_preserve(pic);

#if DEBUG
  fprintf(stderr, "ai = %d\n", pic_gc_arena_preserve(pic));

  fprintf(stderr, "# input expression\n");
  pic_debug(pic, obj);
  fprintf(stderr, "\n");

  fprintf(stderr, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* macroexpand */
  obj = pic_macroexpand(pic, obj);
#if DEBUG
  fprintf(stderr, "## macroexpand completed\n");
  pic_debug(pic, obj);
  fprintf(stderr, "\n");
  fprintf(stderr, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* analyze */
  obj = pic_analyze(pic, obj);
#if DEBUG
  fprintf(stderr, "## analyzer completed\n");
  pic_debug(pic, obj);
  fprintf(stderr, "\n");
  fprintf(stderr, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* resolution */
  obj = pic_resolve(pic, obj);
#if DEBUG
  fprintf(stderr, "## resolver completed\n");
  pic_debug(pic, obj);
  fprintf(stderr, "\n");
  fprintf(stderr, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* codegen */
  irep = pic_codegen(pic, obj);
#if DEBUG
  fprintf(stderr, "## codegen completed\n");
  pic_dump_irep(pic, irep);
#endif

#if DEBUG
  fprintf(stderr, "# compilation finished\n");
  puts("");
#endif

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(irep));

#if VM_DEBUG
  pic_dump_irep(pic, proc->u.irep);
#endif
  return irep;
}

struct pic_proc *
pic_compile(pic_state *pic, pic_value obj)
{
  struct pic_proc *proc;
  jmp_buf jmp, *prev_jmp = pic->jmp;

  if (setjmp(jmp) == 0) {
    pic->jmp = &jmp;
  }
  else {
    /* error occured */
    proc = NULL;
    goto exit;
  }

  proc = pic_proc_new_irep(pic, compile(pic, obj), NULL);

 exit:
  pic->jmp = prev_jmp;

  return proc;
}

static int
scope_global_define(pic_state *pic, const char *name)
{
  struct xh_entry *e;

  if ((e = xh_get(pic->global_tbl, name))) {
    pic_warn(pic, "redefining global");
    return e->val;
  }
  e = xh_put(pic->global_tbl, name, pic->glen++);
  if (pic->glen >= pic->gcapa) {
    pic_error(pic, "global table overflow");
  }
  return e->val;
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  int idx;
  pic_sym gsym;

  gsym = pic_gensym(pic, pic_intern_cstr(pic, name));

  /* push to the global arena */
  idx = scope_global_define(pic, pic_symbol_name(pic, gsym));
  pic->globals[idx] = val;

  /* register to the senv */
  xh_put(pic->lib->senv->tbl, name, gsym);

  /* export! */
  pic_export(pic, pic_intern_cstr(pic, name));
}

static int
global_ref(pic_state *pic, const char *name)
{
  struct xh_entry *e;

  if (! (e = xh_get(pic->lib->senv->tbl, name))) {
    pic_error(pic, "symbol not defined");
  }
  assert(e->val >= 0);
  if (! (e = xh_get(pic->global_tbl, pic_symbol_name(pic, (pic_sym)e->val)))) {
    pic_abort(pic, "logic flaw");
  }
  return e->val;
}

pic_value
pic_ref(pic_state *pic, const char *name)
{
  int gid;

  gid = global_ref(pic, name);
  return pic->globals[gid];
}

void
pic_set(pic_state *pic, const char *name, pic_value value)
{
  int gid;

  gid = global_ref(pic, name);
  pic->globals[gid] = value;
}

void
print_code(pic_state *pic, struct pic_code c)
{
  UNUSED(pic);

  printf("[%2d] ", c.insn);
  switch (c.insn) {
  case OP_NOP:
    puts("OP_NOP");
    break;
  case OP_POP:
    puts("OP_POP");
    break;
  case OP_PUSHNIL:
    puts("OP_PUSHNIL");
    break;
  case OP_PUSHTRUE:
    puts("OP_PUSHTRUE");
    break;
  case OP_PUSHFALSE:
    puts("OP_PUSHFALSE");
    break;
  case OP_PUSHINT:
    printf("OP_PUSHINT\t%d\n", c.u.i);
    break;
  case OP_PUSHCHAR:
    printf("OP_PUSHCHAR\t%c\n", c.u.c);
    break;
  case OP_PUSHCONST:
    printf("OP_PUSHCONST\t%d\n", c.u.i);
    break;
  case OP_GREF:
    printf("OP_GREF\t%i\n", c.u.i);
    break;
  case OP_GSET:
    printf("OP_GSET\t%i\n", c.u.i);
    break;
  case OP_LREF:
    printf("OP_LREF\t%d\n", c.u.i);
    break;
  case OP_LSET:
    printf("OP_LSET\t%d\n", c.u.i);
    break;
  case OP_CREF:
    printf("OP_CREF\t%d\t%d\n", c.u.r.depth, c.u.r.idx);
    break;
  case OP_CSET:
    printf("OP_CSET\t%d\t%d\n", c.u.r.depth, c.u.r.idx);
    break;
  case OP_JMP:
    printf("OP_JMP\t%x\n", c.u.i);
    break;
  case OP_JMPIF:
    printf("OP_JMPIF\t%x\n", c.u.i);
    break;
  case OP_NOT:
    puts("OP_NOT");
    break;
  case OP_CALL:
    printf("OP_CALL\t%d\n", c.u.i);
    break;
  case OP_TAILCALL:
    printf("OP_TAILCALL\t%d\n", c.u.i);
    break;
  case OP_RET:
    puts("OP_RET");
    break;
  case OP_LAMBDA:
    printf("OP_LAMBDA\t%d\n", c.u.i);
    break;
  case OP_CONS:
    puts("OP_CONS");
    break;
  case OP_CAR:
    puts("OP_CAR");
    break;
  case OP_NILP:
    puts("OP_NILP");
    break;
  case OP_CDR:
    puts("OP_CDR");
    break;
  case OP_ADD:
    puts("OP_ADD");
    break;
  case OP_SUB:
    puts("OP_SUB");
    break;
  case OP_MUL:
    puts("OP_MUL");
    break;
  case OP_DIV:
    puts("OP_DIV");
    break;
  case OP_MINUS:
    puts("OP_MINUS");
    break;
  case OP_EQ:
    puts("OP_EQ");
    break;
  case OP_LT:
    puts("OP_LT");
    break;
  case OP_LE:
    puts("OP_LE");
    break;
  case OP_STOP:
    puts("OP_STOP");
    break;
  }
}

void
pic_dump_irep(pic_state *pic, struct pic_irep *irep)
{
  unsigned i;

  printf("## irep %p\n", (void *)irep);
  printf("[clen = %zd, argc = %d, localc = %d]\n", irep->clen, irep->argc, irep->localc);
  printf(":: cv_num = %d\n", irep->cv_num);
  for (i = 0; i < irep->cv_num; ++i) {
    printf(": %d -> %d\n", irep->cv_tbl[i], i);
  }
  for (i = 0; i < irep->clen; ++i) {
    printf("%02x ", i);
    print_code(pic, irep->code[i]);
  }

  for (i = 0; i < irep->ilen; ++i) {
    pic_dump_irep(pic, irep->irep[i]);
  }
}
