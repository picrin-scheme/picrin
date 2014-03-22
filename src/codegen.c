/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/lib.h"
#include "picrin/macro.h"

#if PIC_NONE_IS_FALSE
# define OP_PUSHNONE OP_PUSHFALSE
#else
# error enable PIC_NONE_IS_FALSE
#endif

typedef struct analyze_scope {
  int depth;
  bool varg;
  xvect args, locals, captures; /* rest args variable is counted as a local */
  struct analyze_scope *up;
} analyze_scope;

typedef struct analyze_state {
  pic_state *pic;
  analyze_scope *scope;
  pic_sym rCONS, rCAR, rCDR, rNILP;
  pic_sym rADD, rSUB, rMUL, rDIV;
  pic_sym rEQ, rLT, rLE, rGT, rGE, rNOT;
  pic_sym rVALUES, rCALL_WITH_VALUES;
  pic_sym sCALL, sTAILCALL, sCALL_WITH_VALUES, sTAILCALL_WITH_VALUES;
  pic_sym sREF, sGREF, sRETURN;
} analyze_state;

static bool push_scope(analyze_state *, pic_value);
static void pop_scope(analyze_state *);

#define register_symbol(pic, state, slot, name) do {    \
    state->slot = pic_intern_cstr(pic, name);           \
  } while (0)

#define register_renamed_symbol(pic, state, slot, lib, id) do {         \
    xh_entry *e;                                                        \
    if (! (e = xh_get_int(lib->senv->name, pic_intern_cstr(pic, id))))  \
      pic_error(pic, "internal error! native VM procedure not found");  \
    state->slot = e->val;                                               \
  } while (0)

static analyze_state *
new_analyze_state(pic_state *pic)
{
  analyze_state *state;
  xhash *global_tbl;
  xh_iter it;
  struct pic_lib *stdlib;

  state = (analyze_state *)pic_alloc(pic, sizeof(analyze_state));
  state->pic = pic;
  state->scope = NULL;

  stdlib = pic_find_library(pic, pic_read(pic, "(scheme base)"));

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
  register_renamed_symbol(pic, state, rVALUES, stdlib, "values");
  register_renamed_symbol(pic, state, rCALL_WITH_VALUES, stdlib, "call-with-values");

  register_symbol(pic, state, sCALL, "call");
  register_symbol(pic, state, sTAILCALL, "tail-call");
  register_symbol(pic, state, sCALL_WITH_VALUES, "call-with-values");
  register_symbol(pic, state, sTAILCALL_WITH_VALUES, "tailcall-with-values");
  register_symbol(pic, state, sREF, "ref");
  register_symbol(pic, state, sGREF, "gref");
  register_symbol(pic, state, sRETURN, "return");

  /* push initial scope */
  push_scope(state, pic_nil_value());

  global_tbl = pic->global_tbl;
  for (xh_begin(global_tbl, &it); ! xh_isend(&it); xh_next(&it)) {
    xv_push(&state->scope->locals, &it.e->key);
  }

  return state;
}

static void
destroy_analyze_state(analyze_state *state)
{
  pop_scope(state);
  pic_free(state->pic, state);
}

static bool
analyze_args(pic_state *pic, pic_value formals, bool *varg, xvect *args, xvect *locals)
{
  pic_value v, sym;

  for (v = formals; pic_pair_p(v); v = pic_cdr(pic, v)) {
    sym = pic_car(pic, v);
    if (! pic_sym_p(sym)) {
      return false;
    }
    xv_push(args, &pic_sym(sym));
  }
  if (pic_nil_p(v)) {
    *varg = false;
  }
  else if (pic_sym_p(v)) {
    *varg = true;
    xv_push(locals, &pic_sym(v));
  }
  else {
    return false;
  }

  return true;
}

static bool
push_scope(analyze_state *state, pic_value formals)
{
  pic_state *pic = state->pic;
  analyze_scope *scope;
  bool varg;
  xvect args, locals, captures;

  xv_init(&args, sizeof(pic_sym));
  xv_init(&locals, sizeof(pic_sym));
  xv_init(&captures, sizeof(pic_sym));

  if (analyze_args(pic, formals, &varg, &args, &locals)) {
    scope = (analyze_scope *)pic_alloc(pic, sizeof(analyze_scope));
    scope->up = state->scope;
    scope->depth = scope->up ? scope->up->depth + 1 : 0;
    scope->varg = varg;
    scope->args = args;
    scope->locals = locals;
    scope->captures = captures;

    state->scope = scope;

    return true;
  }
  else {
    xv_destroy(&args);
    xv_destroy(&locals);
    return false;
  }
}

static void
pop_scope(analyze_state *state)
{
  analyze_scope *scope;

  scope = state->scope;
  xv_destroy(&scope->args);
  xv_destroy(&scope->locals);
  xv_destroy(&scope->captures);

  scope = scope->up;
  pic_free(state->pic, state->scope);
  state->scope = scope;
}

static bool
lookup_scope(analyze_scope *scope, pic_sym sym)
{
  pic_sym *arg, *local;
  size_t i;

  /* args */
  for (i = 0; i < scope->args.size; ++i) {
    arg = xv_get(&scope->args, i);
    if (*arg == sym)
      return true;
  }
  /* locals */
  for (i = 0; i < scope->locals.size; ++i) {
    local = xv_get(&scope->locals, i);
    if (*local == sym)
      return true;
  }
  return false;
}

static void
capture_var(analyze_scope *scope, pic_sym sym)
{
  pic_sym *var;
  size_t i;

  for (i = 0; i < scope->captures.size; ++i) {
    var = xv_get(&scope->captures, i);
    if (*var == sym) {
      break;
    }
  }
  if (i == scope->captures.size) {
    xv_push(&scope->captures, &sym);
  }
}

static int
find_var(analyze_state *state, pic_sym sym)
{
  analyze_scope *scope = state->scope;
  int depth = 0;

  while (scope) {
    if (lookup_scope(scope, sym)) {
      if (depth > 0) {
        capture_var(scope, sym);
      }
      return depth;
    }
    depth++;
    scope = scope->up;
  }
  return -1;
}

static void
define_var(analyze_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  analyze_scope *scope = state->scope;

  if (lookup_scope(scope, sym)) {
    pic_warn(pic, "redefining variable");
    return;
  }

  xv_push(&scope->locals, &sym);
}

static pic_value
new_ref(analyze_state *state, int depth, pic_sym sym)
{
  return pic_list3(state->pic,
                   pic_symbol_value(state->sREF),
                   pic_int_value(depth),
                   pic_symbol_value(sym));
}

static pic_value analyze_node(analyze_state *, pic_value, bool);

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
    if (tag == pic->sIF || tag == pic->sBEGIN || tag == state->sTAILCALL || tag == state->sTAILCALL_WITH_VALUES || tag == state->sRETURN) {
      /* pass through */
    }
    else {
      res = pic_list2(pic, pic_symbol_value(state->sRETURN), res);
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, res);
  return res;
}

static pic_value
analyze_global_var(analyze_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  xh_entry *e;
  size_t i;

  if ((e = xh_get_int(pic->global_tbl, sym))) {
    i = e->val;
  }
  else {
    i = pic->glen++;
    if (i >= pic->gcapa) {
      pic_error(pic, "global table overflow");
    }
    xh_put_int(pic->global_tbl, sym, i);
  }
  return pic_list2(pic, pic_symbol_value(state->sGREF), pic_int_value(i));
}

static pic_value
analyze_var(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_sym sym;
  int depth;

  sym = pic_sym(obj);
  if ((depth = find_var(state, sym)) == -1) {
    pic_errorf(pic, "unbound variable %s", pic_symbol_name(pic, sym));
  }

  if (depth == state->scope->depth) {
    return analyze_global_var(state, sym);
  }
  return new_ref(state, depth, sym);
}

static pic_value
analyze_define(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
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

  var = analyze(state, var, false);
  val = analyze(state, val, false);

  return pic_list3(pic, pic_symbol_value(pic->sSETBANG), var, val);
}

static pic_value
analyze_if(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value cond, if_true, if_false;

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

  /* analyze in order */
  cond = analyze(state, pic_list_ref(pic, obj, 1), false);
  if_true = analyze(state, if_true, tailpos);
  if_false = analyze(state, if_false, tailpos);

  return pic_list4(pic, pic_symbol_value(pic->sIF), cond, if_true, if_false);
}

static pic_value
analyze_begin(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value seq;
  bool tail;

  switch (pic_length(pic, obj)) {
  case 1:
    return analyze(state, pic_none_value(), tailpos);
  case 2:
    return analyze(state, pic_list_ref(pic, obj, 1), tailpos);
  default:
    seq = pic_list1(pic, pic_symbol_value(pic->sBEGIN));
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

static pic_value
analyze_set(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value var, val;

  if (pic_length(pic, obj) != 3) {
    pic_error(pic, "syntax error");
  }

  var = pic_list_ref(pic, obj, 1);
  if (! pic_sym_p(var)) {
    pic_error(pic, "syntax error");
  }

  val = pic_list_ref(pic, obj, 2);

  var = analyze(state, var, false);
  val = analyze(state, val, false);

  return pic_list3(pic, pic_symbol_value(pic->sSETBANG), var, val);
}

static pic_value
analyze_quote(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;

  if (pic_length(pic, obj) != 2) {
    pic_error(pic, "syntax error");
  }
  return obj;
}

static pic_value
analyze_lambda(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value formals, args, locals, varg, captures, body;

  if (pic_length(pic, obj) < 2) {
    pic_error(pic, "syntax error");
  }

  formals = pic_car(pic, pic_cdr(pic, obj));

  if (push_scope(state, formals)) {
    analyze_scope *scope = state->scope;
    pic_sym *var;
    size_t i;

    args = pic_nil_value();
    for (i = scope->args.size; i > 0; --i) {
      var = xv_get(&scope->args, i - 1);
      pic_push(pic, pic_sym_value(*var), args);
    }

    varg = scope->varg
      ? pic_true_value()
      : pic_false_value();

    /* To know what kind of local variables are defined, analyze body at first. */
    body = analyze(state, pic_cons(pic, pic_sym_value(pic->sBEGIN), pic_list_tail(pic, obj, 2)), true);

    locals = pic_nil_value();
    for (i = scope->locals.size; i > 0; --i) {
      var = xv_get(&scope->locals, i - 1);
      pic_push(pic, pic_sym_value(*var), locals);
    }

    captures = pic_nil_value();
    for (i = scope->captures.size; i > 0; --i) {
      var = xv_get(&scope->captures, i - 1);
      pic_push(pic, pic_sym_value(*var), captures);
    }

    pop_scope(state);
  }
  else {
    pic_errorf(pic, "invalid formal syntax: ~s", args);
  }

  return pic_list6(pic, pic_sym_value(pic->sLAMBDA), args, locals, varg, captures, body);
}

#define ARGC_ASSERT_GE(n) do {				\
	if (pic_length(pic, obj) < (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

#define FOLD_ARGS(sym) do {                                             \
        obj = analyze(state, pic_car(pic, args), false);                \
        pic_for_each (arg, pic_cdr(pic, args)) {                        \
          obj = pic_list3(pic, pic_symbol_value(sym), obj,              \
                          analyze(state, arg, false));                  \
        }                                                               \
      } while (0)

static pic_value
analyze_add(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
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

static pic_value
analyze_sub(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value args, arg;

  ARGC_ASSERT_GE(1);
  switch (pic_length(pic, obj)) {
  case 2:
    return pic_list2(pic, pic_symbol_value(pic->sMINUS),
                     analyze(state, pic_car(pic, pic_cdr(pic, obj)), false));
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sSUB);
    return obj;
  }
}

static pic_value
analyze_mul(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
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

static pic_value
analyze_div(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value args, arg;

  ARGC_ASSERT_GE(1);
  switch (pic_length(pic, obj)) {
  case 2:
    args = pic_cdr(pic, obj);
    obj = pic_list3(pic, pic_car(pic, obj), pic_float_value(1), pic_car(pic, args));
    return analyze(state, obj, false);
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sDIV);
    return obj;
  }
}

static pic_value
analyze_call(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value seq, elt;
  pic_sym call;

  if (! tailpos) {
    call = state->sCALL;
  } else {
    call = state->sTAILCALL;
  }
  seq = pic_list1(pic, pic_symbol_value(call));
  pic_for_each (elt, obj) {
    seq = pic_cons(pic, analyze(state, elt, false), seq);
  }
  return pic_reverse(pic, seq);
}

static pic_value
analyze_values(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value v, seq;

  if (! tailpos) {
    return analyze_call(state, obj, false);
  }

  seq = pic_list1(pic, pic_symbol_value(state->sRETURN));
  pic_for_each (v, pic_cdr(pic, obj)) {
    seq = pic_cons(pic, analyze(state, v, false), seq);
  }
  return pic_reverse(pic, seq);
}

static pic_value
analyze_call_with_values(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value prod, cnsm;
  pic_sym call;

  if (pic_length(pic, obj) != 3) {
    pic_error(pic, "wrong number of arguments");
  }

  if (! tailpos) {
    call = state->sCALL_WITH_VALUES;
  } else {
    call = state->sTAILCALL_WITH_VALUES;
  }
  prod = analyze(state, pic_list_ref(pic, obj, 1), false);
  cnsm = analyze(state, pic_list_ref(pic, obj, 2), false);
  return pic_list3(pic, pic_symbol_value(call), prod, cnsm);
}

#define ARGC_ASSERT(n) do {				\
	if (pic_length(pic, obj) != (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

#define CONSTRUCT_OP1(op)                                               \
      pic_list2(pic,                                                    \
                pic_symbol_value(op),                                   \
                analyze(state, pic_list_ref(pic, obj, 1), false))

#define CONSTRUCT_OP2(op)                                               \
      pic_list3(pic,                                                    \
                pic_symbol_value(op),                                   \
                analyze(state, pic_list_ref(pic, obj, 1), false),       \
                analyze(state, pic_list_ref(pic, obj, 2), false))

static pic_value
analyze_node(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    return analyze_var(state, obj);
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    if (! pic_list_p(obj)) {
      pic_errorf(pic, "invalid expression given: ~s", obj);
    }

    proc = pic_list_ref(pic, obj, 0);
    if (pic_sym_p(proc)) {
      pic_sym sym = pic_sym(proc);

      if (sym == pic->sDEFINE) {
        return analyze_define(state, obj);
      }
      else if (sym == pic->sLAMBDA) {
        return analyze_lambda(state, obj);
      }
      else if (sym == pic->sIF) {
        return analyze_if(state, obj, tailpos);
      }
      else if (sym == pic->sBEGIN) {
        return analyze_begin(state, obj, tailpos);
      }
      else if (sym == pic->sSETBANG) {
        return analyze_set(state, obj);
      }
      else if (sym == pic->sQUOTE) {
        return analyze_quote(state, obj);
      }
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
      else if (sym == state->rADD) {
        return analyze_add(state, obj, tailpos);
      }
      else if (sym == state->rSUB) {
        return analyze_sub(state, obj);
      }
      else if (sym == state->rMUL) {
        return analyze_mul(state, obj, tailpos);
      }
      else if (sym == state->rDIV) {
        return analyze_div(state, obj);
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
      else if (sym == state->rVALUES) {
        return analyze_values(state, obj, tailpos);
      }
      else if (sym == state->rCALL_WITH_VALUES) {
        return analyze_call_with_values(state, obj, tailpos);
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
    return pic_list2(pic, pic_symbol_value(pic->sQUOTE), obj);
  }
  case PIC_TT_CONT:
  case PIC_TT_ENV:
  case PIC_TT_PROC:
  case PIC_TT_UNDEF:
  case PIC_TT_EOF:
  case PIC_TT_PORT:
  case PIC_TT_ERROR:
  case PIC_TT_SENV:
  case PIC_TT_MACRO:
  case PIC_TT_SC:
  case PIC_TT_LIB:
  case PIC_TT_VAR:
  case PIC_TT_IREP:
    pic_errorf(pic, "invalid expression given: ~s", obj);
  }
  UNREACHABLE();
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
  int argc, localc, capturec;
  xhash *cvs, *lvs;

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
push_resolver_scope(resolver_state *state, pic_value args, pic_value locals, bool varg, pic_value captures)
{
  pic_state *pic = state->pic;
  resolver_scope *scope;
  int i;

  scope = (resolver_scope *)pic_alloc(pic, sizeof(resolver_scope));
  scope->up = state->scope;
  scope->depth = scope->up ? scope->up->depth + 1 : 0;
  scope->lvs = xh_new_int();
  scope->cvs = xh_new_int();
  scope->argc = pic_length(pic, args) + 1;
  scope->localc = pic_length(pic, locals);
  scope->capturec = pic_length(pic, captures);
  scope->varg = varg;

  /* arguments */
  for (i = 1; i < scope->argc; ++i) {
    xh_put_int(scope->lvs, pic_sym(pic_list_ref(pic, args, i - 1)), i);
  }

  /* locals */
  for (i = 0; i < scope->localc; ++i) {
    xh_put_int(scope->lvs, pic_sym(pic_list_ref(pic, locals, i)), scope->argc + i);
  }

  /* closed variables */
  for (i = 0; i < scope->capturec; ++i) {
    xh_put_int(scope->cvs, pic_sym(pic_list_ref(pic, captures, i)), i);
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
  return xh_get_int(state->scope->cvs, sym) != NULL;
}

static pic_value
resolve_lref(resolver_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  int i;

  i = xh_get_int(state->scope->lvs, sym)->val;

  return pic_list2(pic, pic_symbol_value(state->sLREF), pic_int_value(i));
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

  i = xh_get_int(scope->cvs, sym)->val;

  return pic_list3(pic,
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
  pic_sym tag;

  if (! pic_pair_p(obj))
    return obj;

  tag = pic_sym(pic_car(pic, obj));
  if (tag == state->sREF) {
    int depth;
    pic_sym sym;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    sym = pic_sym(pic_list_ref(pic, obj, 2));
    if (depth == 0 && ! is_closed(state, sym)) {
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

    return pic_list6(pic, pic_symbol_value(pic->sLAMBDA), args, locals, pic_bool_value(varg), closes, body);
  }
  else if (tag == pic->sQUOTE) {
    return obj;
  }
  else {
    int ai = pic_gc_arena_preserve(pic);
    pic_value seq = pic_list1(pic, pic_symbol_value(tag)), elt;

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
  /* rest args variable is counted as a local */
  xvect args, locals, captures;
  /* actual bit code sequence */
  pic_code *code;
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
  pic_sym sCALL_WITH_VALUES, sTAILCALL_WITH_VALUES;
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
  register_symbol(pic, state, sCALL_WITH_VALUES, "call-with-values");
  register_symbol(pic, state, sTAILCALL_WITH_VALUES, "tailcall-with-values");

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
create_activation(codegen_context *cxt)
{
  size_t i, n;
  xhash *regs;
  pic_sym *var;
  size_t offset;

  regs = xh_new_int();

  offset = 1;
  for (i = 0; i < cxt->args.size; ++i) {
    var = xv_get(&cxt->args, i);
    xh_put_int(regs, *var, i + offset);
  }
  offset += i;
  for (i = 0; i < cxt->locals.size; ++i) {
    var = xv_get(&cxt->locals, i);
    xh_put_int(regs, *var, i + offset);
  }

  for (i = 0; i < cxt->captures.size; ++i) {
    var = xv_get(&cxt->captures, i);
    if ((n = xh_get_int(regs, *var)->val) <= cxt->args.size) {
      /* copy arguments to capture variable area */
      cxt->code[cxt->clen].insn = OP_LREF;
      cxt->code[cxt->clen].u.i = n;
      cxt->clen++;
    } else {
      /* otherwise, just extend the stack */
      cxt->code[cxt->clen].insn = OP_PUSHNONE;
      cxt->clen++;
    }
  }

  xh_destroy(regs);
}

static void
push_codegen_context(codegen_state *state, pic_value args, pic_value locals, bool varg, pic_value captures)
{
  pic_state *pic = state->pic;
  codegen_context *cxt;
  pic_value var;

  cxt = (codegen_context *)pic_alloc(pic, sizeof(codegen_context));
  cxt->up = state->cxt;
  cxt->varg = varg;

  xv_init(&cxt->args, sizeof(pic_sym));
  xv_init(&cxt->locals, sizeof(pic_sym));
  xv_init(&cxt->captures, sizeof(pic_sym));

  pic_for_each (var, args) {
    xv_push(&cxt->args, &pic_sym(var));
  }
  pic_for_each (var, locals) {
    xv_push(&cxt->locals, &pic_sym(var));
  }
  pic_for_each (var, captures) {
    xv_push(&cxt->captures, &pic_sym(var));
  }

  cxt->code = pic_calloc(pic, PIC_ISEQ_SIZE, sizeof(pic_code));
  cxt->clen = 0;
  cxt->ccapa = PIC_ISEQ_SIZE;

  cxt->irep = pic_calloc(pic, PIC_IREP_SIZE, sizeof(struct pic_irep *));
  cxt->ilen = 0;
  cxt->icapa = PIC_IREP_SIZE;

  cxt->pool = pic_calloc(pic, PIC_POOL_SIZE, sizeof(pic_value));
  cxt->plen = 0;
  cxt->pcapa = PIC_POOL_SIZE;

  state->cxt = cxt;

  create_activation(cxt);
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
  irep->argc = state->cxt->args.size + 1;
  irep->localc = state->cxt->locals.size;
  irep->capturec = state->cxt->captures.size;
  irep->code = pic_realloc(pic, state->cxt->code, sizeof(pic_code) * state->cxt->clen);
  irep->clen = state->cxt->clen;
  irep->irep = pic_realloc(pic, state->cxt->irep, sizeof(struct pic_irep *) * state->cxt->ilen);
  irep->ilen = state->cxt->ilen;
  irep->pool = pic_realloc(pic, state->cxt->pool, sizeof(pic_value) * state->cxt->plen);
  irep->plen = state->cxt->plen;

  /* finalize */
  xv_destroy(&cxt->args);
  xv_destroy(&cxt->locals);
  xv_destroy(&cxt->captures);

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
      cxt->irep = pic_realloc(pic, cxt->irep, sizeof(struct pic_irep *) * cxt->icapa);
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
    int i = 0;

    pic_for_each (elt, pic_cdr(pic, obj)) {
      if (i++ != 0) {
        cxt->code[cxt->clen].insn = OP_POP;
        cxt->clen++;
      }
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
        cxt->pool = pic_realloc(pic, cxt->pool, sizeof(pic_value) * cxt->pcapa);
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
  else if (sym == state->sCALL_WITH_VALUES || sym == state->sTAILCALL_WITH_VALUES) {
    /* stack consumer at first */
    codegen(state, pic_list_ref(pic, obj, 2));
    codegen(state, pic_list_ref(pic, obj, 1));
    /* call producer */
    cxt->code[cxt->clen].insn = OP_CALL;
    cxt->code[cxt->clen].u.i = 1;
    cxt->clen++;
    /* call consumer */
    cxt->code[cxt->clen].insn = (sym == state->sCALL_WITH_VALUES) ? OP_CALL : OP_TAILCALL;
    cxt->code[cxt->clen].u.i = -1;
    cxt->clen++;
    return;
  }
  else if (sym == state->sRETURN) {
    int len = pic_length(pic, obj);
    pic_value elt;

    pic_for_each (elt, pic_cdr(pic, obj)) {
      codegen(state, elt);
    }
    cxt->code[cxt->clen].insn = OP_RET;
    cxt->code[cxt->clen].u.i = len - 1;
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

struct pic_proc *
pic_compile(pic_state *pic, pic_value obj)
{
  struct pic_irep *irep;
  int ai = pic_gc_arena_preserve(pic);

#if DEBUG
  fprintf(stdout, "ai = %d\n", pic_gc_arena_preserve(pic));

  fprintf(stdout, "# input expression\n");
  pic_debug(pic, obj);
  fprintf(stdout, "\n");

  fprintf(stdout, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* macroexpand */
  obj = pic_macroexpand(pic, obj);
#if DEBUG
  fprintf(stdout, "## macroexpand completed\n");
  pic_debug(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* analyze */
  obj = pic_analyze(pic, obj);
#if DEBUG
  fprintf(stdout, "## analyzer completed\n");
  pic_debug(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* resolution */
  obj = pic_resolve(pic, obj);
#if DEBUG
  fprintf(stdout, "## resolver completed\n");
  pic_debug(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %d\n", pic_gc_arena_preserve(pic));
#endif

  /* codegen */
  irep = pic_codegen(pic, obj);
#if DEBUG
  fprintf(stdout, "## codegen completed\n");
  pic_dump_irep(irep);
#endif

#if DEBUG
  fprintf(stdout, "# compilation finished\n");
  puts("");
#endif

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(irep));

  return pic_proc_new_irep(pic, irep, NULL);
}
