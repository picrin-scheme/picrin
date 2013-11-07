#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "xhash/xhash.h"

#define FALLTHROUGH ((void)0)

typedef struct codegen_scope {
  struct codegen_scope *up;

  /* local variables are 1-indexed, 0 is reserved for the callee */
  struct xhash *local_tbl;
  /* rest args variable is counted at localc */
  size_t argc, localc;
  /* if local var i is captured, then dirty_flags[i] == 1 */
  int *dirty_flags;
  bool varg;
} codegen_scope;

static codegen_scope *
new_global_scope(pic_state *pic)
{
  codegen_scope *scope;

  scope = (codegen_scope *)pic_alloc(pic, sizeof(codegen_scope));
  scope->up = NULL;
  scope->local_tbl = pic->global_tbl;
  scope->argc = -1;
  scope->localc = -1;
  scope->dirty_flags = NULL;
  scope->varg = false;
  return scope;
}

static codegen_scope *
new_local_scope(pic_state *pic, pic_value args, codegen_scope *scope)
{
  codegen_scope *new_scope;
  pic_value v;
  int i, l;
  struct xhash *x;

  new_scope = (codegen_scope *)pic_alloc(pic, sizeof(codegen_scope));
  new_scope->up = scope;
  new_scope->local_tbl = x = xh_new();
  new_scope->varg = false;

  i = 1; l = 0;
  for (v = args; pic_pair_p(v); v = pic_cdr(pic, v)) {
    pic_value sym;

    sym = pic_car(pic, v);
    xh_put(x, pic_symbol_name(pic, pic_sym(sym)), i++);
  }
  if (pic_nil_p(v)) {
    /* pass */
  }
  else if (pic_symbol_p(v)) {
    new_scope->varg = true;
    xh_put(x, pic_symbol_name(pic, pic_sym(v)), i + l++);
  }
  else {
    pic_error(pic, "logic flaw");
  }
  new_scope->argc = i;
  new_scope->localc = l;
  new_scope->dirty_flags = (int *)pic_calloc(pic, i + l, sizeof(int));

  return new_scope;
}

static void
destroy_scope(pic_state *pic, codegen_scope *scope)
{
  if (scope->up) {
    xh_destory(scope->local_tbl);
    pic_free(pic, scope->dirty_flags);
  }
  pic_free(pic, scope);
}

static struct pic_irep *
new_irep(pic_state *pic)
{
  struct pic_irep *irep;

  irep = (struct pic_irep *)pic_alloc(pic, sizeof(struct pic_irep));
  irep->code = (struct pic_code *)pic_alloc(pic, sizeof(struct pic_code) * 1024);
  irep->clen = 0;
  irep->ccapa = 1024;
  irep->argc = -1;
  irep->localc = -1;
  irep->varg = false;
  return irep;
}

typedef struct codegen_state {
  pic_state *pic;
  codegen_scope *scope;
  struct pic_irep *irep;
} codegen_state;

static codegen_state *
new_codegen_state(pic_state *pic)
{
  codegen_state *state;

  state = (codegen_state *)pic_alloc(pic, sizeof(codegen_state));
  state->pic = pic;
  state->scope = new_global_scope(pic);

  return state;
}

static void
destroy_codegen_state(pic_state *pic, codegen_state *state)
{
  destroy_scope(pic, state->scope);
  pic_free(pic, state);
}

static codegen_scope *
scope_lookup(codegen_state *state, const char *key, int *depth, int *idx)
{
  codegen_scope *scope = state->scope;
  struct xh_entry *e;
  int d = 0;

 enter:

  e = xh_get(scope->local_tbl, key);
  if (e && e->val >= 0) {
    if (scope->up == NULL) {	/* global */
      *depth = -1;
    }
    else {			/* non-global */
      *depth = d;
    }
    *idx = e->val;
    return scope;
  }
  if (scope->up) {
    scope = scope->up;
    ++d;
    goto enter;
  }
  return NULL;
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

static void codegen_call(codegen_state *, pic_value, bool);
static struct pic_irep *codegen_lambda(codegen_state *, pic_value);

static void
codegen(codegen_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  struct pic_irep *irep = state->irep;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    codegen_scope *s;
    int depth, idx;
    const char *name;

    name = pic_symbol_name(pic, pic_sym(obj));
    s = scope_lookup(state, name, &depth, &idx);
    if (! s) {
      pic_error(pic, "unbound variable");
    }

    switch (depth) {
    case -1:			/* global */
      irep->code[irep->clen].insn = OP_GREF;
      irep->code[irep->clen].u.i = idx;
      irep->clen++;
      break;
    default:			/* nonlocal */
      s->dirty_flags[idx] = 1;
      /* at this stage, lref and cref are not distinguished */
      FALLTHROUGH;
    case 0:			/* local */
      irep->code[irep->clen].insn = OP_CREF;
      irep->code[irep->clen].u.r.depth = depth;
      irep->code[irep->clen].u.r.idx = idx;
      irep->clen++;
      break;
    }
    break;
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    if (! pic_list_p(pic, obj)) {
      pic_error(pic, "invalid expression given");
    }

    proc = pic_car(pic, obj);
    if (pic_symbol_p(proc)) {
      pic_sym sym = pic_sym(proc);

      if (sym == pic->sDEFINE) {
	int idx;
	pic_value var, val;

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
	    pic_error(pic, "syntax error");
	  }
	  val = pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj)));
	}
	if (! pic_symbol_p(var)) {
	  pic_error(pic, "syntax error");
	}

	idx = scope_global_define(pic, pic_symbol_name(pic, pic_sym(var)));

	codegen(state, val, false);
	irep->code[irep->clen].insn = OP_GSET;
	irep->code[irep->clen].u.i = idx;
	irep->clen++;
	irep->code[irep->clen].insn = OP_PUSHFALSE;
	irep->clen++;
	break;
      }
      else if (sym == pic->sLAMBDA) {
	int k = pic->ilen++;
	irep->code[irep->clen].insn = OP_LAMBDA;
	irep->code[irep->clen].u.i = k;
	irep->clen++;

	pic->irep[k] = codegen_lambda(state, obj);
	break;
      }
      else if (sym == pic->sIF) {
	int s,t;
	pic_value if_true, if_false;

	if_false = pic_false_value();
	switch (pic_length(pic, obj)) {
	default:
	  pic_error(pic, "syntax error");
	  break;
	case 4:
	  if_false = pic_car(pic, pic_cdr(pic, pic_cdr(pic, pic_cdr(pic, obj))));
	  FALLTHROUGH;
	case 3:
	  if_true = pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj)));
	}

	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);

	irep->code[irep->clen].insn = OP_JMPIF;
	s = irep->clen++;

	/* if false branch */
	codegen(state, if_false, tailpos);
	irep->code[irep->clen].insn = OP_JMP;
	t = irep->clen++;

	irep->code[s].u.i = irep->clen - s;

	/* if true branch */
	codegen(state, if_true, tailpos);
	irep->code[t].u.i = irep->clen - t;
	break;
      }
      else if (sym == pic->sBEGIN) {
	int i, len;
	pic_value v, seq;

	seq = pic_cdr(pic, obj);
	len = pic_length(pic, seq);
	for (i = 0; i < len; ++i) {
	  v = pic_car(pic, seq);
	  if (i + 1 >= len) {
	    codegen(state, v, tailpos);
	  }
	  else {
	    codegen(state, v, false);
	    irep->code[irep->clen].insn = OP_POP;
	    irep->clen++;
	  }
	  seq = pic_cdr(pic, seq);
	}
	break;
      }
      else if (sym == pic->sSETBANG) {
	codegen_scope *s;
	pic_value var;
	int depth, idx;

	if (pic_length(pic, obj) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = pic_car(pic, pic_cdr(pic, obj));
	if (! pic_symbol_p(var)) {
	  pic_error(pic, "syntax error");
	}

	s = scope_lookup(state, pic_symbol_name(pic, pic_sym(var)), &depth, &idx);
	if (! s) {
	  pic_error(pic, "unbound variable");
	}

	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);

	switch (depth) {
	case -1:			/* global */
	  irep->code[irep->clen].insn = OP_GSET;
	  irep->code[irep->clen].u.i = idx;
	  irep->clen++;
	  break;
	default:			/* nonlocal */
	  s->dirty_flags[idx] = 1;
	  /* at this stage, lset and cset are not distinguished */
	  FALLTHROUGH;
	case 0:			/* local */
	  irep->code[irep->clen].insn = OP_CSET;
	  irep->code[irep->clen].u.r.depth = depth;
	  irep->code[irep->clen].u.r.idx = idx;
	  irep->clen++;
	  break;
	}

	irep->code[irep->clen].insn = OP_PUSHFALSE;
	irep->clen++;
	break;
      }
      else if (sym == pic->sQUOTE) {
	int pidx;

	if (pic_length(pic, obj) != 2) {
	  pic_error(pic, "syntax error");
	}

	pidx = pic->plen++;
	pic->pool[pidx] = pic_car(pic, pic_cdr(pic, obj));
	irep->code[irep->clen].insn = OP_PUSHCONST;
	irep->code[irep->clen].u.i = pidx;
	irep->clen++;
	break;
      }

#define ARGC_ASSERT(n) do {				\
	if (pic_length(pic, obj) != (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

      else if (sym == pic->sCONS) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	irep->code[irep->clen].insn = OP_CONS;
	irep->clen++;
	break;
      }
      else if (sym == pic->sCAR) {
	ARGC_ASSERT(1);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	irep->code[irep->clen].insn = OP_CAR;
	irep->clen++;
	break;
      }
      else if (sym == pic->sCDR) {
	ARGC_ASSERT(1);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	irep->code[irep->clen].insn = OP_CDR;
	irep->clen++;
	break;
      }
      else if (sym == pic->sNILP) {
	ARGC_ASSERT(1);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	irep->code[irep->clen].insn = OP_NILP;
	irep->clen++;
	break;
      }

#define ARGC_ASSERT_GE(n) do {				\
	if (pic_length(pic, obj) < (n) + 1) {		\
	  pic_error(pic, "wrong number of arguments");	\
	}						\
      } while (0)

      else if (sym == pic->sADD) {
	pic_value args;

	ARGC_ASSERT_GE(0);
	switch (pic_length(pic, obj)) {
	case 1:
	  irep->code[irep->clen].insn = OP_PUSHINT;
	  irep->code[irep->clen].u.i = 0;
	  irep->clen++;
	  break;
	case 2:
	  codegen(state, pic_car(pic, pic_cdr(pic, obj)), tailpos);
	  break;
	default:
	  args = pic_cdr(pic, obj);
	  codegen(state, pic_car(pic, args), false);
	  while (pic_length(pic, args) >= 2) {
	    codegen(state, pic_car(pic, pic_cdr(pic, args)), false);
	    irep->code[irep->clen].insn = OP_ADD;
	    irep->clen++;
	    args = pic_cdr(pic, args);
	  }
	  break;
	}
	break;
      }
      else if (sym == pic->sSUB) {
	pic_value args;

	ARGC_ASSERT_GE(1);
	switch (pic_length(pic, obj)) {
	case 2:
	  codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	  irep->code[irep->clen].insn = OP_MINUS;
	  irep->clen++;
	  break;
	default:
	  args = pic_cdr(pic, obj);
	  codegen(state, pic_car(pic, args), false);
	  while (pic_length(pic, args) >= 2) {
	    codegen(state, pic_car(pic, pic_cdr(pic, args)), false);
	    irep->code[irep->clen].insn = OP_SUB;
	    irep->clen++;
	    args = pic_cdr(pic, args);
	  }
	  break;
	}
	break;
      }
      else if (sym == pic->sMUL) {
	pic_value args;

	ARGC_ASSERT_GE(0);
	switch (pic_length(pic, obj)) {
	case 1:
	  irep->code[irep->clen].insn = OP_PUSHINT;
	  irep->code[irep->clen].u.i = 1;
	  irep->clen++;
	  break;
	case 2:
	  codegen(state, pic_car(pic, pic_cdr(pic, obj)), tailpos);
	  break;
	default:
	  args = pic_cdr(pic, obj);
	  codegen(state, pic_car(pic, args), false);
	  while (pic_length(pic, args) >= 2) {
	    codegen(state, pic_car(pic, pic_cdr(pic, args)), false);
	    irep->code[irep->clen].insn = OP_MUL;
	    irep->clen++;
	    args = pic_cdr(pic, args);
	  }
	  break;
	}
	break;
      }
      else if (sym == pic->sDIV) {
	pic_value args;

	ARGC_ASSERT_GE(1);
	switch (pic_length(pic, obj)) {
	case 2:
	  irep->code[irep->clen].insn = OP_PUSHINT;
	  irep->code[irep->clen].u.i = 1;
	  irep->clen++;
	  codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	  irep->code[irep->clen].insn = OP_DIV;
	  irep->clen++;
	  break;
	default:
	  args = pic_cdr(pic, obj);
	  codegen(state, pic_car(pic, args), false);
	  while (pic_length(pic, args) >= 2) {
	    codegen(state, pic_car(pic, pic_cdr(pic, args)), false);
	    irep->code[irep->clen].insn = OP_DIV;
	    irep->clen++;
	    args = pic_cdr(pic, args);
	  }
	  break;
	}
	break;
      }
      else if (sym == pic->sEQ) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	irep->code[irep->clen].insn = OP_EQ;
	irep->clen++;
	break;
      }
      else if (sym == pic->sLT) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	irep->code[irep->clen].insn = OP_LT;
	irep->clen++;
	break;
      }
      else if (sym == pic->sLE) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	irep->code[irep->clen].insn = OP_LE;
	irep->clen++;
	break;
      }
      else if (sym == pic->sGT) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	irep->code[irep->clen].insn = OP_LT;
	irep->clen++;
	break;
      }
      else if (sym == pic->sGE) {
	ARGC_ASSERT(2);
	codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), false);
	codegen(state, pic_car(pic, pic_cdr(pic, obj)), false);
	irep->code[irep->clen].insn = OP_LE;
	irep->clen++;
	break;
      }
    }

    codegen_call(state, obj, tailpos);
    break;
  }
  case PIC_TT_BOOL: {
    if (pic_true_p(obj)) {
      irep->code[irep->clen].insn = OP_PUSHTRUE;
    }
    else {
      irep->code[irep->clen].insn = OP_PUSHFALSE;
    }
    irep->clen++;
    break;
  }
  case PIC_TT_FLOAT: {
    irep->code[irep->clen].insn = OP_PUSHFLOAT;
    irep->code[irep->clen].u.f = pic_float(obj);
    irep->clen++;
    break;
  }
  case PIC_TT_INT: {
    irep->code[irep->clen].insn = OP_PUSHINT;
    irep->code[irep->clen].u.i = pic_int(obj);
    irep->clen++;
    break;
  }
  case PIC_TT_NIL: {
    irep->code[irep->clen].insn = OP_PUSHNIL;
    irep->clen++;
    break;
  }
  case PIC_TT_CHAR: {
    irep->code[irep->clen].insn = OP_PUSHCHAR;
    irep->code[irep->clen].u.c = pic_char(obj);
    irep->clen++;
    break;
  }
  case PIC_TT_STRING:
  case PIC_TT_VECTOR:
  case PIC_TT_BLOB: {
    int pidx;
    pidx = pic->plen++;
    pic->pool[pidx] = obj;
    irep->code[irep->clen].insn = OP_PUSHCONST;
    irep->code[irep->clen].u.i = pidx;
    irep->clen++;
    break;
  }
  case PIC_TT_ENV:
  case PIC_TT_PROC:
  case PIC_TT_UNDEF:
  case PIC_TT_EOF:
  case PIC_TT_PORT: {
    pic_error(pic, "invalid expression given");
  }
  }
}

static void
codegen_call(codegen_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  struct pic_irep *irep = state->irep;
  pic_value seq;
  int i = 0;

  for (seq = obj; ! pic_nil_p(seq); seq = pic_cdr(pic, seq)) {
    pic_value v;

    v = pic_car(pic, seq);
    codegen(state, v, false);
    ++i;
  }
  irep->code[irep->clen].insn = tailpos ? OP_TAILCALL : OP_CALL;
  irep->code[irep->clen].u.i = i;
  irep->clen++;
}

static bool
valid_formal(pic_state *pic, pic_value formal)
{
  if (pic_symbol_p(formal))
    return true;

  while (pic_pair_p(formal)) {
    if (! pic_symbol_p(pic_car(pic, formal))) {
      return false;
    }
    formal = pic_cdr(pic, formal);
  }
  if (pic_nil_p(formal))
    return true;
  if (pic_symbol_p(formal))
    return true;

  return false;
}

static void
lift_cv(pic_state *pic, struct pic_irep *irep)
{
  int i;
  struct pic_code c;

  for (i = 0; i < irep->clen; ++i) {
    c = irep->code[i];
    switch (c.insn) {
    default:
      /* pass */
      break;
    case OP_LAMBDA:
      lift_cv(pic, pic->irep[c.u.i]);
      break;
    case OP_CREF:
    case OP_CSET:
      irep->code[i].u.r.depth--;
      break;
    }
  }
}

static void
slide_cv(pic_state *pic, unsigned *cv_tbl, unsigned cv_num, struct pic_irep *irep, int d)
{
  int i, j;
  struct pic_code c;

  for (i = 0; i < irep->clen; ++i) {
    c = irep->code[i];
    switch (c.insn) {
    default:
      /* pass */
      break;
    case OP_LAMBDA:
      if (pic->irep[c.u.i]->cv_num == 0) {
	slide_cv(pic, cv_tbl, cv_num, pic->irep[c.u.i], d);
      }
      else {
	slide_cv(pic, cv_tbl, cv_num, pic->irep[c.u.i], d + 1);
      }
      break;
    case OP_CREF:
    case OP_CSET:
      if (d != c.u.r.depth)
	break;
      for (j = 0; j < cv_num; ++j) {
	if (c.u.r.idx == cv_tbl[j]) {
	  irep->code[i].u.r.idx = j;
	  break;
	}
      }
      break;
    }
  }
}

static struct pic_irep *
codegen_lambda(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  codegen_scope *prev_scope;
  struct pic_irep *prev_irep, *irep;
  pic_value args, body, v;
  int i, c, k;

  if (pic_length(pic, obj) < 2) {
    pic_error(pic, "syntax error");
  }

  args = pic_car(pic, pic_cdr(pic, obj));
  if (! valid_formal(pic, args)) {
    pic_error(pic, "syntax error");
  }

  /* inner environment */
  prev_irep = state->irep;
  prev_scope = state->scope;

  state->scope = new_local_scope(pic, args, state->scope);
  state->irep = irep = new_irep(pic);
  irep->argc = state->scope->argc;
  irep->localc = state->scope->localc;
  irep->varg = state->scope->varg;
  {
    /* body */
    body = pic_cdr(pic, pic_cdr(pic, obj));
    for (v = body; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
      if (pic_nil_p(pic_cdr(pic, v))) {
	codegen(state, pic_car(pic, v), true);
      }
      else {
	codegen(state, pic_car(pic, v), false);
	irep->code[irep->clen].insn = OP_POP;
	irep->clen++;
      }
    }
    irep->code[irep->clen].insn = OP_RET;
    irep->clen++;

    /* fixup local references */
    for (i = 0; i < irep->clen; ++i) {
      struct pic_code c = irep->code[i];
      switch (c.insn) {
      default:
	/* pass */
	break;
      case OP_CREF:
	if (c.u.r.depth == 0 && ! state->scope->dirty_flags[c.u.r.idx]) {
	  irep->code[i].insn = OP_LREF;
	  irep->code[i].u.i = irep->code[i].u.r.idx;
	}
	break;
      case OP_CSET:
	if (c.u.r.depth == 0 && ! state->scope->dirty_flags[c.u.r.idx]) {
	  irep->code[i].insn = OP_LSET;
	  irep->code[i].u.i = irep->code[i].u.r.idx;
	}
	break;
      }
    }

    /* fixup closed variables */
    c = 0;
    for (i = 0; i < irep->argc + irep->localc; ++i) {
      if (state->scope->dirty_flags[i])
	++c;
    }
    if (c == 0) {
      lift_cv(pic, irep);
      irep->cv_tbl = NULL;
      irep->cv_num = 0;
    }
    else {
      irep->cv_tbl = (unsigned *)pic_calloc(pic, c, sizeof(unsigned));
      k = 0;
      for (i = 0; i < irep->argc + irep->localc; ++i) {
	if (state->scope->dirty_flags[i]) {
	  irep->cv_tbl[k] = i;
	  ++k;
	}
      }
      irep->cv_num = c;
      slide_cv(pic, irep->cv_tbl, irep->cv_num, irep, 0);
    }
  }
  destroy_scope(pic, state->scope);

  state->irep = prev_irep;
  state->scope = prev_scope;

#if VM_DEBUG
  print_irep(pic, irep);
  puts("");
#endif

  return irep;
}

struct pic_proc *
pic_codegen(pic_state *pic, pic_value obj)
{
  struct pic_proc *proc;
  codegen_state *state;

  state = new_codegen_state(pic);

  if (! pic->jmp) {
    jmp_buf jmp;

    if (setjmp(jmp) == 0) {
      pic->jmp = &jmp;
    }
    else {
      /* error occured */
      pic->jmp = NULL;
      return NULL;
    }
  }
  state->irep = new_irep(pic);
  state->irep->argc = 1;
  state->irep->localc = 0;
  codegen(state, pic_expand(pic, obj), false);
  state->irep->code[state->irep->clen].insn = OP_RET;
  state->irep->clen++;
  state->irep->cv_num = 0;
  state->irep->cv_tbl = NULL;

  proc = pic_proc_new(pic, state->irep, NULL);

  destroy_codegen_state(pic, state);

#if VM_DEBUG
  print_irep(pic, proc->u.irep);
#endif

  return proc;
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;
  int idx;

  proc = pic_proc_new_cfunc(pic, cfunc);
  idx = scope_global_define(pic, name);
  pic->globals[idx] = pic_obj_value(proc);
}

void
print_irep(pic_state *pic, struct pic_irep *irep)
{
  int i;

  printf("## irep %p\n", irep);
  printf("[clen = %zd, ccapa = %zd, argc = %d, localc = %d]\n", irep->clen, irep->ccapa, irep->argc, irep->localc);
  for (i = 0; i < irep->clen; ++i) {
    printf("[%2d] ", irep->code[i].insn);
    switch (irep->code[i].insn) {
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
    case OP_PUSHFLOAT:
      printf("OP_PUSHFLOAT\t%f\n", irep->code[i].u.f);
      break;
    case OP_PUSHINT:
      printf("OP_PUSHINT\t%d\n", irep->code[i].u.i);
      break;
    case OP_PUSHCHAR:
      printf("OP_PUSHCHAR\t%c\n", irep->code[i].u.c);
      break;
    case OP_PUSHCONST:
      printf("OP_PUSHCONST\t");
      pic_debug(pic, pic->pool[irep->code[i].u.i]);
      puts("");
      break;
    case OP_GREF:
      printf("OP_GREF\t%i\n", irep->code[i].u.i);
      break;
    case OP_GSET:
      printf("OP_GSET\t%i\n", irep->code[i].u.i);
      break;
    case OP_LREF:
      printf("OP_LREF\t%d\n", irep->code[i].u.i);
      break;
    case OP_LSET:
      printf("OP_LSET\t%d\n", irep->code[i].u.i);
      break;
    case OP_CREF:
      printf("OP_CREF\t%d\t%d\n", irep->code[i].u.r.depth, irep->code[i].u.r.idx);
      break;
    case OP_CSET:
      printf("OP_CSET\t%d\t%d\n", irep->code[i].u.r.depth, irep->code[i].u.r.idx);
      break;
    case OP_JMP:
      printf("OP_JMP\t%d\n", irep->code[i].u.i);
      break;
    case OP_JMPIF:
      printf("OP_JMPIF\t%d\n", irep->code[i].u.i);
      break;
    case OP_CALL:
      printf("OP_CALL\t%d\n", irep->code[i].u.i);
      break;
    case OP_TAILCALL:
      printf("OP_TAILCALL\t%d\n", irep->code[i].u.i);
      break;
    case OP_RET:
      puts("OP_RET");
      break;
    case OP_LAMBDA:
      printf("OP_LAMBDA\t%d\n", irep->code[i].u.i);
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
}
