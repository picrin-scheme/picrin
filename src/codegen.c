#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "xhash/xhash.h"

typedef struct codegen_scope {
  struct codegen_scope *up;

  /* local variables are 1-indexed */
  struct xhash *local_tbl;
  size_t argc;
  int *cv_tbl;
} codegen_scope;

static codegen_scope *
new_global_scope(pic_state *pic)
{
  codegen_scope *scope;

  scope = (codegen_scope *)pic_alloc(pic, sizeof(codegen_scope));
  scope->up = NULL;
  scope->local_tbl = pic->global_tbl;
  scope->argc = -1;
  scope->cv_tbl = NULL;
  return scope;
}

static codegen_scope *
new_local_scope(pic_state *pic, pic_value args, codegen_scope *scope)
{
  codegen_scope *new_scope;
  pic_value v;
  int i;
  struct xhash *x;

  new_scope = (codegen_scope *)pic_alloc(pic, sizeof(codegen_scope));
  new_scope->up = scope;
  new_scope->local_tbl = x = xh_new();

  i = 1;
  for (v = args; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_value sym;

    sym = pic_car(pic, v);
    xh_put(x, pic_symbol_ptr(sym)->name, i++);
  }
  new_scope->argc = i;
  new_scope->cv_tbl = (int *)pic_calloc(pic, i, sizeof(int));

  return new_scope;
}

static void
destroy_scope(pic_state *pic, codegen_scope *scope)
{
  if (scope->up) {
    xh_destory(scope->local_tbl);
    pic_free(pic, scope->cv_tbl);
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
  return irep;
}

static void print_irep(pic_state *, struct pic_irep *);

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
  if (e) {
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
    return e->val;
  }
  e = xh_put(pic->global_tbl, name, pic->glen++);
  if (pic->glen >= pic->gcapa) {
    pic_error(pic, "global table overflow");
  }
  return e->val;
}

static void codegen_call(codegen_state *, pic_value);
static struct pic_irep *codegen_lambda(codegen_state *, pic_value);

static void
codegen(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  struct pic_irep *irep = state->irep;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    codegen_scope *s;
    int depth, idx;
    const char *name;

    name = pic_symbol_ptr(obj)->name;
    s = scope_lookup(state, name, &depth, &idx);
    if (! s) {
      pic_error(pic, "unbound variable");
    }

    if (depth == -1) {		/* global */
      irep->code[irep->clen].insn = OP_GREF;
      irep->code[irep->clen].u.i = idx;
      irep->clen++;
    }
    else if (depth == 0) {	/* local */
      irep->code[irep->clen].insn = OP_LREF;
      irep->code[irep->clen].u.i = idx;
      irep->clen++;
    }
    else {			/* nonlocal */
      /* dirty flag */
      s->cv_tbl[idx] = 1;
      /* dummy code */
      irep->code[irep->clen].insn = OP_PUSHNIL;
      irep->clen++;
    }
    break;
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    proc = pic_car(pic, obj);
    if (pic_eq_p(pic, proc, pic->sDEFINE)) {
      int idx;
      const char *name;

      name = pic_symbol_ptr(pic_car(pic, pic_cdr(pic, obj)))->name;
      idx = scope_global_define(pic, name);

      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));

      irep->code[irep->clen].insn = OP_GSET;
      irep->code[irep->clen].u.i = idx;
      irep->clen++;
      irep->code[irep->clen].insn = OP_PUSHFALSE;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sLAMBDA)) {
      irep->code[irep->clen].insn = OP_LAMBDA;
      irep->code[irep->clen].u.i = pic->ilen;
      irep->clen++;

      pic->irep[pic->ilen++] = codegen_lambda(state, obj);
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sIF)) {
      int s,t;

      codegen(state, pic_car(pic, pic_cdr(pic, obj)));

      irep->code[irep->clen].insn = OP_JMPIF;
      s = irep->clen++;

      /* if false branch */
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, pic_cdr(pic, obj)))));
      irep->code[irep->clen].insn = OP_JMP;
      t = irep->clen++;

      irep->code[s].u.i = irep->clen - s;

      /* if true branch */
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[t].u.i = irep->clen - t;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sBEGIN)) {
      pic_value v, seq;

      seq = pic_cdr(pic, obj);
      for (v = seq; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
	codegen(state, pic_car(pic, v));
	irep->code[irep->clen].insn = OP_POP;
	irep->clen++;
      }
      irep->clen--;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sQUOTE)) {
      int pidx;
      pidx = pic->plen++;
      pic->pool[pidx] = pic_car(pic, pic_cdr(pic, obj));
      irep->code[irep->clen].insn = OP_PUSHCONST;
      irep->code[irep->clen].u.i = pidx;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sCONS)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[irep->clen].insn = OP_CONS;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sCAR)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      irep->code[irep->clen].insn = OP_CAR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sCDR)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      irep->code[irep->clen].insn = OP_CDR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sNILP)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      irep->code[irep->clen].insn = OP_NILP;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sADD)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[irep->clen].insn = OP_ADD;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sSUB)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[irep->clen].insn = OP_SUB;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sMUL)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[irep->clen].insn = OP_MUL;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, pic->sDIV)) {
      codegen(state, pic_car(pic, pic_cdr(pic, obj)));
      codegen(state, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))));
      irep->code[irep->clen].insn = OP_DIV;
      irep->clen++;
      break;
    }
    else {
      codegen_call(state, obj);
      break;
    }
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
    irep->code[irep->clen].insn = OP_PUSHNUM;
    irep->code[irep->clen].u.f = pic_float(obj);
    irep->clen++;
    break;
  }
  case PIC_TT_NIL: {
    irep->code[irep->clen].insn = OP_PUSHNIL;
    irep->clen++;
    break;
  }
  case PIC_TT_STRING: {
    int pidx;
    pidx = pic->plen++;
    pic->pool[pidx] = obj;
    irep->code[irep->clen].insn = OP_PUSHCONST;
    irep->code[irep->clen].u.i = pidx;
    irep->clen++;
    break;
  }
  case PIC_TT_PROC:
  case PIC_TT_UNDEF:
  case PIC_TT_EOF:
  case PIC_TT_PORT: {
    pic_error(pic, "invalid expression given");
  }
  }
}

static void
codegen_call(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  struct pic_irep *irep = state->irep;
  pic_value seq;
  int i = 0;

  for (seq = obj; ! pic_nil_p(seq); seq = pic_cdr(pic, seq)) {
    pic_value v;

    v = pic_car(pic, seq);
    codegen(state, v);
    ++i;
  }
  irep->code[irep->clen].insn = OP_CALL;
  irep->code[irep->clen].u.i = i;
  irep->clen++;
}

static struct pic_irep *
codegen_lambda(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  codegen_scope *prev_scope;
  struct pic_irep *prev_irep, *irep;
  pic_value body, v;
  int i;

  /* inner environment */
  prev_irep = state->irep;
  prev_scope = state->scope;

  state->scope = new_local_scope(pic, pic_car(pic, pic_cdr(pic, obj)), state->scope);
  state->irep = irep = new_irep(pic);
  irep->argc = state->scope->argc;
  {
    /* body */
    body = pic_cdr(pic, pic_cdr(pic, obj));
    for (v = body; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
      codegen(state, pic_car(pic, v));
      irep->code[irep->clen].insn = OP_POP;
      irep->clen++;
    }
    irep->clen--;
    irep->code[irep->clen].insn = OP_RET;
    irep->clen++;

    printf("** dirty **\n");
    for (i = 1; i < state->scope->argc; ++i) {
      if (state->scope->cv_tbl[i])
	printf("%d ", i);
    }
    puts("");
  }
  destroy_scope(pic, state->scope);

  state->irep = prev_irep;
  state->scope = prev_scope;

#if VM_DEBUG
  printf("LAMBDA_%zd:\n", pic->ilen);
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
  codegen(state, obj);
  state->irep->code[state->irep->clen].insn = OP_STOP;
  state->irep->clen++;
  proc = pic_proc_new(pic, state->irep);

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

  proc = pic_proc_new_cfunc(pic, cfunc, pic_undef_value());
  idx = scope_global_define(pic, name);
  pic->globals[idx] = pic_obj_value(proc);
}

static void
print_irep(pic_state *pic, struct pic_irep *irep)
{
  int i;

  printf("## irep %p\n", irep);
  printf("[clen = %zd, ccapa = %zd, argc = %d]\n", irep->clen, irep->ccapa, irep->argc);
  for (i = 0; i < irep->clen; ++i) {
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
    case OP_PUSHNUM:
      printf("OP_PUSHNUM\t%g\n", irep->code[i].u.f);
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
    case OP_JMP:
      printf("OP_JMP\t%d\n", irep->code[i].u.i);
      break;
    case OP_JMPIF:
      printf("OP_JMPIF\t%d\n", irep->code[i].u.i);
      break;
    case OP_CALL:
      printf("OP_CALL\t%d\n", irep->code[i].u.i);
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
    case OP_STOP:
      puts("OP_STOP");
      break;
    }
  }
}
