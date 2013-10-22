#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "xhash/xhash.h"

struct pic_scope {
  struct pic_scope *up;

  struct xhash *local_tbl;
  size_t localc;
};

static struct pic_scope *
new_global_scope(pic_state *pic)
{
  struct pic_scope *scope;

  scope = (struct pic_scope *)pic_alloc(pic, sizeof(struct pic_scope));
  scope->up = NULL;
  scope->local_tbl = pic->global_tbl;
  scope->localc = -1;
  return scope;
}

static struct pic_scope *
new_local_scope(pic_state *pic, pic_value args, struct pic_scope *scope)
{
  struct pic_scope *new_scope;
  pic_value v;
  int i;
  struct xhash *x;

  new_scope = (struct pic_scope *)pic_alloc(pic, sizeof(struct pic_scope));
  new_scope->up = scope;
  new_scope->local_tbl = x = xh_new();

  i = -1;
  for (v = args; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_value sym;

    sym = pic_car(pic, v);
    xh_put(x, pic_symbol_ptr(sym)->name, i--);
  }
  new_scope->localc = -1-i;

  return new_scope;
}

static void
destory_scope(pic_state *pic, struct pic_scope *scope)
{
  if (scope->up) {
    xh_destory(scope->local_tbl);
  }
  pic_free(pic, scope);
}

static bool
scope_lookup(pic_state *pic, const char *key, struct pic_scope *scope, int *depth, int *idx)
{
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
    return true;
  }
  if (scope->up) {
    scope = scope->up;
    ++d;
    goto enter;
  }
  return false;
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

  printf("## irep %p [clen = %zd, ccapa = %zd]\n", irep, irep->clen, irep->ccapa);
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

static struct pic_irep *
new_irep(pic_state *pic)
{
  struct pic_irep *irep;

  irep = (struct pic_irep *)pic_alloc(pic, sizeof(struct pic_irep));
  irep->code = (struct pic_code *)pic_alloc(pic, sizeof(struct pic_code) * 1024);
  irep->clen = 0;
  irep->ccapa = 1024;
  return irep;
}

static void pic_gen_call(pic_state *, struct pic_irep *, pic_value, struct pic_scope *);
static struct pic_irep *pic_gen_lambda(pic_state *, pic_value, struct pic_scope *);

static void
pic_gen(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_scope *scope)
{
  pic_value sDEFINE, sLAMBDA, sIF, sBEGIN, sQUOTE;
  pic_value sCONS, sCAR, sCDR, sNILP;
  pic_value sADD, sSUB, sMUL, sDIV;

  sDEFINE = pic->sDEFINE;
  sLAMBDA = pic->sLAMBDA;
  sIF = pic->sIF;
  sBEGIN = pic->sBEGIN;
  sQUOTE = pic->sQUOTE;
  sCONS = pic->sCONS;
  sCAR = pic->sCAR;
  sCDR = pic->sCDR;
  sNILP = pic->sNILP;
  sADD = pic->sADD;
  sSUB = pic->sSUB;
  sMUL = pic->sMUL;
  sDIV = pic->sDIV;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    bool b;
    int depth, idx;
    const char *name;

    name = pic_symbol_ptr(obj)->name;
    b = scope_lookup(pic, name, scope, &depth, &idx);
    if (! b) {
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
      pic_error(pic, "reference to closed variable not supported");
    }
    break;
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    proc = pic_car(pic, obj);
    if (pic_eq_p(pic, proc, sDEFINE)) {
      int idx;
      const char *name;

      name = pic_symbol_ptr(pic_car(pic, pic_cdr(pic, obj)))->name;
      idx = scope_global_define(pic, name);

      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);

      irep->code[irep->clen].insn = OP_GSET;
      irep->code[irep->clen].u.i = idx;
      irep->clen++;
      irep->code[irep->clen].insn = OP_PUSHFALSE;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sLAMBDA)) {
      irep->code[irep->clen].insn = OP_LAMBDA;
      irep->code[irep->clen].u.i = pic->ilen;
      irep->clen++;

      pic->irep[pic->ilen++] = pic_gen_lambda(pic, obj, scope);
      break;
    }
    else if (pic_eq_p(pic, proc, sIF)) {
      int s,t;

      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);

      irep->code[irep->clen].insn = OP_JMPIF;
      s = irep->clen++;

      /* if false branch */
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, pic_cdr(pic, obj)))), scope);
      irep->code[irep->clen].insn = OP_JMP;
      t = irep->clen++;

      irep->code[s].u.i = irep->clen - s;

      /* if true branch */
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      irep->code[t].u.i = irep->clen - t;
      break;
    }
    else if (pic_eq_p(pic, proc, sBEGIN)) {
      pic_value v, seq;

      seq = pic_cdr(pic, obj);
      for (v = seq; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
	pic_gen(pic, irep, pic_car(pic, v), scope);
	irep->code[irep->clen].insn = OP_POP;
	irep->clen++;
      }
      irep->clen--;
      break;
    }
    else if (pic_eq_p(pic, proc, sQUOTE)) {
      int pidx;
      pidx = pic->plen++;
      pic->pool[pidx] = pic_car(pic, pic_cdr(pic, obj));
      irep->code[irep->clen].insn = OP_PUSHCONST;
      irep->code[irep->clen].u.i = pidx;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCONS)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_CONS;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCAR)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_CAR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCDR)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_CDR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sNILP)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_NILP;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sADD)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_ADD;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sSUB)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_SUB;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sMUL)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_MUL;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sDIV)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), scope);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), scope);
      irep->code[irep->clen].insn = OP_DIV;
      irep->clen++;
      break;
    }
    else {
      pic_gen_call(pic, irep, obj, scope);
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
pic_gen_call(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_scope *scope)
{
  pic_value seq;
  int i = 0;

  seq = pic_reverse(pic, obj);
  for (; ! pic_nil_p(seq); seq = pic_cdr(pic, seq)) {
    pic_value v;

    v = pic_car(pic, seq);
    pic_gen(pic, irep, v, scope);
    ++i;
  }
  irep->code[irep->clen].insn = OP_CALL;
  irep->code[irep->clen].u.i = i;
  irep->clen++;
}

static struct pic_irep *
pic_gen_lambda(pic_state *pic, pic_value obj, struct pic_scope *scope)
{
  struct pic_scope *new_scope;
  pic_value args, body, v;
  struct pic_irep *irep;

  irep = new_irep(pic);

  /* arguments */
  args = pic_car(pic, pic_cdr(pic, obj));
  new_scope = new_local_scope(pic, args, scope);

  /* body */
  body = pic_cdr(pic, pic_cdr(pic, obj));
  for (v = body; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_gen(pic, irep, pic_car(pic, v), scope);
    irep->code[irep->clen].insn = OP_POP;
    irep->clen++;
  }
  irep->clen--;
  irep->code[irep->clen].insn = OP_RET;
  irep->clen++;

  destory_scope(pic, new_scope);

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
  struct pic_scope *global_scope;
  struct pic_proc *proc;
  struct pic_irep *irep;

  global_scope = new_global_scope(pic);

  irep = new_irep(pic);
  proc = pic_proc_new(pic, irep);

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
  pic_gen(pic, irep, obj, global_scope);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

  destory_scope(pic, global_scope);

#if VM_DEBUG
  print_irep(pic, irep);
#endif

  return proc;
}
