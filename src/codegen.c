#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/irep.h"
#include "picrin/proc.h"

static bool
env_lookup(pic_state *pic, pic_value sym, struct pic_env *env, int *depth, int *idx)
{
  pic_value v;
  int d = 0;

 enter:

  v = pic_assq(pic, sym, env->assoc);
  if (! pic_nil_p(v)) {
    if (env->parent == NULL) {		/* global */
      *depth = -1;
    }
    else {			/* non-global */
      *depth = d;
    }
    *idx = (int)pic_float(pic_pair_ptr(v)->cdr);
    return true;
  }
  if (env->parent) {
    env = env->parent;
    ++d;
    goto enter;
  }
  return false;
}

static int
env_global_define(pic_state *pic, pic_value sym)
{
  pic_value f;
  int d, idx;

  if (env_lookup(pic, sym, pic->global_env, &d, &idx)) {
    return idx;
  }

  idx = pic->glen++;
  f = pic_float_value(idx);
  pic->global_env->assoc = pic_acons(pic, sym, f, pic->global_env->assoc);

  return idx;
}

static struct pic_env *
env_new(pic_state *pic, pic_value args, struct pic_env *env)
{
  struct pic_env *inner_env;
  pic_value v, f;
  int i;

  inner_env = (struct pic_env *)pic_alloc(pic, sizeof(struct pic_env));
  inner_env->assoc = pic_nil_value();
  inner_env->parent = env;

  i = -1;
  for (v = args; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_value sym = pic_car(pic, v);

    f = pic_float_value(i--);
    inner_env->assoc = pic_acons(pic, sym, f, inner_env->assoc);
  }

  return inner_env;
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;
  int idx;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->cfunc_p = true;
  proc->u.cfunc = cfunc;
  idx = env_global_define(pic, pic_intern_cstr(pic, name));
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

static void pic_gen_call(pic_state *, struct pic_irep *, pic_value, struct pic_env *);
static struct pic_irep *pic_gen_lambda(pic_state *, pic_value, struct pic_env *);

static void
pic_gen(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_env *env)
{
  pic_value sDEFINE, sLAMBDA, sIF, sBEGIN;
  pic_value sCONS, sCAR, sCDR, sNILP;
  pic_value sADD, sSUB, sMUL, sDIV;

  sDEFINE = pic->sDEFINE;
  sLAMBDA = pic->sLAMBDA;
  sIF = pic->sIF;
  sBEGIN = pic->sBEGIN;
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

    b = env_lookup(pic, obj, env, &depth, &idx);
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

      idx = env_global_define(pic, pic_car(pic, pic_cdr(pic, obj)));

      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);

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

      pic->irep[pic->ilen++] = pic_gen_lambda(pic, obj, env);
      break;
    }
    else if (pic_eq_p(pic, proc, sIF)) {
      int s,t;

      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);

      irep->code[irep->clen].insn = OP_JMPIF;
      s = irep->clen++;

      /* if false branch */
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, pic_cdr(pic, obj)))), env);
      irep->code[irep->clen].insn = OP_JMP;
      t = irep->clen++;

      irep->code[s].u.i = irep->clen - s;

      /* if true branch */
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      irep->code[t].u.i = irep->clen - t;
      break;
    }
    else if (pic_eq_p(pic, proc, sBEGIN)) {
      pic_value v, seq;

      seq = pic_cdr(pic, obj);
      for (v = seq; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
	pic_gen(pic, irep, pic_car(pic, v), env);
	irep->code[irep->clen].insn = OP_POP;
	irep->clen++;
      }
      irep->clen--;
      break;
    }
    else if (pic_eq_p(pic, proc, sCONS)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_CONS;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCAR)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_CAR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCDR)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_CDR;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sNILP)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_NILP;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sADD)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_ADD;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sSUB)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_SUB;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sMUL)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_MUL;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sDIV)) {
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_DIV;
      irep->clen++;
      break;
    }
    else {
      pic_gen_call(pic, irep, obj, env);
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
  case PIC_TT_PROC:
  case PIC_TT_UNDEF:
  case PIC_TT_PORT: {
    pic_error(pic, "invalid expression given");
  }
  }
}

static pic_value
reverse(pic_state *pic, pic_value list, pic_value acc)
{
  if (pic_nil_p(list))
    return acc;
  return reverse(pic, pic_cdr(pic, list), pic_cons(pic, pic_car(pic, list), acc));
}

static void
pic_gen_call(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_env *env)
{
  pic_value seq;
  int i = 0;

  seq = reverse(pic, obj, pic_nil_value());
  for (; ! pic_nil_p(seq); seq = pic_cdr(pic, seq)) {
    pic_value v;

    v = pic_car(pic, seq);
    pic_gen(pic, irep, v, env);
    ++i;
  }
  irep->code[irep->clen].insn = OP_CALL;
  irep->code[irep->clen].u.i = i;
  irep->clen++;
}

static struct pic_irep *
pic_gen_lambda(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_env *inner_env;
  pic_value args, body, v;
  struct pic_irep *irep;

  irep = new_irep(pic);

  /* arguments */
  args = pic_car(pic, pic_cdr(pic, obj));
  inner_env = env_new(pic, args, env);

  /* body */
  body = pic_cdr(pic, pic_cdr(pic, obj));
  for (v = body; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_gen(pic, irep, pic_car(pic, v), inner_env);
    irep->code[irep->clen].insn = OP_POP;
    irep->clen++;
  }
  irep->clen--;
  irep->code[irep->clen].insn = OP_RET;
  irep->clen++;
  pic_free(pic, inner_env);

#if VM_DEBUG
  printf("LAMBDA_%zd:\n", pic->ilen);
  print_irep(pic, irep);
  puts("");
#endif

  return irep;
}

struct pic_proc *
pic_codegen(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_proc *proc;
  struct pic_irep *irep;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->cfunc_p = false;
  proc->u.irep = irep = new_irep(pic);

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
  pic_gen(pic, irep, obj, env);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

#if VM_DEBUG
  print_irep(pic, irep);
#endif

  return proc;
}
