#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/pair.h"

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
  pic_value cell;
  int d, idx;

  if (env_lookup(pic, sym, pic->global_env, &d, &idx)) {
    return idx;
  }

  idx = pic->glen++;
  cell = pic_cons(pic, sym, pic_float_value(idx));
  pic->global_env->assoc = pic_cons(pic, cell, pic->global_env->assoc);

  return idx;
}

static struct pic_env *
env_new(pic_state *pic, pic_value args, struct pic_env *env)
{
  struct pic_env *inner_env;
  pic_value v, cell;
  int i;

  inner_env = (struct pic_env *)pic_alloc(pic, sizeof(struct pic_env));
  inner_env->assoc = pic_nil_value();
  inner_env->parent = env;

  i = -1;
  for (v = args; ! pic_nil_p(v); v = pic_cdr(pic, v)) {
    pic_value sym = pic_car(pic, v);

    cell = pic_cons(pic, sym, pic_float_value(i--));
    inner_env->assoc = pic_cons(pic, cell, inner_env->assoc);
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

void
pic_get_args(pic_state *pic, const char *format, ...)
{
  char c;
  int i = -1;
  va_list ap;

  va_start(ap, format);
  while ((c = *format++)) {
    switch (c) {
    case 'o':
      {
	pic_value *p;

	p = va_arg(ap, pic_value*);
	*p = pic->sp[i];
	i--;
      }
      break;
    case 'f':
      {
	double *f;

	f = va_arg(ap, double *);
	*f = pic_float(pic->sp[i]);
	i--;
      }
      break;
    }
  }
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
      pic_raise(pic, "unbound variable");
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
      pic_raise(pic, "reference to closed variable not supported");
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
    pic_raise(pic, "invalid expression given");
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

  pic_gen(pic, irep, obj, env);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

#if VM_DEBUG
  print_irep(pic, irep);
#endif

  return proc;
}

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x:
# define NEXT ++pc; JUMP;
# define JUMP goto *oplabels[pc->insn];
# define VM_LOOP_END
#else
# define VM_LOOP for (;;) { switch (pc->insn) {
# define CASE(x) case x:
# define NEXT pc++; break
# define JUMP break
# define VM_LOOP_END } }
#endif

#define PUSH(v) (*++pic->sp = (v))
#define POP() (*pic->sp--)
#define POPN(i) ((void)(pic->sp-=i))

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  pic_value val;
  int ai = pic_gc_arena_preserve(pic);

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE, &&L_OP_PUSHNUM,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_JMP, &&L_OP_JMPIF,
    &&L_OP_CALL, &&L_OP_RET, &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR,
    &&L_OP_NILP, &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_STOP
  };
#endif

  pc = proc->u.irep->code;

  /* adjust call frame */
  pic->sp[0] = pic_obj_value(proc);
  pic->ci->argc = 1;
  pic->ci->pc = NULL;
  pic->ci->sp = NULL;

  VM_LOOP {
    CASE(OP_POP) {
      POPN(1);
      NEXT;
    }
    CASE(OP_PUSHNIL) {
      PUSH(pic_nil_value());
      NEXT;
    }
    CASE(OP_PUSHTRUE) {
      PUSH(pic_true_value());
      NEXT;
    }
    CASE(OP_PUSHFALSE) {
      PUSH(pic_false_value());
      NEXT;
    }
    CASE(OP_PUSHNUM) {
      PUSH(pic_float_value(pc->u.f));
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(pic->globals[pc->u.i]);
      NEXT;
    }
    CASE(OP_GSET) {
      pic->globals[pc->u.i] = POP();
      NEXT;
    }
    CASE(OP_LREF) {
      PUSH(pic->ci->sp[pc->u.i]);
      NEXT;
    }
    CASE(OP_JMP) {
      pc += pc->u.i;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(v)) {
	pc += pc->u.i;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value c, v;
      pic_callinfo *ci;
      struct pic_proc *proc;

      c = pic->sp[0];
      proc = pic_proc_ptr(c);
      ci = PUSHCI();
      ci->argc = pc->u.i;
      ci->pc = pc;
      ci->sp = pic->sp;
      if (pic_proc_cfunc_p(c)) {
	v = proc->u.cfunc(pic);
	pic->sp -= ci->argc;
	POPCI();
	PUSH(v);
	pic_gc_arena_restore(pic, ai);
	NEXT;
      }
      else {
	pc = proc->u.irep->code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
    }
    CASE(OP_RET) {
      pic_value v;
      pic_callinfo *ci;

      v = POP();
      ci = POPCI();
      pc = ci->pc;
      pic->sp -= ci->argc;
      PUSH(v);
      NEXT;
    }
    CASE(OP_LAMBDA) {
      struct pic_proc *proc;

      proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc *), PIC_TT_PROC);
      proc->cfunc_p = false;
      proc->u.irep = pic->irep[pc->u.i];
      PUSH(pic_obj_value(proc));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CONS) {
      pic_value a, b;
      pic_gc_protect(pic, a = POP());
      pic_gc_protect(pic, b = POP());
      PUSH(pic_cons(pic, a, b));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CAR) {
      pic_value p;
      p = POP();
      PUSH(pic_car(pic, p));
      NEXT;
    }
    CASE(OP_CDR) {
      pic_value p;
      p = POP();
      PUSH(pic_cdr(pic, p));
      NEXT;
    }
    CASE(OP_NILP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic_nil_p(p)));
      NEXT;
    }
    CASE(OP_ADD) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) + pic_float(b)));
      NEXT;
    }
    CASE(OP_SUB) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) - pic_float(b)));
      NEXT;
    }
    CASE(OP_MUL) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) * pic_float(b)));
      NEXT;
    }
    CASE(OP_DIV) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) / pic_float(b)));
      NEXT;
    }
    CASE(OP_STOP) {
      goto STOP;
    }
  } VM_LOOP_END;

 STOP:
  val = POP();

#if GC_DEBUG
  puts("**VM END STATE**");
  printf("stbase = %p\nsp = %p\n", pic->stbase, pic->sp);
  printf("cibase = %p\nci = %p\n", pic->cibase, pic->ci);
  if (pic->stbase != pic->sp) {
    pic_value *sp;
    printf("* stack trace:");
    for (sp = pic->stbase; pic->sp != sp; ++sp) {
      pic_debug(pic, *sp);
      puts("");
    }
  }
#endif

  return val;
}

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
