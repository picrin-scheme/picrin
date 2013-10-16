#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/irep.h"
#include "picrin/proc.h"

static pic_value
pic_assq(pic_state *pic, pic_value key, pic_value assoc)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return assoc;

  cell = pic_car(pic, assoc);
  if (pic_eq_p(pic, key, pic_car(pic, cell)))
    return cell;

  assoc = pic_cdr(pic, assoc);
  goto enter;
}

static struct pic_pair *
pic_env_lookup(pic_state *pic, pic_value sym, struct pic_env *env)
{
  pic_value v;

 enter:

  v = pic_assq(pic, sym, env->assoc);
  if (! pic_nil_p(v)) {
    return pic_pair_ptr(v);
  }
  if (env->parent) {
    env = env->parent;
    goto enter;
  }

  return NULL;
}

static struct pic_pair *
pic_env_define(pic_state *pic, pic_value sym, struct pic_env *env)
{
  pic_value cell;

  cell = pic_cons(pic, sym, pic_undef_value());
  env->assoc = pic_cons(pic, cell, env->assoc);

  return pic_pair_ptr(cell);
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;
  struct pic_pair *cell;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->cfunc_p = true;
  proc->u.cfunc = cfunc;
  cell = pic_env_define(pic, pic_intern_cstr(pic, name), pic->global_env);
  cell->cdr = pic_obj_value(proc);
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
    case OP_PUSHNIL:
      puts("OP_PUSHNIL");
      break;
    case OP_PUSHNUM:
      printf("OP_PUSHNUM\t%g\n", irep->code[i].u.f);
      break;
    case OP_PUSHUNDEF:
      puts("OP_PUSHUNDEF");
      break;
    case OP_GREF:
      printf("OP_GREF\t%p\n", irep->code[i].u.gvar);
      break;
    case OP_GSET:
      printf("OP_GSET\t%p\n", irep->code[i].u.gvar);
      break;
    case OP_CALL:
      printf("OP_CALL\t%d\n", irep->code[i].u.i);
      break;
    case OP_CONS:
      puts("OP_CONS");
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

static void pic_gen_call(pic_state *, struct pic_irep *, pic_value, struct pic_env *);

static void
pic_gen(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_env *env)
{
  pic_value sDEFINE, sCONS, sADD, sSUB, sMUL, sDIV;

  sDEFINE = pic->sDEFINE;
  sCONS = pic->sCONS;
  sADD = pic->sADD;
  sSUB = pic->sSUB;
  sMUL = pic->sMUL;
  sDIV = pic->sDIV;

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    struct pic_pair *gvar;

    gvar = pic_env_lookup(pic, obj, env);
    if (! gvar) {
      pic_raise(pic, "unbound variable");
    }
    irep->code[irep->clen].insn = OP_GREF;
    irep->code[irep->clen].u.gvar = gvar;
    irep->clen++;
    break;
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    proc = pic_car(pic, obj);
    if (pic_eq_p(pic, proc, sDEFINE)) {
      struct pic_pair *gvar;

      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);

      gvar = pic_env_define(pic, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_GSET;
      irep->code[irep->clen].u.gvar = gvar;
      irep->clen++;
      irep->code[irep->clen].insn = OP_PUSHUNDEF;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sCONS)) {
      /* generate args in reverse order*/
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_CONS;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sADD)) {
      /* generate args in reverse order*/
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_ADD;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sSUB)) {
      /* generate args in reverse order*/
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_SUB;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sMUL)) {
      /* generate args in reverse order*/
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj))), env);
      pic_gen(pic, irep, pic_car(pic, pic_cdr(pic, obj)), env);
      irep->code[irep->clen].insn = OP_MUL;
      irep->clen++;
      break;
    }
    else if (pic_eq_p(pic, proc, sDIV)) {
      /* generate args in reverse order*/
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
  case PIC_TT_UNDEF: {
    irep->code[irep->clen].insn = OP_PUSHUNDEF;
    irep->clen++;
    break;
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
  irep->code[irep->clen].u.i = i - 1;
  irep->clen++;
}

struct pic_proc *
pic_codegen(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_proc *proc;
  struct pic_irep *irep;
  struct pic_code *code;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);

  proc->cfunc_p = false;
  proc->u.irep = irep = (struct pic_irep *)pic_alloc(pic, sizeof(struct pic_irep));
  irep->code = code = (struct pic_code *)pic_alloc(pic, sizeof(struct pic_code) * 1024);
  irep->clen = 0;
  irep->ccapa = 1024;

  pic_gen(pic, irep, obj, env);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

#if VM_DEBUG
  print_irep(pic, irep);
#endif

  return proc;
}

#define VM_LOOP for (;;) { switch (pc->insn) {
#define CASE(x) case x:
#define NEXT pc++; break
#define JUMP break
#define VM_LOOP_END } }

#define PUSH(v) (*pic->sp++ = (v))
#define POP() (*--pic->sp)

#define PUSHCI() (pic->ci++)
#define POPCI() (--pic->ci)

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  int ai = pic_gc_arena_preserve(pic);

  pc = proc->u.irep->code;

  PUSHCI();
  pic->ci->proc = proc;
  pic->ci->argc = 0;

  VM_LOOP {
    CASE(OP_PUSHNIL) {
      PUSH(pic_nil_value());
      NEXT;
    }
    CASE(OP_PUSHNUM) {
      PUSH(pic_float_value(pc->u.f));
      NEXT;
    }
    CASE(OP_PUSHUNDEF) {
      PUSH(pic_undef_value());
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(pc->u.gvar->cdr);
      NEXT;
    }
    CASE(OP_GSET) {
      pc->u.gvar->cdr = POP();
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value c;
      struct pic_proc *proc;
      pic_callinfo *ci;

      pic_gc_protect(pic, c = POP());
      proc = pic_proc_ptr(c);
      ci = PUSHCI();
      ci->proc = proc;
      ci->argc = pc->u.i;
      PUSH(proc->u.cfunc(pic));
      pic->sp -= ci->argc;
      POPCI();
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
  POPCI();
  return POP();
}

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
