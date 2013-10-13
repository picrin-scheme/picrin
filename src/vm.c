#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

enum pic_instruction {
  OP_PUSHNIL,
  OP_PUSHI,
  OP_PUSHUNDEF,
  OP_GREF,
  OP_GSET,
  OP_CONS,
  OP_ADD,
  OP_STOP
};

struct pic_code {
  enum pic_instruction insn;
  union {
    int i;
    struct pic_pair *gvar;
  } u;
};

struct pic_irep {
  struct pic_code *code;
  size_t clen, ccapa;
};

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

static void
pic_gen(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_env *env)
{
  pic_value sDEFINE, sCONS, sADD;

  sDEFINE = pic_intern_cstr(pic, "define");
  sCONS = pic_intern_cstr(pic, "cons");
  sADD = pic_intern_cstr(pic, "add");

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
    else {
      /* not implemented */
      break;
    }
  }
  case PIC_TT_INT: {
    irep->code[irep->clen].insn = OP_PUSHI;
    irep->code[irep->clen].u.i = pic_int(obj);
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

struct pic_proc *
pic_codegen(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_proc *proc;
  struct pic_irep *irep;
  struct pic_code *code;

  proc = (struct pic_proc *)pic_gc_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);

  proc->u.irep = irep = (struct pic_irep *)malloc(sizeof(struct pic_irep));
  irep->code = code = (struct pic_code *)malloc(sizeof(struct pic_code) * 1024);
  irep->clen = 0;
  irep->ccapa = 1024;

  pic_gen(pic, irep, obj, env);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

  return proc;
}

#define VM_LOOP for (;;) { switch (pc->insn) {
#define CASE(x) case x:
#define NEXT pc++; break
#define JUMP break
#define VM_LOOP_END } }

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  pic_value *sp;

  pc = proc->u.irep->code;
  sp = pic->sp;

  VM_LOOP {
    CASE(OP_PUSHNIL) {
      *++sp = pic_nil_value();
      NEXT;
    }
    CASE(OP_PUSHI) {
      *++sp = pic_int_value(pc->u.i);
      NEXT;
    }
    CASE(OP_PUSHUNDEF) {
      *++sp = pic_undef_value();
      NEXT;
    }
    CASE(OP_GREF) {
      *++sp = pc->u.gvar->cdr;
      NEXT;
    }
    CASE(OP_GSET) {
      pc->u.gvar->cdr = *sp--;
      NEXT;
    }
    CASE(OP_CONS) {
      pic_value a, b;
      a = *sp--;
      b = *sp--;
      *++sp = pic_cons(pic, a, b);
      NEXT;
    }
    CASE(OP_ADD) {
      pic_value a, b;
      a = *sp--;
      b = *sp--;
      *++sp = pic_int_value(pic_int(a) + pic_int(b));
      NEXT;
    }
    CASE(OP_STOP) {
      goto STOP;
    }
  } VM_LOOP_END;

 STOP:
  return *sp;
}

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
