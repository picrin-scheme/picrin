#include <stdlib.h>

#include "picrin.h"

enum pic_instruction {
  OP_PUSHNIL,
  OP_PUSHI,
  OP_PUSHUNDEF,
  OP_CONS,
  OP_ADD,
  OP_STOP
};

struct pic_code {
  enum pic_instruction insn;
  union {
    int i;
  } u;
};

struct pic_irep {
  struct pic_code *code;
  size_t clen, ccapa;
};

static void
pic_gen(pic_state *pic, struct pic_irep *irep, pic_value obj, struct pic_env *env)
{
  pic_value sCONS, sADD;

  sCONS = pic_intern_cstr(pic, "cons");
  sADD = pic_intern_cstr(pic, "add");

  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    /* not implemented */
    break;
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    proc = pic_car(pic, obj);
    if (pic_eq_p(pic, proc, sCONS)) {
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
  }
}

struct pic_proc *
pic_codegen(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_proc *proc;
  struct pic_irep *irep;
  struct pic_code *code;

  proc = pic_alloc(pic, sizeof(struct pic_proc));

  proc->u.irep = irep = (struct pic_irep *)malloc(sizeof(struct pic_irep));
  irep->code = code = (struct pic_code *)malloc(sizeof(struct pic_code) * 1024);
  irep->clen = 0;
  irep->ccapa = 1024;

  pic_gen(pic, irep, obj, env);
  irep->code[irep->clen].insn = OP_STOP;
  irep->clen++;

  return proc;
}

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  pic_value *sp;

  pc = proc->u.irep->code;
  sp = pic->sp;

  while (1) {
    switch (pc->insn) {
    case OP_PUSHNIL: {
      *++sp = pic_nil_value();
      break;
    }
    case OP_PUSHI: {
      *++sp = pic_int_value(pc->u.i);
      break;
    }
    case OP_PUSHUNDEF: {
      *++sp = pic_undef_value();
      break;
    }
    case OP_CONS: {
      pic_value a, b;
      a = *sp--;
      b = *sp--;
      *++sp = pic_cons(pic, a, b);
      break;
    }
    case OP_ADD: {
      pic_value a, b;
      a = *sp--;
      b = *sp--;
      *++sp = pic_int_value(pic_int(a) + pic_int(b));
      break;
    }
    case OP_STOP:
      goto STOP;
    }
    pc++;
  }

 STOP:
  return *sp;
}

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
