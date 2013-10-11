#include <stdlib.h>

#include "picrin.h"

enum pic_instruction {
  OP_PUSHI,
  OP_ADD,
  OP_STOP
};

struct pic_code {
  enum pic_instruction insn;
  union {
    int i;
  } u;
};

struct pic_proc {
  union {
    struct pic_code *code;
  } u;
};

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  pic_value *sp;

  pc = proc->u.code;
  sp = pic->sp;

  while (1) {
    switch (pc->insn) {
    case OP_PUSHI: {
      *++sp = pic_int_value(pc->u.i);
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

int
main()
{
  pic_state *pic;
  struct pic_proc proc;
  struct pic_code *code;
  pic_value v;

  pic = pic_open();

  proc.u.code = code = (struct pic_code *)malloc(sizeof(struct pic_code) * 256);
  code[0].insn = OP_PUSHI;
  code[0].u.i = 1;
  code[1].insn = OP_PUSHI;
  code[1].u.i = 2;
  code[2].insn = OP_ADD;
  code[3].insn = OP_STOP;

  v = pic_run(pic, &proc, pic_nil_value());

  pic_debug(pic, v);

  pic_close(pic);

  return 0;
}
