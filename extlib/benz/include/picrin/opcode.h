/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OPCODE_H
#define PICRIN_OPCODE_H

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_opcode {
  OP_NOP,
  OP_POP,
  OP_PUSHUNDEF,
  OP_PUSHNIL,
  OP_PUSHTRUE,
  OP_PUSHFALSE,
  OP_PUSHINT,
  OP_PUSHFLOAT,
  OP_PUSHCHAR,
  OP_PUSHEOF,
  OP_PUSHCONST,
  OP_GREF,
  OP_GSET,
  OP_LREF,
  OP_LSET,
  OP_CREF,
  OP_CSET,
  OP_JMP,
  OP_JMPIF,
  OP_NOT,
  OP_CALL,
  OP_TAILCALL,
  OP_RET,
  OP_LAMBDA,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_NILP,
  OP_SYMBOLP,
  OP_PAIRP,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_EQ,
  OP_LT,
  OP_LE,
  OP_GT,
  OP_GE,
  OP_STOP
};

#define PIC_INIT_CODE_I(code, op, ival) do {    \
    code.insn = op;                             \
    code.a = ival;                              \
  } while (0)

#if DEBUG

PIC_INLINE void
pic_dump_code(pic_code c)
{
  switch (c.insn) {
  case OP_NOP:
    puts("OP_NOP");
    break;
  case OP_POP:
    puts("OP_POP");
    break;
  case OP_PUSHUNDEF:
    puts("OP_PUSHUNDEF");
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
  case OP_PUSHINT:
    printf("OP_PUSHINT\t%d\n", c.a);
    break;
  case OP_PUSHFLOAT:
    printf("OP_PUSHFLAOT\t%d\n", c.a);
    break;
  case OP_PUSHCHAR:
    printf("OP_PUSHCHAR\t%c\n", c.a);
    break;
  case OP_PUSHEOF:
    puts("OP_PUSHEOF");
    break;
  case OP_PUSHCONST:
    printf("OP_PUSHCONST\t%d\n", c.a);
    break;
  case OP_GREF:
    printf("OP_GREF\t%i\n", c.a);
    break;
  case OP_GSET:
    printf("OP_GSET\t%i\n", c.a);
    break;
  case OP_LREF:
    printf("OP_LREF\t%d\n", c.a);
    break;
  case OP_LSET:
    printf("OP_LSET\t%d\n", c.a);
    break;
  case OP_CREF:
    printf("OP_CREF\t%d\t%d\n", c.a, c.b);
    break;
  case OP_CSET:
    printf("OP_CSET\t%d\t%d\n", c.a, c.b);
    break;
  case OP_JMP:
    printf("OP_JMP\t%x\n", c.a);
    break;
  case OP_JMPIF:
    printf("OP_JMPIF\t%x\n", c.a);
    break;
  case OP_NOT:
    puts("OP_NOT");
    break;
  case OP_CALL:
    printf("OP_CALL\t%d\n", c.a);
    break;
  case OP_TAILCALL:
    printf("OP_TAILCALL\t%d\n", c.a);
    break;
  case OP_RET:
    puts("OP_RET");
    break;
  case OP_LAMBDA:
    printf("OP_LAMBDA\t%d\n", c.a);
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
  case OP_SYMBOLP:
    puts("OP_SYMBOLP");
    break;
  case OP_PAIRP:
    puts("OP_PAIRP");
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
  case OP_EQ:
    puts("OP_EQ");
    break;
  case OP_LT:
    puts("OP_LT");
    break;
  case OP_LE:
    puts("OP_LE");
    break;
  case OP_GT:
    puts("OP_GT");
    break;
  case OP_GE:
    puts("OP_GE");
    break;
  case OP_STOP:
    puts("OP_STOP");
    break;
  }
}

PIC_INLINE void
pic_dump_irep(struct pic_irep *irep)
{
  size_t i;

  printf("## irep %p\n", (void *)irep);
  printf("# argc     = %d\n", irep->argc);
  printf("# localc   = %d\n", irep->localc);
  printf("# capturec = %d\n", irep->capturec);

  for (i = 0; i < irep->ncode; ++i) {
    printf("%02x: ", i);
    pic_dump_code(irep->u.s.code[i]);
  }

  for (i = 0; i < irep->nirep; ++i) {
    pic_dump_irep(irep->u.s.irep[i].i);
  }
}

#endif

#if defined(__cplusplus)
}
#endif

#endif
