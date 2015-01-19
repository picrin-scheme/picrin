/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_IREP_H
#define PICRIN_IREP_H

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_opcode {
  OP_NOP,
  OP_POP,
  OP_PUSHNIL,
  OP_PUSHTRUE,
  OP_PUSHFALSE,
  OP_PUSHINT,
  OP_PUSHCHAR,
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
  OP_MINUS,
  OP_EQ,
  OP_LT,
  OP_LE,
  OP_STOP
};

struct pic_code {
  enum pic_opcode insn;
  union {
    int i;
    char c;
    struct {
      int depth;
      int idx;
    } r;
  } u;
};

struct pic_irep {
  PIC_OBJECT_HEADER
  pic_sym name;
  pic_code *code;
  int argc, localc, capturec;
  bool varg;
  struct pic_irep **irep;
  pic_value *pool;
  pic_sym *syms;
  size_t clen, ilen, plen, slen;
};

pic_value pic_analyze(pic_state *, pic_value);
struct pic_irep *pic_codegen(pic_state *, pic_value);

static inline void
pic_dump_code(pic_code c)
{
  printf("[%2d] ", c.insn);
  switch (c.insn) {
  case OP_NOP:
    puts("OP_NOP");
    break;
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
  case OP_PUSHINT:
    printf("OP_PUSHINT\t%d\n", c.u.i);
    break;
  case OP_PUSHCHAR:
    printf("OP_PUSHCHAR\t%c\n", c.u.c);
    break;
  case OP_PUSHCONST:
    printf("OP_PUSHCONST\t%d\n", c.u.i);
    break;
  case OP_GREF:
    printf("OP_GREF\t%i\n", c.u.i);
    break;
  case OP_GSET:
    printf("OP_GSET\t%i\n", c.u.i);
    break;
  case OP_LREF:
    printf("OP_LREF\t%d\n", c.u.i);
    break;
  case OP_LSET:
    printf("OP_LSET\t%d\n", c.u.i);
    break;
  case OP_CREF:
    printf("OP_CREF\t%d\t%d\n", c.u.r.depth, c.u.r.idx);
    break;
  case OP_CSET:
    printf("OP_CSET\t%d\t%d\n", c.u.r.depth, c.u.r.idx);
    break;
  case OP_JMP:
    printf("OP_JMP\t%x\n", c.u.i);
    break;
  case OP_JMPIF:
    printf("OP_JMPIF\t%x\n", c.u.i);
    break;
  case OP_NOT:
    puts("OP_NOT");
    break;
  case OP_CALL:
    printf("OP_CALL\t%d\n", c.u.i);
    break;
  case OP_TAILCALL:
    printf("OP_TAILCALL\t%d\n", c.u.i);
    break;
  case OP_RET:
    printf("OP_RET\t%d\n", c.u.i);
    break;
  case OP_LAMBDA:
    printf("OP_LAMBDA\t%d\n", c.u.i);
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

static inline void
pic_dump_irep(struct pic_irep *irep)
{
  unsigned i;

  printf("## irep %p\n", (void *)irep);
  printf("[clen = %zd, argc = %d, localc = %d, capturec = %d]\n", irep->clen, irep->argc, irep->localc, irep->capturec);
  for (i = 0; i < irep->clen; ++i) {
    printf("%02x ", i);
    pic_dump_code(irep->code[i]);
  }

  for (i = 0; i < irep->ilen; ++i) {
    pic_dump_irep(irep->irep[i]);
  }
}

#if defined(__cplusplus)
}
#endif

#endif
