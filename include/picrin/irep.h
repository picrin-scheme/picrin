#ifndef IREP_H__
#define IREP_H__

enum pic_opcode {
  OP_POP,
  OP_PUSHNIL,
  OP_PUSHTRUE,
  OP_PUSHFALSE,
  OP_PUSHNUM,
  OP_PUSHCONST,
  OP_GREF,
  OP_GSET,
  OP_LREF,
  OP_JMP,
  OP_JMPIF,
  OP_CALL,
  OP_RET,
  OP_LAMBDA,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_NILP,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_STOP
};

struct pic_code {
  enum pic_opcode insn;
  union {
    double f;
    int i;
  } u;
};

struct pic_irep {
  struct pic_code *code;
  size_t clen, ccapa;
  int argc;
};

#endif
