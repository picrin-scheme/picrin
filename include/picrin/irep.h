#ifndef IREP_H__
#define IREP_H__

enum pic_opcode {
  OP_POP,
  OP_PUSHNIL,
  OP_PUSHTRUE,
  OP_PUSHFALSE,
  OP_PUSHFLOAT,
  OP_PUSHINT,
  OP_PUSHCONST,
  OP_GREF,
  OP_GSET,
  OP_LREF,
  OP_LSET,
  OP_CREF,
  OP_CSET,
  OP_JMP,
  OP_JMPIF,
  OP_CALL,
  OP_TAILCALL,
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
  OP_EQ,
  OP_LT,
  OP_LE,
  OP_STOP
};

struct pic_code {
  enum pic_opcode insn;
  union {
    double f;
    int i;
    struct {
      short depth;
      short idx;
    } c;
  } u;
};

struct pic_irep {
  struct pic_code *code;
  size_t clen, ccapa;
  int argc;
  bool varg;
};

void print_irep(pic_state *, struct pic_irep *);

#endif
