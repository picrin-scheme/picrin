#ifndef IREP_H__
#define IREP_H__

enum pic_instruction {
  OP_PUSHNIL,
  OP_PUSHNUM,
  OP_PUSHUNDEF,
  OP_GREF,
  OP_GSET,
  OP_CALL,
  OP_CONS,
  OP_ADD,
  OP_STOP
};

struct pic_code {
  enum pic_instruction insn;
  union {
    double f;
    int i;
    struct pic_pair *gvar;
  } u;
};

struct pic_irep {
  struct pic_code *code;
  size_t clen, ccapa;
};

#endif
