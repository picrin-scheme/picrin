/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_IREP_H__
#define PICRIN_IREP_H__

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_opcode {
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
      short depth;
      short idx;
    } r;
  } u;
};

struct pic_irep {
  PIC_OBJECT_HEADER
  struct pic_code *code;
  int argc, localc;
  unsigned *cv_tbl, cv_num;
  bool varg;
  struct pic_irep **irep;
  pic_value *pool;
  size_t clen, ilen, plen;
};

pic_value pic_analyze(pic_state *, pic_value);
struct pic_irep *pic_codegen(pic_state *pic, pic_value obj);

void pic_dump_irep(pic_state *, struct pic_irep *);

#if defined(__cplusplus)
}
#endif

#endif
