/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VM_H
#define PICRIN_VM_H

#if defined(__cplusplus)
extern "C" {
#endif

enum {
  OP_NOP       = 0,
  OP_POP       = 1,
  OP_PUSHUNDEF = 2,
  OP_PUSHNIL   = 3,
  OP_PUSHTRUE  = 4,
  OP_PUSHFALSE = 5,
  OP_PUSHINT   = 6,
  OP_PUSHFLOAT = 7,
  OP_PUSHCHAR  = 8,
  OP_PUSHEOF   = 9,
  OP_PUSHCONST = 10,
  OP_GREF      = 11,
  OP_GSET      = 12,
  OP_LREF      = 13,
  OP_LSET      = 14,
  OP_CREF      = 15,
  OP_CSET      = 16,
  OP_JMP       = 17,
  OP_JMPIF     = 18,
  OP_NOT       = 19,
  OP_CALL      = 20,
  OP_TAILCALL  = 21,
  OP_RET       = 22,
  OP_LAMBDA    = 23,
  OP_CONS      = 24,
  OP_CAR       = 25,
  OP_CDR       = 26,
  OP_NILP      = 27,
  OP_SYMBOLP   = 28,
  OP_PAIRP     = 29,
  OP_ADD       = 30,
  OP_SUB       = 31,
  OP_MUL       = 32,
  OP_DIV       = 33,
  OP_EQ        = 34,
  OP_LT        = 35,
  OP_LE        = 36,
  OP_GT        = 37,
  OP_GE        = 38,
  OP_STOP      = 39
};

#if defined(__cplusplus)
}
#endif

#endif
