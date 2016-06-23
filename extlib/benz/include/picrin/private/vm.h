/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VM_H
#define PICRIN_VM_H

#if defined(__cplusplus)
extern "C" {
#endif

enum {
  OP_CONST,                     /* CONST   i, const  */
  OP_AREF,                      /* AREF    i, n      */
  OP_ASET,                      /* ASET    i, n      */
  OP_LREF,                      /* LREF    i, n, d   */
  OP_LSET,                      /* LSET    i, n, d   */
  OP_GREF,                      /* GREF    i, symb   */
  OP_GSET,                      /* GSET    i, symb   */
  OP_LAMBDA,                    /* LAMBDA  i, irep   */
  OP_IF,                        /* IF      i, k1, k2 */
  OP_CALL,                      /* CALL    i, n      */
  OP_HALT                       /* HALT    i         */
};

#define MKOP1(OP,A,B,C) 0
#define MKOP2(OP,A,Bx)
#define MKOP3(OP,A,B) MKOP1(OP,A,B,0)

#define GETOP(OP) (OP & 0xff)
#define GETA(OP)  (OP >> 24)
#define GETB(OP)  ((OP >> 16) & 0xff)
#define GETBx(OP) ((OP >> 8) & 0xffff)
#define GETC(OP)  ((OP >> 8) & 0xff)

typedef unsigned char uchar;

struct list_head {
  struct list_head *prev, *next;
};

struct irep {
  struct list_head list;
  int refc;
  uchar argc, localc, varg;
  uint32_t *code;
  struct irep **irep;
  pic_value *pool;
  size_t ncode, nirep, npool;
};

void pic_irep_incref(pic_state *, struct irep *);
void pic_irep_decref(pic_state *, struct irep *);

#if defined(__cplusplus)
}
#endif

#endif
