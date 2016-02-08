/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_IREP_H
#define PICRIN_IREP_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct {
  int insn;
  int a;
  int b;
} pic_code;

struct pic_list {
  struct pic_list *prev, *next;
};

struct pic_irep {
  struct pic_list list;
  unsigned refc;
  int argc, localc, capturec;
  bool varg;
  pic_code *code;
  struct pic_irep **irep;
  int *ints;
  double *nums;
  struct pic_object **pool;
  size_t ncode, nirep, nints, nnums, npool;
};

void pic_irep_incref(pic_state *, struct pic_irep *);
void pic_irep_decref(pic_state *, struct pic_irep *);

#if defined(__cplusplus)
}
#endif

#endif
