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
  union {
    struct {
      pic_code *code;
      int *ints;
      double *nums;
      union irep_node {
        struct pic_irep *i;
      } *irep;
    } s;
  } u;
  struct pic_object **pool;     /* pool of heap objects */
  size_t ncode, nirep, nints, nnums, npool;
};

void pic_irep_incref(pic_state *, struct pic_irep *);
void pic_irep_decref(pic_state *, struct pic_irep *);

#if defined(__cplusplus)
}
#endif

#endif
