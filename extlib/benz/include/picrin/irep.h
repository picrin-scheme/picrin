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
      int code_offset;
      int ints_offset;
      int nums_offset;
      int irep_offset;
    } p;
    struct {
      pic_code *code;
      int *ints;
      double *nums;
      union irep_node {
        int offset;
        struct pic_irep *i;
      } *irep;
    } s;
  } u;
  pic_value *pool;              /* pool of heap objects */
  size_t ncode, nirep, nints, nnums, npool;
};

void pic_irep_incref(pic_state *, struct pic_irep *);
void pic_irep_decref(pic_state *, struct pic_irep *);

pic_value pic_expand(pic_state *, pic_value, struct pic_env *);
pic_value pic_analyze(pic_state *, pic_value);
struct pic_irep *pic_codegen(pic_state *, pic_value);
struct pic_proc *pic_compile(pic_state *, pic_value, struct pic_env *);

#if defined(__cplusplus)
}
#endif

#endif
