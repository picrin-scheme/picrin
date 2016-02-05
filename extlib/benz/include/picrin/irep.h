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
  union {
    int i;
    struct {
      int depth;
      int idx;
    } r;
  } u;
} pic_code;

struct pic_list {
  struct pic_list *prev, *next;
};

struct pic_irep {
  struct pic_list list;
  int refc;
  pic_code *code;
  int argc, localc, capturec;
  bool varg;
  struct pic_irep **irep;
  pic_value *pool;
  size_t plen;
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
