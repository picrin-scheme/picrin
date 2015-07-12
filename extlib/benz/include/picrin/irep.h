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
    char c;
    struct {
      int depth;
      int idx;
    } r;
  } u;
} pic_code;

struct pic_irep {
  PIC_OBJECT_HEADER
  pic_code *code;
  int argc, localc, capturec;
  bool varg;
  struct pic_irep **irep;
  pic_value *pool;
  size_t clen, ilen, plen, slen;
};

pic_sym *pic_resolve(pic_state *, pic_value, struct pic_env *);
pic_value pic_expand(pic_state *, pic_value, struct pic_env *);
pic_value pic_analyze(pic_state *, pic_value);
struct pic_irep *pic_codegen(pic_state *, pic_value);
struct pic_proc *pic_compile(pic_state *, pic_value, struct pic_env *);

#if defined(__cplusplus)
}
#endif

#endif
