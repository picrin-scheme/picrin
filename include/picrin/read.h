/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_READ_H__
#define PICRIN_READ_H__

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_typecase {
  PIC_CASE_DEFAULT,
  PIC_CASE_FOLD,
};

struct pic_trie {
  struct pic_trie *table[256];
  struct pic_proc *proc;
};

struct pic_reader {
  short typecase;
  xhash labels;
  struct pic_trie *trie;
};

void pic_init_reader(pic_state *);

void pic_define_reader(pic_state *, const char *, pic_func_t);

struct pic_trie *pic_trie_new(pic_state *);
void pic_trie_delete(pic_state *, struct pic_trie *);

#if defined(__cplusplus)
}
#endif

#endif
