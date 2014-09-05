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

enum pic_reader_type {
  PIC_READER_C,
  PIC_READER_PROC,
};

typedef pic_value pic_reader_func_t(pic_state *, struct pic_port *, const char *);

struct pic_trie {
  struct pic_trie *table[256];
  enum pic_reader_type type;
  union {
    pic_reader_func_t *func;
    struct pic_proc *proc;
  } u;
};

struct pic_reader {
  short typecase;
  xhash labels;
  struct pic_trie *trie;
};

void pic_init_reader(pic_state *);

void pic_define_reader_c(pic_state *, const char *, pic_reader_func_t);
void pic_define_reader(pic_state *, const char *, struct pic_proc *);

struct pic_trie *pic_trie_new(pic_state *);
void pic_trie_delete(pic_state *, struct pic_trie *);

#if defined(__cplusplus)
}
#endif

#endif
