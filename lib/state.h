/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STATE_H
#define PICRIN_STATE_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "khash.h"
#include "vm.h"

struct lib {
  struct string *name;
  struct env *env;
  struct dict *exports;
};

struct callinfo {
  int argc, retc;
  const struct code *ip;
  pic_value *fp;
  struct irep *irep;
  struct context *cxt;
  int regc;
  pic_value *regs;
  struct context *up;
};

KHASH_DECLARE(oblist, struct string *, struct identifier *)
KHASH_DECLARE(ltable, const char *, struct lib)

struct pic_state {
  pic_allocf allocf;
  void *userdata;

  struct checkpoint *cp;
  struct cont *cc;

  pic_value *sp;
  pic_value *stbase, *stend;

  struct callinfo *ci;
  struct callinfo *cibase, *ciend;

  const struct code *ip;

  const char *lib;

  pic_value features;

  khash_t(oblist) oblist;       /* string to symbol */
  int ucnt;
  pic_value globals;            /* weak */
  pic_value macros;             /* weak */
  khash_t(ltable) ltable;
  struct list_head ireps;

  bool gc_enable;
  struct heap *heap;
  struct object **arena;
  size_t arena_size, arena_idx;

  pic_value err;

  pic_panicf panicf;
};

struct heap *pic_heap_open(pic_state *);
void pic_heap_close(pic_state *, struct heap *);

#if defined(__cplusplus)
}
#endif

#endif
