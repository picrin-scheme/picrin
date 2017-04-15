/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STATE_H
#define PICRIN_STATE_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "khash.h"
#include "object.h"

KHASH_DECLARE(oblist, struct string *, struct symbol *)

struct context {
  PIC_JMPBUF jmp;

  /* vm */
  const code_t *pc;
  struct frame *sp;
  struct frame *fp;
  struct irep *irep;

  code_t tmpcode[2];

  struct context *prev;
};

struct pic_state {
  pic_allocf allocf;
  void *userdata;

  struct context *cxt, default_cxt;

  size_t ai;
  pic_value dyn_env;

  pic_value features;           /* list of symbols */
  khash_t(oblist) oblist;       /* string to symbol */
  pic_value globals;            /* dict */

  bool gc_enable;
  struct heap *heap;
  struct object **arena;
  size_t arena_size;

  pic_value halt;               /* top continuation */

  pic_panicf panicf;
};

struct heap *pic_heap_open(pic_state *);
void pic_heap_close(pic_state *, struct heap *);

pic_value pic_global_ref(pic_state *pic, pic_value uid);
void pic_global_set(pic_state *pic, pic_value uid, pic_value value);

void pic_vm_tear_off(pic_state *pic);

#if defined(__cplusplus)
}
#endif

#endif
