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
  size_t ai;

  /* vm */
  const code_t *pc;
  struct frame *sp;
  struct frame *fp;
  struct irep *irep;

  code_t tmpcode[2];
  pic_value conts;
  bool reset;

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

#define MKCALL(cxt,argc)                                                \
  ((argc) < 256                                                         \
   ? ((cxt)->tmpcode[0] = OP_CALL, (cxt)->tmpcode[1] = (argc), (cxt)->tmpcode) \
   : (pic_error(pic, "too many arguments", 1, pic_int_value(pic, (argc))), NULL))

#define CONTEXT_VINITK(pic,cxt,proc,k,n,ap) do {        \
    int i;                                              \
    (cxt)->pc = MKCALL((cxt), (n) + 1);                 \
    (cxt)->sp = pic_make_frame_unsafe(pic, (n) + 3);    \
    (cxt)->sp->regs[0] = (proc);                        \
    (cxt)->sp->regs[1] = k;                             \
    for (i = 0; i < (n); ++i) {                         \
      (cxt)->sp->regs[i + 2] = va_arg(ap, pic_value);   \
    }                                                   \
    (cxt)->fp = NULL;                                   \
    (cxt)->irep = NULL;                                 \
  } while (0)

#define CONTEXT_INITK(pic,cxt,proc,k,n,argv) do {       \
    int i;                                              \
    (cxt)->pc = MKCALL((cxt), (n) + 1);                 \
    (cxt)->sp = pic_make_frame_unsafe(pic, (n) + 3);    \
    (cxt)->sp->regs[0] = (proc);                        \
    (cxt)->sp->regs[1] = k;                             \
    for (i = 0; i < (n); ++i) {                         \
      (cxt)->sp->regs[i + 2] = (argv)[i];               \
    }                                                   \
    (cxt)->fp = NULL;                                   \
    (cxt)->irep = NULL;                                 \
  } while (0)

#define CONTEXT_VINIT(pic,cxt,proc,n,ap) do {           \
    int i;                                              \
    (cxt)->pc = MKCALL((cxt), (n));                     \
    (cxt)->sp = pic_make_frame_unsafe(pic, (n) + 2);    \
    (cxt)->sp->regs[0] = (proc);                        \
    for (i = 0; i < (n); ++i) {                         \
      (cxt)->sp->regs[i + 1] = va_arg(ap, pic_value);   \
    }                                                   \
    (cxt)->fp = NULL;                                   \
    (cxt)->irep = NULL;                                 \
  } while (0)

#define CONTEXT_INIT(pic,cxt,proc,n,argv) do {          \
    int i;                                              \
    (cxt)->pc = MKCALL((cxt), (n));                     \
    (cxt)->sp = pic_make_frame_unsafe(pic, (n) + 2);    \
    (cxt)->sp->regs[0] = (proc);                        \
    for (i = 0; i < (n); ++i) {                         \
      (cxt)->sp->regs[i + 1] = (argv)[i];               \
    }                                                   \
    (cxt)->fp = NULL;                                   \
    (cxt)->irep = NULL;                                 \
  } while (0)

void pic_vm(pic_state *pic, struct context *cxt);

#if defined(__cplusplus)
}
#endif

#endif
